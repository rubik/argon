{-# LANGUAGE TemplateHaskell #-}
-- Real example taken from Stack's source code (Setup.hs)
ensureCompiler sopts = do
    let wc = whichCompiler (soptsWantedCompiler sopts)
    when (getGhcVersion (soptsWantedCompiler sopts) < $(mkVersion "7.8")) $ do
        $logWarn "stack will almost certainly fail with GHC below version 7.8"
        $logWarn "Valiantly attempting to run anyway, but I know this is doomed"
        $logWarn "For more information, see: https://github.com/commercialhaskell/stack/issues/648"
        $logWarn ""

    -- Check the available GHCs
    menv0 <- getMinimalEnvOverride

    msystem <-
        if soptsUseSystem sopts
            then getSystemCompiler menv0 wc
            else return Nothing

    Platform expectedArch _ <- asks getPlatform

    let needLocal = case msystem of
            Nothing -> True
            Just _ | soptsSkipGhcCheck sopts -> False
            Just (system, arch) ->
                not (isWanted system) ||
                arch /= expectedArch
        isWanted = isWantedCompiler (soptsCompilerCheck sopts) (soptsWantedCompiler sopts)

    -- If we need to install a GHC, try to do so
    mtools <- if needLocal
        then do
            getSetupInfo' <- runOnce (getSetupInfo (soptsStackSetupYaml sopts) =<< asks getHttpManager)

            localPrograms <- asks $ configLocalPrograms . getConfig
            installed <- listInstalled localPrograms

            -- Install GHC
            ghcVariant <- asks getGHCVariant
            config <- asks getConfig
            ghcPkgName <- parsePackageNameFromString ("ghc" ++ ghcVariantSuffix ghcVariant)
            let installedCompiler =
                    case wc of
                        Ghc -> getInstalledTool installed ghcPkgName (isWanted . GhcVersion)
                        Ghcjs -> getInstalledGhcjs installed isWanted
            compilerTool <- case installedCompiler of
                Just tool -> return tool
                Nothing
                    | soptsInstallIfMissing sopts -> do
                        si <- getSetupInfo'
                        downloadAndInstallCompiler
                            si
                            (soptsWantedCompiler sopts)
                            (soptsCompilerCheck sopts)
                            (soptsGHCBindistURL sopts)
                    | otherwise -> do
                        throwM $ CompilerVersionMismatch
                            msystem
                            (soptsWantedCompiler sopts, expectedArch)
                            ghcVariant
                            (soptsCompilerCheck sopts)
                            (soptsStackYaml sopts)
                            (fromMaybe
                                ("Try running \"stack setup\" to install the correct GHC into "
                                <> T.pack (toFilePath (configLocalPrograms config)))
                                $ soptsResolveMissingGHC sopts)

            -- Install msys2 on windows, if necessary
            platform <- asks getPlatform
            mmsys2Tool <- case platform of
                Platform _ Cabal.Windows | not (soptsSkipMsys sopts) ->
                    case getInstalledTool installed $(mkPackageName "msys2") (const True) of
                        Just tool -> return (Just tool)
                        Nothing
                            | soptsInstallIfMissing sopts -> do
                                si <- getSetupInfo'
                                osKey <- getOSKey platform
                                VersionedDownloadInfo version info <-
                                    case Map.lookup osKey $ siMsys2 si of
                                        Just x -> return x
                                        Nothing -> error $ "MSYS2 not found for " ++ T.unpack osKey
                                let tool = Tool (PackageIdentifier $(mkPackageName "msys2") version)
                                Just <$> downloadAndInstallTool (configLocalPrograms config) si info tool (installMsys2Windows osKey)
                            | otherwise -> do
                                $logWarn "Continuing despite missing tool: msys2"
                                return Nothing
                _ -> return Nothing

            return $ Just (compilerTool, mmsys2Tool)
        else return Nothing

    mpaths <- case mtools of
        Nothing -> return Nothing
        Just (compilerTool, mmsys2Tool) -> do
            let idents = catMaybes [Just compilerTool, mmsys2Tool]
            paths <- mapM extraDirs idents
            return $ Just $ mconcat paths

    menv <-
        case mpaths of
            Nothing -> return menv0
            Just ed -> do
                config <- asks getConfig
                let m = augmentPathMap (edBins ed) (unEnvOverride menv0)
                mkEnvOverride (configPlatform config) (removeHaskellEnvVars m)

    when (soptsUpgradeCabal sopts) $ do
        unless needLocal $ do
            $logWarn "Trying to upgrade Cabal library on a GHC not installed by stack."
            $logWarn "This may fail, caveat emptor!"
        upgradeCabal menv wc

    case mtools of
        Just (ToolGhcjs cv, _) -> ensureGhcjsBooted menv cv (soptsInstallIfMissing sopts)
        _ -> return ()

    when (soptsSanityCheck sopts) $ sanityCheck menv wc

    return mpaths
