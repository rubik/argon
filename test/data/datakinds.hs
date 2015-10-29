{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

data JobDescription = JobOne
                    | JobTwo
                    | JobThree
  deriving (Show, Eq)

data SJobDescription :: JobDescription -> * where
    SJobOne :: { jobOneN :: Int } -> SJobDescription JobOne
    SJobTwo :: SJobDescription JobTwo
    SJobThree :: { jobThreeN :: Int } -> SJobDescription JobThree

taskOneWorker :: SJobDescription JobOne -> IO ()
taskOneWorker t = do
    putStrLn $ "Job: " ++ (show $ jobOneN t)

main :: IO ()
main = taskOneWorker (SJobOne 10)
