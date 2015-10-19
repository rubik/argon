module Argon.Types (ComplexityResult, AnalysisResult)
    where


type ComplexityResult = (Int, Int, String, Int)
type AnalysisResult = Either String [ComplexityResult]
