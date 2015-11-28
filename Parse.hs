module Parse (
    parseM, readProblem
) where

    import Control.Monad (liftM2)
    import Data.List (group, sort)

    import Logic

    readProblem :: FilePath -> IO Problem
    readProblem = fmap parseS . readFile

    parseM :: IO Problem
    parseM = fmap parseS getContents

    parseS :: String -> Problem
    parseS = (\[w,x,y,z] -> parse (read w) (read x) (read y) (read z)) . lines

    parse :: [Value] -> [Amount] -> [([Index], [Index], Time)] -> Time -> Problem
    parse vs as rs = Problem (zipWith Element vs as) (map transform rs)

    transform :: ([Index], [Index], Time) -> Rule
    transform (i, o, t) = Rule (collect i) (collect o) t

    collect :: [Index] -> [Prod]
    collect = liftM2 zip id (map length . group . sort)
