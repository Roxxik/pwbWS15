{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

import           Control.Lens     (makeLenses, over)
import           Control.Monad
import           Data.Function
import           Data.LabeledTree
import           Data.List
import           Data.Maybe

data Problem = Problem { _elems :: [Element]
                       , _rules :: [Rule]
                       , _ptime :: Time
                       }
    deriving (Show)

data Storage = Storage { _sto   :: [Element]
                       , _stime :: Time
                       }
    deriving (Show)

data Element = Element { _value  :: Value
                       , _amount :: Amount
                       }
    deriving (Show)

data Rule = Rule { _inp   :: [Prod]
                 , _outp  :: [Prod]
                 , _rtime :: Time
                 }
    deriving (Show)

type Prod = (Index, Amount)
type Index = Int
type Amount = Int
type Time = Int
type Value = Int


makeLenses ''Element
makeLenses ''Storage

main :: IO ()
main = parseM >>= play

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

play :: Problem -> IO ()
play p = do
    let (rs, s) = mkStorage p
    let t = unfoldTree (\x -> (x, if _stime x > 0 then step rs x else [])) s
    print p
    print (maximumPathBy (compare `on` getScore) t)


maximumPathBy :: (a -> a -> Ordering) -> Tree k a -> (a,[k])
maximumPathBy cmp = reduceTree f g z
    where
        f root = h cmp (root, [])
        g k (a, ks) = Just . h cmp (a, k:ks)
        z = Nothing

        h :: (a -> a -> Ordering) -> (a, b) -> Maybe (a, b) -> (a, b)
        h cmp' = maybeF (maxBy (cmp' `on` fst))

        maybeF :: (a -> b -> b) ->  b -> Maybe a -> b
        maybeF = flip . maybe id

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y = case cmp x y of
    GT -> x
    _  -> y

mkStorage :: Problem -> ([Rule], Storage)
mkStorage p = (_rules p, Storage (_elems p) (_ptime p))

getScore :: Storage -> Int
getScore = sum . map getElemScore . _sto

getElemScore :: Element -> Int
getElemScore = liftM2 (*) _amount _value

step :: [Rule] -> Storage -> [(Index, Storage)]
step rs s = zip [0..] $ map (apply s) rs

apply :: Storage -> Rule -> Storage
apply s r = Storage elems' time'
    where
        inp = _inp r
        outp = _outp r
        elems = _sto s
        melems = takeRes elems inp
        melems' = fmap (produce enoughTime outp) melems
        elems' = fromMaybe elems melems'
        enoughTime = isJust mtime
        mtime = takeTime (_stime s) (_rtime r)
        time' = fromMaybe 0 mtime

produce :: Bool -> [Prod] -> [Element] -> [Element]
produce enoughTime outp = if enoughTime then flip deliver outp else id

deliver :: [Element] -> [Prod] -> [Element]
deliver = foldl put

put :: [Element] -> (Index, Amount) -> [Element]
put es (i, a) = adjust (over amount (+ a)) i es

takeRes :: [Element] -> [Prod] -> Maybe [Element]
takeRes = foldM get

get :: [Element] -> Prod -> Maybe [Element]
get es (i, a) | _amount (es !! i) >= a = Just $ adjust (over amount (subtract a)) i es
              | otherwise              = Nothing

takeTime :: Time -> Time -> Maybe Time
takeTime = natSubtract

natSubtract :: (Num a, Ord a) => a -> a -> Maybe a
natSubtract a b = if a - b >= 0 then Just (a - b) else Nothing

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i = foldr g [] . zip [0..]
    where
        g (i', x) xs = (if i == i' then f x else x) : xs
