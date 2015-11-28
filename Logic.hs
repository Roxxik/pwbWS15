{-# LANGUAGE TemplateHaskell #-}
module Logic (
    Problem(..), Storage(..), Element(..), Rule(..), Prod, Index, Amount, Time, Value,
    step, getScore, mkStorage
) where

import           Control.Lens  (makeLenses, over)
import           Control.Monad
import           Data.Maybe

data Problem = Problem { elems :: [Element]
                       , rules :: [Rule]
                       , ptime :: Time
                       }
    deriving (Show)

data Storage = Storage { sto   :: [Element]
                       , stime :: Time
                       }
    deriving (Show)

data Element = Element { value  :: Value
                       , _amount :: Amount
                       }
    deriving (Show)

data Rule = Rule { inp   :: [Prod]
                 , outp  :: [Prod]
                 , rtime :: Time
                 }
    deriving (Show)

type Prod = (Index, Amount)
type Index = Int
type Amount = Int
type Time = Int
type Value = Int


makeLenses ''Element

mkStorage :: Problem -> ([Rule], Storage)
mkStorage p = (rules p, Storage (elems p) (ptime p))

getScore :: Storage -> Int
getScore = sum . map getElemScore . sto

getElemScore :: Element -> Int
getElemScore = liftM2 (*) _amount value

step :: [Rule] -> Storage -> [(Index, Storage)]
step rs s = zip [0..] $ map (apply s) rs

apply :: Storage -> Rule -> Storage
apply s r = Storage elements' time'
    where
        input = inp r
        output = outp r
        elements = sto s
        melems = takeRes elements input
        melems' = fmap (produce enoughTime output) melems
        elements' = fromMaybe elements melems'
        enoughTime = isJust mtime
        mtime = takeTime (stime s) (rtime r)
        time' = fromMaybe 0 mtime

produce :: Bool -> [Prod] -> [Element] -> [Element]
produce enoughTime output = if enoughTime then flip deliver output else id

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
