import           Data.Function (on)

import           Data.LabeledTree
import           Logic
import           Parse


main :: IO ()
main = parseM >>= play

play :: Problem -> IO ()
play p = do
    let (rs, s) = mkStorage p
    let t = unfoldTree (\x -> (x, if stime x > 0 then step rs x else [])) s
    print p
    print (maximumPathBy (compare `on` getScore) t)


maximumPathBy :: (a -> a -> Ordering) -> Tree k a -> (a, [k])
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
