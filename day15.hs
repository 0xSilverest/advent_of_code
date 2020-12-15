import Control.Arrow ((&&&))
import Data.IntMap hiding (foldl') 
import qualified Data.IntMap.Strict as M
import Data.List

data PassedMap = PM {sMap :: IntMap Int, i :: Int, prev :: Int}

initPassed (x : xs) = foldl' f (PM mempty 0 x) xs
                      where f (PM map i prev) x = PM (M.insert prev i map) (i + 1) x

step :: PassedMap -> PassedMap
step (PM sm i p) = PM (M.insert p i sm) (i + 1) check
                   where
                      check = case sm M.!? p of
                                Nothing -> 0
                                Just n  -> i - n

iter 0 x = x
iter n x = iter (n - 1) $ step x

part1 inp = prev (iter (2020 - length inp) (initPassed inp))
part2 inp = prev (iter (30000000 - length inp) (initPassed inp))

main :: IO ()
main = do 
    let inp = [9, 19, 1, 6, 0, 5, 4] 
    print $ (part1 &&& part2) inp
