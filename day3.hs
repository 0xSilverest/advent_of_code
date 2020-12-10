import Control.Arrow
import Data.Set (Set)
import qualified Data.Set as S

type Slope = (Int, Int)

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

foundTree :: Int -> Int -> Set Int -> Bool
foundTree w x = S.member (x `mod ` w)

getNum :: (Int, [Set Int]) -> Int
getNum = (`treeCount` (3, 1))

treeCount :: (Int, [Set Int]) -> Slope -> Int
treeCount (w, tMap) (r, d) = length . filter (uncurry (foundTree w)) . zip [0, r..] . takeThat d $ tMap
                             where takeThat _ [] = []
                                   takeThat n xs@(x : _) = x : takeThat n (drop n xs)

randomSlopes :: (Int, [Set Int]) -> Int
randomSlopes i = product . map (treeCount i) $ slopes

parseInput :: String -> (Int, [Set Int])
parseInput = (width &&& map readTrees) . lines
    where 
        readTrees = S.fromList . map fst . filter ((== '#') . snd) . zip [0..] 
        width = length . head

main :: IO ()
main = readFile "input/day3.txt" >>= print . randomSlopes . parseInput
