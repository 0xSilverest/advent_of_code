import qualified Data.Set as S
import Data.List.Split
import Data.List

getUnion :: [[S.Set Char]] -> Int
getUnion = sum . map (length . foldl1' S.union)

getInter :: [[S.Set Char]] -> Int
getInter = sum . map (length . foldl1' S.intersection)

parseInp :: String -> [[S.Set Char]]
parseInp = map (map S.fromList . lines) . splitOn "\n\n"

main :: IO ()
main = readFile "input/day6.txt" >>= print . getInter . parseInp
