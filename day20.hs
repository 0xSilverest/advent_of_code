import Data.List (foldl')
import Text.Regex.Applicative
import Data.Foldable (asum)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

data Tile = Tile Int [String]

testChar :: Char -> Int
testChar c = case c of 
               '#' -> 1
               _   -> 0

toInd :: String -> Int
toInd s = max (toInt s) (toInt $ reverse s)
    where toInt = foldl' f 0 . map testChar
          f acc x = 2 * acc + x

sides :: [String] -> [String]
sides = sequence [head, last, map head, map last]

ints :: [String] -> [Int]
ints strs = toInd <$> sides strs

freqs :: [Int] -> IntMap Int
freqs = M.fromListWith (+) . flip  zip (repeat 1)

testTile :: [Tile] -> IntMap Int
testTile = freqs . M.elems . freqs . (>>= toEdge)
      
toEdge :: Tile -> [Int]
toEdge (Tile _ m) = map toInd $ sides m

parseInp :: String -> [Tile]
parseInp = fromJust . (=~ parser)
        where parser = many tile
              tile = Tile <$> id <*> many line <* sym '\n'
              id = string "Tile " *> int <* string ":\n"
              line = many (asum (map sym ".#")) <* sym '\n'
              int = read <$> many (psym isDigit)

isCorner :: IntMap Int -> [String] -> Bool
isCorner fs m = let es = map toInd . sides $ m
                in length (filter ((== 1). (fs M.!)) es) == 2 

part1 :: [Tile] -> Int
part1 ps = let fs = freqs . (>>= toEdge) $ ps
           in product [id | Tile id m <- ps, isCorner fs m]

main :: IO()
main = readFile "input/day20.txt" >>= print . part1 . parseInp
