import System.IO
import Data.List

findSum1 :: [Int] -> [Int]
findSum1 xs = nub [x * y | x <- xs, y <- xs, x + y == 2020]

findSum2 :: [Int] -> [Int]
findSum2 xs = nub [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

wordsSep :: String -> [String]
wordsSep = words

readThat :: IO [String]
readThat = do file <- openFile "input/day1.txt" ReadMode
              content <- hGetContents file
              return $ wordsSep content 

main :: IO ()
main = do 
    input <- readThat
    let nums = map read input :: [Int]
    putStr $ (show . findSum2) nums
