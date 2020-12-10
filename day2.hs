import System.IO

signs = "-:" 

type RulePw = (Int, Int, Char, String)    

-- Part 1
occ :: Char -> String -> Int
occ c str = length (filter (==c) str)

isValid :: RulePw -> Bool
isValid (min, max, c, str) = (x >= min) && (x <= max)
                        where x = occ c str 

-- Part 2 
isOnPlace :: RulePw -> Bool
isOnPlace (first, second, c, str) = str !! (first - 1) == c && str !! (second - 1) /= c 
                                  || str !! (first - 1) /= c && str !! (second - 1) == c 
convSign :: String -> String
convSign []      = []
convSign (c:cs) | elem c signs = ' ' : convSign cs
                | otherwise    = c : convSign cs

convType :: [String] -> RulePw
convType [min, max, c, pw] = (read min, read max, c !! 0, pw)

lineSep :: String -> [String]
lineSep str = lines str

readThat :: IO [String]
readThat = do file <- openFile "input/day2.txt" ReadMode
              content <- hGetContents file
              return $ lineSep content

main :: IO ()
main = do fileInput <- readThat
          --let inputParse = map read (fileInput) :: [(Int, Int, Char, String)]
          let parsedInp = map (convType . words . convSign) fileInput
          putStr $ show (length (filter isOnPlace parsedInp))
