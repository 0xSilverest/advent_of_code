{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Text.ParserCombinators.ReadP as R
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.ParserCombinators.ReadP
import Data.Char

data Rule = Char Char 
          | Seq [Int] 
          | Or [Int] [Int]
        deriving (Show, Eq)

countPred :: (a -> Bool) -> [a] -> Int
countPred p = length . filter p

count :: Eq a => a -> [a] -> Int
count = countPred . (==)

nums :: R.ReadP Int
nums = read <$> R.munch1 isDigit

spaced :: ReadP [Int]
spaced = nums `sepBy1` char ' '

parseChar, parseSeq, parseOr :: ReadP Rule
parseChar = Char <$> between (char '"') (char '"') get
parseSeq = Seq <$> spaced
parseOr = Or <$> spaced <*  string " | " <*> spaced

newtype RuleDecl = Decl { getDecl :: (Int, Rule) }

parseDecl :: ReadP (Int, Rule)
parseDecl = (,) <$> nums <*  string ": " <*> (parseChar +++ parseSeq +++ parseOr)

instance Read RuleDecl where
    readsPrec _ = readP_to_S (Decl <$> parseDecl)

parseRules :: M.Map Int Rule -> Int -> ReadP String
parseRules declMap idx = case declMap M.! idx of
    Char c -> string [c]
    Seq is -> allParsers is
    Or is js -> allParsers is +++ allParsers js
    where allParsers [] = return []
          allParsers (i:is) = liftA2 (++) (parseRules declMap i) (allParsers is)

main :: IO ()
main = do
    input <- TIO.readFile "input/day19.txt"
    let [rulesStr, messages] = map T.unpack . T.splitOn "\n\n" $ input
        map1 = M.unions . map (uncurry M.singleton . getDecl . read) . lines $ rulesStr
        map2 = M.fromList [(8, Or [42] [42, 8]), (11, Or [42, 31] [42, 11, 31])] <> map1
        root  = parseRules map1 0
        root' = parseRules map2 0
        isValid p s = not $ null [x | (x, "") <- readP_to_S p s]
    print . countPred (isValid root ) . lines $ messages
    print . countPred (isValid root') . lines $ messages
