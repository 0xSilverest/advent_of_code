import Control.Arrow ((&&&))
import Text.ParserCombinators.Parsec 
import Data.Functor

token' :: Parser a -> Parser a
token' p = p <* spaces

paren :: Parser a -> Parser a
paren  p = string "(" *> p <* string ")"

nums :: Parser Int 
nums = read <$> many1 digit 

parseMul = symb "*" $> (*)

parsePlus = symb "+" $> (+)

symb :: String -> Parser String
symb = token' . string 

expr :: Parser Int
expr = chainl1 term (parsePlus <|> parseMul)

term :: Parser Int
term = token' (nums <|> paren expr)

expr' :: Parser Int
expr' = chainl1 (chainl1 term' parsePlus) parseMul 
        
term' :: Parser Int
term' = token' (nums <|> paren expr')

part1 inp = sum <$> traverse (parse expr "") inp

part2 inp = sum <$> traverse (parse expr' "") inp

main :: IO ()
main = readFile "input/day18.txt" >>= print . (part1 &&& part2) . lines
