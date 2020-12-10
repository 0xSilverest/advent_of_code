import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)

type Bag = String
type BagRule = (Bag, [(Bag, Int)])

parseLine :: Parser BagRule
parseLine = do
    tone <- many1 letter
    space
    color <- many1 letter
    let parent = unwords [tone, color]
    space
    string "bags"
    space
    string "contain"
    space
    children <- try $ many1 noMoreBags <|> many1 containedBag
    return (parent, children)

containedBag :: Parser (Bag, Int)
containedBag = do
    d <- digit
    space
    tone <- many1 letter
    space
    color <- many1 letter
    space
    optional $ try $ string "bags"
    optional $ try $ string "bag"
    try (char ',' <|> char '.')
    optional $ try space
    return (unwords [tone, color], digitToInt d)

noMoreBags :: Parser (Bag, Int)
noMoreBags = do
    s <- string "no other bags."
    return (s, 0)

main :: IO ()
main = do
    contents <- lines <$> readFile "input/day7.txt"
    let Right rules = traverse (parse parseLine "input.txt") contents
    let rulesMap = M.fromList rules
    print $ length (filter (containsGold rulesMap) (M.keys rulesMap)) --PART 1
    print $ contains rulesMap "shiny gold" --PART 2

containsGold :: M.Map Bag [(Bag, Int)] -> Bag -> Bool
containsGold rules bag = ("shiny gold" `elem` map fst (M.findWithDefault [] bag rules)) || any (containsGold rules . fst) (M.findWithDefault [] bag rules)

contains :: M.Map Bag [(Bag, Int)] -> Bag -> Int
contains rules bag = sum (map snd $ M.findWithDefault [] bag rules) + sum (zipWith (*) (map snd $ M.findWithDefault [] bag rules) (map (contains rules . fst) $ M.findWithDefault [] bag rules))
