module Data(
Hand,
Card(..),
getHand,
switchAce,
getNums,
getSuits
) where


import Data.List
import Data.List.Split


type Hand = [Card]
data Card = Card { num :: Int, suit :: Char } deriving (Show)


toNum :: String -> Int
toNum str
    | str == "J" = 11
    | str == "Q" = 12
    | str == "K" = 13
    | str == "A" = 14
    | otherwise  = read str


mkCard :: String -> Card
mkCard str = Card { num = toNum $ init str, suit = last str }


getHand :: String -> Hand
getHand test = map mkCard $ splitOn "," test


-- [2, 3, 4, 5, 14] -> [1, 2, 3, 4, 5]
switchAce :: [Int] -> [Int]
switchAce nums = sort $ map (\num -> if num == 14 then 1 else num) nums


getNums :: Hand -> [Int]
getNums = map num


getSuits :: Hand -> [Char]
getSuits = map suit
