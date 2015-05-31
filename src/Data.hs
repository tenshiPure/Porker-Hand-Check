module Data(Hand, Card(..), getHand, switchAce, getNums, getSuits ) where

import Data.List
import Data.List.Split

type Hand = [Card]
data Card = Card { num :: Int, suit :: Char } deriving (Show)

toNum str
    | str == "J" = 11
    | str == "Q" = 12
    | str == "K" = 13
    | str == "A" = 14
    | otherwise  = read str

mkCard str = Card { num = toNum $ init str, suit = last str }

getHand test = map mkCard $ splitOn "," test

switchAce nums = sort $ map (\num -> if num == 14 then 1 else num) nums

getNums hand = sort $ map num hand

getSuits = map suit
