module Checker( getHandName, checkers ) where

import Data.List
import Data

isRoyalStraightFlush hand = isStraightFlush hand && head (getNums hand) == 10

isStraightFlush hand = isFlush hand && isStraight hand

isFlush hand = (1 ==) $ length $ nub $ getSuits hand

isStraight hand = isContinuation (getNums hand) || isContinuation (switchAce $ getNums hand)

isContinuation (x:y:zs) | x + 1 == y = isContinuation (y:zs)
isContinuation (x:y:zs) | x + 1 /= y = False
isContinuation [x]                   = True

kinds hand = maximum $ map length $ group $ getNums hand

pairs hand = length $ filter (\l -> length l == 2) $ group $ getNums hand

checkers = [
            ("Royal Straight Flush", isRoyalStraightFlush),
            ("Straight Flush",       isStraightFlush),
            ("Four Of A Kind",       \hand -> kinds hand == 4),
            ("Flush",                isFlush),
            ("Straight",             isStraight),
            ("Three Of A Kind",      \hand -> kinds hand == 3),
            ("Two Pair",             \hand -> pairs hand == 2),
            ("One Pair",             \hand -> pairs hand == 1)
           ]

getHandName :: [(String, Hand -> Bool)] -> Hand -> String
getHandName ((label, checker):xs) hand |      checker hand  = label
getHandName ((label, checker):xs) hand | not (checker hand) = getHandName xs hand
getHandName [] _ = "Garbage"
