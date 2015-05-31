module Checker(
getHandName,
checkers
) where


import Data.List

import Data


isRoyalStraightFlush :: Hand -> Bool
isRoyalStraightFlush hand = isStraightFlush hand && head (getNums hand) == 10

isStraightFlush :: Hand -> Bool
isStraightFlush hand = isFlush hand && isStraight hand


isFlush :: Hand -> Bool
isFlush hand = (1 ==) $ length $ nub $ getSuits hand


isStraight :: Hand -> Bool
isStraight hand = (isContinuation nums) || (isContinuation $ switchAce nums)
    where nums = sort $ getNums hand


isContinuation :: [Int] -> Bool
isContinuation (x:y:zs) | x + 1 == y = isContinuation (y:zs)
isContinuation (x:y:zs) | x + 1 /= y = False
isContinuation [x]                   = True


kinds :: Hand -> Int
kinds hand = maximum $ map length $ group $ getNums hand


pairs :: Hand -> Int
pairs hand = length $ filter (\l -> length l == 2) $ group $ getNums hand


checkers :: [(String, Hand -> Bool)]
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
getHandName ((label, checker):xs) hand | checker hand == True = label
getHandName ((label, checker):xs) hand | checker hand /= True = getHandName xs hand
getHandName [] _ = "Garbage"
