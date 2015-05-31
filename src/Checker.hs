module Checker(
getHandName,
checkers
) where


import Data.List

import Data


-- (Straight Flush) and (continuation is start at 10)
isRoyalStraightFlush :: Hand -> Bool
isRoyalStraightFlush hand = isStraightFlush hand && head (getNums hand) == 10


-- (Flush) and (Straight)
isStraightFlush :: Hand -> Bool
isStraightFlush hand = isFlush hand && isStraight hand


-- [s, s, d, d, c] -> [s, d, c] -> 3
-- [s, s, s, s, s] -> [s] -> 1
isFlush :: Hand -> Bool
isFlush hand = (1 ==) $ length $ nub $ getSuits hand


-- (check continuation [2, 3, 4, 5, 14]) || (check continuation [1, 2, 3, 4, 5])
isStraight :: Hand -> Bool
isStraight hand = (isContinuation nums) || (isContinuation $ switchAce nums)
    where nums = sort $ getNums hand


isContinuation :: [Int] -> Bool
isContinuation (x:y:zs) | x + 1 == y = isContinuation (y:zs)
isContinuation (x:y:zs) | x + 1 /= y = False
isContinuation [x]                   = True


-- group [1, 1, 2, 2, 3] -> map length [[1, 1], [2, 2], 3] -> maximum [2, 2, 1] -> 2
kinds :: Hand -> Int
kinds hand = maximum $ map length $ group $ getNums hand


-- group [1, 1, 2, 2, 3] -> filter length == 2 [[1, 1], [2, 2], 3] -> length [[1, 1], [2, 2]] -> 2
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


-- find first hit or Garbage
getHandName :: [(String, Hand -> Bool)] -> Hand -> String
getHandName ((label, checker):xs) hand | checker hand == True = label
getHandName ((label, checker):xs) hand | checker hand /= True = getHandName xs hand
getHandName [] _ = "Garbage"
