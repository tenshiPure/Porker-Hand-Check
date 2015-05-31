import Data
import Checker

main :: IO ()
main = do
    let tests = [
                    "10c,Jc,Qc,Kc,Ac",
                    "Jc,10c,9c,8c,7c",
                    "4c,4d,4h,4s,Ac",
                    "5s,5h,5d,8s,8h",
                    "As,Js,10s,6s,3s",
                    "10s,Jd,Qs,Kh,Ac",
                    "As,2d,3s,4h,5c",
                    "4s,2d,5s,Ah,3c",
                    "3c,3d,3h,Qs,2h",
                    "Kc,Ks,9h,9d,Jh",
                    "2d,2h,Qh,7h,6c",
                    "Ah,Kh,Qs,10h,2c"
                ]

    let hands = map getHand tests

    putStrLn $ unlines $ map (getHandName checkers) hands

    {-
        $ ./porker

        Royal Straight Flush
        Straight Flush
        Four Of A Kind
        Three Of A Kind
        Flush
        Straight
        Straight
        Straight
        Three Of A Kind
        Two Pair
        One Pair
        Garbage
    -}
