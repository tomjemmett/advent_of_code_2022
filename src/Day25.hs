module Day25
  ( day25,
  )
where

import Common

day25 :: AOCSolution
day25 = (: []) . decimalToSnafu . sum . map (snafuToDecimal . reverse) . lines

snafuToDecimal :: String -> Int
snafuToDecimal [] = 0
snafuToDecimal (x : xs) =
  snafuToDecimal xs * 5 + case x of
    '2' -> 2
    '1' -> 1
    '0' -> 0
    '-' -> -1
    '=' -> -2
    _ -> undefined

decimalToSnafu :: Int -> String
decimalToSnafu 0 = ""
decimalToSnafu d = decimalToSnafu a ++ ["=-012" !! b]
  where
    (a, b) = (d + 2) `divMod` 5
