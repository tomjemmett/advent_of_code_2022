module Day21
  ( day21,
  )
where

import Common
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as H
import Text.Parsec ((<|>))
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Input = HashMap String Monkey

type Monkey = Either (String, Char, String) Int

day21 :: AOCSolution
day21 input = show <$> ([part1, part2] <*> pure (parseInput input))

parseInput :: String -> H.HashMap String Monkey
parseInput = H.fromList . parse' (p `P.sepEndBy` P.newline) id
  where
    p :: Parser (String, Monkey)
    p = do
      name <- P.letter `P.manyTill` P.string ": "
      monkey <- P.choice [pOp, pNum]
      pure (name, monkey)
    pOp :: Parser Monkey
    pOp = do
      l <- P.many P.letter <* P.char ' '
      o <- P.anyChar
      r <- P.char ' ' *> P.many P.letter
      pure $ Left (l, o, r)
    pNum :: Parser Monkey
    pNum = Right <$> number

getOp :: Char -> Int -> Int -> Int
getOp = \case
  '+' -> (+)
  '-' -> (-)
  '*' -> (*)
  '/' -> div
  _ -> undefined

part1 :: Input -> Int
part1 i = go "root"
  where
    go :: String -> Int
    go n = case i ! n of
      Right v -> v
      Left (go -> l, getOp -> o, go -> r) -> l `o` r

part2 :: Input -> Int
part2 i = case (go rl, go rr) of
  (Left f, Right n) -> f n
  (Right n, Left f) -> f n
  _ -> undefined
  where
    Left (rl, _, rr) = i ! "root"
    go "humn" = Left id
    go n = case i ! n of
      Right v -> Right v
      Left v -> g v
    g (l, o, r) = case (lv, o, rv) of
      (Left f, '+', Right n) -> Left $ f . subtract n
      (Left f, '-', Right n) -> Left $ f . (+ n)
      (Left f, '*', Right n) -> Left $ f . (`div` n)
      (Left f, '/', Right n) -> Left $ f . (* n)
      (Right n, '+', Left f) -> Left $ f . subtract n
      (Right n, '-', Left f) -> Left $ f . (n -)
      (Right n, '*', Left f) -> Left $ f . (`div` n)
      (Right n, '/', Left f) -> Left $ f . div n
      (Right a, _, Right b) -> Right $ getOp o a b
      _ -> undefined
      where
        lv = go l
        rv = go r
