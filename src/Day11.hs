module Day11
-- (
--   day11
-- )
where

import Common
import Control.Monad.State
import Data.Bifunctor
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P

type MonkeyInstruction  = (Int -> Int, Int, Int, Int)
type MonkeyInstructions = M.Map Int MonkeyInstruction
type Items = M.Map Int [Int]

day11 :: AOCSolution
day11 input = go mq mi (repeat 0) <$> [((`div` 3), 20), ((`mod` p2), 10000)]
  where
    (mq, mi) = parseInput input
    p2 = product . map ((\(_, d, _, _) -> d) . snd) $ M.toList mi

parseInput :: String -> (Items, MonkeyInstructions)
parseInput input = (mq, mi)
  where
    i = parse' parseMonkies id input
    mq = M.fromList $ map (second fst) i
    mi = M.fromList $ map (second snd) i
    -- 
    parseMonkies :: Parser [(Int, ([Int], MonkeyInstruction))]
    parseMonkies = parseMonkey `P.sepEndBy` P.string "\n\n"
    -- 
    parseMonkey :: Parser (Int, ([Int], MonkeyInstruction))
    parseMonkey = do
      i <- P.string "Monkey " *> number <* P.char ':' <* P.newline
      items <- P.string "  Starting items: " *> number `P.sepBy` P.string ", " <* P.newline
      op <- P.string "  Operation: new = old " *> P.anyChar
      n <- P.char ' ' *> (pN1 P.<|> pN2) <* P.newline
      d <- P.string "  Test: divisible by " *> number <* P.newline
      tt <- P.string "    If true: throw to monkey " *> number <* P.newline
      tf <- P.string "    If false: throw to monkey " *> number
      -- return value
      pure (i, (items, (getOp n op, d, tt, tf)))
      where
        pN1 = P.try $ const <$> number
        pN2 = id <$ P.string "old"
        getOp :: (Int -> Int) -> Char -> (Int -> Int)
        getOp n = \case
          '*' -> \x -> n x * x
          '+' -> \x -> n x + x
          _   -> undefined

go :: Items -> MonkeyInstructions -> [Int] -> (Int -> Int, Int) -> String
go _ _ a (_, 0) = show $ getMax 0 0 a
  where
    getMax :: Int -> Int -> [Int] -> Int
    getMax a b [] = a * b
    getMax a b (x:xs)
      | x > a     = getMax x a xs
      | x > b     = getMax a x xs
      | otherwise = getMax a b xs
go mq mi a (wm, n) = go mq' mi a' (wm, pred n)
  where
    (b, mq') = runState (goRound wm mi) mq
    a'= zipWith (+) a b

goRound :: (Int -> Int) -> MonkeyInstructions -> State (M.Map Int [Int]) [Int]
goRound wm mi = traverse (monkeyRound wm mi) k
  where
    k = M.keys mi
    monkeyRound :: (Int -> Int) -> MonkeyInstructions -> Int -> State (M.Map Int [Int]) Int
    monkeyRound wm mi i = do
      s <- get
      let
        mq = s M.! i
        instrs = mi M.! i
      monkeyGo wm instrs mq
      modify $ M.insert i []
      pure $ length mq
    monkeyGo :: (Int -> Int) -> MonkeyInstruction -> [Int] -> State (M.Map Int [Int]) ()
    monkeyGo _ _ [] = pure ()
    monkeyGo wm instrs@(op, d, tt, tf) (q:qs) = do
      let
        worry = wm $ op q
        to = if worry `mod` d == 0 then tt else tf
      modify $ M.insertWith (flip (++)) to [worry]
      monkeyGo wm instrs qs
