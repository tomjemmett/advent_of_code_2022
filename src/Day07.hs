{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day07
  ( day07
  )
where

import Common
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

data CLI = Cd String | Ls | File (Int, String) | Dir String deriving (Show)

type DirPath = [String]

type DirStructure = M.Map DirPath ([DirPath], Int)

day07 :: AOCSolution
day07 input = show <$> ([part1, part2] <*> pure i)
  where
    i = go $ parseInput input

part1 :: [Int] -> Int
part1 = sum . filter (<= 100000)

part2 :: [Int] -> Int
part2 i = minimum d
  where
    f = 70000000 - head i
    n = 30000000 - f
    d = filter (>= n) i

go :: DirStructure -> [Int]
go = snd . flip f ["/"]
  where
    f :: DirStructure -> DirPath -> (Int, [Int])
    f m d =
      if null dirs
        then (size, [size])
        else (r, r : concatMap snd ds)
      where
        (dirs, size) = m M.! d
        ds = map (f m) dirs
        r = size + sum (map fst ds)

parseInput :: String -> DirStructure
parseInput = M.fromList . navigate [] . map (parse' pCLI id) . lines
  where
    navigate _ [] = []
    navigate cwd (x : xs) = case x of
      Cd ".." -> navigate (tail cwd) xs
      Cd d -> navigate (d : cwd) xs
      Ls -> lsDir cwd xs
    lsDir cwd x = (cwd, r) : navigate cwd xs
      where
        (r, xs) = getDirContents [] 0 x
        getDirContents dirs size [] = ((dirs, size), [])
        getDirContents dirs size (x : xs) = case x of
          Cd d -> ((dirs, size), x : xs)
          Dir d -> getDirContents ((d : cwd) : dirs) size xs
          File (i, _) -> getDirContents dirs (size + i) xs

pCLI :: Parser CLI
pCLI = pCd <|> pLs <|> pDir <|> pFile
  where
    pCd = P.try $ P.string "$ cd " *> (Cd <$> pString)
    pLs = P.try $ Ls <$ P.string "$ ls"
    pDir = P.try $ P.string "dir " *> (Dir <$> pString)
    pFile = do
      s <- number
      P.char ' '
      f <- pString
      pure (File (s, f))

pString :: Parser String
pString = P.many P.anyChar
