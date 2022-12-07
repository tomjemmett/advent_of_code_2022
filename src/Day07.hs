module Day07 (
  day07
) where

import Common
import Control.Monad.State
import Data.Bifunctor
import qualified Data.Map as M
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

data CLI = Cd String | Ls | File (Int, String) | Dir String deriving (Show)
type DirPath = [String]
type DirStructure = M.Map DirPath ([DirPath], Int)

day07 :: AOCSolution
day07 input = show <$> ([part1, part2] <*> pure i)
  where
    i = updateSizes [] $ parseInput input

part1 :: DirStructure -> Int
part1 = sum . filter (<= 100000) . map snd . M.elems
part2 :: DirStructure -> Int
part2 i = minimum d
  where
    f = 70000000 - (snd $ i M.! ["/"])
    n = 30000000 - f
    d = filter (>=n) $ map (snd . snd) $ M.toList i

updateSizes :: [DirPath] -> DirStructure -> DirStructure
updateSizes done m
  | (fst . head) k == ["/"] = m
  | otherwise = updateSizes done' m'
  where
    k = filter (not . flip elem done . fst) .
      map (second snd) . M.toList .
      M.filter ((==[]) . fst) $ m
    t = traverse fn k
    m' = execState t m
    done' = foldr (:) done $ map fst k
    fn :: ([String], Int) -> State (M.Map [String] ([[String]], Int)) ()
    fn (d, s) = do
      m <- get
      let
        (p, (dirs, ps)) = head . M.toList . M.filter (elem d . fst) $ m
        dirs' = filter (/= d) dirs
        ps' = ps + s
      put $ M.insert p (dirs', ps') m
      pure ()

parseInput :: String -> DirStructure
parseInput = M.fromList . navigate [] . map (parse' pCLI id) . lines
  where
    navigate _ [] = []
    navigate cwd (x:xs) = case x of
      Cd ".." -> navigate (tail cwd) xs
      Cd d    -> navigate (d:cwd) xs
      Ls      -> lsDir cwd xs 
    lsDir cwd x = (cwd, r):(navigate cwd xs)
      where
        (r, xs) = getDirContents [] 0 x
        getDirContents dirs size [] = ((dirs, size), [])
        getDirContents dirs size (x:xs) = case x of
          Cd d        -> ((dirs, size), x:xs)
          Dir d       -> getDirContents ((d:cwd):dirs) size xs
          File (i, _) -> getDirContents dirs (size + i) xs

pCLI :: Parser CLI
pCLI = pCd <|> pLs <|> pDir <|> pFile
  where
    pCd = P.try $ P.string "$ cd " *> (Cd <$> pString)
    pLs = P.try $ const Ls <$> P.string "$ ls"
    pDir = P.try $ P.string "dir " *> (Dir <$> pString)
    pFile = do
      s <- number
      P.char ' '
      f <- pString
      pure (File (s, f))

pString :: Parser String
pString = P.many P.anyChar