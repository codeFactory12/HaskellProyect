{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UndecidableInstances #-}
module Scanner where

import Data.Char (isSpace, isAlphaNum)
import Data.Fixed (E0)

type Col = Int
type Line = Int
type Value = String
type Input = String

data Token = Token Type Value Line Col

data Type = String
          | OpenBlock
          | EndBlock
          | Keyword
          | EndSlide
          | Error
          | OpenCodeBlock
          | EndCodeBlock
          | OpenNumberedList
          | EndNumberedList
          | Bold
          | Italic
          | Highlight
         deriving (Eq, Ord)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show String = "String: "
    show OpenBlock = "OpenBlock: "
    show EndBlock = "EndBlock: "
    show Keyword = "Keyword: "
    show Error = "Error: "
    show EndSlide = "EndSlide: "
    show OpenCodeBlock = "OpenCodeBlock: "
    show EndCodeBlock = "EndCodeBlock: "
    show OpenNumberedList = "OpenNumberedList: "
    show EndNumberedList = "EndNumberedList: "
    show Bold = "Bold: "
    show Italic = "Italic: "
    show Highlight = "Highlight: "

instance (Eq Type) => (Eq Token) where
    (Token String _  _ _) == (Token String _  _ _) = True
    (Token Bold _  _ _) == (Token Bold _  _ _) = True
    (Token Italic _  _ _) == (Token Italic _  _ _) = True
    (Token Highlight _  _ _) == (Token Highlight _  _ _) = True
    (Token OpenBlock s1  _ _) == (Token OpenBlock s2  _ _) = True
    (Token EndBlock s1  _ _) == (Token EndBlock s2  _ _) = True
    (Token OpenCodeBlock s1  _ _) == (Token OpenCodeBlock s2  _ _) = True
    (Token EndCodeBlock s1  _ _) == (Token EndCodeBlock s2  _ _) = True
    (Token OpenNumberedList s1  _ _) == (Token OpenNumberedList s2  _ _) = True
    (Token EndNumberedList s1  _ _) == (Token EndNumberedList s2  _ _) = True
    (Token Keyword k1  _ _) == (Token Keyword k2  _ _) = k1 == k2
    (Token Error s1  _ _) == (Token Error s2  _ _) = s1 == s2
    (Token EndSlide s1  _ _) == (Token EndSlide s2  _ _) = s1 == s2
    (Token t1 s1  _ _) == (Token t2 s2  _ _) = s1 == s2

instance Ord Token where
     compare x y | x == y = EQ
                 | x <= y = LT
                 | otherwise = GT
     (Token t1 s1 _ _ ) <= (Token t2 s2 _ _ ) = t1 < t2 || (t1 == t2 && s1 <= s2)

scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x:xs) l c
  | x == '!' = if not (null xs) && head xs == ' ' then Token Keyword [x, head xs] l c: scan xs l (c+1) else Token Error [x] l c : scan xs l (c+ 1)
  | x == '-' = if isSlideEnd xs then Token EndSlide ['-','-','-'] l c : scan (drop 2 xs) l (c+2) else Token Error [x] l c : scan xs l (c + 1)
  | x == '{' = if head xs == '\n' then Token OpenBlock [x] l c : scan xs l (c+1) else Token Error [x] l c : scan xs l (c+1)
  | x == '}' = if head xs == '\n' then Token EndBlock [x] l c : scan xs l (c+1) else Token Error [x] l c : scan xs l (c+1)
  | x == '<' = handleOpenCodeBlock x xs l c
  | x == '>' = if head xs == '&' then Token EndCodeBlock [x,head xs] l c : scan xs l (c+1) else Token Error [x] l c : scan xs l (c+1)
  | x == '*' = handleOpenNumberedList x xs l c
  | x == '$' = if head xs == '*' then Token EndNumberedList [x,head xs] l c : scan xs l (c+1) else Token Error [x] l c : scan xs l (c+1)
  | x == '%' = let (link, rest) = span ( /= '\n') xs
               in Token Keyword [x] l c : Token String link l c : scan rest l c
  | x == '#' = let (hashes, rest) = span (== '#') xs
                in if length (x:hashes) <= 6 then Token Keyword (x:hashes) l c : scan rest l (c + length (x:hashes)) else Token Error [x] l c : scan xs l (c+1)
  | isAlphaNum x =  let (alphaList, rest) = span (\c -> isAlphaNum c ||  c == ' ') xs
                   in Token String (x : alphaList) l c : scan rest l (c + length (x : alphaList))
  | isBold x xs = let bold = untilEndBold (drop 1 xs)
                        in Token Bold bold l c : scan (drop (length bold + 3) xs) (l+ nJumps bold 0) (c + length bold)
  | x == '+' = let italic = untilEndItalic xs
                        in Token Italic italic l c : scan (drop (length italic + 1) xs) (l+ nJumps italic 0) (c + length italic)
  | x == '_' = let highlight = untilEndHighlight xs
                        in Token Highlight highlight l c : scan (drop (length highlight + 1) xs) (l+ nJumps highlight 0) (c + length highlight)
  | isEndRow x xs = Token Keyword [x, head xs] l c : scan xs l c
  | isSplit x xs = Token Keyword [x, head xs] l c : scan (drop 2 xs) l c
  | x == '|' = Token Keyword [x] l c: scan xs l (c+1)
  | x == ';' = scan (dropWhile (/= '\n') xs) l 1
  | isBreak x xs = Token Keyword [x,head xs] l c : scan (drop 1 xs) l c 
  | x == '\n' = scan xs (l+1) 1
  | otherwise = scan xs l (c+1)

isSlideEnd :: Input -> Bool
isSlideEnd [] = False
isSlideEnd (x:xs)
            | x == '-' = (not (null xs) && head xs == '-') || False
            | otherwise = False
isSplit :: Char -> String -> Bool
isSplit x xs| x == '|' = head xs == '|'
             |otherwise = False


handleOpenNumberedList :: Char -> Input -> Line -> Col -> [Token]
handleOpenNumberedList x xs l c
    | head xs == '$' = let code = untilEndList (drop 2 xs)
                           blockLength = length code + 2
                           newLines = nJumps code 0
                        in Token OpenNumberedList [x, head xs] l c :
                        Token String code l c :
                        scan (drop blockLength xs) (l + newLines + 1) c
    | otherwise = scan xs l (c+1)

handleOpenCodeBlock :: Char -> Input -> Line -> Col -> [Token]
handleOpenCodeBlock x xs l c
    | head xs == '&' = let list = untilEndBlock (drop 2 xs)
                           blockLength = length list + 2
                           newLines = nJumps list 0
                       in Token OpenCodeBlock [x, head xs] l c :
                          Token String list l c :
                          scan (drop blockLength xs) (l + newLines + 1) c
    | otherwise = scan xs l (c+1)

nJumps :: String -> Int -> Int
nJumps [] n = n
nJumps (x:xs) n = if x == '\n' then nJumps xs (n + 1) else nJumps xs n

untilEndBlock :: String -> String
untilEndBlock [] = []
untilEndBlock (x:xs)
    | x == '>' && head xs == '&' = []
    | otherwise = x : untilEndBlock xs

untilEndList :: String -> String
untilEndList [] = []
untilEndList (x:xs)
    | x == '$' && head xs == '*' = []
    | isAlphaNum x = x: untilEndList xs
    | x == ' ' = x:untilEndList xs
    | x == '\n' = x:untilEndList xs
    | otherwise = []

untilEndBold :: String -> String
untilEndBold [] = []
untilEndBold (x:xs)
    | x == '+' && head xs == '+' = []
    | isAlphaNum x = x: untilEndList xs
    | x == ' ' = x:untilEndList xs
    | x == '\n' = x:untilEndList xs
    | otherwise = []

untilEndItalic :: String -> String
untilEndItalic [] = []
untilEndItalic (x:xs)
    | x == '+' = []
    | isAlphaNum x = x: untilEndList xs
    | x == ' ' = x:untilEndList xs
    | x == '\n' = x:untilEndList xs
    | otherwise = []

untilEndHighlight :: String -> String
untilEndHighlight [] = []
untilEndHighlight (x:xs)
    | x == '_' = []
    | isAlphaNum x = x: untilEndList xs
    | x == ' ' = x:untilEndList xs
    | x == '\n' = x:untilEndList xs
    | otherwise = []

takeproob :: String -> (Bool, String)
takeproob xs = (take 2 xs == "++", drop 2 xs)

isBold :: Char -> String -> Bool
isBold x xs| x == '+' = head xs == '+'
             |otherwise = False

isBreak :: Char -> String -> Bool
isBreak x xs| x == '\n' = head xs == '\n'
             |otherwise = False

isEndRow :: Char -> String -> Bool
isEndRow x xs|x =='|' = head xs =='\n'
             |otherwise = False