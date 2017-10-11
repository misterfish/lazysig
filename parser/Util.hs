module Util
    ( anyChar
    , toLowerString
    , toUpperString
    , upperFirst
    , trim
    , digitsToInt
    , charRange, charRangeFrom, charRangeTo
    , numCharRange, numCharRangeFrom, numCharRangeTo
    , slurp
    , oneOfIgnoreCase
    , stringIgnoreCase
    , sp
    , sps
    , varId, varIdCh, varId1Ch
    , alphaCh, alphaLowerCh, alphaUpperCh,
    ) where

import Data.Char
    ( isSpace
    , toUpper
    , toLower
    , digitToInt
    , chr
    , ord
    )

import Text.ParserCombinators.Parsec
    ( GenParser
    , string
    , oneOf
    , try
    , many
    , satisfy
    , (<|>)
    )

upperFirst :: String -> String
upperFirst [] = []
upperFirst (x:xs) = toUpper x : xs

toLowerString :: String -> String
toLowerString [] = ""
toLowerString (x:xs) = toLower x : toLowerString xs

toUpperString :: String -> String
toUpperString [] = ""
toUpperString (x:xs) = toUpper x : toUpperString xs

-- | not efficient.
trim :: String -> String
trim = reverse . trim' . reverse . trim' where
    trim' "" = ""
    trim' (x:xs)
      | isSpace x = trim xs
      | otherwise = x:xs

digitsToInt :: String -> Int
digitsToInt "" = error "digitsToInt: empty string"
digitsToInt s = digitsToInt' (0 :: Double) (reverse s) where
    digitsToInt' _ [] = 0
    digitsToInt' m (x:xs) =
        digitToInt x * (floor $ 10 ** m) + digitsToInt' (m + 1) xs

slurp :: String -> IO String
slurp = readFile

charRange :: Char -> Char -> Maybe [Char]
charRange l r
  | l > r = Nothing
  | otherwise = Just $ map chr [ord l .. ord r]

charRangeFrom :: Char -> Int -> [Char]
charRangeFrom l n = map chr [ordl .. ordl + n - 1] where
    ordl = ord l

charRangeTo :: Char -> Int -> [Char]
charRangeTo r n = map chr [ordr - n + 1 .. ordr] where
    ordr = ord r

-- | e.g. a0-a5, a0-, -a5
numCharRange :: Char -> Int -> Int -> Maybe [String]
numCharRange c nl nr
  | nl > nr = Nothing
  | otherwise = Just $ map ((c:) . show) [nl .. nr]

numCharRangeFrom :: Char -> Int -> Int -> [String]
numCharRangeFrom c nl num = map ((c:) . show) [nl .. nl + num - 1]

numCharRangeTo :: Char -> Int -> Int -> [String]
numCharRangeTo c nr num = map ((c:) . show) [nr - num + 1 .. nr]

---- | parsers.

oneOfIgnoreCase :: String -> GenParser Char st Char
oneOfIgnoreCase str = oneOf $ toLowerString str ++ toUpperString str

stringIgnoreCase :: String -> GenParser Char st String
stringIgnoreCase str = str' l' <|> str' u' where
    l' = toLowerString str
    u' = toUpperString str
    str' = try . string

alphaCh'l :: [Char]
alphaCh'l = "abcdefghijklmnopqrstuvwxyz"

alphaCh'u :: [Char]
alphaCh'u = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

numCh' :: [Char]
numCh' = "0123456789"

sp :: GenParser Char st Char
sp = oneOf " \t\n"

-- | zero or more.
sps :: GenParser Char st String
sps = many sp

anyChar :: GenParser Char st Char
anyChar = satisfy $ const True

alphaCh :: GenParser Char st Char
alphaCh = oneOf $ alphaCh'l ++ alphaCh'u

alphaLowerCh :: GenParser Char st Char
alphaLowerCh = oneOf $ alphaCh'l

alphaUpperCh :: GenParser Char st Char
alphaUpperCh = oneOf $ alphaCh'u

varId :: GenParser Char st String
varId = (:) <$> varId1Ch <*> many varIdCh

varId1Ch :: GenParser Char st Char
varId1Ch = oneOf $ "_" ++ alphaCh'l

varIdCh :: GenParser Char st Char
varIdCh = oneOf $ "_'" ++ alphaCh'l ++ alphaCh'u ++ numCh'

