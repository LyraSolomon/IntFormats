{-# LANGUAGE FlexibleContexts #-}
-- |Includes functions for converting from a string to an integer, with the base determined automatically,
--  and from an integer to a string, with the user specifying the base. Supports negative numbers.
--
--  For examples, see the [test source](https://github.com/LyraSolomon/IntFormats/tree/master/test/Text/IntFormatsSpec.hs)
--
--  @'parseInt' . 'showInt' True _ == Right@ for all inputs, however the reverse is not necessarily true, even in the case of
--  valid input, since 'parseInt' accepts some rather creative capitalization that 'showInt' does not produce.
module Text.IntFormats (IntFormat(..), parseInt, intParser, showInt) where

import Text.Parsec hiding (digit)
import Text.Parsec.Char (alphaNum)
import Data.Char (toUpper)
import Data.List (foldl1')
import Test.QuickCheck (Arbitrary, arbitrary, oneof)

-- |'IntFormat' specifies which base is used for 'showInt'
data IntFormat =
  -- |@0d99@, etc. This will be more commonly used without the prefix.
  Decimal |
  -- |Hexadecimal with lowercase digits, such as @0xff@
  Hexadecimal |
  -- |@0o77@, etc. In some programs, octal is denoted with a leading zero, however that is not used here.
  Octal |
  -- |@0b11@, etc.
  Binary |
  -- |Hexadecimal with uppercase digits. This only affects the digits A-F, not the prefix. For example, @0xFF@
  HexUpper deriving (Show, Eq)

-- |The same as 'parseInt', except the parsec monad isn't evaluated. This is intended for use as part of
--  a larger parser. If you simply want to get a value, use 'parseInt'
intParser :: (Integral a, Stream s m Char) => ParsecT s u m a
intParser = do 
  negative <- option ' ' (char '-')
  value <- (char '0' >> parseBase) <|> parseDec
  notFollowedBy (alphaNum <|> char '.')
  if negative == '-' then return (-value) else return value

-- |Converts a string to an integer if able, otherwise returns an error message.
--  The number may begin with @0x@, @0d@, @0o@, or @0b@ to specify the numerical base. If no prefix is provided, base 10 is assumed.
--  The prefix is case insensitive. If the number is negative, the sign goes in front of the prefix. The number must
--  not have leading spaces. The number may have characters following it, as long as it is not immediately followed by
--  a digit, a letter, or a decimal point.
parseInt :: Integral a => String -> Either String a
parseInt = mapLeft show . parse intParser ""

parseBase :: (Integral a, Stream s m Char) => ParsecT s u m a
parseBase = (char 'x' >> parseHex) <|> (char 'o' >> parseOct) <|> (char 'b' >> parseBin) <|> (char 'd' >> parseDec) <|>
            (char 'X' >> parseHex) <|> (char 'O' >> parseOct) <|> (char 'B' >> parseBin) <|> (char 'D' >> parseDec) <|> parseDec <|> return 0

-- |Converts a integer to a string, with the chosen numerical base.
showInt :: Integral a
  => Bool      -- ^ Whether to include the prefix @0x@, @0d@, @0o@, or @0b@. A value of True is generally recommended for bases other than decimal.
  -> IntFormat -- ^ Which numerical base to use, and if it is hexadecimal, whether the digits are uppercase or lowercase
  -> a         -- ^ The number to be converted
  -> String    -- ^ The output value.
showInt prefix base n = if n < 0 then '-' : showInt' base (-n) else showInt' base n
  where showInt' Decimal     = (if prefix then ("0d"++) else id) . digits 10
        showInt' Hexadecimal = (if prefix then ("0x"++) else id) . digits 16
        showInt' HexUpper    = (if prefix then ("0x"++) else id) . map toUpper . digits 16
        showInt' Octal       = (if prefix then ("0o"++) else id) . digits 8
        showInt' Binary      = (if prefix then ("0b"++) else id) . digits 2

undigits :: Integral a => a -> String -> a
undigits n = foldl1' (\a b -> a*n+b) . map undigit

digits :: Integral a => a -> a -> String
digits n x = if x `div` n == 0 then [digit x] else digits n (x `div` n) ++ [digit (x `mod` n)]

parseDec :: (Integral a, Stream s m Char) => ParsecT s u m a
parseDec = undigits 10 <$> many1 (oneOf "1234567890") <?> "one or more decimal digits"

parseHex :: (Integral a, Stream s m Char) => ParsecT s u m a
parseHex = undigits 16 <$> many1 (oneOf "1234567890abcdefABCDEF") <?> "one or more dexadecimal digits"

parseOct :: (Integral a, Stream s m Char) => ParsecT s u m a
parseOct = undigits 8 <$> many1 (oneOf "12345670") <?> "one or more octal digits"

parseBin :: (Integral a, Stream s m Char) => ParsecT s u m a
parseBin = undigits 2 <$> many1 (oneOf "01") <?> "one or more binary undigits"

undigit :: Integral a => Char -> a
undigit '0' = 0
undigit '1' = 1
undigit '2' = 2
undigit '3' = 3
undigit '4' = 4
undigit '5' = 5
undigit '6' = 6
undigit '7' = 7
undigit '8' = 8
undigit '9' = 9
undigit 'a' = 10
undigit 'b' = 11
undigit 'c' = 12
undigit 'd' = 13
undigit 'e' = 14
undigit 'f' = 15
undigit 'A' = 10
undigit 'B' = 11
undigit 'C' = 12
undigit 'D' = 13
undigit 'E' = 14
undigit 'F' = 15
undigit _   = undefined

digit :: Integral a => a -> Char
digit 0  = '0'
digit 1  = '1'
digit 2  = '2'
digit 3  = '3'
digit 4  = '4'
digit 5  = '5'
digit 6  = '6'
digit 7  = '7'
digit 8  = '8'
digit 9  = '9'
digit 10 = 'a'
digit 11 = 'b'
digit 12 = 'c'
digit 13 = 'd'
digit 14 = 'e'
digit 15 = 'f'
digit _  = undefined

instance Arbitrary IntFormat where
  arbitrary = oneof $ map return [Hexadecimal, HexUpper, Decimal, Octal, Binary]

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x
