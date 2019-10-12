module Text.IntFormatsSpec (spec) where

import Data.Char (isAlpha, isDigit)
import Test.Hspec
import Text.Read (readEither)
import Test.QuickCheck

import Text.IntFormats

showInt' :: Bool -> IntFormat -> Int -> String
showInt' = showInt

parseInt' :: String -> Either String Int
parseInt' = parseInt

spec :: Spec
spec = do
  describe "showInt" $ do
    it "separates digits in base 16" $
      showInt' False Hexadecimal 0x98765430 `shouldBe` "98765430"
    it "prints digits >9 in lowercase" $
      showInt' True Hexadecimal 0xafbecd `shouldBe` "0xafbecd"
    it "prints digits >9 in uppercase" $
      showInt' True HexUpper 0xafbecd `shouldBe` "0xAFBECD"
    it "only uses uppercase when needed" $
      showInt' False HexUpper 0x400 `shouldBe` "400"
    it "separates digits in base 10" $
      showInt' False Decimal 1234567890 `shouldBe` "1234567890"
    it "has correct decimal prefix" $
      showInt' True Decimal 321 `shouldBe` "0d321"
    it "separates digits in base 8" $
      showInt' False Octal 0o72546310 `shouldBe` "72546310"
    it "has correct octal prefix" $
      showInt' True Octal 0o1234 `shouldBe` "0o1234"
    it "separates digits in base 2" $
      showInt' False Binary 1000 `shouldBe` "1111101000"
    it "has correct binary prefix" $
      showInt' True Binary 100 `shouldBe` "0b1100100"
    it "extends show" $ property $
      equivalentFuncs (showInt' False Decimal) show
    it "works with negative numbers" $ property $
      \(p, m, n) -> showInt' p m (-(abs n)) === (if n == 0 then "" else "-") ++ showInt' p m (abs n)
    it "shows zero" $
      showInt' False Decimal 0 `shouldBe` "0"
  describe "parseInt" $ do
    it "hexadecimal with lowercase prefix" $
      parseInt' "0xaBcDEf01" `shouldBe` Right 0xabcdef01
    it "hexadecimal with uppercase prefix" $
      parseInt' "0X29876FF3" `shouldBe` Right 0x29876ff3
    it "decimal with lowercase prefix" $
      parseInt' "0d9087654321" `shouldBe` Right 9087654321
    it "decimal with uppercase prefix" $
      parseInt' "0D12345" `shouldBe` Right 12345
    it "octal with lowercase prefix" $
      parseInt' "0o71062543" `shouldBe` Right 0o71062543
    it "octal with uppercase prefix" $
      parseInt' "0O11111" `shouldBe` Right 0o11111
    it "binary with lowercase prefix" $
      parseInt' "0b1110" `shouldBe` Right 14
    it "binary with uppercase prefix" $
      parseInt' "0B101010" `shouldBe` Right 42
    it "zero with prefix" $
      parseInt' "0b0" `shouldBe` Right 0
    it "inverts showInt" $ property $
      \(m, n) -> (parseInt' . showInt True m) (abs n) === Right (abs n)
    it "extends read" $ property $
      \s -> parseInt' s `equalRight` readEither s
    it "zero without prefix" $
      parseInt' "0" `shouldBe` Right 0
    it "doesn't require prefix for base 10" $ property $
      equivalentFuncs (parseInt' . show . abs) (Right . abs)
    it "fails on leading alphabetical characters" $ property $
      null . parseInt' . filter isAlpha
    it "fails on empty string" $
      parseInt' "" `shouldSatisfy` null
    it "fails on 8 or 9 in octal" $ property $
      \n -> '8' `elem` show (abs n) || '9' `elem` show (abs n) ==> (null . parseInt') ("0o" ++ show (abs n :: Int))
    it "accepts leading zeros" $ property $
      \(n, m) -> m `mod` 5 /= 0 ==> Right (abs n) === parseInt' (replicate (m `mod` 5) '0' ++ show (abs n))
    it "fails on 2 to 9 in binary" $ property $
      \n -> length (show (abs n)) /= length (filter (\x->x=='0'||x=='1') (show (abs n))) ==> (null . parseInt') ("0b" ++ show (abs n :: Int))
    it "fails on a to f in decimal" $ property $
      \n -> length (showInt' False Hexadecimal (abs n)) /= length (filter isDigit (showInt' False Hexadecimal (abs n))) ==>
              (null . parseInt') (showInt False Hexadecimal (abs n))
    it "fails on trailing '.'" $
      parseInt' "12." `shouldSatisfy` null
    it "stops at separator" $
      parseInt' "12 34" `shouldBe` Right 12
    it "fails on invalid prefix" $ property $
      \c -> isAlpha c && c `notElem` "xdobXDOB" ==> null (parseInt' (c:"100"))
    it "works with negative numbers" $ property $
      \n b -> equivalentFuncs (parseInt' . ('-':) . showInt' True b . abs) (Right . (0-) . abs) n

equivalentFuncs :: (Eq b, Show b) => (a -> b) -> (a -> b) -> a -> Property
equivalentFuncs = (.) (<*>) $ (<$>) (===)

equalRight :: (Eq a, Show a) => Either t a -> Either s a -> Bool
equalRight (Left _) _ = True
equalRight _ (Left _) = True
equalRight (Right a) (Right b) = a == b
