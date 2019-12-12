
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Data.TypedDigitsSpec (main, spec) where


import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Proxy

import Data.TypedDigits


--isInverseOf :: Eq a => (a -> b) -> (b -> a) -> a -> Bool
--isInverseOf f g x =
--  g (f x) == x
--
--
--
--sameAs :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
--sameAs f g x =
--  f x == g x
--
--dflor = D.digits `sameAs` DD.digits 10
--
-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec
--
--digitsImpls :: [(String, Int -> [Int])]
--digitsImpls = [
--    ("digits",      D.digits)
--  , ("digitsIter",  D.digitsIter)
--  , ("digitsPFree", D.digitsPFree)
--  ]

-- Let's do things in base 12.
type TestBase1 = 12

testBase1 :: Int
testBase1 = fromIntegral $ getBaseT (Proxy :: Proxy TestBase1)

-- digits can be added - up to their valid range
propAddable :: Property
propAddable =
  forAll (choose (0, testBase1-1)) $ \x ->
    let y = testBase1 - x - 1
        
        xd = digit' @TestBase1 x
        yd = digit' @TestBase1 y
    in  getVal (xd <+> yd) == testBase1 - 1


spec :: Spec
spec = do 
  describe "Digits" $ do
    it "can be added (up to their valid range)" $
      property propAddable
    it "have minBound 0" $ 
      getVal (minBound :: Digit TestBase1) `shouldBe` 0
    it "have maxBound = base - 1" $ 
      getVal (maxBound :: Digit TestBase1) `shouldBe` (testBase1 - 1)
  describe "digit" $ do
    it "yields Nothing if passed a negative" $
      digit @TestBase1 (-1) `shouldBe` Nothing
    it "yields Nothing if passed the base itself" $
      digit @TestBase1 testBase1 `shouldBe` Nothing
    it "yields the corresponding digit if passed anything between" $
      forM_ [0 .. testBase1 - 1] $ \n -> 
        let x = digit @TestBase1 n
        in  fmap getVal x `shouldBe` Just n

