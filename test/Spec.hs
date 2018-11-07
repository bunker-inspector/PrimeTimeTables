module Main where

import Test.Hspec
import PrimeTime
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "PrimeTime.isPrime" $ do
        it "can determine if an integer is prime" $ do
            (isPrime 7) `shouldBe` (True :: Bool)

    describe "PrimeTime.isNotPrime" $ do
        it "can determine if an integer is not prime" $ do
            (isNotPrime 9) `shouldBe` (True :: Bool)

    describe "PrimeTime.fill" $ do
        it "can pad a string has a determined width" $ do
            (fill 5 '-' "") `shouldBe` ("-----" :: String)

    describe "PrimeTime.getNPrimes" $ do
        it "can get the first n prime numbers" $ do
            (getNPrimes 5) `shouldBe` ([2,3,5,7,11] :: [Int])

    describe "PrimeTime.fill" $ do
        it "will retun input if it already exceeds determined width" $ do
            (fill 3 '-' "----") `shouldBe` ("----" :: String)

    describe "PrimeTime.primeProducts" $ do
        it "can return the product of every combination of the first n prime numbers" $ do
            (primeProducts 1) `shouldBe` ([4] :: [Integer])
            (primeProducts 2) `shouldBe` ([4,6,6,9] :: [Integer])
