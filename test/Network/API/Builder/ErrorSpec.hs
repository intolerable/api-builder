{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.API.Builder.ErrorSpec where

import Data.Semigroup ((<>))
import Network.API.Builder.Error

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec spec

instance Arbitrary a => Arbitrary (APIError a) where
  arbitrary = oneof
    [ APIError <$> arbitrary
    , pure InvalidURLError
    , ParseError <$> arbitrary
    , pure EmptyError
    ]

spec :: Spec
spec =
  describe "APIError" $ do
    describe "Eq" $ do
      it "should check for equality properly" $ do
        HTTPError undefined == (HTTPError undefined :: APIError ())
          `shouldBe` False
        APIError () `shouldBe` APIError ()
        InvalidURLError `shouldBe` (InvalidURLError :: APIError ())

      prop "ParseError s == ParseError s" $ \s ->
        ParseError s == (ParseError s :: APIError ())

      prop "APIError x == APIError x" $ \(x :: String) ->
        APIError x == APIError x

    describe "Semigroup" $ do
      prop "(x <> y) <> z == x <> (y <> z)" $ \(x :: APIError ()) y z ->
        (x <> y) <> z == x <> (y <> z)
