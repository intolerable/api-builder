module Network.API.Builder.ErrorSpec where

import Network.API.Builder.Error

import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

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
