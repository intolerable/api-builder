module Network.API.Builder.QuerySpec where

import Network.API.Builder.Query

import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "ToQuery" $ do

    it "can convert an Integer" $ do
      toQuery "integer" (5 :: Integer) `shouldBe`
        [("integer", "5")]

    it "can convert a Boolean" $ do
      toQuery "bool" False `shouldBe` [("bool", "false")]

    it "can convert a list of Ints" $ do
      toQuery "list" ([1,2,3,4,5] :: [Int]) `shouldBe` [("list", "1,2,3,4,5")]

    it "can convert Maybe values" $ do
      toQuery "maybe" (Just False) `shouldBe` [("maybe", "false")]
      toQuery "maybe" (Nothing :: Maybe Bool) `shouldBe` []

    prop "toQuery x y == toQuery x (Just y)" $ \(y :: Int) ->
      toQuery "x" y == toQuery "x" (Just y)

skip :: String -> Expectation -> Expectation
skip _ _ = return ()
