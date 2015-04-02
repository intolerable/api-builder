module Network.API.Builder.RoutesSpec where

import Network.API.Builder.Routes

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "routeURL" $ do
    it "should be able to create a basic url" $ do
      routeURL "" (Route [ "api", "index" ] [ ] "GET")
        `shouldBe` "/api/index"
      routeURL "" (Route [ "api", "index.json" ] [ ] "GET")
        `shouldBe` "/api/index.json"
      routeURL "" (Route [ ] [ "test" =. False ] "GET")
        `shouldBe` "?test=false"
      routeURL "" (Route [ "about.json" ] [ "test" =. True ] "GET")
        `shouldBe` "/about.json?test=true"
      routeURL "" (Route [ "about.json" ] [ "test" =. True ] "GET")
        `shouldBe` "/about.json?test=true"

