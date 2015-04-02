module Network.API.Builder.SendSpec where

import Network.API.Builder.Builder
import Network.API.Builder.Routes
import Network.API.Builder.Send

import qualified Network.HTTP.Conduit as HTTP
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Sendable" $ do
    describe "()" $ do
      it "should be able to construct a basic request" $ do
        case send exampleBuilder (Route ["api.json"] [] "GET") () of
          Just req -> do
            HTTP.secure req `shouldBe` True
            HTTP.host req `shouldBe` "example.com"
            HTTP.method req `shouldBe` "GET"
            HTTP.port req `shouldBe` 443
            HTTP.queryString req `shouldBe` ""
            HTTP.path req `shouldBe` "/api.json"
          Nothing -> expectationFailure "req construction failed"

exampleBuilder :: Builder
exampleBuilder = basicBuilder "example" "https://example.com"
