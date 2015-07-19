module Network.API.Builder.SendSpec where

import Network.API.Builder.Builder
import Network.API.Builder.Routes
import Network.API.Builder.Send

import Data.Aeson
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
      it "should be able to construct request with query params" $ do
        case send exampleBuilder (Route ["api.json"] ["a_query" =. True] "GET") () of
          Just req -> do
            HTTP.secure req `shouldBe` True
            HTTP.host req `shouldBe` "example.com"
            HTTP.method req `shouldBe` "GET"
            HTTP.port req `shouldBe` 443
            HTTP.queryString req `shouldBe` "?a_query=true"
            HTTP.path req `shouldBe` "/api.json"
          Nothing -> expectationFailure "req construction failed"
      it "should be able to construct a POST request" $ do
        case send exampleBuilder (Route ["api.json"] ["query" =. False] "POST") () of
          Just req -> do
            HTTP.secure req `shouldBe` True
            HTTP.host req `shouldBe` "example.com"
            HTTP.method req `shouldBe` "POST"
            HTTP.port req `shouldBe` 443
            HTTP.queryString req `shouldBe` ""
            HTTP.path req `shouldBe` "/api.json"
            case HTTP.requestBody req of
              HTTP.RequestBodyBS "query=false" -> return ()
              _ -> expectationFailure "incorrect POST body"
          Nothing -> expectationFailure "req construction failed"

    describe "PostQuery" $ do
      it "should be able to construct a POST request with no body" $ do
        case send exampleBuilder (Route ["api.json"] ["query" =. False] "POST") PostQuery of
          Just req -> do
            HTTP.secure req `shouldBe` True
            HTTP.host req `shouldBe` "example.com"
            HTTP.method req `shouldBe` "POST"
            HTTP.port req `shouldBe` 443
            HTTP.queryString req `shouldBe` "?query=false"
            HTTP.path req `shouldBe` "/api.json"
            case HTTP.requestBody req of
              HTTP.RequestBodyLBS "" -> return ()
              _ -> expectationFailure "incorrect POST body"
          Nothing -> expectationFailure "req construction failed"

    describe "Value" $ do
      it "should be able to construct a POST request with the correct JSON body" $ do
        case send exampleBuilder (Route ["api.json"] ["query" =. False] "POST") $ object ["hello" .= Null] of
          Just req -> do
            HTTP.secure req `shouldBe` True
            HTTP.host req `shouldBe` "example.com"
            HTTP.method req `shouldBe` "POST"
            HTTP.port req `shouldBe` 443
            HTTP.queryString req `shouldBe` "?query=false"
            HTTP.path req `shouldBe` "/api.json"
            case HTTP.requestBody req of
              HTTP.RequestBodyLBS "{\"hello\":null}" -> return ()
              _ -> expectationFailure "incorrect POST body"
          Nothing -> expectationFailure "req construction failed"

exampleBuilder :: Builder
exampleBuilder = basicBuilder "example" "https://example.com"
