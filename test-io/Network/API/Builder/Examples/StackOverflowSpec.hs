module Network.API.Builder.Examples.StackOverflowSpec where

import Network.API.Builder.Examples.StackOverflow

import Control.Monad
import Data.List (nub)
import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Text as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "getAnswers" $
    it "can get answers" $
      getAnswers >>= \case
        Left _ -> expectationFailure "getAnswers failed"
        Right (Questions qs) -> do
          nub qs `shouldMatchList` qs
          forM_ qs $ \q -> do
            tags q `shouldSatisfy` isNubbed
            title q `shouldSatisfy` (not . Text.null)

isNubbed :: Ord a => [a] -> Bool
isNubbed = f Set.empty
  where
    f _ [] = True
    f s (x:xs) = not (Set.member x s) && f (Set.insert x s) xs

