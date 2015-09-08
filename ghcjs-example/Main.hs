-- |

module Main where

import           Network.API.Builder.Examples.StackOverflow

main :: IO ()
main = do
  res <- getAnswers
  print res
