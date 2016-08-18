module Main where

import System.Environment
import Data.Pacemaker
import Data.Default (def)
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  args <- getArgs
  let opts = case args of
               [] -> def
               [x] -> def { dailyStartTime = 3600 * read x }
               [x, y] -> PacemakerOptions { dailyStartTime = 3600 * read x, pacePerPomo = read y }
  BL.interact (makeSchedule opts)
