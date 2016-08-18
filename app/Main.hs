module Main where

import Data.Pacemaker
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = BL.interact makeSchedule
