{-# LANGUAGE OverloadedStrings #-}
module Data.Pacemaker where

import Text.ICalendar
import Data.Char
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Default (def)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

parseWordCount :: String -> Int
parseWordCount = fromMaybe 0 . readMaybe . takeWhile isDigit

wordCountToTime :: Int -> (Int, Int)
wordCountToTime wc = undefined

-- | Pacemaker's output is missing the PRODID needed for parsing. This function
-- adds it in.
insertProdID :: ByteString -> ByteString
insertProdID = BL.intercalate "\n" . ins . BL.split '\n'
  where ins (a:b:xs) = a:b:prodIDline:xs
        prodIDline = "PRODID:-//hacksw/handcal//NONSGML v1.0//EN"

makeSchedule :: ByteString -> ByteString
makeSchedule = BL.pack . show . parseICalendar def "STDIN"
