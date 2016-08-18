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
insertProdID :: [ByteString] -> [ByteString]
insertProdID (a:b:xs) = a:b:prodIDline:xs
  where prodIDline = "PRODID:-//hacksw/handcal//NONSGML v1.0//EN"

correctFormatWith :: [([ByteString] -> [ByteString])] -> ByteString -> ByteString
correctFormatWith fs = BL.intercalate "\n" . fs' . BL.split '\n'
  where fs' = foldr (.) id fs

correctFormat :: ByteString -> ByteString
correctFormat = correctFormatWith [ insertProdID ]

parseScheduleFile :: FilePath -> IO (Either String ([VCalendar], [String]))
parseScheduleFile path = do
  file <- BL.readFile path
  return $ parseICalendar def path (correctFormat file)

makeSchedule :: ByteString -> ByteString
makeSchedule = BL.pack . show . parseICalendar def "STDIN" . correctFormat
