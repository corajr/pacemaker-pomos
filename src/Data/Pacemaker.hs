{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Pacemaker ( module Data.Pacemaker.Event
                      , module Data.Pacemaker ) where

import Data.Pacemaker.Event
import Text.ICalendar
import Data.Char
import Data.List (mapAccumL)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Monoid ((<>))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Default (def)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Lazy as Map
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

insertDTstampAndUID :: [ByteString] -> [ByteString]
insertDTstampAndUID = concat . snd . mapAccumL f 0
  where f i "BEGIN:VEVENT" = (i + 1, ["BEGIN:VEVENT", "DTSTAMP:20151013T080000Z", "UID:uid" <> BL.pack (show i) <> "@example.com"])
        f i x = (i, [x])

correctFormatWith :: [([ByteString] -> [ByteString])] -> ByteString -> ByteString
correctFormatWith fs = BL.intercalate "\n" . fs' . BL.split '\n'
  where fs' = foldr (.) id fs

correctFormat :: ByteString -> ByteString
correctFormat = correctFormatWith [ insertProdID
                                  , insertDTstampAndUID
                                  ]

transformVCalendar :: VCalendar -> VCalendar
transformVCalendar cal = cal { vcEvents = transformVEvents (vcEvents cal) }

eventToDateAndWordCount :: VEvent -> (Day, Int)
eventToDateAndWordCount (VEvent {..}) =
  ( maybe (error "Not a date") (dateValue . dtStartDateValue) veDTStart
  , parseWordCount (maybe "" (Text.unpack . summaryValue) veSummary))

wordCountToPomos :: Int -> Int
wordCountToPomos x = x `div` 125

halfHoursFrom :: UTCTime -> [UTCTime]
halfHoursFrom z = iterate (hh `addUTCTime`) z
  where hh = fromInteger (30*60)

pomosToHalfHours :: Int -> [(Int, Int)]
pomosToHalfHours = go 0
  where go :: Int -> Int -> [(Int, Int)]
        go n i | i == 0 = []
               | i < 3 = [(n, n + i)]
               | otherwise = (n, n + 3) : go (n + 4) (i - 3)


pomosToTimeBlocks :: Day -> Int -> [(UTCTime, UTCTime)]
pomosToTimeBlocks date i = map (\(a,b) -> (hh !! a, hh !! b)) (pomosToHalfHours i)
  where startTime = UTCTime date (15*3600)
        hh = halfHoursFrom startTime

transformVEvents :: EventMap -> EventMap
transformVEvents evts = eventsToMap (concatMap f dateswcs)
  where dateswcs = map eventToDateAndWordCount (Map.elems evts)
        f (date, wc) = []

parseScheduleFile :: FilePath -> IO (Either String ([VCalendar], [String]))
parseScheduleFile path = do
  file <- BL.readFile path
  return $ parseScheduleText path file

parseScheduleText :: String -> ByteString -> Either String ([VCalendar], [String])
parseScheduleText path = parseICalendar def path . correctFormat

makeSchedule :: ByteString -> ByteString
makeSchedule = either (BL.pack) makeCal . parseScheduleText "STDIN" . correctFormat
  where makeCal = printICalendar def . transformVCalendar . head . fst
