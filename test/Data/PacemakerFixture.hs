{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.PacemakerFixture where

import Data.Pacemaker
import Text.ICalendar.Types
import GHC.Exts
import Data.Time
import qualified Data.Map.Lazy as Map

import Data.String.Here (here, hereFile)
import qualified Data.ByteString.Lazy.Char8 as BL

exampleCalendar :: BL.ByteString
exampleCalendar = BL.pack [hereFile|test/example.ics|]

examplePreamble :: BL.ByteString
examplePreamble = BL.pack [here|
BEGIN:VCALENDAR
VERSION:2.0
BEGIN:VEVENT
|]

exampleCorrectedPreamble :: BL.ByteString
exampleCorrectedPreamble = BL.pack [here|
BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
|]

exampleEvent :: EventMap
exampleEvent =
  eventsToMap [ SimpleEvent "750 words" "" (mkStartDate (fromGregorian 2016 8 20)) (mkEndDate (fromGregorian 2016 8 20))
              ]

exampleStartDate :: Day
exampleStartDate = fromGregorian 2016 8 20

exampleStartHour :: UTCTime
exampleStartHour = UTCTime exampleStartDate (15*3600)

halfHours :: [UTCTime]
halfHours = halfHoursFrom exampleStartHour

exampleEvents :: EventMap
exampleEvents =
  eventsToMap [ SimpleEvent "" "" (mkStartDT (UTCDateTime (hh !! 0))) (mkEndDT (UTCDateTime (hh !! 3)))
              , SimpleEvent "" "" (mkStartDT (UTCDateTime (hh !! 4))) (mkEndDT (UTCDateTime (hh !! 7)))
              ]
  where date = fromGregorian 2016 8 20
        hh = halfHours
