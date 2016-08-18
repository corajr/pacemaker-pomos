{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.PacemakerFixture where

import Data.Pacemaker (EventMap)
import Data.Pacemaker.Event
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
exampleEvent = Map.singleton ("uid0@example.com",Nothing) evt
  where evt = mkEvent 0 "" "" (mkStartDate (fromGregorian 2016 8 20)) (mkEndDate (fromGregorian 2016 8 20))

exampleEvents :: EventMap
exampleEvents = Map.empty

