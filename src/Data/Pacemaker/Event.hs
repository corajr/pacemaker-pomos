{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Pacemaker.Event where

import Text.ICalendar.Types
import GHC.Exts
import Data.Time
import Data.List (mapAccumL)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Map.Lazy as Map

type EventMap = Map.Map (Text, Maybe (Either Date DateTime)) VEvent

data SimpleEvent = SimpleEvent
  { evtSummary :: Text
  , evtDescription :: Text
  , evtStart :: DTStart
  , evtEnd :: Either DTEnd DurationProp
  } deriving (Eq, Show)

basicDesc :: Text -> Description
basicDesc desc =
  Description { descriptionValue = desc
              , descriptionAltRep = Nothing
              , descriptionLanguage = Nothing
              , descriptionOther = OtherParams (fromList [])}

basicSummary :: Text -> Summary
basicSummary summary =
  Summary { summaryValue = summary
          , summaryAltRep = Nothing
          , summaryLanguage = Just (Language "en-us")
          , summaryOther = OtherParams (fromList [])}

mkStartDate :: Day -> DTStart
mkStartDate start = DTStartDate { dtStartDateValue = Date { dateValue = start }
                                , dtStartOther = OtherParams (fromList [])
                                }

mkStartDT :: DateTime -> DTStart
mkStartDT start = DTStartDateTime { dtStartDateTimeValue = start
                                  , dtStartOther = OtherParams (fromList [])}

mkEndDate :: Day -> Either DTEnd DurationProp
mkEndDate date = Left (DTEndDate { dtEndDateValue = Date { dateValue = date }
                                 , dtEndOther = OtherParams (fromList [])})

mkEndDT :: DateTime -> Either DTEnd DurationProp
mkEndDT dt = Left (DTEndDateTime { dtEndDateTimeValue = dt
                                 , dtEndOther = OtherParams (fromList [])})

mkDuration :: Duration -> Either DTEnd DurationProp
mkDuration duration = Right (DurationProp { durationValue = duration
                                          , durationOther = OtherParams (fromList [])
                                          })

mkUID :: Int -> UID
mkUID i =
  UID {uidValue = "uid" <> fromString (show i) <> "@example.com", uidOther = OtherParams (fromList [])}

mkEvent :: Int -> SimpleEvent -> VEvent
mkEvent uid (SimpleEvent {..}) =
  VEvent { veDTStamp = DTStamp {dtStampValue = UTCTime (fromGregorian 2015 10 13) 0, dtStampOther = OtherParams (fromList [])}
         , veUID = mkUID uid
         , veClass = Class {classValue = Public, classOther = OtherParams (fromList [])}
         , veDTStart = Just evtStart
         , veCreated = Nothing
         , veDescription = Just (basicDesc evtDescription)
         , veGeo = Nothing
         , veLastMod = Nothing
         , veLocation = Nothing
         , veOrganizer = Nothing
         , vePriority = Priority {priorityValue = 0, priorityOther = OtherParams (fromList [])}
         , veSeq = Sequence {sequenceValue = 0, sequenceOther = OtherParams (fromList [])}
         , veStatus = Nothing
         , veSummary = Just (basicSummary evtSummary)
         , veTransp = Transparent {timeTransparencyOther = OtherParams (fromList [])}
         , veUrl = Nothing
         , veRecurId = Nothing
         , veRRule = fromList []
         , veDTEndDuration = Just evtEnd
         , veAttach = fromList [], veAttendee = fromList [], veCategories = fromList [], veComment = fromList [], veContact = fromList [], veExDate = fromList [], veRStatus = fromList [], veRelated = fromList [], veResources = fromList [], veRDate = fromList [], veAlarms = fromList [], veOther = fromList []
         }

eventsToMap :: [SimpleEvent] -> EventMap
eventsToMap = Map.fromList . map fromEvtUid . snd . mapAccumL renumber 0
  where fromEvtUid evt@(VEvent {..}) = ((uidValue veUID, Nothing), evt)
        renumber i x = (i+1, mkEvent i x)
