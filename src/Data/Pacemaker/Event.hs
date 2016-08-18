{-# LANGUAGE OverloadedStrings #-}
module Data.Pacemaker.Event where

import Text.ICalendar.Types
import GHC.Exts
import Data.Time
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Map.Lazy as Map

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

mkEvent :: Int -> Text -> Text -> DTStart -> Either DTEnd DurationProp -> VEvent
mkEvent uid desc summary start end =
  VEvent { veDTStamp = DTStamp {dtStampValue = UTCTime (fromGregorian 2015 10 13) 0, dtStampOther = OtherParams (fromList [])}
         , veUID = UID {uidValue = "uid" <> fromString (show uid) <> "@example.com", uidOther = OtherParams (fromList [])}
         , veClass = Class {classValue = Public, classOther = OtherParams (fromList [])}
         , veDTStart = Just start
         , veCreated = Nothing
         , veDescription = Just (basicDesc desc)
         , veGeo = Nothing
         , veLastMod = Nothing
         , veLocation = Nothing
         , veOrganizer = Nothing
         , vePriority = Priority {priorityValue = 0, priorityOther = OtherParams (fromList [])}
         , veSeq = Sequence {sequenceValue = 0, sequenceOther = OtherParams (fromList [])}
         , veStatus = Nothing
         , veSummary = Just (basicSummary summary)
         , veTransp = Transparent {timeTransparencyOther = OtherParams (fromList [])}
         , veUrl = Nothing
         , veRecurId = Nothing
         , veRRule = fromList []
         , veDTEndDuration = Just end
         , veAttach = fromList [], veAttendee = fromList [], veCategories = fromList [], veComment = fromList [], veContact = fromList [], veExDate = fromList [], veRStatus = fromList [], veRelated = fromList [], veResources = fromList [], veRDate = fromList [], veAlarms = fromList [], veOther = fromList []
         }
