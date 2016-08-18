{-# LANGUAGE OverloadedStrings #-}
module Data.Pacemaker.Event where

import Text.ICalendar.Types
import GHC.Exts
import Data.Time
import qualified Data.Map.Lazy as Map

basicEvent :: VEvent
basicEvent = VEvent { veDTStamp = DTStamp {dtStampValue = UTCTime (fromGregorian 2015 10 13) 0, dtStampOther = OtherParams (fromList [])}
                    , veUID = UID {uidValue = "uid0@example.com", uidOther = OtherParams (fromList [])}
                    , veClass = Class {classValue = Public, classOther = OtherParams (fromList [])}
                    , veDTStart = Just (DTStartDate {dtStartDateValue = Date {dateValue = fromGregorian 2016 08 20}
                                                    , dtStartOther = OtherParams (fromList [])})
                    , veCreated = Nothing
                    , veDescription = Just (Description {descriptionValue = "Write 273 words to stay on track. http://bit.ly/2b2sYvU"
                                                        , descriptionAltRep = Nothing
                                                        , descriptionLanguage = Nothing
                                                        , descriptionOther = OtherParams (fromList [])})
                    , veGeo = Nothing
                    , veLastMod = Nothing
                    , veLocation = Just (Location {locationValue = "Your Favorite Spot", locationAltRep = Nothing, locationLanguage = Nothing, locationOther = OtherParams (fromList [])})
                    , veOrganizer = Nothing
                    , vePriority = Priority {priorityValue = 0, priorityOther = OtherParams (fromList [])}
                    , veSeq = Sequence {sequenceValue = 0, sequenceOther = OtherParams (fromList [])}
                    , veStatus = Nothing, veSummary = Just (Summary {summaryValue = "273 Words", summaryAltRep = Nothing, summaryLanguage = Just (Language "en-us")
                                                                    , summaryOther = OtherParams (fromList [])})
                    , veTransp = Transparent {timeTransparencyOther = OtherParams (fromList [])}
                    , veUrl = Nothing
                    , veRecurId = Nothing
                    , veRRule = fromList []
                    , veDTEndDuration = Just (Left (DTEndDate {dtEndDateValue = Date {dateValue = fromGregorian 2016 08 20}
                                                              , dtEndOther = OtherParams (fromList [])}))
                    , veAttach = fromList [], veAttendee = fromList [], veCategories = fromList [], veComment = fromList [], veContact = fromList [], veExDate = fromList [], veRStatus = fromList [], veRelated = fromList [], veResources = fromList [], veRDate = fromList [], veAlarms = fromList [], veOther = fromList []
                    }

mkEvent :: VEvent
mkEvent = undefined