{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.PacemakerSpec (main, spec) where

import Data.Either (isRight)
import Test.Hspec
import Test.QuickCheck
import Data.Default (def)
import qualified Data.Map.Lazy as Map
import Data.Time
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Pacemaker
import Data.PacemakerFixture

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseWordCount" $ do
    it "turns the word count string into an Int" $ property $
      \(NonNegative i) -> let wc = show i ++ " words"
                          in parseWordCount wc === i
  describe "insertProdID" $ do
    it "corrects the calendar input to contain a prodID" $
      correctFormatWith [insertProdID] examplePreamble `shouldBe` exampleCorrectedPreamble
  describe "parseScheduleText" $ do
    it "applies corrections to the format and parses the schedule" $
      parseScheduleText "test/example.ics" exampleCalendar `shouldSatisfy` isRight
  describe "eventToDateAndWordCount" $ do
    it "extracts the date and word count from an event" $ property $
      \((NonNegative i), date) -> let evts = eventsToMap [SimpleEvent { evtSummary = (fromString (show i)) <> " words"
                                                                      , evtDescription = ""
                                                                      , evtStart = mkStartDate date
                                                                      , evtEnd = mkEndDate date
                                                                      }]
                                      evt = head (Map.elems evts)
                                  in eventToDateAndWordCount evt === (date, i)
  describe "wordCountToPomos" $ do
    it "takes a number of words and returns a number of pomodoros based on an avg rate" $ property $
      \(NonNegative i) -> wordCountToPomos def i >= i `div` (pacePerPomo def)
  describe "pomosToHalfHours" $ do
    it "takes a number of pomodoros and returns the indices of half-hours" $ do
      pomosToHalfHours 1 `shouldBe` [(0,1)]
      pomosToHalfHours 2 `shouldBe` [(0,2)]
      pomosToHalfHours 3 `shouldBe` [(0,3)]
      pomosToHalfHours 4 `shouldBe` [(0,3), (4,5)]
      pomosToHalfHours 5 `shouldBe` [(0,3), (4,6)]
      pomosToHalfHours 6 `shouldBe` [(0,3), (4,7)]
      pomosToHalfHours 7 `shouldBe` [(0,3), (4,7), (8, 9)]
    it "will always require ceil(i/3) blocks of time" $ property $
      \(Positive i) -> length (pomosToHalfHours i) === ceiling (fromIntegral i / 3)
  describe "pomosToTimeBlocks" $ do
    it "takes a number of pomodoros and returns start and end times, starting at 9am" $ do
      let d = exampleStartDate
          hh = halfHours
      pomosToTimeBlocks def d 100 1 `shouldBe` [(hh !! 0, hh !! 1, 100)]
  describe "transformVEvents" $ do
    it "takes an EventMap and splits the events into 3-pomo blocks (with half-hour breaks)" $ do
      transformVEvents def exampleEvent `shouldBe` exampleEvents
  describe "makeSchedule" $ do
    it "turns an all-day Pacemaker schedule into a Pomodoro-friendly block schedule" $ do
      pending
