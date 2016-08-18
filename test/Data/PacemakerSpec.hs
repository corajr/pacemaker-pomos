{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.PacemakerSpec (main, spec) where

import Data.Either (isRight)
import Test.Hspec
import Test.QuickCheck
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
  describe "wordCountToTimes" $ do
    it "converts a word count to a list of max 3-pomo blocks with half hour breaks" $ do
      pending
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
  describe "transformVEvents" $ do
    it "takes an EventMap and splits the events into 3-pomo blocks (with half-hour breaks)" $ do
      pending
      transformVEvents exampleEvent `shouldBe` exampleEvents
  describe "makeSchedule" $ do
    it "turns an all-day Pacemaker schedule into a Pomodoro-friendly block schedule" $ do
      pending
