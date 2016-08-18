{-# LANGUAGE QuasiQuotes #-}
module Data.PacemakerSpec (main, spec) where

import Data.Either (isRight)
import Test.Hspec
import Test.QuickCheck
import Data.Pacemaker
import Data.PacemakerFixture

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
  describe "transformVEvents" $ do
    it "takes an EventMap and splits the events into 3-pomo blocks (with half-hour breaks)" $
      transformVEvents exampleEvent `shouldBe` exampleEvents
  describe "makeSchedule" $ do
    it "turns an all-day Pacemaker schedule into a Pomodoro-friendly block schedule" $ do
      pending
