{-# LANGUAGE QuasiQuotes #-}
module Data.PacemakerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.String.Here (here, hereFile)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Pacemaker

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

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

spec :: Spec
spec = do
  describe "parseWordCount" $ do
    it "turns the word count string into an Int" $ property $
      \(NonNegative i) -> let wc = show i ++ " words"
                          in parseWordCount wc === i
  describe "wordCountToTime" $ do
    it "converts a word count to a start and end time" $ do
      pending
  describe "insertProdID" $ do
    it "corrects the calendar input to contain a prodID" $
      correctFormatWith [insertProdID] examplePreamble `shouldBe` exampleCorrectedPreamble
  describe "makeSchedule" $ do
    it "turns an all-day Pacemaker schedule into a Pomodoro-friendly block schedule" $ do
      pending
