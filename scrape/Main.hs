{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Lens           (_Wrapped)
import Control.Lens.Operators
import Control.Monad          (void)
import Formatting
import Network.HTTP.Req       (MonadHttp, Req, defaultHttpConfig, runReq)
import System.Directory       (createDirectoryIfMissing)
import System.FilePath        ((</>))
import Text.Pretty.Simple     (pShowNoColor)

import Flights

import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified Leonardo          as LEO
import qualified XContest2         as XC

scrapeSite :: Bool -> String -> Req [Flight] -> IO ()
scrapeSite showSite fileName getFlights = do
  let scrapeDir = "site/scraped"
  void $ createDirectoryIfMissing False scrapeDir
  let file = scrapeDir </> fileName <> ".html"
  putStrLn $ "Populating '" <> file <> "'"
  flights <- runReq defaultHttpConfig getFlights
  putStrLn $ "Found " <> show (length flights) <> " flights."
  sequence_ $ fprint formatFlight <$> flights
  TIO.writeFile
    file
    (renderFlights showSite flights)

formatFlight :: Format r (Flight -> r)
formatFlight = later $ \flight ->
  bprint ("- " % stext % " flew " % fixed 2 % "km from " % stext % " on " % build % ".\n")
    (flight ^. flightPilot . pilotName)
    (flight ^. flightDistance . distanceKm)
    (flight ^. flightSiteName . _Wrapped)
    (flight ^. flightDate)

getFlightsAt :: MonadHttp m => [Site -> Int -> m [Flight]] -> Site -> Int -> m [Flight]
getFlightsAt getters site n =
  mconcat <$> traverse (\f -> f site n) getters

getLongestFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getLongestFlightsAt site n = do
  flights <- getFlightsAt
    [ XC.getLongestFlightsAt
    , LEO.getLongestFlightsAt
    ] site n
  pure . take n . removeConsecutiveSimilar . sortFlightsByLengthDesc $ flights

getRecentFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getRecentFlightsAt site n = do
  flights <- getFlightsAt
    [ XC.getRecentFlightsAt
    , LEO.getRecentFlightsAt
    ] site n
  pure . take n  . removeConsecutiveSimilar . sortFlightsByDateDesc $ flights

main :: IO ()
main = do
  scrapeSite True "longestCanberra"     (getLongestFlightsAt canberra    20)
  scrapeSite False "longestSpringHill"  (getLongestFlightsAt springHill  10)
  scrapeSite False "longestCollector"   (getLongestFlightsAt collector   10)
  scrapeSite False "longestLakeGeorge"  (getLongestFlightsAt lakeGeorge  10)
  scrapeSite False "longestLanyon"      (getLongestFlightsAt lanyon      5)
  scrapeSite False "longestPigHill"     (getLongestFlightsAt pigHill     5)
  scrapeSite False "longestHoneysuckle" (getLongestFlightsAt honeysuckle 5)
  scrapeSite False "longestBowning"     (getLongestFlightsAt bowning     5)
  scrapeSite False "longestArgalong"    (getLongestFlightsAt argalong    5)
  scrapeSite False "longestCastleHill"  (getLongestFlightsAt castleHill  5)
  scrapeSite False "longestBooroomba"   (getLongestFlightsAt booroomba   5)
  scrapeSite False "longestCarols"      (getLongestFlightsAt carols      5)

  scrapeSite True "recentCanberra" (getRecentFlightsAt canberra 20)
