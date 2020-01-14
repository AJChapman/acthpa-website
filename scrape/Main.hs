{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Monad (void)
import Network.HTTP.Req (Req, defaultHttpConfig, runReq)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import Flights

import qualified XContest as XC
import qualified Data.Text.IO as TIO

scrapeSite :: Bool -> String -> Req [Flight] -> IO ()
scrapeSite showSite fileName getFlights = do
  let scrapeDir = "site/scraped"
  void $ createDirectoryIfMissing False scrapeDir
  let file = scrapeDir </> fileName <> ".html"
  putStrLn $ "Populating '" <> file <> "'"
  flights <- runReq defaultHttpConfig getFlights
  putStrLn $ "Found " <> show (length flights) <> " flights."
  TIO.writeFile
    file
    (renderFlights showSite flights)

main :: IO ()
main = do
  scrapeSite True "longestCanberra"    (XC.getLongestFlightsAt canberra    75000 20)
  scrapeSite False "longestSpringHill"  (XC.getLongestFlightsAt springHill  5000  5)
  scrapeSite False "longestCollector"   (XC.getLongestFlightsAt collector   1000  5)
  scrapeSite False "longestLakeGeorge"  (XC.getLongestFlightsAt lakeGeorge  1000  5)
  scrapeSite False "longestLanyon"  (XC.getLongestFlightsAt lanyon  5000  5)
  scrapeSite False "longestPigHill"     (XC.getLongestFlightsAt pigHill     5000  5)
  scrapeSite False "longestHoneysuckle" (XC.getLongestFlightsAt honeysuckle 5000  5)
  scrapeSite False "longestBowning"     (XC.getLongestFlightsAt bowning     5000  5)
  scrapeSite False "longestArgalong"    (XC.getLongestFlightsAt argalong    5000  5)
  scrapeSite False "longestCastleHill"  (XC.getLongestFlightsAt castleHill  5000  5)
  scrapeSite False "longestBooroomba"   (XC.getLongestFlightsAt booroomba   5000  5)
  scrapeSite False "longestCarols"      (XC.getLongestFlightsAt carols      5000  5)

  scrapeSite True "recentCanberra" (XC.getRecentFlightsAt canberra 75000 20)
