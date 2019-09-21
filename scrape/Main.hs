{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Network.HTTP.Req (Req, defaultHttpConfig, runReq)

import Flights (Flight, argalong, booroomba, bowning, canberra, carols,
                castleHill, collector, getLongestFlightsAt, getRecentFlightsAt,
                honeysuckle, lakeGeorge, lanyon, pigHill, renderFlights,
                springHill)

import qualified Data.Text.IO as TIO

scrapeSite :: Bool -> String -> Req [Flight] -> IO ()
scrapeSite showSite fileName getFlights = do
  let file = "site/scraped/" <> fileName <> ".html"
  putStrLn $ "Populating '" <> file <> "'"
  flights <- runReq defaultHttpConfig getFlights
  putStrLn $ "Found " <> show (length flights) <> " flights."
  TIO.writeFile
    file
    (renderFlights showSite flights)

main :: IO ()
main = do
  scrapeSite True "longestCanberra"    (getLongestFlightsAt canberra    75000 40)
  scrapeSite False "longestSpringHill"  (getLongestFlightsAt springHill  5000  5)
  scrapeSite False "longestCollector"   (getLongestFlightsAt collector   1000  5)
  scrapeSite False "longestLakeGeorge"  (getLongestFlightsAt lakeGeorge  1000  5)
  scrapeSite False "longestLanyon"  (getLongestFlightsAt lanyon  5000  5)
  scrapeSite False "longestPigHill"     (getLongestFlightsAt pigHill     5000  5)
  scrapeSite False "longestHoneysuckle" (getLongestFlightsAt honeysuckle 5000  5)
  scrapeSite False "longestBowning"     (getLongestFlightsAt bowning     5000  5)
  scrapeSite False "longestArgalong"    (getLongestFlightsAt argalong    5000  5)
  scrapeSite False "longestCastleHill"  (getLongestFlightsAt castleHill  5000  5)
  scrapeSite False "longestBooroomba"   (getLongestFlightsAt booroomba   5000  5)
  scrapeSite False "longestCarols"      (getLongestFlightsAt carols      5000  5)

  scrapeSite True "recentCanberra" (getRecentFlightsAt canberra 75000 40)
