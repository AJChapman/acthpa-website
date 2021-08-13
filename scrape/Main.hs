{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Lens           (_Wrapped)
import Control.Lens.Operators
import Control.Monad          (void)
import Control.Monad.IO.Class
import Data.Text              (Text)
import Formatting
import Network.HTTP.Client    (CookieJar)
import Network.HTTP.Req       (MonadHttp, defaultHttpConfig, runReq)
import Prelude                hiding (log)
import System.Directory       (createDirectoryIfMissing)
import System.FilePath        ((</>))

import Flights

import qualified Data.Text.IO       as TIO
import qualified Leonardo           as LEO
import           XContest.Browse    (xContestLogin)
import           XContest.Passwords
import qualified XContest2          as XC

scrapeSite :: MonadHttp m => (Text -> m ()) -> Bool -> String -> m [Flight] -> m ()
scrapeSite log showSite fileName getFlights = do
  let scrapeDir = "site/scraped"
  void $ liftIO $ createDirectoryIfMissing False scrapeDir
  let file = scrapeDir </> fileName <> ".html"
  log $ sformat ("Populating '"%string%"'") file
  flights <- getFlights
  log $ sformat ("Found "%int%" flights.") $ length flights
  sequence_ $ log . sformat formatFlight <$> flights
  liftIO $ TIO.writeFile
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

getLongestFlightsAt :: MonadHttp m => CookieJar -> Site -> Int -> m [Flight]
getLongestFlightsAt xcCookies site n = do
  flights <- getFlightsAt
    [ XC.getLongestFlightsAt xcCookies
    , LEO.getLongestFlightsAt
    ] site n
  pure . take n . removeConsecutiveSimilar . sortFlightsByLengthDesc $ flights

getRecentFlightsAt :: MonadHttp m => CookieJar -> Site -> Int -> m [Flight]
getRecentFlightsAt xcCookies site n = do
  flights <- getFlightsAt
    [ XC.getRecentFlightsAt xcCookies
    , LEO.getRecentFlightsAt
    ] site n
  pure . take n  . removeConsecutiveSimilar . sortFlightsByDateDesc $ flights

main :: IO ()
main = runReq defaultHttpConfig $ do
  xcCookies <- xContestLogin xContestUsername xContestPassword
  let scrape = scrapeSite (liftIO . TIO.putStrLn)
      getRecent = getRecentFlightsAt xcCookies
      getLongest = getLongestFlightsAt xcCookies
  scrape True "longestCanberra"     (getLongest canberra    20)
  scrape False "longestSpringHill"  (getLongest springHill  10)
  scrape False "longestCollector"   (getLongest collector   10)
  scrape False "longestLakeGeorge"  (getLongest lakeGeorge  10)
  scrape False "longestLanyon"      (getLongest lanyon      5)
  scrape False "longestPigHill"     (getLongest pigHill     5)
  scrape False "longestHoneysuckle" (getLongest honeysuckle 5)
  scrape False "longestBowning"     (getLongest bowning     5)
  scrape False "longestArgalong"    (getLongest argalong    5)
  scrape False "longestCastleHill"  (getLongest castleHill  5)
  scrape False "longestBooroomba"   (getLongest booroomba   5)
  scrape False "longestCarols"      (getLongest carols      5)

  scrape True "recentCanberra" (getRecent canberra 20)
