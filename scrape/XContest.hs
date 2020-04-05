{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module XContest
  ( xContestFlights
  , getFlightsAt
  , getLongestFlightsAt
  , getRecentFlightsAt
  , flightsFromHtml
  , flightFromFlightRow
  , pilotFromFlightRow
  , rowPilotName
  , cellFlightKms
  , rowFlightDate
  , rowFlightUrl
  , cellSiteName
  , cellAircraftName
  , cellAircraftType
  , xContestDay
  ) where

import Control.Lens           (Fold, Traversal', at, ix, only, to, _Show)
import Control.Lens.Operators
import Data.Maybe             (mapMaybe)
import Data.Text              (Text)
import Data.Text.Lens         (unpacked)
import Data.Time              (Day)
import Network.HTTP.Req       (MonadHttp, Option, Scheme (Https))
import Text.Taggy.Lens        (Element, allAttributed, allNamed, attr,
                               attributed, attrs, children, content, contents,
                               element, elements, html, named)

import qualified Data.Text      as T
import qualified Data.Text.Lazy as L

import Flights
import XContest.Browse
import XContest.Parse

getFlightsAt :: MonadHttp m => Text -> Int -> Site -> m [Flight]
getFlightsAt sort num site =
  getFlightsHtmlAt sort site <&> flightsFromHtml <&> take num

getLongestFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getLongestFlightsAt site num =
  getLongestFlightsHtmlAt site <&> flightsFromHtml <&> take num

getRecentFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getRecentFlightsAt site num =
  getRecentFlightsHtmlAt site <&> flightsFromHtml <&> take num

xContestFlights :: MonadHttp m => Option 'Https -> m [Flight]
xContestFlights = fmap flightsFromHtml . xContestFlightsHtml

flightsFromHtml :: L.Text -> [Flight]
flightsFromHtml t = t ^.. html
  . allNamed (only "table")
  . allAttributed (ix "class" . only "flights")
  . allNamed (only "tr")
  & mapMaybe flightFromFlightRow

flightFromFlightRow :: Element -> Maybe Flight
flightFromFlightRow row =
  let cells = row ^.. elements . allNamed (only "td")
      url = makeAbsoluteUrlOf xContestOrg =<< row ^? rowFlightUrl
  in Flight
    <$> pilotFromFlightRow row
    <*> cells ^? traverse . cellSiteName
    <*> cells ^? traverse . cellFlightKms
    <*> (Aircraft <$> cells ^? cellAircraftType <*> Just (cells ^? cellAircraftName))
    <*> row ^? rowFlightDate
    <*> url

pilotFromFlightRow :: Element -> Maybe Pilot
pilotFromFlightRow row =
  mkPilot <$> row ^? rowPilotName

rowPilotName :: Fold Element Text
rowPilotName = allAttributed (ix "class" . only "plt")
  . element
  . contents

cellFlightKms :: Traversal' Element Distance
cellFlightKms = attributed (ix "class" .  only "km")
  . elements
  . named (only "strong")
  . contents
  . unpacked
  . _Show

cellSiteName :: Fold Element SiteName
cellSiteName = allNamed (only "a")
  . attributed (ix "class" . only "lau")
  . contents
  . to SiteName

cellAircraftType :: Fold [Element] AircraftType
cellAircraftType =
  ix 7
  . elements
  . named (only "div")
  . attr "class"
  . to xcAircraftType
  where
    xcAircraftType :: Maybe Text -> AircraftType
    xcAircraftType Nothing = Paraglider
    xcAircraftType (Just t) =
      case T.unpack t of
        ('h':'g':_) -> HangGlider
        _           -> Paraglider

cellAircraftName :: Fold [Element] AircraftName
cellAircraftName =
  ix 7
  . elements
  . named (only "div")
  . attr "title"
  . traverse
  . to mkAircraftName

rowFlightDate :: Traversal' Element Day
rowFlightDate = children
  . ix 1
  . element
  . children
  . ix 0
  . content
  . xContestDay

rowFlightUrl :: Fold Element Text
rowFlightUrl = children
  . ix 9
  . allNamed (only "a")
  . attrs . at "href" . traverse

