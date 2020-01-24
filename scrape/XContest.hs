{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module XContest
  ( xContestFlights
  , flightsFromHtml
  , getFlightsAt
  , getLongestFlightsAt
  , getRecentFlightsAt
  , flightFromFlightRow
  , pilotFromFlightRow
  , rowPilotName
  , cellFlightKms
  , rowFlightDate
  , rowFlightUrl
  , cellSiteName
  , cellAircraftName
  , cellAircraftType
  ) where

import Control.Lens             (Fold, Prism', Traversal', at, from, ix, only,
                                 prism, to, _Show, _Wrapped)
import Control.Lens.Operators
import Control.Monad.IO.Class   (liftIO)
import Data.Maybe               (mapMaybe)
import Data.Text                (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Data.Text.Lens           (unpacked)
import Data.Time                (Day, defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Req         (GET (..), MonadHttp, NoReqBody (..), Option,
                                 Scheme (Https), Url, https, lbsResponse, req,
                                 req', responseBody, (/:), (=:))
import Text.Taggy.Lens          (Element, allAttributed, allNamed, attr,
                                 attributed, attrs, children, content, contents,
                                 element, elements, html, named)

import qualified Data.Text as T
-- import qualified Data.Text.IO   as TIO
import qualified Data.Text.Lazy as L

import Flights

xContestFlights :: MonadHttp m => Option 'Https -> m [Flight]
xContestFlights opts = do
  req' GET xContestFlightSearchUrl NoReqBody opts (\rq _ -> liftIO $ print rq)
  r <- req GET
    xContestFlightSearchUrl
    NoReqBody
    lbsResponse
    opts
  let page = r & responseBody & decodeUtf8With lenientDecode
  -- liftIO $ TIO.writeFile "result.html" (L.toStrict page)
  page & flightsFromHtml & pure

flightsFromHtml :: L.Text -> [Flight]
flightsFromHtml t = t ^.. html
  . allNamed (only "table")
  . allAttributed (ix "class" . only "flights")
  . allNamed (only "tr")
  & mapMaybe flightFromFlightRow

flightFromFlightRow :: Element -> Maybe Flight
flightFromFlightRow row =
  let cells = row ^.. elements . allNamed (only "td")
      url = makeAbsoluteUrlOf xContestOrg (row ^? rowFlightUrl)
  in Flight
    <$> pilotFromFlightRow row
    <*> cells ^? traverse . cellSiteName
    <*> cells ^? traverse . cellFlightKms
    <*> (Aircraft <$> cells ^? cellAircraftType <*> Just (cells ^? cellAircraftName))
    <*> row ^? rowFlightDate
    <*> url

pilotFromFlightRow :: Element -> Maybe Pilot
pilotFromFlightRow row =
  Pilot <$> row ^? rowPilotName

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

cellAircraftName :: Traversal' [Element] AircraftName
cellAircraftName =
  ix 7
  . elements
  . named (only "div")
  . attr "title"
  . traverse
  . from _Wrapped

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

xContestDay :: Prism' Text Day
xContestDay =
  let dateFmt = "%d.%m.%y"
      locale = defaultTimeLocale -- TODO: AEST?
  in prism
       (T.pack . formatTime locale dateFmt)
       (parseTimeM True locale dateFmt . T.unpack)

xContestOrg :: Text
xContestOrg = "www.xcontest.org"

xContestOptions :: Text -> Site -> Option scheme
xContestOptions sort site =
  let radiusInMetres = site ^. siteRadius
  in "filter[point]"            =: formatLL (site ^. siteLocation)
    <> "filter[radius]"        =: (radiusInMetres :: Int)
    -- <> "filter[mode]"          =: ("START" :: Text)
    -- <> "filter[date_mode]"     =: ("dmy" :: Text)
    -- <> "filter[date]"          =: ("" :: Text)
    -- <> "filter[value_mode]"    =: ("dst" :: Text)
    -- <> "filter[min_value_dst]" =: ("" :: Text)
    -- <> "filter[catg]"          =: ("" :: Text)
    -- <> "filter[route_types]"   =: ("" :: Text)
    -- <> "filter[avg]"           =: ("" :: Text)
    -- <> "filter[pilot]"         =: ("" :: Text)
    <> "list[sort]"            =: sort -- pts, time_start
    <> "list[dir]"             =: ("down" :: Text)

getFlightsAt :: MonadHttp m => Text -> Site -> Int -> m [Flight]
getFlightsAt sort site top =
  take top <$> xContestFlights (xContestOptions sort site)

getLongestFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getLongestFlightsAt = getFlightsAt "pts"

getRecentFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getRecentFlightsAt = getFlightsAt "time_start"

xContestFlightSearchUrl :: Url 'Https
xContestFlightSearchUrl =
  https xContestOrg /: "world" /: "en" /: "flights-search"

