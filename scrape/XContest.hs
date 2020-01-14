{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module XContest
  ( xContestFlights
  , getFlightsAt
  , getLongestFlightsAt
  , getRecentFlightsAt
  , flightFromFlightRow
  , pilotFromFlightRow
  , rowPilotName
  , cellFlightKms
  , rowFlightTime
  , rowFlightUrl
  , xContestDay
  ) where

import Control.Lens             (Fold, Prism', Traversal', at, ix, only, prism,
                                 to, _Show)
import Control.Lens.Operators
import Control.Monad.IO.Class   (liftIO)
import Data.Geodetic.LL         (LL (..))
import Data.Maybe               (mapMaybe)
import Data.Text                (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Data.Text.Lens           (unpacked)
import Data.Time                (Day, defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Req         (GET (..), MonadHttp, NoReqBody (..), Option,
                                 Scheme (Https), Url, https, lbsResponse, req,
                                 req', responseBody, (/:), (=:))
import Text.Taggy.Lens          (Element, allAttributed, allNamed, attributed,
                                 attrs, children, content, contents, element,
                                 elements, html, named)

import qualified Data.Text      as T
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
  let cells = row ^.. allNamed (only "td")
      url = makeAbsoluteUrlOf xContestOrg (row ^? rowFlightUrl)
  in Flight
    <$> pilotFromFlightRow row
    <*> cells ^? traverse . cellSite
    <*> cells ^? traverse . cellFlightKms
    <*> row ^? rowFlightTime
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

cellSite :: Fold Element Site
cellSite = allNamed (only "a")
  . attributed (ix "class" . only "lau")
  . contents
  . to Site

rowFlightTime :: Traversal' Element Day
rowFlightTime = children
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

xContestOptions :: Text -> LL -> Int -> Option scheme
xContestOptions sort loc radiusInMetres =
  "filter[point]"            =: formatLL loc
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

getFlightsAt :: MonadHttp m => Text -> LL -> Int -> Int -> m [Flight]
getFlightsAt sort loc radiusInMetres top =
  take top <$> xContestFlights (xContestOptions sort loc radiusInMetres)

getLongestFlightsAt :: MonadHttp m => LL -> Int -> Int -> m [Flight]
getLongestFlightsAt = getFlightsAt "pts"

getRecentFlightsAt :: MonadHttp m => LL -> Int -> Int -> m [Flight]
getRecentFlightsAt = getFlightsAt "time_start"

xContestFlightSearchUrl :: Url 'Https
xContestFlightSearchUrl =
  https xContestOrg /: "world" /: "en" /: "flights-search"

