{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Flights
  ( Pilot(..)
  , pilotName
  , Site(..)
  , siteName
  , Distance(..)
  , distanceKm
  , Flight(..)
  , flightPilot
  , flightSite
  , flightDistance
  , flightDate
  , flightUrl
  , xContestFlights
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
  , renderFlights
  , canberra
  , springHill
  , collector
  , lakeGeorge
  , lanyon
  , pigHill
  , honeysuckle
  , bowning
  , argalong
  , castleHill
  , booroomba
  , carols
  , formatLL
  , makeAbsoluteUrlOf
  ) where

import Prelude hiding (div, head, id)

import Control.Lens                  (Fold, Prism', Traversal', at, ix,
                                      makeLenses, only, prism, to, (&), (.~),
                                      (^.), (^..), (^?), _Show)
import Control.Monad                 (when)
import Control.Monad.Catch.Pure      (runCatch)
import Control.Monad.IO.Class        (liftIO)
import Data.Either.Combinators       (rightToMaybe, isRight)
import Data.Fixed                    (Centi)
import Data.Geodetic.LL              (LL (..), lat, lon)
import Data.Maybe                    (mapMaybe)
import Data.Text                     (Text)
import Data.Text.Encoding.Error      (lenientDecode)
import Data.Text.Lazy.Encoding       (decodeUtf8With)
import Data.Text.Lens                (unpacked)
import Data.Time                     (Day, defaultTimeLocale, formatTime,
                                      parseTimeM)
import Formatting                    (format, (%))
import Formatting.Formatters         (float)
import Network.HTTP.Req              (GET (..), MonadHttp, NoReqBody (..),
                                      Option, Scheme (Https), Url, https,
                                      lbsResponse, req, req', responseBody,
                                      (/:), (=:))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              (Html, a, table, td, text, textValue, th,
                                      tr, (!))
import Text.Blaze.Html5.Attributes   (href)
import Text.Taggy.Lens               (Element, allAttributed, allNamed,
                                      attributed, attrs, children, content,
                                      contents, element, elements, html, named)
import Text.URI                      (Authority (..), URI, isPathAbsolute,
                                      makeAbsolute, mkHost, mkScheme, mkURI,
                                      render)
import Text.URI.Lens                 (authHost, uriAuthority)

import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO
import qualified Data.Text.Lazy as L

newtype Pilot = Pilot
  { _pilotName       :: Text
  } deriving (Eq, Ord, Show)

$(makeLenses ''Pilot)

newtype Site = Site
  { _siteName :: Text
  } deriving (Eq, Ord, Show)

$(makeLenses ''Site)

newtype Distance = Km
  { _distanceKm :: Centi
  } deriving (Eq, Ord)
  deriving newtype (Show, Read)

$(makeLenses ''Distance)

data Flight = Flight
  { _flightPilot    :: Pilot
  , _flightSite     :: Site
  , _flightDistance :: Distance
  , _flightDate     :: Day
  , _flightUrl      :: URI
  } deriving (Eq, Ord, Show)

$(makeLenses ''Flight)

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

makeAbsoluteUrlOf :: Text -> Maybe Text -> Maybe URI
makeAbsoluteUrlOf _ Nothing = Nothing
makeAbsoluteUrlOf host' (Just path) = rightToMaybe . runCatch $ do
  uri <- mkURI path
  if uri ^. uriAuthority & isRight
    then pure uri
    else do
      scheme <- mkScheme "https"
      host <- mkHost host'
      let authority = Authority Nothing host Nothing
      pure $ uri & uriAuthority .~ (Right authority)
                 & makeAbsolute scheme

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

canberra, lakeGeorge, lanyon, collector, bowning, honeysuckle, carols, springHill, pigHill, argalong, castleHill, booroomba :: LL
canberra    = LL 149.104562 (-35.300332)
springHill  = LL 149.08332 (-35.09403)
collector   = LL 149.37467 (-34.97003)
lakeGeorge  = LL 149.37425 (-35.09573)
lanyon      = LL 149.1076 (-35.4837)
-- gearys      = LL 149.37425 (-35.09573)
pigHill     = LL 148.89708 (-35.22075)
honeysuckle = LL 148.318 (-35.1354)
bowning     = LL 148.83158 (-34.77817)
argalong    = LL 148.36 (-35.3117)
castleHill  = LL 149.034 (-35.47985)
booroomba   = LL 149.00287 (-35.49765)
carols      = LL 148.679 (-34.9144)

xContestFlightSearchUrl :: Url 'Https
xContestFlightSearchUrl =
  https xContestOrg /: "world" /: "en" /: "flights-search"

formatLL :: LL -> L.Text
formatLL loc = format (float % " " % float) (loc ^. lat) (loc ^. lon)

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

renderDate :: Day -> Text
renderDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

renderDistance :: Distance -> Html
renderDistance d = (d ^. distanceKm & show & T.pack) <> "km" & text

flightRow :: Bool -> Flight -> Html
flightRow showSite flight = tr $ do
  td . text $ flight ^. flightPilot . pilotName
  when showSite (td . text $ flight ^. flightSite . siteName)
  td $ a ! href (flight ^. flightUrl & render & textValue) $ flight ^. flightDistance & renderDistance
  td . text $  flight ^. flightDate & renderDate

renderFlights :: Bool -> [Flight] -> Text
renderFlights showSite flights =
  L.toStrict . renderHtml . table $ do
    tr $ do
      th "Name"
      when showSite $ th "Site"
      th "Distance"
      th "Date"
    mapM_ (flightRow showSite) flights
