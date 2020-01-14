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

import Control.Lens                  (makeLenses)
import Control.Lens.Operators
import Control.Monad                 (when)
import Control.Monad.Catch.Pure      (runCatch)
import Data.Either.Combinators       (isRight, rightToMaybe)
import Data.Fixed                    (Centi)
import Data.Geodetic.LL              (LL (..), lat, lon)
import Data.Text                     (Text)
import Data.Time                     (Day, defaultTimeLocale, formatTime)
import Formatting                    (format, (%))
import Formatting.Formatters         (float)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              (Html, a, table, td, text, textValue, th,
                                      tr, (!))
import Text.Blaze.Html5.Attributes   (href)

import Text.URI      (Authority (..), URI, makeAbsolute, mkHost, mkScheme,
                      mkURI, render)
import Text.URI.Lens (uriAuthority)

import qualified Data.Text      as T
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

formatLL :: LL -> L.Text
formatLL loc = format (float % " " % float) (loc ^. lat) (loc ^. lon)

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

