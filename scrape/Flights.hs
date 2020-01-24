{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Flights
  ( Pilot
  , mkPilot
  , pilotName
  , SiteName(..)
  , Distance(..)
  , distanceKm
  , LeonardoId(..)
  , Site(..)
  , siteName
  , siteLocation
  , siteRadius
  , siteLeonardoId
  , AircraftType(..)
  , AircraftName
  , mkAircraftName
  , Aircraft(..)
  , HasAircraft(..)
  , Flight(..)
  , flightPilot
  , flightSiteName
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
  , arboretum
  , binalong
  , formatLL
  , makeAbsoluteUrlOf
  , sortFlightsByLengthAsc
  , sortFlightsByLengthDesc
  , sortFlightsByDateAsc
  , sortFlightsByDateDesc
  , removeConsecutiveBy
  , removeConsecutiveSimilar
  ) where

import Prelude hiding (div, head, id)

import Control.Lens                  (makeClassy, makeLenses, makeWrapped, to,
                                      view, _Wrapped)
import Control.Lens.Operators
import Control.Monad                 (when)
import Control.Monad.Catch.Pure      (runCatch)
import Data.Either.Combinators       (isRight, rightToMaybe)
import Data.Fixed                    (Centi)
import Data.Geodetic.LL              (LL (..), lat, lon)
import Data.List                     (sortOn)
import Data.Maybe                    (fromMaybe)
import Data.String                   (IsString)
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

class Similar a where
  isSimilarTo :: a -> a -> Bool

  (~=) :: a -> a -> Bool
  (~=) = isSimilarTo

instance Similar Day where
  isSimilarTo = (==)

newtype Pilot = Pilot
  { _pilotName       :: Text
  } deriving (Eq, Ord, Show)
$(makeLenses ''Pilot)

mkPilot :: Text -> Pilot
mkPilot = Pilot . T.toTitle . T.strip

instance Similar Pilot where
  isSimilarTo = (==)

newtype SiteName = SiteName Text
  deriving (Eq, Ord, Show, IsString)
$(makeWrapped ''SiteName)

instance Similar SiteName where
  isSimilarTo = (==)

newtype Distance = Km
  { _distanceKm :: Centi
  } deriving newtype (Eq, Show, Read, Num, Ord)
$(makeLenses ''Distance)

distanceTolerance :: Distance
distanceTolerance = Km 0.3

instance Similar Distance where
  isSimilarTo lhs rhs =
    (< distanceTolerance) . abs $ lhs - rhs

data AircraftType = Paraglider | HangGlider
  deriving (Eq, Ord)

instance Show AircraftType where
  show Paraglider = "Paraglider"
  show HangGlider = "Hang Glider"

showAircraftType :: AircraftType -> Text
showAircraftType = T.pack . show

instance Similar AircraftType where
  isSimilarTo = (==)

newtype AircraftName = AircraftName
  { _aircraftNameText :: Text
  }
  deriving (Eq, Ord, Show, IsString)
$(makeLenses ''AircraftName)

mkAircraftName :: Text -> AircraftName
mkAircraftName = AircraftName . T.toTitle . T.strip

instance Similar AircraftName where
  isSimilarTo = (==)

data Aircraft = Aircraft
  { _aircraftType :: AircraftType
  , _aircraftName :: Maybe AircraftName
  } deriving (Eq, Ord, Show)
$(makeClassy ''Aircraft)

instance Similar Aircraft where
  isSimilarTo lhs rhs =
    (lhs ^. aircraftName) == (rhs ^. aircraftName)
    && (lhs ^. aircraftType) == (rhs ^. aircraftType)

data Flight = Flight
  { _flightPilot    :: Pilot
  , _flightSiteName :: SiteName
  , _flightDistance :: Distance
  , _flightAircraft :: Aircraft
  , _flightDate     :: Day
  , _flightUrl      :: URI
  } deriving (Eq, Ord, Show)
$(makeLenses ''Flight)

instance Similar Flight where
  isSimilarTo lhs rhs =
    -- If the flights have the same URL then they are definitely the same, but if they don't
    -- then they may still be the same, e.g. same flight from different websites.
    (lhs ^. flightUrl) == (rhs ^. flightUrl)
    || (
      (lhs ^. flightPilot) ~= (rhs ^. flightPilot)
      -- NB: We're not comparing site name, as this can differ.
      -- We should change this once we know the site's location,
      -- but this doesn't matter too much, as what are the chances that the same
      -- pilot flies the same distance on the same day from two different sites?
      && (lhs ^. flightDistance) ~= (rhs ^. flightDistance)
      && (lhs ^. flightDate) ~= (rhs ^. flightDate)
    )

data LeonardoId = Unknown | SiteId Int | ClubId Text
  deriving (Eq, Ord, Show)

data Site = Site
  { _siteName       :: SiteName
  , _siteLocation   :: LL
  , _siteRadius     :: Int
  , _siteLeonardoId :: LeonardoId
  } deriving (Eq, Ord, Show)
$(makeLenses ''Site)

canberra, lakeGeorge, lanyon, collector, bowning, honeysuckle, carols, springHill, pigHill, argalong, castleHill, booroomba, binalong, arboretum :: Site
canberra    = Site "Canberra"                   (LL 149.104562 (-35.300332)) 75000 (ClubId "0.15")
springHill  = Site "Spring Hill"                (LL 149.08332 (-35.09403))    5000 (SiteId 11640) -- also 42740?
collector   = Site "Collector"                  (LL 149.37467 (-34.97003))    1000 (SiteId 13734)
lakeGeorge  = Site "Lake George (South Launch)" (LL 149.37425 (-35.09573))    1000 (SiteId 43074) -- also 11642, 60895?
lanyon      = Site "Lanyon"                     (LL 149.1076 (-35.4837))      5000 (SiteId 32541)
-- gearys      = Site ""                          (LL 149.37425 (-35.09573))  5000   (SiteId 13644)
pigHill     = Site "Pig Hill"                   (LL 148.89708 (-35.22075))    5000 (SiteId 11641) -- Also 42741?
honeysuckle = Site "Honeysuckle (Tumut)"        (LL 148.318 (-35.1354))       5000 (SiteId 32741)
bowning     = Site "Bowning"                    (LL 148.83158 (-34.77817))    5000 Unknown
argalong    = Site "Argalong"                   (LL 148.36 (-35.3117))        5000 (SiteId 32895)
castleHill  = Site "Castle Hill"                (LL 149.034 (-35.47985))      5000 Unknown
booroomba   = Site "Booroomba"                  (LL 149.00287 (-35.49765))    5000 Unknown
carols      = Site "Carol's"                    (LL 148.679 (-34.9144))       5000 (SiteId 13634)
binalong    = Site "Binalong"                   (LL 148.587 (-34.6396))       5000 (SiteId 18914)
arboretum   = Site "Canberra Arboretum"         (LL 149.081 (-35.2906))       5000 (SiteId 54670)

formatLL :: LL -> L.Text
formatLL loc = format (float % " " % float) (loc ^. lat) (loc ^. lon)

renderDate :: Day -> Text
renderDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

renderDistance :: Distance -> Html
renderDistance d = (d ^. distanceKm & show & T.pack) <> "km" & text

flightRow :: Bool -> Flight -> Html
flightRow showSite flight = tr $ do
  td . text $ flight ^. flightPilot . pilotName
  when showSite (td . text $ flight ^. flightSiteName . _Wrapped)
  td . text $ flight ^. flightAircraft . aircraftType . to showAircraftType
  td . text . fromMaybe "" $ flight ^? flightAircraft . aircraftName . traverse . aircraftNameText
  td $ a ! href (flight ^. flightUrl & render & textValue) $ flight ^. flightDistance & renderDistance
  td . text $  flight ^. flightDate & renderDate

renderFlights :: Bool -> [Flight] -> Text
renderFlights showSite flights =
  L.toStrict . renderHtml . table $ do
    tr $ do
      th "Name"
      when showSite $ th "Site"
      th "Aircraft"
      th "Model"
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
      pure $ uri & uriAuthority .~ Right authority
                 & makeAbsolute scheme

sortFlightsByLengthDesc :: [Flight] -> [Flight]
sortFlightsByLengthDesc = reverse . sortFlightsByLengthAsc

sortFlightsByLengthAsc :: [Flight] -> [Flight]
sortFlightsByLengthAsc = sortOn (view flightDistance)

sortFlightsByDateDesc :: [Flight] -> [Flight]
sortFlightsByDateDesc = reverse . sortFlightsByDateAsc

sortFlightsByDateAsc :: [Flight] -> [Flight]
sortFlightsByDateAsc = sortOn (view flightDate)

removeConsecutiveBy :: (a -> a -> Bool) -> [a] -> [a]
removeConsecutiveBy f (x:y:xs) =
  if f x y
    then removeConsecutiveBy f (x : xs) -- Retain the first in the list (arbitrary decision)
    else x : removeConsecutiveBy f (y:xs)
removeConsecutiveBy _ lst = lst

removeConsecutiveSimilar :: Similar a => [a] -> [a]
removeConsecutiveSimilar = removeConsecutiveBy isSimilarTo
