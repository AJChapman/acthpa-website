{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
module Page
  ( Page(..)
  , pageTitle
  , pageContent
  , pageUrl
  , pageTeaser
  , pageImage
  , pageAuthor
  , pagePublished
  , pageEventStart
  , pageEventFinish
  , pathToRootPath
  , pageFilePath
  , relativeUrl
  , pageRelativeUrl
  , pageRelativizeUrl
  , isActivePage
  ) where

import Control.Lens                 (makeLenses)
import Control.Lens.Operators
import Data.Aeson                   (FromJSON, ToJSON)
import Data.Aeson.Generic.Shorthand (CamelFields, GenericToFromJSON (..))
import Data.Binary.Instances.Time   ()
import Data.Text                    (Text)
import Data.Text.Lens               (unpacked)
import Data.Time                    (Day)
import Development.Shake.Classes    (Binary)
import Development.Shake.FilePath   ((</>))
import GHC.Generics                 (Generic)
import System.FilePath              (joinPath, splitDirectories)

import qualified Data.Text as T

-- | Page is the data structure that we load from MarkDown files.
-- It has a lot of `Maybe` fields, because most metadata is optional.
-- We also keep it around in other data structures (for now).
data Page = Page
  { _pageTitle       :: Text
  , _pageContent     :: Text
  , _pageUrl         :: Text -- E.g. "info/faq", with the filepath for that url being "info/faq/index.html"
  , _pageTeaser      :: Maybe Text
  , _pageImage       :: Maybe Text
  , _pageAuthor      :: Maybe Text
  , _pagePublished   :: Maybe Day
  , _pageEventStart  :: Maybe Day
  , _pageEventFinish :: Maybe Day
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Page)
$(makeLenses ''Page)

pathToRootPath :: FilePath -> FilePath
pathToRootPath p =
  let depth = length (splitDirectories p) - 1
  in joinPath . take depth $ repeat "../"

pageFilePath :: Page -> FilePath
pageFilePath page =
  -- We change /foo/bar.html into /foo/bar/index.html to give it a clean path (when a server serves the index automatically)
  page ^. pageUrl . unpacked </> "index.html"

relativeUrl :: Page -> FilePath -> FilePath
relativeUrl from to =
  pathToRootPath (pageFilePath from) </> to

pageRelativeUrl :: Page -> Page -> FilePath
pageRelativeUrl from to =
  relativeUrl from (to ^. pageUrl . unpacked)

pageRelativizeUrl :: Page -> Page -> Page
pageRelativizeUrl from to =
  to & pageUrl . unpacked %~ relativeUrl from

isActivePage :: Page -> Page -> Bool
isActivePage activePage page =
  (page ^. pageUrl) `T.isPrefixOf` (activePage ^. pageUrl)

