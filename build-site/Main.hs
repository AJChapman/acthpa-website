{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main
  ( main
  , Home(..)
  ) where

import Control.Lens                 (at, ix, makeLenses, (&), (.~), (<&>), (?~),
                                     (^.), (^?))
import Control.Monad                (void)
import Data.Aeson                   (FromJSON, ToJSON, Value (..), encode,
                                     object, toJSON, (.=))
import Data.Aeson.Generic.Shorthand (CamelFields, GenericToFromJSON (..))
import Data.Aeson.Lens              (_Object, _String)
import Data.Binary.Instances.Time   ()
import Data.Text                    (Text)
import Data.Time                    (Day, getZonedTime, localDay,
                                     zonedTimeToLocalTime)
import Development.Shake            (Action, copyFileChanged, forP,
                                     getDirectoryFiles, liftIO, readFile',
                                     writeFile')
import Development.Shake.Classes    (Binary)
import Development.Shake.FilePath   (dropDirectory1, (-<.>), (</>))
import Development.Shake.Forward    (cacheAction)
import GHC.Generics                 (Generic)
import Slick                        (compileTemplate', convert, markdownToHTML,
                                     slick, substitute)
import System.FilePath              (joinPath, splitDirectories)
import Text.Mustache                (Template, checkedSubstitute,
                                     compileTemplate)
import Text.Mustache.Types          (mFromJSON)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

outputFolder :: FilePath
outputFolder = "gen/"

-- | Page is the data structure that we load from MarkDown files.
-- It has a lot of `Maybe` fields, because most metadata is optional.
-- We also keep it around in other data structures (for now).
data Page = Page
  { _pageTitle       :: Text
  , _pageContent     :: Text
  , _pageUrl         :: Text
  , _pageTeaser      :: Maybe Text
  , _pageImage       :: Maybe Text
  , _pageAuthor      :: Maybe Text
  , _pagePublished   :: Maybe Day
  , _pageEventStart  :: Maybe Day
  , _pageEventFinish :: Maybe Day
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Page)
$(makeLenses ''Page)

data Home = Home
  { _homePage           :: Page
  , _homeUpcomingEvents :: [Page]
  , _homeLatestNews     :: [Page]
  , _homeRandomStory    :: [Page]
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Home)
$(makeLenses ''Home)

data EventList = EventList
  { _elPage   :: Page
  , _elEvents :: [Page]
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] EventList)
$(makeLenses ''EventList)

data Site = Site
  { _siteHome   :: Home
  , _siteNow    :: EventList
  , _siteFuture :: EventList
  , _sitePast   :: EventList
  -- , _siteInfo    :: Info
  -- , _siteAdvice  :: Advice
  -- , _siteStories :: Stories
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Site)

writeOutFileWithTemplate :: (ToJSON a) => Template -> FilePath -> a -> Action ()
writeOutFileWithTemplate template relPath obj =
  writeFile'
    (outputFolder </> relPath)
    (T.unpack $ renderWithTemplate (pathToRootPath relPath) template obj)

pathToRootPath :: FilePath -> FilePath
pathToRootPath p =
  let depth = length (splitDirectories p) - 1
  in joinPath . take depth $ repeat "../"

renderWithTemplate :: (ToJSON a) => FilePath -> Template -> a -> Text
renderWithTemplate pathToRoot t d =
  substitute t (toJSON d & _Object . at "root" ?~ String (T.pack pathToRoot))

copyStaticFiles :: Action ()
copyStaticFiles = do
  filePaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*", "fonts//*"]
  void $ forP filePaths $ \filePath ->
    copyFileChanged ("site" </> filePath) (outputFolder </> filePath)

loadPage :: FilePath -> Action Page
loadPage srcPath = cacheAction ("build" :: Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding page: " <> srcPath
  content <- readFile' srcPath
  -- load page content and metadata as JSON blob
  pageData <- markdownToHTML . T.pack $ content
  let url = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPageUrl = _Object . at "url" ?~ String url
      lTeaser = _Object . at "teaser" . traverse . _String
      teaser = pageData ^? lTeaser
  liftIO . putStrLn . show $ teaser
  teaser' <- traverse markdownToHTML teaser
  let teaser'' = teaser' ^? traverse . _Object . ix "content"
  liftIO . putStrLn . show $ teaser'
  let withTeaser = _Object . at "teaser" .~ teaser''
  -- Add additional metadata we've been able to compute
  let fullPageData = pageData & withPageUrl & withTeaser
  liftIO . putStrLn . show $ fullPageData
  convert fullPageData

data Tense = Future | Present | Past
  deriving (Eq, Ord, Show)

pageTense :: Day -> Page -> Maybe Tense
pageTense today Page{..} =
  case _pageEventStart of
    Nothing -> Nothing
    Just start ->
      if start > today
        then Just Future
        else case _pageEventFinish of
          Nothing -> Just Past
          Just finish ->
            if finish < today
              then Just Past
              else Just Present

inFuture, inPast, inPresent :: Day -> Page -> Bool
inFuture today page =
  pageTense today page == Just Future
inPresent today page =
  pageTense today page == Just Present
inPast today page =
  pageTense today page == Just Past

buildHome :: Home -> Action ()
buildHome home = do
  homeT <- compileTemplate' "site/templates/index.html"
  writeOutFileWithTemplate homeT "index.html" home

buildEvents :: FilePath -> EventList -> Action ()
buildEvents path EventList{..} = do
  eventListT <- compileTemplate' "site/templates/eventList.html"
  let eventListJson = (object [ "eventList" .= toJSON _elEvents ])
  eventList <- substitute' eventListT eventListJson
  defaultT <- compileTemplate' "site/templates/default.html"
  newContent <- substituteInContent (_elPage ^. pageContent) (object [ "eventList" .= eventList ])
  let page = _elPage & pageContent .~ newContent
  writeOutFileWithTemplate defaultT path page

substitute' :: ToJSON k => Template -> k -> Action Text
substitute' tpl v =
  let (errs, result) = checkedSubstitute tpl (mFromJSON v)
  in case errs of
    [] -> pure result
    _  -> fail $ show errs

substituteInContent :: ToJSON k => Text -> k -> Action Text
substituteInContent t v =
  let tpl = compileTemplate "" t
  in case tpl of
    Left err   -> fail $ show err
    Right tpl' -> substitute' tpl' v

buildSite :: Site -> Action ()
buildSite Site{..} = do
  buildHome _siteHome
  buildEvents "future/index.html" _siteFuture
  buildEvents "past/index.html" _sitePast

buildRules :: Action ()
buildRules = do
  copyStaticFiles
  eventPaths <- getDirectoryFiles "." ["site/events//*.md"]
  allEventPages <- forP eventPaths loadPage
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  let futureEvents  = filter (inFuture today) allEventPages
      pastEvents    = filter (inPast today) allEventPages
      currentEvents = filter (inPresent today) allEventPages
  homePage'  <- loadPage "site/index.md"
  nowPage    <- loadPage "site/now.md"
  futurePage <- loadPage "site/future.md"
  pastPage   <- loadPage "site/past.md"
  let home = Home
        homePage'
        (take 2 futureEvents)
        (take 2 pastEvents)
        [] -- TODO: random story
      site = Site
        home
        (EventList nowPage currentEvents)
        (EventList futurePage futureEvents)
        (EventList pastPage pastEvents)
  buildSite site

main :: IO ()
main = slick buildRules
