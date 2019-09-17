{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main
  ( main
  , tablesClass
  , elementsClass
  , addTableClassToTables
  , htmlChunks
  ) where

import Control.Lens        (Prism', Traversal', only, prism', re, toListOf,
                            (%~), (&), (?~), (^?), (<&>))
import Control.Lens.Plated (deep)
import Data.List           (isSuffixOf)
import Data.Text           (Text)
import Data.Text.Lens      (packed)
import Hakyll              hiding (template)
import Hakyll.Menu         (addToMenu, getMenu)
import System.FilePath     (takeBaseName, takeDirectory, (</>))
import Text.Pandoc         (ReaderOptions (..), def, githubMarkdownExtensions,
                            pandocExtensions)
import Text.Taggy.Lens     (Node, attr, children, element, html, named)

import qualified Data.Text.Lazy as L

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -av --delete _site/ farfromthere.net:acthpa.farfromthere.net/"
  }

main :: IO ()
main = acthpa 

acthpa :: IO ()
acthpa = hakyllWith config $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/**" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler

  defaultPage "activities.md"
  defaultPage "about.md"
  cleanPage "about/*"
  defaultPage "flying.md"
  defaultPage "flying/*"
  defaultPage "articles/*"
  match "articles.md" $ listingPage "articles/*"
  match "flying-ACT.md" $ listingPage "flying-ACT/*"
  defaultPage "flying-ACT/*"

  match "features/*" $ compile $
    contentContext >>= withTemplate "templates/feature.html"

  match "scraped/*" $ compile getResourceBody

  match "index.md" $ do
    addToMenu
    route (setExtension ".html")
    compile $ do
      ctx <- contentContext
      let features = loadAll "features/*"
          indexContext = listField "features" ctx features <> ctx
      withTemplate "templates/index.html" indexContext

defaultPage :: Pattern -> Rules ()
defaultPage p = match p $ do
  addToMenu
  cleanPageRules

cleanPageRules :: Rules ()
cleanPageRules = do
  route cleanRoute
  compile $ contentContext >>= withDefaultTemplate

cleanPage :: Pattern -> Rules ()
cleanPage p = match p cleanPageRules

listingPage :: Pattern -> Rules ()
listingPage items = do
  addToMenu
  route cleanRoute
  compile $ do
    ctx <- contentContext
    itemsContent <- loadAll (items .&&. hasNoVersion) -- Menu items show up, but have version "menu"
    let itemsContext =
          listField "items" ctx (pure itemsContent)
          <> ctx
    contentCompiler
      >>= loadAndApplyTemplate "templates/items-list.html" itemsContext
      >>= loadAndApplyTemplate "templates/default.html" itemsContext
      >>= relativizeUrls
      >>= cleanIndexUrls

defaultTemplate :: Identifier
defaultTemplate = "templates/default.html"

withDefaultTemplate :: Context String -> Compiler (Item String)
withDefaultTemplate = withTemplate defaultTemplate

withTemplate :: Identifier -> Context String -> Compiler (Item String)
withTemplate templatePath ctx =
  contentCompiler >>= applyTemplateAndFixUrls templatePath ctx

applyTemplateAndFixUrls :: Identifier -> Context String -> Item String -> Compiler (Item String)
applyTemplateAndFixUrls templatePath ctx item =
  item
    &   loadAndApplyTemplate templatePath ctx
    >>= relativizeUrls
    >>= cleanIndexUrls

contentCompiler :: Compiler (Item String)
contentCompiler =
  cached "Main.contentCompiler" $
  let readerOptions = def
        { readerExtensions = githubMarkdownExtensions <> pandocExtensions }
  in do
    ctx <- contentContext
    raw <- getResourceBody
    md <- applyAsTemplate ctx raw
    str <- readPandocWith readerOptions md
    saveSnapshot "content" (str & writePandoc <&> addTableClassToTables)

contentContext :: Compiler (Context String)
contentContext = do
  menu <- getMenu
  longestCanberra    <- loadBody "scraped/longestCanberra.html"
  longestSpringHill  <- loadBody "scraped/longestSpringHill.html"
  longestCollector   <- loadBody "scraped/longestCollector.html"
  longestLakeGeorge  <- loadBody "scraped/longestLakeGeorge.html"
  longestLanyon      <- loadBody "scraped/longestLanyon.html"
  longestPigHill     <- loadBody "scraped/longestPigHill.html"
  longestHoneysuckle <- loadBody "scraped/longestHoneysuckle.html"
  longestBowning     <- loadBody "scraped/longestBowning.html"
  longestArgalong    <- loadBody "scraped/longestArgalong.html"
  longestCastleHill  <- loadBody "scraped/longestCastleHill.html"
  longestBooroomba   <- loadBody "scraped/longestBooroomba.html"
  longestCarols      <- loadBody "scraped/longestCarols.html"
  recentCanberra     <- loadBody "scraped/recentCanberra.html"
  pure . mconcat $
    [ metadataField
    , defaultContext
    , constField "menu" menu
    , constField "longestCanberra"    longestCanberra 
    , constField "longestSpringHill"  longestSpringHill 
    , constField "longestCollector"   longestCollector 
    , constField "longestLakeGeorge"  longestLakeGeorge 
    , constField "longestLanyon"      longestLanyon 
    , constField "longestPigHill"     longestPigHill 
    , constField "longestHoneysuckle" longestHoneysuckle 
    , constField "longestBowning"     longestBowning 
    , constField "longestArgalong"    longestArgalong 
    , constField "longestCastleHill"  longestCastleHill 
    , constField "longestBooroomba"   longestBooroomba 
    , constField "longestCarols"      longestCarols
    , constField "recentCanberra"     recentCanberra
    -- , listField "breadcrumbs" defaultContext getBreadcrumbs
    ]

-- crumbsContext :: [Identifier] -> Context String
-- crumbsContext path =
--   listField "crumbs" contentContext (flip loadSnapshot "content" `traverse` path)

--------------------------------------------------------------------------------
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "/index.html"

--------------------------------------------------------------------------------
addTableClassToTables :: String -> String
addTableClassToTables =
  packed %~ htmlChunks . traverse . nodeTablesClass ?~ "table"

htmlChunks :: Prism' Text [Node]
htmlChunks = prism' joinNodes getNodes where
  joinNodes :: [Node] -> Text
  joinNodes = L.toStrict . mconcat . toListOf (traverse . re html)
  getNodes :: Text -> Maybe [Node]
  getNodes t = ("<html>" <> L.fromStrict t <> "</html>") ^? html . element . children

tablesClass :: Traversal' L.Text (Maybe Text)
tablesClass = elementsClass "table"

nodeTablesClass :: Traversal' Node (Maybe Text)
nodeTablesClass = nodeElementsClass "table"

elementsClass :: Text -> Traversal' L.Text (Maybe Text)
elementsClass elt =
  html . nodeElementsClass elt

nodeElementsClass :: Text -> Traversal' Node (Maybe Text)
nodeElementsClass elt =
  element
  . deep (named (only elt))
  . attr "class"
