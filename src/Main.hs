{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Function   ((&))
import Data.List       (isSuffixOf)
import Hakyll.Menu (addToMenu, getMenu)
import Hakyll
import System.FilePath (takeBaseName, takeDirectory, (</>))

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -av _site/ farfromthere.net:acthpa.farfromthere.net/"
  }

main :: IO ()
main = hakyllWith config $ do
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

  match (fromList
          [ "About.md"
          , "Activities.md"
          , "Flying-ACT.md"
          ]) $ do
    addToMenu
    route cleanRoute
    -- let ctx = crumbsContext ["index.md"] <> contentContext
    compile $ contentContext >>= withDefaultTemplate

  -- match "Activities/*" $ do
  --   route cleanRoute
  --   let ctx = crumbsContext ["index.md", "Activities.md"] <> contentContext
  --   compile $ withDefaultTemplate ctx

  match "Articles/*" $ do
    addToMenu
    route cleanRoute
    compile $ do
      ctx <- contentContext
      -- let ctx = crumbsContext ["index.md", "Articles.md"] <> contentContext
      contentCompiler
        >>= applyTemplateAndFixUrls defaultTemplate ctx

  match "Articles.md" $ do
    addToMenu
    route cleanRoute
    compile $ do
      ctx <- contentContext
      let articlesContext =
            -- crumbsContext ["index.md"] <>
            listField "articles" ctx (loadAll "Articles/*")
            <> ctx
      contentCompiler
        >>= loadAndApplyTemplate "templates/articles.html" articlesContext
        >>= loadAndApplyTemplate "templates/default.html" articlesContext
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "Flying-ACT/*" $ do
    addToMenu
    route cleanRoute
    -- let ctx = crumbsContext ["index.md", "Flying-ACT.md"] <> contentContext
    compile $ contentContext >>= withDefaultTemplate

  match "features/*" $ compile $
    contentContext >>= withTemplate "templates/feature.html"

  match "index.md" $ do
    addToMenu
    route (setExtension ".html")
    compile $ do
      ctx <- contentContext
      let features = loadAll "features/*"
          indexContext = listField "features" ctx features <> ctx
      withTemplate "templates/index.html" indexContext

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
contentCompiler = pandocCompiler >>= saveSnapshot "content"

contentContext :: Compiler (Context String)
contentContext = do
  menu <- getMenu
  pure . mconcat $
    [ metadataField
    , defaultContext
    , constField "menu" menu
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

