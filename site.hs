--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.List       (isSuffixOf)
import Hakyll
import System.FilePath (takeBaseName, takeDirectory, (</>))


--------------------------------------------------------------------------------

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
          , "Articles.md"
          , "Flying-ACT.md"
          ]) $ do
    route cleanRoute
    compile $ withDefaultTemplate defaultContext

  match "Activities/*" $ do
    route cleanRoute
    compile $ withDefaultTemplate defaultContext

  match "Articles/*" $ do
    route cleanRoute
    compile $ withDefaultTemplate defaultContext

  match "Flying-ACT/*" $ do
    route cleanRoute
    compile $ withDefaultTemplate defaultContext

  match "features/*" $ compile $
    withTemplate "templates/feature.html" featureContext

  match "index.md" $ do
    route (setExtension ".html")
    compile $ do
      let features = loadAll "features/*"
          indexContext =
            listField "features" featureContext features <>
            metadataField <> defaultContext
      withTemplate "templates/index.html" indexContext

withDefaultTemplate :: Context String -> Compiler (Item String)
withDefaultTemplate = withTemplate "templates/default.html"

withTemplate :: Identifier -> Context String -> Compiler (Item String)
withTemplate templatePath ctx =
  pandocCompiler
    >>= loadAndApplyTemplate templatePath ctx
    >>= relativizeUrls
    >>= cleanIndexUrls

featureContext :: Context String
featureContext = metadataField <> defaultContext

--------------------------------------------------------------------------------
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pat replacement)
  where
    pat = "/index.html"
    replacement = const ""

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "/index.html"
