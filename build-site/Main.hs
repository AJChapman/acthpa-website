{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
  ) where

import Control.Lens               ()
import Control.Monad              ()
import Data.Aeson                 ()
import Data.Aeson.Lens            ()
import Data.Text                  (Text)
import Development.Shake          ()
import Development.Shake.Classes  (Binary)
import Development.Shake.FilePath ()
import Development.Shake.Forward  ()
import GHC.Generics               (Generic)
import Slick                      ()

import qualified Data.Text as T

outputFolder :: FilePath
outputFolder = "gen/"

copyStaticFiles :: Action ()
copyStaticFiles = do
  filePaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*", "fonts//*"]
  void $ forP filePaths $ \filePath ->
    copyFileChanged ("site" </> filePath) (outputFolder </> filePath)

buildRules :: Action ()
buildRules = do
  copyStaticFiles

main :: IO ()
main = slick buildRules
