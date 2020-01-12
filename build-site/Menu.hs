{-# LANGUAGE OverloadedStrings #-}
module Menu
  ( MenuItem(..)
  , Menu(..)
  , ToMenuItem(..)
  , menuText
  , menuHtml
  ) where

import Control.Lens.Operators
import Data.List.NonEmpty            (NonEmpty)
import Data.Text                     (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              ((!))

import qualified Data.Text.Lazy              as L
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Page

data MenuItem =
  BranchItem Page (NonEmpty MenuItem)
  | LeafItem Page
  deriving (Eq, Ord)

newtype Menu = Menu (NonEmpty MenuItem)

class ToMenuItem a where
  toMenuItem :: a -> MenuItem

instance ToMenuItem Page where
  toMenuItem = LeafItem

menuText :: Page -> Menu -> Text
menuText page = L.toStrict . renderHtml . menuHtml page

menuHtml :: Page -> Menu -> H.Html
menuHtml activePage (Menu items) = submenuHtml activePage items

submenuHtml :: Page -> NonEmpty MenuItem -> H.Html
submenuHtml activePage =
  H.ul . foldMap (menuItemHtml activePage)

menuItemHtml :: Page -> MenuItem -> H.Html
menuItemHtml activePage (BranchItem page children) =
  H.li
    ( pageLink activePage page
      <> submenuHtml activePage children ! A.class_ "dropdown"
    ) ! A.class_ "has-dropdown"

menuItemHtml activePage (LeafItem page) =
  H.li $ pageLink activePage page

pageLink :: Page -> Page -> H.Html
pageLink activePage page =
  H.a (H.toHtml $ page ^. pageTitle)
    ! A.href (H.toValue $ pageRelativeUrl activePage page)
    & if isActivePage activePage page
        then (! A.class_ "active")
        else id
