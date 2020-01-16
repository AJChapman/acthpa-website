{-# LANGUAGE OverloadedStrings #-}
module Menu
  ( MenuItem(..)
  , Menu(..)
  , ToMenuItem(..)
  , menuText
  , menuHtml
  , pageListMenuItem
  ) where

import Control.Lens                  (Lens')
import Control.Lens.Operators
import Data.List.NonEmpty            (NonEmpty, nonEmpty)
import Data.Text                     (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              ((!))

import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Page

data MenuItem =
  BranchItem Page (NonEmpty MenuItem)
  | LeafItem Page
  deriving (Eq, Ord)

itemPage :: Lens' MenuItem Page
itemPage f (LeafItem page) = LeafItem <$> f page
itemPage f (BranchItem page children) =
  (`BranchItem` children) <$> f page

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
menuItemHtml activePage item@(BranchItem _ children) =
  H.li
    ( menuItemLink activePage item
      <> submenuHtml activePage children ! A.class_ "dropdown"
    ) ! A.class_ "has-dropdown"
menuItemHtml activePage item@(LeafItem _) =
  H.li $ menuItemLink activePage item

menuItemLink :: Page -> MenuItem -> H.Html
menuItemLink activePage item =
  let page = item ^. itemPage
  in H.a (H.toHtml $ page ^. pageTitle)
    ! A.href (H.toValue $ pageRelativeUrl activePage page)
    & if isActiveMenuItem activePage item
        then (! A.class_ "active")
        else id

pageListMenuItem :: Page -> [Page] -> MenuItem
pageListMenuItem listPage subPages =
    case nonEmpty subPages of
      Nothing -> LeafItem listPage
      Just pages ->
        BranchItem listPage (toMenuItem <$> pages)

isActiveMenuItem :: Page -> MenuItem -> Bool
isActiveMenuItem activePage (LeafItem page) = page == activePage
isActiveMenuItem activePage (BranchItem page _) =
  if page ^. pageUrl == ""
    then page == activePage
    else (page ^. pageUrl) `T.isPrefixOf` (activePage ^. pageUrl)

