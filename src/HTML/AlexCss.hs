{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  AlexCss.hs
Description :  Generation of the CSS for the website.
Maintainer  :  <AlexGagne>

This module generates the CSS for the website
-}

module AlexCss (generateCss) where

import           Clay
import qualified Data.Text.Internal.Lazy as T
import           System.Directory

bodyStyle :: Css
bodyStyle = body ? do
  background  aquamarine
  fontFamily  ["Helvetica Neue"] [sansSerif]

generateCss :: T.Text
generateCss = render bodyStyle
