{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  AlexCss.hs
Description :  File containing CSS generation for the main pages of the website
Maintainer  :  <AlexGagne>
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
