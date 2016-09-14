{-# LANGUAGE OverloadedStrings #-}

module AlexCss (generateCss) where

import Clay
import qualified Data.Text as T
import System.Directory

bodyStyle :: Css
bodyStyle = body ? do
  background  aquamarine
  fontFamily  ["Helvetica Neue"] [sansSerif]

generateCss :: T.Text
generateCss = render bodyStyle