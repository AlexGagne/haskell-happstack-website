{-# LANGUAGE OverloadedStrings #-}

module AlexCss where

import Clay
import qualified Data.Text.Internal.Text as Text
import Data.Text.Lazy
import System.Directory

bodyStyle :: Css
bodyStyle = body ? do
  background  aquamarine
  fontFamily  ["Helvetica Neue"] [sansSerif]

generateCss :: Text
generateCss = toStrict $ render bodyStyle

generateCssFile :: IO ()
generateCssFile = writeFile filepath generateCss
                    where 
                        filepath = getCurrentDirectory ++ "/test.css"