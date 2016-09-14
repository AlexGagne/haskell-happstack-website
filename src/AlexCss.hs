{-# LANGUAGE OverloadedStrings #-}

module AlexCss where

import Clay
import Data.Text
import System.Directory

bodyStyle :: Css
bodyStyle = body ? do
  background  aquamarine
  fontFamily  ["Helvetica Neue"] [sansSerif]

generateCss :: Text
generateCss = render bodyStyle

generateCssFile :: IO ()
generateCssFile = writeFile filepath generateCss
                    where 
                        filepath = concat getCurrentDirectory "/test.css"