module Main where

import qualified AlexHtml as Html
import qualified AlexCss as Css

main :: IO ()
main = do   Css.generateCssFile 
            Html.main