{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module AlexHtml (homepage, echo) where

import qualified AlexCss                     as Css
import           Control.Monad
import           Data.Data
import qualified Data.Text                   as T
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


homepage :: H.Html
homepage = template "My homepage" $ do
                H.h1 "Hello!"
                H.p  "Welcome to this website! "

echo :: String -> H.Html
echo msg =
    template "echo" $ do
      H.p $ "echo says: " >> H.toHtml msg
      H.p "Change the url to echo something else."

template :: T.Text -> H.Html -> H.Html
template title body =
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
            css
        H.body $ do
            body
            H.p $ H.a ! A.href "/" $ "back home"

css :: H.Html
css = H.style ! A.type_ "text/css" $ H.toHtml Css.generateCss
