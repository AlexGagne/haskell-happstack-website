{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module AlexHtml (homepage, echo) where

import           Control.Monad
import           Data.Data
import qualified Data.Text                       as T
import qualified Happstack.Server                as Happ
import           Text.Blaze.Html5                ((!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import qualified AlexCss                         as Css


{- TODO
- Separate Happ from HTML generating code. This module should only generate HTML and not care what
- the rest of the app does with it.
-}

homepage :: Happ.ServerPart Happ.Response
homepage = Happ.ok $ template "My homepage" $ do
                H.h1 "Hello!"
                H.p $ "Welcome to this website! "

echo :: Happ.ServerPart Happ.Response
echo =
  Happ.path $ \msg ->
    Happ.ok $ template "echo" $ do
      H.p $ "echo says: " >> H.toHtml (msg :: String)
      H.p "Change the url to echo something else."

template :: T.Text -> H.Html -> Happ.Response
template title body = Happ.toResponse $ 
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
            css
        H.body $ do
            body
            H.p $ H.a ! A.href "/" $ "back home"

css :: H.Html
css = H.style ! A.type_ "text/css" $ H.toHtml Css.generateCss
