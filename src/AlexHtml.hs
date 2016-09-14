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

homepage :: Happ.ServerPart Happ.Response
homepage = Happ.ok $ template "My homepage" $ do
                H.h1 "Hello!"
                H.p $ "Welcome to this website!"

echo :: Happ.ServerPart Happ.Response
echo =
  Happ.path $ \msg ->
    Happ.ok $ template "echo" $ do
      H.p $ "echo says: " >> H.toHtml (msg :: String)
      H.p "Change the url to echo something else."

styleSheet :: H.AttributeValue -> H.Html
styleSheet s = H.link ! A.href s ! A.rel "stylesheet" ! A.type_ "text/css"

template :: T.Text -> H.Html -> Happ.Response
template title body = Happ.toResponse $ do
    H.html $ do
        H.head $ do 
            css
            H.title (H.toHtml title)
        H.body $ do
            body
            H.p $ H.a ! A.href "/" $ "back home"

css :: H.Html
css = H.style ! A.type_ "text/css" $ H.toHtml Css.generateCss
