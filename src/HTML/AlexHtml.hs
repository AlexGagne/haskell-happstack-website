{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{- |
Module      :  AlexHtml.hs
Description :  File containing Html generation for the main pages of the website
Maintainer  :  <AlexGagne>

This module includes Html generation for the main pages of the website. This website only currently includes a webpage.
-}

module AlexHtml (homepage) where

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
                mainBody welcome

template :: T.Text -> H.Html ->  H.Html
template title body =
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
            css
        H.body $ do
            body
            footer $ H.p $ H.a ! A.href "/" $ "back home"

-- Text content

welcome :: H.Html
welcome = do
    H.p "Welcome to this website! I am Alex Gagné and I am an undergraduate student at Polytechnique Montréal. "
    H.p $ H.toHtml $
          "I enjoy learning about many aspects of software engineering and development such as " ++ 
          "algorithms, distributed and parallel systems and web development. " ++
          "What I really enjoy the most, however, is always learning new technologies and frameworks. " ++
          "This is the main reason why I created this website, to learn Haskell but also to "
    H.p $ do 
        H.toHtml $
          "This website was coded by me in my spare time using Haskell and Happstack. " ++
          "The full code is available on Github: "
        H.a ! A.href "https://github.com/AlexGagne/haskell-happstack-website" $ "Code"



-- Useful divs and ids

menu :: H.Html -> H.Html
menu entry = H.div entry ! A.id "menu" 

mainBody :: H.Html -> H.Html
mainBody body = H.div body ! A.id "main_body" 

footer :: H.Html -> H.Html
footer foot = H.div foot ! A.id "footer"

-- To be later used to include code snippets
hsCode :: H.Html -> H.Html
hsCode code = H.div code ! A.class_ "haskell"


-- Access to the CSS generating code
css :: H.Html
css = H.style ! A.type_ "text/css" $ H.toHtml Css.generateCss