{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


{- |
Module      :  AlexHtml.hs
Description :  Generation of the HTML for the website.
Maintainer  :  <AlexGagne>

This module generates the HTML for the website
-}


module AlexHtml (renderBlogPosts) where

import qualified AlexCss                     as Css
import qualified AlexDb                      as Db
import           Data.Text                   (Text, pack)
import           Data.Text.Lazy              (fromStrict)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Markdown               (def, markdown)


renderBlogPosts :: IO H.Html
renderBlogPosts = do
  posts <- Db.getAllBlogPosts
  return $ template "Welcome" (concatenateHtml $ renderAllPosts posts) False

template :: Text -> H.Html -> Bool -> H.Html
template title body showBackHome =
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
            css
        H.body $ do
            body
            if showBackHome
            then H.p $ H.a ! A.href "/" $ "back home"
            else H.text $ pack ""


concatenateHtml :: [H.Html] -> H.Html
concatenateHtml [] = H.text $ pack ""
concatenateHtml (x:xs) = blogPost $ do
                          H.p x
                          concatenateHtml xs

renderAllPosts :: [Db.Post] -> [H.Html]
renderAllPosts posts = map renderPost posts

renderPost :: Db.Post -> H.Html
renderPost post = markdown def $ fromStrict $ Db.content post

-- Useful divs and ids

menu :: H.Html -> H.Html
menu entry = H.div entry ! A.id "menu"

blogPost :: H.Html -> H.Html
blogPost post = H.div post ! A.id "blog_post"

footer :: H.Html -> H.Html
footer foot = H.div foot ! A.id "footer"

-- To be later used to include code snippets
hsCode :: H.Html -> H.Html
hsCode code = H.div code ! A.class_ "haskell"


-- Generation of embedded css
css :: H.Html
css = H.style ! A.type_ "text/css" $ H.toHtml Css.generateCss
