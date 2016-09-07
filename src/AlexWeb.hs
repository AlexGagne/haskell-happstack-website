{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

import           Control.Applicative ((<$>), optional)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Lazy (unpack)
import           Happstack.Lite
import           Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as I

module AlexWeb where

main :: IO ()
main = do
    config <- I.cmdArgs aConfig
    H.simpleHTTP (hConf config) myApp

myApp :: ServerPart Response
myApp = msum
    [ dir "echo"    $ echo
    , dir "query"   $ queryParams
    , dir "form"    $ formPage
    , homePage
    ]

template :: T.Text -> Html.Html -> H.Response
template title body = H.toResponse $
  Html.html $ do
    Html.head $ Html.title (Html.toHtml title)
    Html.body $ do
      body
      Html.p $ Html.a ! Attr.href "/" $ "back home"

homePage :: ServerPart Response
homePage =
    ok $ template "home page" $ do
        H.h1 "Hello!"
        H.p "Writing applications with happstack-lite is fast and simple!"
        H.p "Check out these killer apps."
        H.p $ a ! href "/echo/secret%20message"  $ "echo"
        H.p $ a ! href "/query?foo=bar" $ "query parameters"
        H.p $ a ! href "/form"          $ "form processing"

echo :: ServerPart Response
echo =
    path $ \(msg :: String) ->
        ok $ template "echo" $ do
            p $ "echo says: " >> toHtml msg
            p "Change the url to echo something else."        

queryParams :: ServerPart Response
queryParams =
    do mFoo <- optional $ lookText "foo"
        ok $ template "query params" $ do
            p $ "foo is set to: " >> toHtml (show mFoo)
            p $ "change the url to set it to something else."

formPage :: ServerPart Response
formPage = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
        do method GET
           ok $ template "form" $
              form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
                label ! A.for "msg" $ "Say something clever"
                input ! type_ "text" ! A.id "msg" ! name "msg"
                input ! type_ "submit" ! value "Say it!"

    processForm :: ServerPart Response
    processForm =
        do method POST
           msg <- lookText "msg"
           ok $ template "form" $ do
             H.p "You said:"
             H.p (toHtml msg)


-- Config
--------------------------------------------------------------------------------

data Config =
  Config { port :: Int, timeout :: Int } deriving ( Show, Eq, Data, Typeable )

hConf :: Config -> H.Conf
hConf (Config {..}) = H.nullConf { H.timeout = timeout, H.port = port }

aConfig :: Config
aConfig =
  Config { port    = 8000  &= I.help "Port number"
                           &= I.typ "INT"
         , timeout = 30    &= I.help "Timeout"
                           &= I.typ "SECONDS"
         }
    &= I.summary "AlexWeb server"
    &= I.program "server"