{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module AlexHtml where

import           Control.Monad
import           Data.Data
import qualified Data.Text                       as T
import qualified Happstack.Server                as Happ
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as I
import           Text.Blaze.Html5                ((!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

main :: IO ()
main = do
  config <- I.cmdArgs aConfig
  Happ.simpleHTTP (hConf config) myApp

myApp :: Happ.ServerPart Happ.Response
myApp = msum
  [ Happ.dir "echo" echo,
    homepage
  ]

template :: T.Text -> H.Html -> Happ.Response
template title body = Happ.toResponse $ H.docTypeHtml $ do
    H.html $ do
        H.head $ H.title (H.toHtml title)
        H.body $ do
            body
            H.p $ H.a ! A.href "/" $ "back home"

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

-- Config
--------------------------------------------------------------------------------

data Config =
  Config { port :: Int, timeout :: Int } deriving ( Show, Eq, Data, Typeable )

hConf :: Config -> Happ.Conf
hConf (Config {..}) = Happ.nullConf { Happ.timeout = timeout, Happ.port = port }

aConfig :: Config
aConfig =
  Config { port    = 8000  &= I.help "Port number"
                           &= I.typ "INT"
         , timeout = 30    &= I.help "Timeout"
                           &= I.typ "SECONDS"
         }
    &= I.summary "AlexWeb server"
    &= I.program "server"
