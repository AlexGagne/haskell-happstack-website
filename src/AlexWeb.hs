{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{- |
Module      :  AlexWeb.hs
Description :  File containing the main starting point for the server.
Maintainer  :  <AlexGagne>

This module defines the configuration for the server and handles routing
-}

module AlexWeb where

import qualified AlexHtml                        as Html
import           Control.Monad
import           Data.Data
import qualified Data.Text                       as T
import qualified Happstack.Server                as Happ
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as I

main :: IO ()
main = do
  config <- I.cmdArgs aConfig
  Happ.simpleHTTP (hConf config) myApp

myApp :: Happ.ServerPart Happ.Response
myApp = msum[Happ.dir "resume" $ Happ.serveDirectory Happ.DisableBrowsing ["data.txt"] "data",
             Happ.ok $ Happ.toResponse Html.homepage]

-- Config
--------------------------------------------------------------------------------

data Config =
  Config { port :: Int, timeout :: Int } deriving ( Show, Eq, Data, Typeable )

hConf :: Config -> Happ.Conf
hConf Config {..} = Happ.nullConf { Happ.timeout = timeout, Happ.port = port }

aConfig :: Config
aConfig =
  Config { port    = 8000  &= I.help "Port number"
                           &= I.typ "INT"
         , timeout = 30    &= I.help "Timeout"
                           &= I.typ "SECONDS"
         }
    &= I.summary "AlexWeb server"
    &= I.program "server"
