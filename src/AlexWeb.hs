{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module AlexWeb where

import           Control.Monad
import           Data.Data
import qualified Data.Text                       as T
import qualified Happstack.Server                as Happ
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as I
import qualified System.Environment              as E
import qualified AlexHtml                        as Html

main :: IO ()
main = do
  config <- I.cmdArgs aConfig
  Happ.simpleHTTP (hConf config) myApp

myApp :: Happ.ServerPart Happ.Response
myApp = msum
  [ Happ.dir "echo" Html.echo,
    Happ.dir "bdtest" bdTest,
    Html.homepage
  ]

bdTest :: Happ.ServerPart Happ.Response
bdTest = Happ.ok $ Happ.toResponse $ bdValue
        where bdValue <- liftIO $ getBDValue

getBDValue :: IO String
getBDValue = E.getEnv "MONGODB_URI"

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
