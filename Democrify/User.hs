{-# LANGUAGE OverloadedStrings    #-}

module User where

import           Control.Applicative   (optional, (<$>))
import           Control.Monad         (msum)
import           Data.ByteString.Char8 (ByteString)
import           Happstack.Server
import           Data.Text             (Text)


simpleTest :: ServerPart Response
simpleTest = ok $ toResponse $ ("Works!" :: String)

runServer :: IO ()
runServer = simpleHTTP nullConf{port = 8686} simpleTest