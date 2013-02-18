{-# LANGUAGE OverloadedStrings #-}

module User where

import           Control.Applicative   (optional, (<$>))
import           Control.Monad         (msum)
import           Data.ByteString.Char8 (ByteString)
import           Data.IORef
import           Data.Text             (Text)
import           Happstack.Server
import           System.IO.Unsafe      (unsafePerformIO)

resourcePath :: IORef FilePath
resourcePath = unsafePerformIO $ newIORef ""

simpleTest :: ServerPart Response
simpleTest = ok $ toResponse $ ("Works!" :: String)

runServer :: IO ()
runServer = simpleHTTP nullConf{port = 8686} simpleTest
