{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Control.Concurrent (forkIO)
import           Control.Exception  (bracket)
import           Data.Acid
import           Data.Acid.Local
import           Data.IORef         (writeIORef)
import           Foreign
import           HSObjC

-- Democrify modules
import           Acid
import           Queue
import           User

runHaskellPart :: IO ()
runHaskellPart = do
    path <- statePath
    bracket (openLocalStateFrom path testQueue)
            (createCheckpointAndClose)
            (\acid -> do createArchive acid
                         writeIORef playQueue acid
                         forkIO $ dbLoop 0
                         runServer )

-- Start the mainloop!
main = do
    forkIO runHaskellPart -- Start webserver
    c_NSApplicationMain 0 nullPtr -- Start Cocoa app
