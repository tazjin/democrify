{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Control.Concurrent     (forkIO)
import           Control.Exception      (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid
import           Data.Acid.Local
import           Data.IORef             (writeIORef, readIORef)
import           Data.Text              (unpack)
import           Foreign
import           HSObjC


-- Democrify modules
import           Acid
import           Queue
import           User

-- |Function to empty the queue from ObjC
extEmptyQueue :: IO ()
extEmptyQueue = do
    acid <- readIORef playQueue
    update acid EmptyQueue

setResourcePath :: Id -> IO ()
setResourcePath p = do
    runId $ do
        path <- fromId p
        liftIO $ writeIORef resourcePath $ unpack path
    return ()

runHaskellPart :: IO ()
runHaskellPart = do
    path <- statePath
    bracket (openLocalStateFrom path initialPlayQueue)
            (createCheckpointAndClose)
            (\acid -> do createArchive acid
                         writeIORef playQueue acid
                         forkIO $ dbLoop 0
                         runServer )

-- Start the mainloop!
main = do
    forkIO runHaskellPart -- Start webserver
    c_NSApplicationMain 0 nullPtr -- Start Cocoa app

foreign export ccall extEmptyQueue   :: IO ()
foreign export ccall setResourcePath :: Id -> IO ()