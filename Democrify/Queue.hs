{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}


{-| This module contains the queue. -}

module Queue where

import           Acid
import           Control.Applicative ((<$>))
import           Control.Concurrent
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid
import           Data.Acid.Advanced  (query', update')
import           Data.Data           (Data, Typeable)
import           Data.IORef
import qualified Data.Sequence       as SQ
import           Data.Text           (Text, unpack)
import           HSObjC
import           System.Directory    (createDirectoryIfMissing,
                                      getHomeDirectory)
import           System.IO.Unsafe    (unsafePerformIO)


initialPlayQueue :: PlayQueue
initialPlayQueue = PlayQueue SQ.empty

playQueue :: IORef (AcidState PlayQueue)
playQueue = unsafePerformIO $ newIORef undefined

resourcePath :: IORef FilePath
resourcePath = unsafePerformIO $ newIORef ""

setResourcePath :: Id -> IO ()
setResourcePath p = do
    runId $ do
        path <- fromId p
        liftIO $ writeIORef resourcePath $ unpack path
    return ()

-- |Gets the folder @~/Library/Application Support/Democrify@ and creates it if it doesn't exist
statePath :: IO FilePath
statePath = do
    path <- (++ "/Library/Application Support/Democrify/") <$> getHomeDirectory
    createDirectoryIfMissing False path
    return path

-- |Part of the DB loop that sorts the queue
loopPartSort :: AcidState PlayQueue -> IO ()
loopPartSort = flip update SortQueue

-- |DB maintenance loop. Sorts the queue (every 30 seconds) and creates a checkpoint (every 30 minutes)
dbLoop :: Int -> IO ()
dbLoop n = do
    acid <- readIORef playQueue
    loopPartSort acid
    newN <- case n of
        60 -> createCheckpoint acid >> return 0
        n  -> return $ n + 1
    threadDelay 30000000
    dbLoop newN

testQueue :: PlayQueue
testQueue = PlayQueue $ SQ.fromList
    [ SpotifyTrack 1 "Röyksopp" "Teppefall" "5jlvtJ8S9STmSGm56lu2LB"
    , SpotifyTrack 1 "The Sounds" "It's So Easy" "6UXamUYDogS7rqGjZpwZ8p"
    , SpotifyTrack 2 "Dominik Eulberg" "Offenbach - Original Mix" "0GgCVHoVoWXFkfpR0rnU50"
    , SpotifyTrack 3 "Extrawelt" "Soopertrack" "5vIHGNhS9AH1aTiRgsHSna"]


-- |Returns the Spotify ID for the next track and updates the 'currentTrack'-
--  This is a prime example of what Haskell is not supposed to look like! :-)
getNextTrack :: IO Id
getNextTrack = do
    acid <- readIORef playQueue
    next <- update' acid GetQueueHead
    runId $ return $ tId next

foreign export ccall getNextTrack    :: IO Id
foreign export ccall setResourcePath :: Id -> IO ()
