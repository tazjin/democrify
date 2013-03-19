{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}


{-| This module contains the queue. -}

module Queue where

import           Acid
import           Control.Applicative    ((<$>))
import           Control.Concurrent
import           Control.Monad          (forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid
import           Data.Acid.Advanced     (update')
import           Data.Data              (Data, Typeable)
import           Data.IORef
import qualified Data.Sequence          as SQ
import           Data.Random.Extras     (shuffleSeq)
import           Data.Text              (Text, unpack)
import qualified Data.Text              as T
import           HSObjC
import           System.Directory       (createDirectoryIfMissing,
                                         getHomeDirectory)
import           System.IO.Unsafe       (unsafePerformIO)
import           WebAPI
import Data.Random.Source.DevRandom (DevRandom(..))
import Data.Random (runRVar)

initialPlayQueue :: PlayQueue
initialPlayQueue = PlayQueue SQ.empty

playQueue :: IORef (AcidState PlayQueue)
playQueue = unsafePerformIO $ newIORef undefined

currentTrack :: IORef (Maybe SpotifyTrack)
currentTrack = unsafePerformIO $ newIORef Nothing

getTrackData :: [Text] -> IO [Maybe SpotifyTrack]
getTrackData trackIds = forM (map (T.drop 14) trackIds) $ \t -> do
    track <- identifyTrack t
    threadDelay 125000 -- Evade Web API querying limit. Should be done with libspotify as well but ICBA right now
    return track

-- |Shuffles the play queue through a weird combination of things :(
shuffleQueue :: IO ()
shuffleQueue = do
    acid <- readIORef playQueue
    queue <- query acid GetQueue
    shuffled <- SQ.fromList <$> (runRVar (shuffleSeq queue) DevURandom)
    update acid $ PutQueue shuffled
    update acid SortQueue

-- |Gets the folder @~/Library/Application Support/Democrify/queue@ and creates it if it doesn't exist
statePath :: IO FilePath
statePath = do
    path <- (++ "/Library/Application Support/Democrify/queue") <$> getHomeDirectory
    createDirectoryIfMissing False path
    return path

-- |Gets the folder @~/Library/Application Support/Democrify@ and creates it if it doesn't exist
prefsPath :: IO FilePath
prefsPath = do
    path <- (++ "/Library/Application Support/Democrify") <$> getHomeDirectory
    createDirectoryIfMissing False path
    return $ path ++ "/democrify.config"

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
    threadDelay 15000000
    dbLoop newN


-- |Returns the Spotify ID for the next track and updates the 'currentTrack'-
--  This is a prime example of what Haskell is not supposed to look like! :-)
getNextTrack :: IO Id
getNextTrack = do
    acid <- readIORef playQueue
    next <- update acid GetQueueHead
    setCurrentTrack $ tId next
    runId $ return $ tId next

-- |Sets the currently playing track by requesting it from the Spotify Lookup API
--  based on the track ID. If the track is not found no track will be set.
setCurrentTrack :: Text -> IO ()
setCurrentTrack track = do
    song <- identifyTrack track
    writeIORef currentTrack song


foreign export ccall getNextTrack    :: IO Id
foreign export ccall shuffleQueue    :: IO ()
