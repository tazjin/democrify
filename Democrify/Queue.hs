{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}


{-| This module contains the queue. -}

module Queue where

import           Acid
import           Control.Applicative          ((<$>))
import           Control.Concurrent
import           Control.Monad                (forM, forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Acid
import           Data.Acid.Advanced           (update')
import           Data.Acid.Local              (createCheckpointAndClose, createArchive)
import           Data.Data                    (Data, Typeable)
import           Data.IORef
import           Data.Random                  (runRVar)
import           Data.Random.Extras           (shuffleSeq)
import           Data.Random.Source.DevRandom (DevRandom (..))
import qualified Data.Sequence                as SQ
import           Data.Text                    (Text, unpack)
import qualified Data.Text                    as T
import           HSObjC
import           System.Directory             (createDirectoryIfMissing,
                                               getHomeDirectory)
import           System.IO.Unsafe             (unsafePerformIO)
import           WebAPI

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

-- |Gracefully shuts down the state and archives
gracefulQuit :: IO ()
gracefulQuit = do
    acid <- readIORef playQueue
    createArchive acid
    createCheckpointAndClose acid

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

-- |DB maintenance loop. Sorts the queue (every 30 seconds)
dbLoop :: AcidState PlayQueue -> IO ()
dbLoop acid = do
    loopPartSort acid
    threadDelay 15000000
    dbLoop acid


-- |Sets the currently playing track by requesting it from the Spotify Lookup API
--  based on the track ID. If the track is not found no track will be set.
setCurrentTrack :: Text -> IO ()
setCurrentTrack track = do
    song <- identifyTrack track
    writeIORef currentTrack song

foreign export ccall shuffleQueue    :: IO ()
foreign export ccall gracefulQuit    :: IO ()
