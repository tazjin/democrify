{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_HADDOCK -ignore-exports   #-}

{-| This module contains the acid state types and functions. Template Haskell causes
    a linker issue so everything is done manually. Fuck yeah! -}
module Acid where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Exception    (bracket)
import           Control.Monad.Reader (ask)
import           Control.Monad.State
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Data            (Data, Typeable)
import           Data.SafeCopy
import           Data.Sequence        (Seq (..), (<|), (|>))
import qualified Data.Sequence        as SQ
import           Data.Text            (Text)
import           Data.Typeable
import           Data.Word

-- * Data types

-- |A single Spotify track
data SpotifyTrack = SpotifyTrack {
    votes  :: Int,
    artist :: Text,
    track  :: Text,
    tId    :: Text -- Track ID in Spotify
} deriving (Read, Show, Data, Typeable)


instance Ord SpotifyTrack where
    -- |Reverse sort order. Sorry :)
    (SpotifyTrack v1 _ _ _) `compare` (SpotifyTrack v2 _ _ _) = v2 `compare` v1

instance Eq SpotifyTrack where
    -- |Is equal if track ID is equal
    (SpotifyTrack _ _ _ t1) == (SpotifyTrack _ _ _ t2) = t1 == t2

-- |Upvotes a 'SpotifyTrack'. I only included this as a separate function because I
--  wanted a function named upvote ;D
upvote :: SpotifyTrack -> SpotifyTrack
upvote t@SpotifyTrack{..} = t { votes = votes + 1 }

-- |Mighty admin upvote!
upvote9000 :: SpotifyTrack -> SpotifyTrack
upvote9000 t@SpotifyTrack{..} = t { votes = votes + 9000 }

-- |The play queue
data PlayQueue = PlayQueue { queue :: Seq SpotifyTrack }
    deriving (Eq, Ord, Read, Show, Data, Typeable)


instance SafeCopy SpotifyTrack where
    putCopy SpotifyTrack{..} =
        contain $ do safePut votes
                     safePut artist
                     safePut track
                     safePut tId
    getCopy =
        contain $ SpotifyTrack <$> safeGet
                               <*> safeGet
                               <*> safeGet
                               <*> safeGet
    version = 0
    kind = base
    errorTypeName _ = "Acid.SpotifyTrack"

instance SafeCopy PlayQueue where
    putCopy (PlayQueue q) = contain $ safePut q
    getCopy = contain $ PlayQueue <$> safeGet
    version = 0
    kind = base
    errorTypeName _ = "Acid.PlayQueue"

-- * Acid state query functions

-- |Played when the queue is empty but the ObjC part requests a song
rickroll :: SpotifyTrack
rickroll = SpotifyTrack 0 "Rick Astley" "Never Gonna Give You Up" "6JEK0CvvjDjjMUBFoXShNZ"

-- |Alternative "-roll"
polkaroll :: SpotifyTrack
polkaroll = SpotifyTrack 0 "Säkkijärven Polka" "Solistiyhtye Suomi" "5CqLCUgOFplIyscaoSNkPm"

-- |Alternative "-roll"
psyroll :: SpotifyTrack
psyroll = SpotifyTrack 0 "Psy" "Gangnam Style" "1R2SZUOGJqqBiLuvwKOT2Y"

-- |Gets the next track without modifying the queue.
--  No use-case yet but it could come in handy? :]
peekNext :: Query PlayQueue SpotifyTrack
peekNext = head' <$> queue <$> ask
    where
        head' s = if SQ.null s then polkaroll else SQ.index s 0

-- |Gets the entire play queue
getQueue :: Query PlayQueue (Seq SpotifyTrack)
getQueue = queue <$> ask

-- |\"Puts" (replaces) the entire queue
putQueue :: Seq SpotifyTrack -> Update PlayQueue ()
putQueue q = put $ PlayQueue q

-- * Acid state update functions

-- |Gets the first song in the queue and removes it from the queue.
getQueueHead :: Update PlayQueue SpotifyTrack
getQueueHead = do
    q@PlayQueue{..} <- get
    if SQ.null queue
        then return polkaroll
        else do put $ q { queue = SQ.drop 1 queue }
                return $ SQ.index queue 0

-- |Adds a track to the tail of the queue. If duplicates are allowed
--  a track can be added twice, if duplicates are disallowed existing
--  tracks will be upvoted.
addTrackToQueue :: Bool ->  SpotifyTrack -> Update PlayQueue SpotifyTrack
addTrackToQueue d t = do
    q@PlayQueue{..} <- get
    if d then put $ q { queue = queue |> t }
         else case SQ.findIndexL (t ==) queue of
                Nothing -> put $ q { queue = queue |> t }
                Just i  -> put $ q { queue = SQ.adjust upvote i queue }
    return t

-- |Forcefully adds a track to the tail of the queue. This is only exposed in the
--  administration interface
forceAddTrack :: SpotifyTrack -> Update PlayQueue SpotifyTrack
forceAddTrack t = do
    q@PlayQueue{..} <- get
    put $ q { queue = t <| queue }
    return t

-- |Upvotes a track and does nothing if the track is not in the queue
--  (ain't nobody got time for this)
upvoteTrack :: Text -> Update PlayQueue ()
upvoteTrack t = do
    q@PlayQueue{..} <- get
    case SQ.findIndexL (\tr -> tId tr == t) queue of
        Nothing -> return ()
        Just i  -> (put $ q { queue = SQ.adjust upvote i queue }) >> return ()

-- |Sorts the queue. By doing this inside an 'Update' we ensure
--  atomicity and thus that no tracks are forgotten! :)
sortQueue :: Update PlayQueue ()
sortQueue = do
    q@PlayQueue{..} <- get
    put $ q { queue = SQ.unstableSort queue }

-- |Removes a track from the queue (admin only)
removeTrack :: Text -> Update PlayQueue ()
removeTrack track = do
    q@PlayQueue{..} <- get
    put $ q { queue = SQ.filter (\t -> tId t /= track) queue}

-- |Upvotes a track over 9000 (admin only)
adminUpvote :: Text -> Update PlayQueue ()
adminUpvote t = do
    q@PlayQueue{..} <- get
    case SQ.findIndexL (\tr -> tId tr == t) queue of
        Nothing -> return ()
        Just i  -> (put $ q { queue = SQ.adjust upvote9000 i queue }) >> return ()

-- |Empties the queue (Who would've guessed?)
emptyQueue :: Update PlayQueue ()
emptyQueue = do
    q@PlayQueue{..} <- get
    put $ q { queue = SQ.empty }

-- * Acid state's nitty gritty details
data PeekNext = PeekNext
data GetQueue = GetQueue
data GetQueueHead = GetQueueHead

data PutQueue = PutQueue (Seq SpotifyTrack)
data AddTrackToQueue = AddTrackToQueue Bool SpotifyTrack
data ForceAddTrack = ForceAddTrack SpotifyTrack
data UpvoteTrack = UpvoteTrack Text
data SortQueue = SortQueue
data RemoveTrack = RemoveTrack Text
data AdminUpvote = AdminUpvote Text
data EmptyQueue = EmptyQueue

deriving instance Typeable PeekNext
deriving instance Typeable GetQueue
deriving instance Typeable GetQueueHead

deriving instance Typeable PutQueue
deriving instance Typeable AddTrackToQueue
deriving instance Typeable ForceAddTrack
deriving instance Typeable UpvoteTrack
deriving instance Typeable SortQueue
deriving instance Typeable RemoveTrack
deriving instance Typeable AdminUpvote
deriving instance Typeable EmptyQueue

instance SafeCopy PeekNext where
    putCopy PeekNext = contain $ return ()
    getCopy = contain $ return PeekNext

instance SafeCopy GetQueue where
    putCopy GetQueue = contain $ return ()
    getCopy = contain $ return GetQueue

instance SafeCopy GetQueueHead where
    putCopy GetQueueHead = contain $ return ()
    getCopy = contain $ return GetQueueHead

instance SafeCopy PutQueue where
    putCopy (PutQueue q) = contain $ safePut q
    getCopy = contain $ PutQueue <$> safeGet

instance SafeCopy AddTrackToQueue where
    putCopy (AddTrackToQueue d t) = contain $ (safePut d >> safePut t)
    getCopy = contain $ AddTrackToQueue <$> safeGet <*> safeGet

instance SafeCopy ForceAddTrack where
    putCopy (ForceAddTrack t) = contain $ safePut t
    getCopy = contain $ ForceAddTrack <$> safeGet

instance SafeCopy UpvoteTrack where
    putCopy (UpvoteTrack t) = contain $ safePut t
    getCopy = contain $ UpvoteTrack <$> safeGet

instance SafeCopy SortQueue where
    putCopy SortQueue = contain $ return ()
    getCopy = contain $ return SortQueue

instance SafeCopy RemoveTrack where
    putCopy (RemoveTrack t) = contain $ safePut t
    getCopy = contain $ RemoveTrack <$> safeGet

instance SafeCopy AdminUpvote where
    putCopy (AdminUpvote t) = contain $ safePut t
    getCopy = contain $ AdminUpvote <$> safeGet

instance SafeCopy EmptyQueue where
    putCopy EmptyQueue = contain $ return ()
    getCopy = contain $ return EmptyQueue

instance Method PeekNext where
    type MethodResult PeekNext = SpotifyTrack
    type MethodState PeekNext = PlayQueue

instance Method GetQueue where
    type MethodResult GetQueue = Seq SpotifyTrack
    type MethodState GetQueue = PlayQueue

instance Method GetQueueHead where
    type MethodResult GetQueueHead = SpotifyTrack
    type MethodState GetQueueHead = PlayQueue

instance Method AddTrackToQueue where
    type MethodResult AddTrackToQueue = SpotifyTrack
    type MethodState AddTrackToQueue = PlayQueue

instance Method ForceAddTrack where
    type MethodResult ForceAddTrack = SpotifyTrack
    type MethodState ForceAddTrack = PlayQueue

instance Method PutQueue where
    type MethodResult PutQueue = ()
    type MethodState PutQueue = PlayQueue

instance Method UpvoteTrack where
    type MethodResult UpvoteTrack = ()
    type MethodState UpvoteTrack = PlayQueue

instance Method SortQueue where
    type MethodResult SortQueue = ()
    type MethodState SortQueue = PlayQueue

instance Method RemoveTrack where
    type MethodResult RemoveTrack = ()
    type MethodState RemoveTrack = PlayQueue

instance Method AdminUpvote where
    type MethodResult AdminUpvote = ()
    type MethodState AdminUpvote = PlayQueue

instance Method EmptyQueue where
    type MethodResult EmptyQueue = ()
    type MethodState EmptyQueue = PlayQueue

instance QueryEvent PeekNext
instance QueryEvent GetQueue

instance UpdateEvent PutQueue
instance UpdateEvent GetQueueHead
instance UpdateEvent AddTrackToQueue
instance UpdateEvent ForceAddTrack
instance UpdateEvent UpvoteTrack
instance UpdateEvent SortQueue
instance UpdateEvent RemoveTrack
instance UpdateEvent AdminUpvote
instance UpdateEvent EmptyQueue

instance IsAcidic PlayQueue where
    acidEvents = [ QueryEvent (\PeekNext     -> peekNext)
                 , QueryEvent (\GetQueue     -> getQueue)
                 , UpdateEvent (\(PutQueue q) -> putQueue q)
                 , UpdateEvent (\GetQueueHead -> getQueueHead)
                 , UpdateEvent (\(AddTrackToQueue d t) -> addTrackToQueue d t)
                 , UpdateEvent (\(ForceAddTrack t) -> forceAddTrack t)
                 , UpdateEvent (\(UpvoteTrack t) -> upvoteTrack t)
                 , UpdateEvent (\SortQueue   -> sortQueue)
                 , UpdateEvent (\(RemoveTrack t) -> removeTrack t)
                 , UpdateEvent (\(AdminUpvote t) -> adminUpvote t)
                 , UpdateEvent (\EmptyQueue -> emptyQueue)]
