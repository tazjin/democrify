{-| Very "to-the-point" bindings to the Spotify Web API
-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebAPI (identifyTrack) where

--import Acid
import           Acid
import           Control.Applicative  (pure, (<$>), (<*>))
import           Control.Exception
import           Data.Aeson
import           Data.Text            (Text, unpack)
import           Network.HTTP.Conduit
import           Prelude              hiding (catch)

instance FromJSON (Text -> SpotifyTrack) where
    parseJSON (Object v) = do
        o <- v .: "track"
        (a:_) <- o .: "artists"
        SpotifyTrack <$> pure 0
                     <*> a .: "name"
                     <*> o .: "name"

-- |Requests a track ID from the Spotify lookup service
identifyTrack :: Text -> IO (Maybe SpotifyTrack)
identifyTrack trackId = do
    r <- catch (simpleHttp $ trackURL trackId) (\e -> return $ const "" (e :: HttpException))
    case decode r of
        Nothing -> return Nothing
        Just f  -> return $ Just $ f trackId

-- |Spotify API URL to track metadata (in JSON)
trackURL :: Text -- ^ Track ID
         -> String
trackURL t = let trackId = unpack t
             in  "http://ws.spotify.com/lookup/1/.json?uri=spotify:track:" ++ trackId
