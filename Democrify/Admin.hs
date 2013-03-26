{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Admin where

import           Control.Applicative         (optional, (<$>), (<*>))
import           Control.Monad               (mzero)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Monoid                 (mempty)
import Control.Exception.Base (catch)
import           Data.Acid.Advanced          (query', update')
import           Data.Yaml
import           Data.Foldable               (forM_)
import           Data.IORef
import           Data.Sequence               (Seq (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Happstack.Server
import           Text.Blaze                  (toValue, (!))
import           Text.Blaze.Html5            (toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesFileExist)
import Prelude hiding (catch)

-- Democrify modules
import           Acid
import           Queue

-- |Democrify preferences type
data Preferences = Preferences {
      duplicates  :: Bool -- ^ If this is set to @True@, a duplicate song will be added
                          --   to the queue again. If it is @False@ a duplicate song
                          --   will be upvoted.
    , autoShuffle :: Bool -- ^ If this is @True@ the queue will be automatically re-shuffled
                          --   after a song or a playlist is added
    , repeatAll   :: Bool -- ^ If this is @True@ the queue will be "looped", i.e. a song that
                          --   has been played will be added to the queue again. Good for long
                          --   parties with small playlists.
}

-- JSON instances to be used for YAML config files
instance ToJSON Preferences where
    toJSON Preferences{..} = object [ "duplicates"  .= duplicates
                                    , "autoshuffle" .= autoShuffle
                                    , "repeatall"   .= repeatAll ]

instance FromJSON Preferences where
    parseJSON (Object v) = Preferences        <$>
                           v .: "duplicates"  <*>
                           v .: "autoshuffle" <*>
                           v .: "repeatall"
    parseJSON _          = mzero

defaultPreferences :: Preferences
defaultPreferences = Preferences False False False

preferences :: IORef Preferences
preferences = unsafePerformIO $ newIORef defaultPreferences

-- |Updates preferences (in-memory and on disk)
updatePrefs :: Preferences -> IO ()
updatePrefs prefs = do
    path <- prefsPath
    encodeFile path prefs
    writeIORef preferences prefs

-- |Loads the preferences from disk. If no preference file exists
--  'defaultPreferences' will be loaded.
loadPrefs :: IO ()
loadPrefs = do
    path <- prefsPath
    mprefs <- catch (decodeFile path) (initPrefs path)
    case mprefs of
        Nothing -> updatePrefs defaultPreferences
        Just p  -> writeIORef preferences p
  where
    -- If for some reason the preferences can not be read
    -- they will be (re-)initialized with default options
    initPrefs :: FilePath -> ParseException -> IO (Maybe Preferences)
    initPrefs path _ = do encodeFile path defaultPreferences
                          return $ Just defaultPreferences

-- |Gets the current preferences
getPrefs :: IO Preferences
getPrefs = readIORef preferences

-- |This creates the admin queue view (including the remove and "Vote over 9000" option)
adminQueue :: (Seq SpotifyTrack) -> H.Html
adminQueue queue = do
    H.div ! A.class_ "row" $ do
        H.br
        forM_ queue (\SpotifyTrack{..} -> do
            H.div ! A.class_ "row" ! A.id "adminc" $ do
                H.div ! A.class_ "large-2 small-3 columns" $
                    H.img ! A.onclick "void(0)" ! A.class_ "delete" ! A.id (toValue tId) ! A.src "http://placehold.it/80x80&text=DELETE"
                H.div ! A.class_ "large-2 small-3 columns" $
                    H.img ! A.onclick "void(0)" ! A.class_ "next" ! A.id (toValue tId) ! A.src "http://placehold.it/80x80&text=NEXT"
                H.div ! A.class_ "large-8 columns trackitem" $ do
                    H.span ! A.class_ "track" $ do toHtml track
                                                   " ("
                                                   toHtml votes
                                                   ")"
                    H.br
                    H.span ! A.class_ "artist" $ do " by "
                                                    toHtml artist
            H.hr)
        H.div ! A.class_ "row" $ do
            H.div ! A.class_ "large-2 small-3 columns" $
                H.img ! A.src "http://placehold.it/80x80&text=:("
            H.div ! A.class_ "large-10 columns trackitem" $ do
                H.span ! A.class_ "oh-no" $ "Oh no! There is nothing more in the queue! What will happen now?"

-- |Admin web interface with the ability to set the few preferences that we have
adminPrefs :: Preferences -> H.Html
adminPrefs Preferences{..} = do
-- repeat all
  H.div ! A.class_ "row" $ do
    H.div ! A.class_ "switch small-4 columns" $ do
      H.input ! A.id "roff" ! A.name "repeat" ! A.type_ "radio" ! A.checked ""
      H.label ! A.for "roff" ! A.onclick "" $ "Repeat off"

      H.input ! A.id "ron" ! A.name "repeat" ! A.type_ "radio"
      H.label ! A.for "ron" ! A.onclick "" $ "Repeat on"

      H.span $ mempty
    H.div ! A.class_ "small-8 columns" $
      H.p $ "If repeat is activated a played song will be re-added at the end of the queue."
-- auto shuffle
  H.div ! A.class_ "row" $ do
    H.div ! A.class_ "switch small-4 columns" $ do
      H.input ! A.id "soff" ! A.name "shuffle" ! A.type_ "radio" ! A.checked ""
      H.label ! A.for "soff" ! A.onclick "" $ "Shuffle off"

      H.input ! A.id "son" ! A.name "shuffle" ! A.type_ "radio"
      H.label ! A.for "son" ! A.onclick "" $ "Shuffle on"

      H.span $ mempty
    H.div ! A.class_ "small-8 columns" $
      H.p $ "When shuffle is activated the queue will be shuffled when a song / playlist is added."
-- duplicates
  H.div ! A.class_ "row" $ do
    H.div ! A.class_ "switch small-4 columns" $ do
      H.input ! A.id "doff" ! A.name "duplicates" ! A.type_ "radio" ! A.checked ""
      H.label ! A.for "doff" ! A.onclick "" $ "Duplicates off"

      H.input ! A.id "don" ! A.name "duplicates" ! A.type_ "radio"
      H.label ! A.for "don" ! A.onclick "" $ "Duplicates on"

      H.span $ mempty
    H.div ! A.class_ "small-8 columns" $
      H.p $ "If duplicates are allowed a song can be added to the queue twice."


-- |Deletes a track when requested by the admin
adminDeleteHandler :: Text -- ^ Track id
                   -> ServerPart Response
adminDeleteHandler track = do
    acid <- liftIO $ readIORef playQueue
    update' acid $ RemoveTrack track
    ok $ toResponse $ ("removed" :: Text)

-- |Upvotes OVER 9000!
adminUpvoteHandler :: Text
                   -> ServerPart Response
adminUpvoteHandler track = do
    acid <- liftIO $ readIORef playQueue
    update' acid $ AdminUpvote track
    ok $ toResponse $ ("over9000" :: Text)
