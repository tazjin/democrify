{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ForeignFunctionInterface #-}


module User where

import           Control.Applicative         (optional, (<$>))
import           Control.Monad               (msum, when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Acid.Advanced          (query', update')
import           Data.ByteString.Char8       (ByteString)
import           Data.Foldable               (forM_)
import           Data.IORef
import           Data.Monoid                 (mempty)
import qualified Data.Sequence               as SQ
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Happstack.Server
import           System.IO.Unsafe            (unsafePerformIO)
import           Text.Blaze                  (toValue, (!))
import           Text.Blaze.Html5            (toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           HSObjC
import           Data.Maybe             (isJust)

-- Democrify modules
import           Acid
import           Admin
import           Queue
import           WebAPI

-- *Helpers
-- |This contains a global IORef to the Resource folder inside the Application bundle. All web assets are stored in Resources/web
resourcePath :: IORef FilePath
resourcePath = unsafePerformIO $ newIORef ""

-- |Direct path to the web resources
webResources :: IO FilePath
webResources = (++ "/web/") <$> readIORef resourcePath

-- |Default layout including Foundation stylesheets
defaultLayout :: Text     -- ^ Title
              -> [H.Html] -- ^ Headers
              -> H.Html   -- ^ Body
              -> ServerPart Response
defaultLayout  title headers body = ok $ toResponse $
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.name "apple-mobile-web-app-capable" ! A.content "yes"
            H.meta ! A.name "viewport" ! A.content "width=device-width, user-scalable=no"
            -- Stylesheets
            H.link ! A.href "/foundation.min.css" ! A.rel "stylesheet"
            H.link ! A.href "/app.css" ! A.rel "stylesheet"
            -- Favicon
            H.link ! A.href "/democrify_small.png" ! A.rel "icon"
            -- Scripts
            H.script ! A.src "/jquery.js" $ mempty
            H.script ! A.src "/foundation.min.js" $ mempty
            H.script ! A.src "/jquery.cookie.js" $ mempty
            H.script ! A.src "/app.js" $ mempty
            sequence_ headers
        H.body $ do
            H.nav ! A.class_ "top-bar" $ do
                H.ul ! A.class_ "title-area" $ do
                    H.li ! A.class_ "name" $
                        H.h1 $ H.a ! A.href "/" $ do
                            H.img ! A.alt "Logo" ! A.src "/democrify_small.png" ! A.style "height:45px;float:left;"
                            H.span ! A.style "margin-left:5px;float:right;" $ toHtml ("Democrify" :: Text)
                    H.li ! A.class_ "toggle-topbar menu-icon" $
                        H.a ! A.href "#" $ H.span $ "menu"
                H.section ! A.class_ "top-bar-section" $ do
                    H.ul ! A.class_ "right" $ do
                        H.li ! A.class_ "divider" $ mempty
                        H.li $
                            H.a ! A.href "/add" $ toHtml ("Add song" :: Text)
            body
            H.div ! A.class_ "row" $ H.div ! A.class_ "small-12 columns" $ H.footer $ do
                H.hr
                H.p ! A.style "text-align:center;" $ "Powered by Democrify"

-- |Displays the user facing queue list
queueView :: ServerPart Response
queueView = do
    acid <- liftIO $ readIORef playQueue
    current <- liftIO $ displayCurrentTrack
    queue <- query' acid GetQueue
    defaultLayout "Democrify - Queue" [] $ do
        H.div ! A.class_ "row" $ H.div ! A.class_ "small-12 columns" $ do
            H.br
            current
            H.div ! A.class_ "row" $ H.div ! A.class_ "small-10 columns" $
                queueNum queue
            H.hr
            forM_ queue (\SpotifyTrack{..} -> do
                H.div ! A.class_ "row" $ do
                    H.div ! A.class_ "small-3 large-2 columns" $
                        H.img ! A.onclick "void(0)" ! A.class_ "vote" ! A.id (toValue tId) ! A.src "/upvote_bw.png"
                    H.div ! A.class_ "large-10 columns trackitem" $ do
                        H.span ! A.class_ "track" $ toHtml track
                        H.br
                        H.span ! A.class_ "artist" $ do toHtml (" by " :: Text)
                                                        toHtml artist
                H.hr)
            H.div ! A.class_ "row" $ do
                H.div ! A.class_ "small-3 large-2 columns" $
                    H.img ! A.src "http://placehold.it/80x80&text=:("
                H.div ! A.class_ "large-10 columns trackitem" $ do
                    H.span ! A.class_ "oh-no" $ toHtml ("Oh no! There is nothing more in the queue! What will happen now?" :: Text)

queueNum :: SQ.Seq SpotifyTrack -> H.Html
queueNum s | l == 1    = H.span ! A.class_ "queuenum" $ "1 song in the queue"
           | otherwise = H.span ! A.class_ "queuenum" $ toHtml $ T.append (T.pack $ show $ l) " songs in the queue"
  where
    l = SQ.length s

displayCurrentTrack :: IO H.Html
displayCurrentTrack = do
    current <- readIORef currentTrack
    let content = case current of
                    Nothing -> do
                        H.p ! A.class_ "oh-no" $ toHtml ("No track is playing right now!" :: Text)
                    Just SpotifyTrack{..} -> H.a ! A.href (toValue $ T.append "spotify:track:" tId) $ do
                        H.br
                        H.span ! A.class_ "track" $ do toHtml ("Current track:" :: Text)
                                                       H.br
                                                       toHtml track
                        H.br
                        H.span ! A.class_ "artist" $ do toHtml ("by " :: Text)
                                                        toHtml artist
    return $ H.div ! A.class_ "row current" $ do
        H.div ! A.class_ "two columns mobile-one" $
            H.img ! A.src "/current.gif"
        H.div ! A.class_ "ten columns" $ content

-- |Page that displays the song adding interface
addSongView :: ServerPart Response
addSongView =
    defaultLayout "Democrify - Add song"
                  [ H.script ! A.src "/addsong.js" $ mempty ] $ do
        H.style $ toHtml ("body{background-color: #222 !important;} footer{color:white;}" :: Text)
        H.div ! A.class_ "row collapse" $ do
            H.div ! A.class_ "ten mobile-two columns" $
                H.input ! A.id "search" ! A.type_ "text"
            H.div ! A.class_ "one mobile-one columns" $
                H.a ! A.class_ "button expand postfix" ! A.id "searchbutton"  $
                    toHtml ("Search" :: Text)
            H.div ! A.class_ "one mobile-one columns" $ H.form ! A.class_ "custom" $ do
                H.select ! A.style "display:none;" ! A.id "searchtype" $ do
                    H.option ! A.selected "" $ toHtml ("Track" :: Text)
                    H.option $ toHtml ("Artist" :: Text)
                    H.option $ toHtml ("Album" :: Text)
                H.div ! A.class_ "custom dropdown" $ do
                    H.a ! A.href "#" ! A.class_ "selector" $ mempty
                    H.a ! A.href "#" ! A.class_ "current" $ toHtml ("Track" :: Text)
                    H.ul $ do
                        H.li $ toHtml ("Track" :: Text)
                        H.li $ toHtml ("Artist" :: Text)
                        H.li $ toHtml ("Album" :: Text)
        H.div ! A.class_ "row" ! A.id "resultcontainer" ! A.class_ "twelve columns" $ mempty

-- |Upvotes a song based on the ID
upvoteHandler :: Text -> ServerPart Response
upvoteHandler song = do
    acid <- liftIO $ readIORef playQueue
    update' acid $ UpvoteTrack song
    ok $ toResponse $ T.append "Upvoted " song

-- |Adds a song to the queue based on its ID. If the song cannot be
--  found a 404 will be returned, if the song is already in the queue
--  it will be upvoted.
addHandler :: Text -> ServerPart Response
addHandler trackId = do
    track <- liftIO $ identifyTrack trackId
    Preferences{..} <- liftIO getPrefs
    case track of
        Nothing  -> notFound $ toResponse $ ("notfound" :: Text)
        (Just t) -> do acid <- liftIO $ readIORef playQueue
                       when autoShuffle (liftIO shuffleQueue)
                       update' acid $ AddTrackToQueue duplicates t
                       ok $ toResponse $ ("ok" :: Text)

adminHandler :: ServerPart Response
adminHandler = do
    acid <- liftIO $ readIORef playQueue
    queue <- query' acid GetQueue
    defaultLayout "Democrify - Admin"
                  [ H.script ! A.src "/admin.js" $ mempty ]
                  (adminQueue queue)

showPrefs :: ServerPart Response
showPrefs = do
    prefs <- liftIO getPrefs
    defaultLayout "Democrify - Settings" [] (adminPrefs prefs)

-- |This is supplised with the NSArray that contains all the NSStrings to the URLs.
loadPlaylist :: Id -> IO ()
loadPlaylist pl = do
    acid <- readIORef playQueue
    Preferences{..} <- liftIO getPrefs
    runId $ do
        trackIds <- fromId pl
        mTracks <- liftIO $ getTrackData trackIds
        let tracks = map (\(Just t) -> t) $ filter isJust mTracks
        forM_ tracks $ \t -> update' acid $ AddTrackToQueue duplicates t
    when autoShuffle shuffleQueue
    return ()

-- * Happstack things

-- |This contains the routing function for Happstack. I don't have time for type-safe routing in this project! :D
democrify :: ServerPart Response
democrify = liftIO webResources >>= \resPath -> msum
    [ nullDir >> queueView
    , dir "upvote" $ path $ \song -> upvoteHandler song
    , dir "add" $ nullDir >> addSongView
    , dir "add" $ path $ \song -> addHandler song
    , serveDirectory DisableBrowsing [] resPath
    , dir "admin" $ nullDir >> host "localhost:8686" adminHandler
    , dir "admin" $ dir "vote" $ host "localhost:8686" $ path $ \song -> adminUpvoteHandler song
    , dir "admin" $ dir "delete" $ host "localhost:8686" $ path $ \song -> adminDeleteHandler song
    , dir "admin" $ dir "config" $ host "localhost:8686" $ showPrefs
    ]

runServer :: IO ()
runServer = simpleHTTP nullConf{port = 8686} democrify

foreign export ccall loadPlaylist    :: Id -> IO ()
