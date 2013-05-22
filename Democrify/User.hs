{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}


module User where

import           Control.Applicative           (optional, (<$>))
import           Control.Monad                 (msum, when)
import           Control.Monad.IO.Class        (liftIO)
import           Data.ByteString.Char8         (ByteString)
import           Data.Foldable                 (forM_)
import           Data.IORef
import           Data.Maybe                    (isJust)
import           Data.Monoid                   (mempty)
import qualified Data.Sequence                 as SQ
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Lazy                (toStrict)
import qualified Data.Text.Lazy                as TL
import           HSObjC
import           Network.HTTP.Types.Status
import           Network.Wai.Handler.Warp
import           System.IO.Unsafe              (unsafePerformIO)
import           Text.Blaze                    (toValue, (!))
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              (toHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Web.Scotty

-- Democrify modules
import           Acid
import           Admin
import           Queue
import           WebAPI

-- This is an orphan instance!
instance Parsable Text where
    parseParam = either (Left . id) (Right . toStrict) . parseParam

-- |This function renders 'Html' as 'Text' and passes it to scotty
html' = html . renderHtml

-- *Helpers
-- |This contains a global IORef to the Resource folder inside the Application bundle. All web assets are stored in Resources/web
resourcePath :: IORef FilePath
resourcePath = unsafePerformIO $ newIORef ""

-- |Direct path to the web resources
webResources :: IO FilePath
webResources = (++ "/web/") <$> readIORef resourcePath

-- |Default layout including Foundation stylesheets
defaultLayout :: TL.Text     -- ^ Title
              -> [H.Html] -- ^ Additional scripts
              -> H.Html   -- ^ Body
              -> ActionM ()
defaultLayout title scripts body =
  -- The weird-looking line below makes sure that the content header is set after executing html'.
  -- This is done to prevent it from being overridden.
  flip (>>) (header "Content-Type" "text/html; charset=utf-8") $ html' $ H.docTypeHtml $ do
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
            H.script ! A.src "/custom.modernizr.js" $ mempty
        H.body $ do
            H.nav ! A.class_ "top-bar" $ do
                H.ul ! A.class_ "title-area" $ do
                    H.li ! A.class_ "name" $
                        H.h1 $ H.a ! A.href "/" $ do
                            H.img ! A.alt "Logo" ! A.src "/democrify_small.png" ! A.style "height:45px;float:left;"
                            H.span ! A.style "margin-left:5px;float:right;" $ toHtml ("Democrify" :: Text)
                    H.li ! A.class_ "toggle-topbar menu-icon" $
                        H.a ! A.href "#" $ H.span "menu"
                H.section ! A.class_ "top-bar-section" $
                    H.ul ! A.class_ "right" $ do
                        H.li ! A.class_ "divider" $ mempty
                        H.li $
                            H.a ! A.href "/add" $ toHtml ("Add song" :: Text)
            body
            H.div ! A.class_ "row" $ H.div ! A.class_ "small-12 columns" $ H.footer $ do
                H.hr
                H.p ! A.style "text-align:center;" $ "Powered by Democrify"
            H.script ! A.src "/jquery.js" $ mempty
            H.script ! A.src "/foundation.min.js" $ mempty
            H.script ! A.src "/jquery.cookie.js" $ mempty
            H.script ! A.src "/app.js" $ mempty
            sequence_ scripts


-- |Displays the user facing queue list
queueView :: ActionM ()
queueView = do
    current <- liftIO displayCurrentTrack
    queue <- dfQuery GetQueue
    defaultLayout "Democrify - Queue" [] $
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
                H.div ! A.class_ "large-10 columns trackitem" $
                    H.span ! A.class_ "oh-no" $ toHtml ("Oh no! There is nothing more in the queue! What will happen now?" :: Text)

queueNum :: SQ.Seq SpotifyTrack -> H.Html
queueNum s | l == 1    = H.span ! A.class_ "queuenum" $ "1 song in the queue"
           | otherwise = H.span ! A.class_ "queuenum" $ toHtml $ T.append (T.pack $ show l) " songs in the queue"
  where
    l = SQ.length s

displayCurrentTrack :: IO H.Html
displayCurrentTrack = do
    current <- readIORef currentTrack
    let content = case current of
                    Nothing ->
                        H.p ! A.class_ "oh-no" $ toHtml ("No track is playing right now!" :: Text)
                    Just SpotifyTrack{..} -> H.a ! A.href (toValue $ TL.append "spotify:track:" tId) $ do
                        H.br
                        H.span ! A.class_ "track" $ do toHtml ("Current track:" :: Text)
                                                       H.br
                                                       toHtml track
                        H.br
                        H.span ! A.class_ "artist" $ do toHtml ("by " :: Text)
                                                        toHtml artist
    return $ H.div ! A.class_ "row current" $ do
        H.div ! A.class_ "small-3 large-2 columns" $
            H.img ! A.src "/current.gif"
        H.div ! A.class_ "large-10 columns" $ content

-- |Page that displays the song adding interface
addSongView :: ActionM ()
addSongView =
    defaultLayout "Democrify - Add song"
                  [ H.script ! A.src "/addsong.js" $ mempty ] $ do
        H.style $ toHtml ("body{background-color: #222 !important;} footer{color:white;}" :: Text)
        H.div ! A.class_ "row collapse" $ do
            H.div ! A.class_ "large-10 small-6 columns" $
                H.input ! A.id "search" ! A.type_ "text"
            H.div ! A.class_ "large-1 small-3 columns" $
                H.a ! A.class_ "button expand postfix" ! A.id "searchbutton"  $
                    toHtml ("Search" :: Text)
            H.div ! A.class_ "large-1 small-3 columns" $ H.form ! A.class_ "custom" $
                H.select ! A.id "searchtype" $ do
                    H.option ! A.selected "" $ "Track"
                    H.option "Artist"
                    H.option "Album"
        H.div ! A.class_ "row" ! A.id "resultcontainer" ! A.class_ "small-12 columns" $ mempty

-- |Upvotes a song based on the ID
upvoteHandler :: TL.Text -> ActionM ()
upvoteHandler song = do
    dfUpdate $ UpvoteTrack song
    text $ TL.append "Upvoted " song

-- |Adds a song to the queue based on its ID. If the song cannot be
--  found a 404 will be returned, if the song is already in the queue
--  it will be upvoted.
addHandler :: TL.Text -> ActionM ()
addHandler trackId = do
    track <- liftIO $ identifyTrack trackId
    Preferences{..} <- liftIO getPrefs
    case track of
        Nothing  -> status (mkStatus 404 "Not found") >> text "notfound"
        (Just t) -> do when autoShuffle (liftIO shuffleQueue)
                       dfUpdate $ AddTrackToQueue duplicates t
                       text "ok"

adminHandler :: ActionM ()
adminHandler = do
    queue <- dfQuery GetQueue
    defaultLayout "Democrify - Admin"
                  [ H.script ! A.src "/admin.js" $ mempty ]
                  (adminQueue queue)

showPrefs :: ActionM ()
showPrefs = do
    prefs <- liftIO getPrefs
    defaultLayout "Democrify - Settings" [] (adminPrefs prefs)

-- |This is supplised with the NSArray that contains all the NSStrings to the URLs.
loadPlaylist :: Id -> IO ()
loadPlaylist pl = do
    Preferences{..} <- liftIO getPrefs
    runId $ do
        trackIds <- fmap (map TL.fromStrict) $ fromId pl
        mTracks <- liftIO $ getTrackData trackIds
        let tracks = map (\(Just t) -> t) $ filter isJust mTracks
        forM_ tracks $ \t -> dfUpdate $ AddTrackToQueue duplicates t
    when autoShuffle shuffleQueue
    return ()

-- * Scotty things

-- |This serves static files
serveStatic f = liftIO webResources >>= (\resPath -> file $ resPath ++ f)

-- |This contains the routing function for Happstack. I don't have time for type-safe routing in this project! :D
--democrify :: ActionM
democrify :: ScottyM ()
democrify = do
    get "/"                     queueView
    get "/upvote/:song"         $ param "song" >>= upvoteHandler
    get "/add"                  addSongView
    get "/add/:song"            $ param "song" >>= addHandler
    get "/:file"                serveStatic

-- |This is the Scotty Application representing the admin handler.
adminRoutes :: ScottyM ()
adminRoutes = liftIO webResources >>= \resPath -> do
    get "/admin"                adminHandler
    get "/admin/vote/:song"     $ param "song" >>= adminUpvoteHandler
    get "/admin/delete/:song"   $ param "song" >>= adminDeleteHandler
    get "/admin/config"         showPrefs
    get "/:file"                serveStatic

runServer :: IO ()
runServer = scotty 8686 democrify

runAdminServer :: IO ()
runAdminServer = scottyOpts adminOpts adminRoutes
    where
        adminOpts = Options 0 $ defaultSettings { settingsPort = 1337, settingsHost = "127.0.0.1" }

foreign export ccall loadPlaylist    :: Id -> IO ()
