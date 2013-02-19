{-# LANGUAGE OverloadedStrings, RecordWildCards      #-}

module User where

import           Control.Applicative   (optional, (<$>))
import           Control.Monad         (msum)
import           Data.ByteString.Char8 (ByteString)
import           Data.IORef
import           Data.Text             (Text)
import qualified Data.Text as T
import           Happstack.Server
import           System.IO.Unsafe      (unsafePerformIO)
import           Control.Monad.IO.Class (liftIO)
import           Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 (toHtml)
import           Data.Acid.Advanced  (query', update')
import Data.Foldable (forM_)
import qualified Data.Sequence        as SQ
import Data.Monoid (mempty)

-- Democrify modules
import Acid
import Queue

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
            H.link ! A.href "foundation.min.css" ! A.rel "stylesheet"
            H.link ! A.href "app.css" ! A.rel "stylesheet"
            -- Scripts
            H.script ! A.src "/foundation.min.js" $ mempty
            H.script ! A.src "/jquery.cookie.js" $ mempty
            H.script ! A.src "/app.js" $ mempty
            sequence_ headers
        H.body $ do
            H.nav ! A.class_ "top-bar" $ do
                H.ul $ do
                    H.li $
                        H.img ! A.alt "Logo" ! A.src "/democrify_small.png" ! A.style "height:45px;"
                    H.li ! A.class_ "name" $
                        H.h1 $ H.a $ toHtml ("Democrify" :: Text)
                    H.li ! A.class_ "toggle-topbar" $
                        H.a ! A.href "#" $ mempty
                H.section $ do
                    H.ul ! A.class_ "right" $ do
                        H.li ! A.class_ "divider" $ mempty
                        H.li $
                            H.a ! A.href "/add" $ toHtml ("Add song" :: Text)
            body

-- |Displays the user facing queue list
queueView :: ServerPart Response
queueView = do
    acid <- liftIO $ readIORef playQueue
    current <- liftIO $ displayCurrentTrack
    queue <- query' acid GetQueue
    defaultLayout "Democrify - Queue" [] $ do
        H.div ! A.class_ "row" $ H.div ! A.class_ "twelve columns" $ do
            H.br
            current
            H.br
            forM_ queue (\SpotifyTrack{..} -> do
                H.div ! A.class_ "row" $ do
                    H.div ! A.class_ "two columns mobile-one" $
                        H.img ! A.onclick "void(0)" ! A.class_ "vote" ! A.id (toValue tId) ! A.src "/upvote_bw.png"
                    H.div ! A.class_ "ten columns trackitem" $ do
                        H.span ! A.class_ "track" $ toHtml track
                        H.br
                        H.span ! A.class_ "artist" $ do toHtml (" by " :: Text)
                                                        toHtml artist
                H.hr)
            H.div ! A.class_ "row" $ do
                H.div ! A.class_ "two columns mobile-one" $
                    H.img ! A.src "http://placehold.it/80x80&text=:("
                H.div ! A.class_ "ten columns trackitem" $ do
                    H.span ! A.class_ "oh-no" $ toHtml ("Oh no! There is nothing more in the queue! What will happen now?" :: Text)
        H.div ! A.class_ "row" $ H.div ! A.class_ "twelve columns" $ H.footer $ do
            H.hr
            H.p ! A.style "text-align:center;" $ "Powered by Democrify"

displayCurrentTrack :: IO H.Html
displayCurrentTrack = do
    current <- readIORef currentTrack
    let content = case current of
                    Nothing -> do
                        H.p ! A.class_ "oh-no" $ toHtml ("No track is playing right now!" :: Text)
                    Just SpotifyTrack{..} -> do
                        H.span ! A.class_ "track" $ toHtml track
                        H.br
                        H.span ! A.class_ "artist" $ do toHtml ("by " :: Text)
                                                        toHtml artist
    return $ H.div ! A.class_ "row current" $ do
        H.div ! A.class_ "two columns mobile-one" $
            H.img ! A.src "/current.gif"
        H.div ! A.class_ "ten columns" $ content


upvoteHandler :: Text -> ServerPart Response
upvoteHandler song = do
    acid <- liftIO $ readIORef playQueue
    queue <- update' acid $ UpvoteTrack song
    ok $ toResponse $ T.append "Upvoted " song

-- * Happstack things

-- |This contains the routing function for Happstack. I don't have time for type-safe routing in this project! :D
democrify :: ServerPart Response
democrify = liftIO webResources >>= \resPath -> msum
    [ nullDir >> queueView
    , dir "upvote" $ path $ \song -> upvoteHandler song
    , serveDirectory DisableBrowsing [] resPath
    ]

runServer :: IO ()
runServer = simpleHTTP nullConf{port = 8686} democrify
