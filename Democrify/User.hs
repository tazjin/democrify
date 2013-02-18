{-# LANGUAGE OverloadedStrings, RecordWildCards      #-}

module User where

import           Control.Applicative   (optional, (<$>))
import           Control.Monad         (msum)
import           Data.ByteString.Char8 (ByteString)
import           Data.IORef
import           Data.Text             (Text)
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
    H.html ! A.class_ "no-js" $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.name "viewport" ! A.content "width=device-width, user-scalable=no"
            -- Stylesheets
            H.link ! A.href "foundation.min.css" ! A.rel "stylesheet"
            H.link ! A.href "app.css" ! A.rel "stylesheet"
            -- Scripts
            H.script ! A.src "/modernizr.foundation.js" $ mempty
            H.script ! A.src "/foundation.min.js" $ mempty
            H.script ! A.src "/app.js" $ mempty
            sequence_ headers
        H.body $ do
            H.nav ! A.class_ "top-bar" $ do
                H.ul $ do
                    H.li $
                        H.img ! A.alt "Logo" ! A.src "/democrify_small.png" ! A.style "height:45px;"
                    H.li ! A.class_ "name" $
                        H.h1 $ H.a ! A.href "#" $ toHtml ("Democrify" :: Text)
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
    queue <- query' acid GetQueue
    defaultLayout "Democrify - Queue" [] $ forM_ queue (\SpotifyTrack{..} ->
        H.div ! A.class_ "row" $
            H.div ! A.class_ "twelve columns" $ do
                H.span ! A.class_ "artist" $ (toHtml artist)
                toHtml (" - " :: Text)
                H.span ! A.class_ "track" $ (toHtml track))

-- * Happstack things

-- |This contains the routing function for Happstack. I don't have time for type-safe routing in this project! :D
democrify :: ServerPart Response
democrify = liftIO webResources >>= \path -> msum
    [ nullDir >> queueView
    , serveDirectory DisableBrowsing [] path
    ]

runServer :: IO ()
runServer = simpleHTTP nullConf{port = 8686} democrify
