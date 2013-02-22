{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Admin where

import           Control.Applicative         (optional, (<$>))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Acid.Advanced          (query', update')
import           Data.Foldable               (forM_)
import           Data.IORef
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Happstack.Server
import           Text.Blaze                  (toValue, (!))
import           Text.Blaze.Html5            (toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Data.Sequence        (Seq (..))

-- Democrify modules
import           Acid
import           Queue

-- |This creates the admin queue view (including the remove and "Vote over 9000" option)
adminQueue :: (Seq SpotifyTrack) -> H.Html
adminQueue queue = do
    H.div ! A.class_ "row" $ H.div ! A.class_ "twelve columns" $ do
        H.br
        forM_ queue (\SpotifyTrack{..} -> do
            H.div ! A.class_ "row" ! A.id "adminc" $ do
                H.div ! A.class_ "two columns mobile-one" $
                    H.img ! A.onclick "void(0)" ! A.class_ "delete" ! A.id (toValue tId) ! A.src "http://placehold.it/80x80&text=DELETE"
                H.div ! A.class_ "two columns mobile-one" $
                    H.img ! A.onclick "void(0)" ! A.class_ "next" ! A.id (toValue tId) ! A.src "http://placehold.it/80x80&text=NEXT"
                H.div ! A.class_ "eight columns trackitem" $ do
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
