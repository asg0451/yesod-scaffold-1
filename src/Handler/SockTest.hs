{-# LANGUAGE ScopedTypeVariables #-}
module Handler.SockTest where

import           Import

import qualified Data.Text.Lazy         as T
import           Text.Julius            (RawJS (..))

import           Control.Concurrent.STM as S
import           Network.WebSockets     (ConnectionException (..))
import           Yesod.WebSockets

import qualified Data.Map               as M
-- not reqd

import qualified Network.WebSockets     as W

import           Data.Aeson

import           Data.Conduit

getSockTestR :: Handler Html
getSockTestR = do
    webSockets chatApp
    defaultLayout $
        do setTitle "watwatwat!"
           outputId <- newIdent
           formId   <- newIdent
           inputId  <- newIdent
           $(widgetFile "socktest")

chatApp :: WebSocketsT Handler ()
chatApp = do
    connection <- ask
    sendTextData ("welcome to the ws test. please send name." :: T.Text)
    (name :: T.Text) <- receiveData
    sockMap <- appSocketsMap <$> appChatStuff <$> getYesod
    liftIO $ S.atomically $ modifyTVar sockMap $ M.insert name connection
    smap <- liftIO $ readTVarIO sockMap
    liftIO $ print $ M.size smap
    sendTextData $ "Welcome " <> name
    sendBinaryData $ encode $ object [("boop" .= ("beep" :: String))]
    writeChan <- appChatChan <$> appChatStuff <$> getYesod
    readChan <-
        liftIO $
        S.atomically $
        do writeTChan writeChan $ name <> " has joined the chat"
           dupTChan writeChan
    (race_
         (forever $
          do val <- liftIO $ S.atomically (readTChan readChan)
             e <- sendTextData val
             liftIO $ print "sent data")
         (sourceWS $$
          mapM_C
              (\(msg :: T.Text) ->
                    do liftIO $
                           S.atomically $
                           writeTChan writeChan $ name <> ": " <> msg
                       liftIO $ print "received data"))) `catch`
        (\(e :: ConnectionException)    -- not actually doing any handling
           ->
              do case e of
                     CloseRequest code reason ->
                         liftIO $
                         S.atomically $ modifyTVar sockMap $ M.delete name
                     ConnectionClosed ->
                         liftIO $
                         S.atomically $ modifyTVar sockMap $ M.delete name
                     ParseException s -> return ()
                 print e) `catch`
        (\(e :: SomeException) ->
              liftIO $
              do print e
                 S.atomically $ modifyTVar sockMap $ M.delete name)
    smap <- liftIO $ readTVarIO sockMap
    liftIO $ print $ M.size smap
    liftIO $
        mapM_
            (\c ->
                  W.sendTextData c ("someone left" :: T.Text)) $
        smap
