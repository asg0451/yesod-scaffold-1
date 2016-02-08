{-# LANGUAGE ScopedTypeVariables #-}
module Handler.SockTest where

import           Import

import qualified Data.Text.Lazy                as T

import           Control.Concurrent.STM        as S
import           Yesod.WebSockets

import           Control.Concurrent.STM.TVar
import qualified Data.Map                      as M
-- not reqd
import qualified Network.WebSockets.Connection as W


getSockTestR :: Handler Html
getSockTestR = do
    webSockets chatApp
    defaultLayout $
        do setTitle "watwatwat!"
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
    writeChan <- appChatChan <$> appChatStuff <$> getYesod
    readChan <-
        liftIO $
        S.atomically $
        do writeTChan writeChan $ name <> " has joined the chat"
           dupTChan writeChan
    race_
        (forever $
         do val <- liftIO $ S.atomically (readTChan readChan)
            sendTextData val)
        (sourceWS $$
         mapM_C
             (\(msg :: T.Text) ->
                   liftIO $
                   S.atomically $ writeTChan writeChan $ name <> ": " <> msg))
