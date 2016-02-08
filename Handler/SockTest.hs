{-# LANGUAGE ScopedTypeVariables #-}
module Handler.SockTest where

import           Import

import qualified Data.Text.Lazy         as T

import           Control.Concurrent.STM as S
import           Yesod.WebSockets

getSockTestR :: Handler Html
getSockTestR = do
    webSockets chatApp
    defaultLayout $
        do setTitle "watwatwat!"
           $(widgetFile "socktest")

chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("welcome to the ws test. please sendname." :: T.Text)
    (name :: T.Text) <- receiveData
    sendTextData $ "Welcome " <> name
    writeChan <- fmap appChatChan getYesod
    readChan <-
        liftIO $
        S.atomically $
        do writeTChan writeChan $ name <> "has joined the chat"
           dupTChan writeChan
    race_
        (forever $
         do val <- liftIO $ S.atomically (readTChan readChan)
            print val
            sendTextData val)
        (sourceWS $$
         mapM_C
             (\(msg :: T.Text) ->
                   liftIO $
                   S.atomically $ writeTChan writeChan $ name <> ": " <> msg))
--    sendClose ("wat" :: T.Text)
