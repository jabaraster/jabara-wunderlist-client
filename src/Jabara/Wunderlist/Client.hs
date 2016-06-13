{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Jabara.Wunderlist.Client (
    getTasks
  , getTasksAsJson
  , getLists
  , module Jabara.Wunderlist.Client.Types
) where

import Control.Lens((^.))
import Data.Aeson (FromJSON, Value(..))
import Data.Maybe (fromJust)
import Data.Vector (toList)
import Jabara.Wunderlist.Client.Types
import Network.HTTP.Client (Request, parseUrl)
import Network.HTTP.Simple (httpJSON, addRequestHeader, getResponseBody)

access :: (FromJSON a) => Credential -> Request -> IO a
access cre req = do
    res <- httpJSON $ addRequestHeader "X-Access-Token" (cre^.credentialAccessToken)
                    $ addRequestHeader "X-Client-ID" (cre^.credentialClientId)
                      req
    pure $ fromJust $ getResponseBody res

getLists :: Credential -> IO [List]
getLists cre = access cre "https://a.wunderlist.com/api/v1/lists"

taskUrlBase :: String
taskUrlBase = "https://a.wunderlist.com/api/v1/tasks?list_id="

getTasks :: Credential -> WunderlistId -> IO [Task]
getTasks cre wid = do
    req <- parseUrl (taskUrlBase ++ (show wid))
    access cre req

getTasksAsJson :: Credential -> WunderlistId -> IO [Value]
getTasksAsJson cre wid = do
    req <- parseUrl (taskUrlBase ++ (show wid))
    val::Value <- access cre req
    case val of
        Array ary -> pure $ toList ary
        _         -> error $ "error '" ++ (show val) ++ "'"
