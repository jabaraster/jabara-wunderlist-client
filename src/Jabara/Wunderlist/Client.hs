{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Jabara.Wunderlist.Client (
    getLists
  , getListUsers
  , getTasks
  , getListNotes
  , getTasksAsJson
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

-- | 本当はリストを限定して、そのリストにアクセスできるユーザを
-- 取得したい. それにはクエリパラメータ list_id を付ければいい・・・
-- とドキュメントにはあるのだが、実際に操作すると500が返ってくる・・・
getListUsers :: Credential -> IO [ListUser]
getListUsers cre = access cre "https://a.wunderlist.com/api/v1/users"

getLists :: Credential -> IO [List]
getLists cre = access cre "https://a.wunderlist.com/api/v1/lists"

taskUrlBase :: String
taskUrlBase = "https://a.wunderlist.com/api/v1/tasks?list_id="

getTasks :: Credential -> ListId -> IO [Task]
getTasks cre lid =
    parseUrl (taskUrlBase ++ (show lid))
      >>= access cre

getTasksAsJson :: Credential -> ListId -> IO [Value]
getTasksAsJson cre lid = do
    req <- parseUrl (taskUrlBase ++ (show lid))
    val::Value <- access cre req
    case val of
        Array ary -> pure $ toList ary
        _         -> error $ "error '" ++ (show val) ++ "'"
getListNotes :: Credential -> ListId -> IO [Note]
getListNotes cre lid =
    parseUrl ("https://a.wunderlist.com/api/v1/notes?list_id=" ++ (show lid))
      >>= access cre
