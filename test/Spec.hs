{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens ((^.))
import Data.Aeson (Value(..), decode)
import Data.Aeson.Lens (key)
import Data.ByteString.Lazy.Char8 as LB (readFile)
import Data.Maybe (fromJust)
import qualified Data.Text as T (Text, pack, append)
import Data.Vector (toList)

main :: IO ()
main = do
    c <- LB.readFile "./tasks.json"
    val::Value <- pure $ fromJust $ decode c
    tasks <- case val of
        Array ary -> pure $ toList ary
        _         -> error $ "error '" ++ (show val) ++ "'"

    print $ map (textValue "title") tasks

    Prelude.putStrLn "end."

textValue ::T.Text -> Value -> T.Text
textValue prop val = case (Just val) ^. key prop of
    Nothing  -> ""
    Just (String t) -> "\"" `T.append` t `T.append` "\""
    Just (Number n) -> T.pack $ show n
    Just (Bool   b) -> T.pack $ show b
    Just (Null    ) -> ""
    Just (_       ) -> error $ "error '" ++ (show val) ++ "'"

