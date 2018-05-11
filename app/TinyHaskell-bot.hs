{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits
import Network.Discord

  -- import AST
-- import Parser
-- import Typing
import Lib
import DiscordSecret (token)

instance DiscordAuth IO where
  auth    = return $ Bot token
  version = return "0.9.0"
  runIO   = id

data MnemonicHandler

instance EventMap MnemonicHandler (DiscordApp IO) where
  type Domain   MnemonicHandler = Message
  type Codomain MnemonicHandler = ()

  mapEvent p (m@Message{ messageContent = c
                       , messageChannel = ch
                       , messageAuthor = User{userIsBot = bot, userId = uid}}
             )
    | bot = return ()
    | ":p " `T.isPrefixOf` c = do
        v <- ("-- version: " ++) <$> version
        let code = drop 3 (T.unpack c)
            res = "<@" ++ show uid ++ ">, I did. " ++ v ++ "\n```haskell\n" ++ parsing code ++ "```"
        void $ doFetch $ CreateMessage ch (T.pack res) Nothing
    | ":t " `T.isPrefixOf` c = do
        v <- ("-- version: " ++) <$> version
        let code = drop 3 (T.unpack c)
            res = "<@" ++ show uid ++ ">, I did. " ++ v ++ "\n```haskell\n" ++ typing code ++ "```"
        void $ doFetch $ CreateMessage ch (T.pack res) Nothing
    | "!help" `T.isPrefixOf` c = do
        v <- ("version: " ++) <$> version
        void $ doFetch $ CreateMessage ch (T.pack (v ++ "\n" ++ "not yet")) Nothing
    | otherwise = return ()

type TypeCheckApp = (MessageCreateEvent :<>: MessageUpdateEvent) :> MnemonicHandler

instance EventHandler TypeCheckApp IO

main :: IO ()
main = runBot (Proxy :: Proxy (IO TypeCheckApp))
