{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

-- Raft

import           ClientTypes
import           Consensus.Types
import           Network.Protocol
import qualified TestStore as TS

------------------------------------------------------------------------

main :: IO ()
main = Warp.run port app
  where
    port = 3000
    app = WaiWS.websocketsOr
             WS.defaultConnectionOptions
             (wsApp (TestProtocol TS.empty))
             staticApp

staticApp :: Wai.Application
staticApp = WaiStatic.staticApp (WaiStatic.defaultFileServerSettings "static")

------------------------------------------------------------------------

data TestProtocol = TestProtocol
    { ts :: TS.TestStore
    }

instance Protocol TestProtocol where

    type Request TestProtocol = Cmd B.ByteString Int
    type Response TestProtocol = ClientResponse B.ByteString Int

    step tp cmd = case cmd of
          CmdSet k v ->
              let s' = TS.testStore 0 [v] (Term 0) (ts tp) in
              (tp{ts=s'}, Just $ RspSetOK k v)
          CmdGet k -> let rsp = case TS.testQuery 0 (ts tp) of
                                    Just (v, _) -> RspGetOK k v
                                    Nothing -> RspGetFail k
                      in (tp, Just $ rsp)
          CmdSleep n -> (tp, Nothing)

------------------------------------------------------------------------

wsApp :: ( Protocol p
         , WS.WebSocketsData (Request p)
         , WS.WebSocketsData (Response p)
         ) => p -> WS.ServerApp
wsApp p0 pdc = do
    conn <- WS.acceptRequest pdc
    loop conn p0
  where
    loop conn p = do
      cmd <- WS.receiveData conn
      let (p', m'rsp) = step p cmd
      case m'rsp of
          Just rsp -> WS.sendTextData conn rsp
          Nothing  -> return ()
      loop conn p'
