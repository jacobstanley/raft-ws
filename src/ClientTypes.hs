{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -w #-}

module ClientTypes (
    Cmd(..)
  , ClientResponse(..)
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.WebSockets as WS

------------------------------------------------------------------------

data Cmd k v =
    CmdSet k v
  | CmdGet k
  | CmdSleep Int
  | CmdHelp (Maybe B.ByteString)
  -- | CmdUse Host PortNumber
  -- | CmdPause
  -- | CmdDump
  deriving (Read, Show)

data ClientResponse k v =
    RspSetOK k v
  | RspGetOK k v
  | RspGetFail k
  deriving (Read, Show, Eq)

------------------------------------------------------------------------

instance WS.WebSocketsData (Cmd B.ByteString Int) where
    fromLazyByteString = read . L.unpack
    toLazyByteString   = L.pack . show

instance WS.WebSocketsData (ClientResponse B.ByteString Int) where
    fromLazyByteString = read . L.unpack
    toLazyByteString   = L.pack . show

