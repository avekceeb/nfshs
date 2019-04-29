
module Data.RpcHandmade.RpcPack where

import           System.IO
import           GHC.Base hiding (Opaque)
import           GHC.Show
import           GHC.Enum
import           Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word
import           Data.Functor
import           Data.Int
import           GHC.Real
import           Data.Tuple (swap)
import           Data.Maybe (fromJust)
import           Data.RpcHandmade.Types


packString :: String -> Put
packString s = do
  let l = (fromIntegral (length s))::Int32
  putInt32le l
  putByteString $ BC.pack s


packOpaque :: BL.ByteString -> Put
packOpaque bs = do
  -- TODO : pad to 4
  let l = (fromIntegral (BL.length bs))::Int32
  putInt32le l
  putByteString $ BL.toStrict bs


packMsgType :: MsgType -> Put
packMsgType t = do
  putInt32le $ (fromIntegral (fromEnum (t))::Int32)

{-
packMsgBody :: Smth -> Put
packMsgBody (Smth i j) = do
  putInt32le i
  putInt32le j
-}
packMsgBody :: MsgBody -> Put
packMsgBody (MsgBody t c r) = do
  packMsgType t
  case t of
    CALL -> packRpcCall c
    REPLY -> packRpcReply r


packRpcCall :: Maybe CallBody -> Put
packRpcCall b = do
  case b of
    Just x -> putInt32le (foo x)
    --Nothing ->


packRpcReply :: Maybe ReplyBody -> Put
packRpcReply b = do
  case b of
    Just x -> packString $ bar x
    --Nothing ->


packRpcMsg :: RpcMsg -> Put
packRpcMsg (RpcMsg xid msg) = do
  putInt32le xid
  packMsgBody msg
  --packMsgType t
  --putInt32le (yy j)
  --putInt32le (zz j)
