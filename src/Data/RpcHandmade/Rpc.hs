
module Data.RpcHandmade.Rpc
  ( packRpcMsg
  , unpackRpcMsg
  ) where


import           System.IO
import           GHC.Base hiding (Opaque)
import           GHC.Show
import           GHC.Enum
import           Data.List
import qualified Data.ByteString as B
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


unpackOpaque :: Get Opaque
unpackOpaque = do
  l <- getInt32le
  v <- getByteString (fromIntegral (l)::Int)
  return $! Opaque l v

packOpaque :: B.ByteString -> Put
packOpaque bs = do
    -- TODO : pad to 4
    let l = (fromIntegral (B.length bs))::Int32
    putInt32le l
    putByteString bs

unpackMsgType :: Get MsgType
unpackMsgType = do
    val <- getInt32le
    return $! toEnum (fromIntegral (val)::Int)

packMsgType :: MsgType -> Put
packMsgType t = do
    putInt32le $ (fromIntegral (fromEnum (t))::Int32)

unpackCallBody :: Get CallBody
unpackCallBody = do
    foo <- getInt32le
    return $! CallBody foo


unpackReplyBody :: Get ReplyBody
unpackReplyBody = do
    bar <- unpackOpaque
    return $! ReplyBody bar


unpackMsgBody :: Get MsgBody
unpackMsgBody = do
    typ <- unpackMsgType
    case typ of
        CALL -> do
            c <- unpackCallBody
            return $! MsgBody typ (Just c) (Nothing)
        REPLY -> do
            c <- unpackReplyBody
            return $! MsgBody typ (Nothing) (Just c)
--        _ -> do
--            return $! MsgBody typ (Nothing) (Nothing)


unpackRpcMsg :: Get RpcMsg
unpackRpcMsg = do
    xid <- getInt32le
    msg <- unpackMsgBody
    return $! RpcMsg xid msg


packMsgBody :: Smth -> Put
packMsgBody (Smth i j) = do
    putInt32le i
    putInt32le j

--packRpcMsg :: Put Dummy
packRpcMsg :: Dummy -> Put
packRpcMsg (Dummy i d t) = do
    putInt32le i
    packMsgBody d
    packMsgType t
    --putInt32le (yy j)
    --putInt32le (zz j)
