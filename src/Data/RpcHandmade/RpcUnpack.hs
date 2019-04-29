
module Data.RpcHandmade.RpcUnpack where

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


unpackString :: Get String
unpackString = do
  l <- getInt32le
  v <- getByteString (fromIntegral (l)::Int)
  -- TODO: check length
  let s = BC.unpack v
  return $! s


unpackOpaque :: Get Opaque
unpackOpaque = do
  l <- getInt32le
  v <- getByteString (fromIntegral (l)::Int)
  return $! Opaque l v


unpackMsgType :: Get MsgType
unpackMsgType = do
    val <- getInt32le
    return $! toEnum (fromIntegral (val)::Int)

unpackCallBody :: Get CallBody
unpackCallBody = do
    foo <- getInt32le
    return $! CallBody foo


unpackReplyBody :: Get ReplyBody
unpackReplyBody = do
    bar <- unpackString
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
