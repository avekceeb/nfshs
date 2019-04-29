{-# LANGUAGE DuplicateRecordFields #-}
-- https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/duplicate-record-fields
-- https://wiki.haskell.org/Name_clashes_in_record_fields
-- https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields

module Data.Rpc.Rpc where

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


data Auth_flavor = AUTH_NONE | AUTH_SYS | AUTH_SHORT | AUTH_DH | RPCSEC_GSS deriving (Eq, Show)
instance Enum Auth_flavor where
  fromEnum AUTH_NONE = 0
  fromEnum AUTH_SYS = 1
  fromEnum AUTH_SHORT = 2
  fromEnum AUTH_DH = 3
  fromEnum RPCSEC_GSS = 6
  toEnum 0 = AUTH_NONE
  toEnum 1 = AUTH_SYS
  toEnum 2 = AUTH_SHORT
  toEnum 3 = AUTH_DH
  toEnum 6 = RPCSEC_GSS

data Opaque_auth = Opaque_auth
  { flavor :: Auth_flavor
  , body :: String
  } deriving (Show)

data Msg_type = CALL | REPLY deriving (Eq, Show)
instance Enum Msg_type where
  fromEnum CALL = 0
  fromEnum REPLY = 1
  toEnum 0 = CALL
  toEnum 1 = REPLY

data Reply_stat = MSG_ACCEPTED | MSG_DENIED deriving (Eq, Show)
instance Enum Reply_stat where
  fromEnum MSG_ACCEPTED = 0
  fromEnum MSG_DENIED = 1
  toEnum 0 = MSG_ACCEPTED
  toEnum 1 = MSG_DENIED

data Accept_stat = SUCCESS | PROG_UNAVAIL | PROG_MISMATCH | PROC_UNAVAIL | GARBAGE_ARGS | SYSTEM_ERR deriving (Eq, Show)
instance Enum Accept_stat where
  fromEnum SUCCESS = 0
  fromEnum PROG_UNAVAIL = 1
  fromEnum PROG_MISMATCH = 2
  fromEnum PROC_UNAVAIL = 3
  fromEnum GARBAGE_ARGS = 4
  fromEnum SYSTEM_ERR = 5
  toEnum 0 = SUCCESS
  toEnum 1 = PROG_UNAVAIL
  toEnum 2 = PROG_MISMATCH
  toEnum 3 = PROC_UNAVAIL
  toEnum 4 = GARBAGE_ARGS
  toEnum 5 = SYSTEM_ERR

data Reject_stat = RPC_MISMATCH | AUTH_ERROR deriving (Eq, Show)
instance Enum Reject_stat where
  fromEnum RPC_MISMATCH = 0
  fromEnum AUTH_ERROR = 1
  toEnum 0 = RPC_MISMATCH
  toEnum 1 = AUTH_ERROR

data Auth_stat = AUTH_OK | AUTH_BADCRED | AUTH_REJECTEDCRED | AUTH_BADVERF | AUTH_REJECTEDVERF | AUTH_TOOWEAK | AUTH_INVALIDRESP | AUTH_FAILED | AUTH_KERB_GENERIC | AUTH_TIMEEXPIRE | AUTH_TKT_FILE | AUTH_DECODE | AUTH_NET_ADDR | RPCSEC_GSS_CREDPROBLEM | RPCSEC_GSS_CTXPROBLEM deriving (Eq, Show)
instance Enum Auth_stat where
  fromEnum AUTH_OK = 0
  fromEnum AUTH_BADCRED = 1
  fromEnum AUTH_REJECTEDCRED = 2
  fromEnum AUTH_BADVERF = 3
  fromEnum AUTH_REJECTEDVERF = 4
  fromEnum AUTH_TOOWEAK = 5
  fromEnum AUTH_INVALIDRESP = 6
  fromEnum AUTH_FAILED = 7
  fromEnum AUTH_KERB_GENERIC = 8
  fromEnum AUTH_TIMEEXPIRE = 9
  fromEnum AUTH_TKT_FILE = 10
  fromEnum AUTH_DECODE = 11
  fromEnum AUTH_NET_ADDR = 12
  fromEnum RPCSEC_GSS_CREDPROBLEM = 13
  fromEnum RPCSEC_GSS_CTXPROBLEM = 14
  toEnum 0 = AUTH_OK
  toEnum 1 = AUTH_BADCRED
  toEnum 2 = AUTH_REJECTEDCRED
  toEnum 3 = AUTH_BADVERF
  toEnum 4 = AUTH_REJECTEDVERF
  toEnum 5 = AUTH_TOOWEAK
  toEnum 6 = AUTH_INVALIDRESP
  toEnum 7 = AUTH_FAILED
  toEnum 8 = AUTH_KERB_GENERIC
  toEnum 9 = AUTH_TIMEEXPIRE
  toEnum 10 = AUTH_TKT_FILE
  toEnum 11 = AUTH_DECODE
  toEnum 12 = AUTH_NET_ADDR
  toEnum 13 = RPCSEC_GSS_CREDPROBLEM
  toEnum 14 = RPCSEC_GSS_CTXPROBLEM

data Rpc_msg = Rpc_msg
  { xid :: Int32
  , body :: Rpc_msg_body
  } deriving (Show)

data Rpc_msg_body = Rpc_msg_body
  { mtype :: Msg_type
  , cbody :: Maybe Call_body
  , rbody :: Maybe Reply_body
  } deriving (Show)

data Call_body = Call_body
  { rpcvers :: Int32
  , prog :: Int32
  , vers :: Int32
  , proc :: Int32
  , cred :: Opaque_auth
  , verf :: Opaque_auth
  } deriving (Show)

data Reply_body = Reply_body
  { stat :: Reply_stat
  , areply :: Maybe Accepted_reply
  , rreply :: Maybe Rejected_reply
  } deriving (Show)

data Rpc_mismatch_info = Rpc_mismatch_info
  { low :: Int32
  , high :: Int32
  } deriving (Show)

data Rpc_reply_data = Rpc_reply_data
  { stat :: Accept_stat
  , results :: Maybe String
  , mismatch_info :: Maybe Rpc_mismatch_info
  } deriving (Show)

data Accepted_reply = Accepted_reply
  { verf :: Opaque_auth
  , reply_data :: Rpc_reply_data
  } deriving (Show)

data Rejected_reply = Rejected_reply
  { stat :: Reject_stat
  , mismatch_info :: Maybe Rpc_mismatch_info
  , astat :: Maybe Auth_stat
  } deriving (Show)

data Authsys_parms = Authsys_parms
  { stamp :: Int32
  , machinename :: String
  , uid :: Int32
  , gid :: Int32
  , gids :: [Int32]
  } deriving (Show)

---------------------------

packMsgType :: Msg_type -> Put
packMsgType t = do
  putInt32le $ (fromIntegral (fromEnum (t))::Int32)


packMsgBody :: Rpc_msg_body -> Put
packMsgBody (Rpc_msg_body t _ _) = do
  packMsgType t
  case t of
    CALL -> putInt32le 0xaa
    REPLY -> putInt32le 0xbb
