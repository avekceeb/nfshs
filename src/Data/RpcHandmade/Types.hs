
module Data.RpcHandmade.Types where

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


data Opaque = Opaque
    { len :: Int32
    , val :: B.ByteString
    } deriving (Show)


-- Example of enum
data MsgType = CALL | REPLY deriving (Show, Eq)

instance Enum MsgType where
  fromEnum CALL = 0
  fromEnum REPLY = 1
  toEnum 0 = CALL
  toEnum 1 = REPLY

-- .. OR : trick from stackoverflow
{-
instance Enum MsgType where
    fromEnum = fromJust . flip lookup table
    toEnum   = fromJust . flip lookup (map swap table)
table = [(CALL, 0), (REPLY, 1)]
-}

-- Example of switched union
data MsgBody = MsgBody
    { mtype :: MsgType
    , cbody :: Maybe CallBody
    , rbody :: Maybe ReplyBody
    } deriving (Show)


data CallBody = CallBody
    { foo :: Int32
    } deriving (Show)


data ReplyBody = ReplyBody
    { bar :: String
    } deriving (Show)


data RpcMsg = RpcMsg
    { xid :: Int32
    , msg :: MsgBody
    } deriving (Show)
