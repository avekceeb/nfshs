{-
https://wiki.haskell.org/Dealing_with_binary_data
https://hackage.haskell.org/package/binary-0.8.6.0/docs/Data-Binary-Get.html
-}

import           System.IO
import           GHC.Base hiding (Opaque)
import           System.Environment
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.RpcHandmade.Rpc
import           Data.RpcHandmade.Types

-- from GHC.Base
-- ???
-- data Opaque = forall a. O a

-- rpcmsg :: BL.ByteString
-- rpcmsg = BLC.pack "\xff\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x41\x42\x43\x09\x00\x00\x00\x01\x00\x00\x00"

sample :: BL.ByteString
sample = BLC.pack "Hello World!"
sample' :: B.ByteString
sample' = BC.pack "Bye World!"
main :: IO ()
main = do
    --print $ runGet unpackRpcMsg rpcmsg
    -- stack exec rpc-exe | hexdump -C
    --BL.putStr $ runPut (packRpcMsg (Dummy 5 (Smth 9 15) REPLY))
    -- BL.putStr $ runPut (packRpcMsg (RpcMsg 0x11223344 (MsgBody CALL (Just (CallBody 5)) Nothing)))
    BL.putStr $ runPut (packRpcMsg (RpcMsg 0x11223344
      (MsgBody REPLY Nothing
        (Just (ReplyBody "Hey There...")) )))
    --BL.putStr $ runPut (packOpaque sample)
