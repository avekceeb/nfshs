{-# LANGUAGE DuplicateRecordFields #-}

module Data.Rpc.Nfs4 where

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

FALSE = 0
TRUE = 1
AUTH_NONE = 0
AUTH_SYS = 1
RPCSEC_GSS = 6
data Authsys_parms = Authsys_parms
  { stamp :: Int32
  , machinename :: String
  , uid :: Int32
  , gid :: Int32
  , gids :: [Int32]
  } deriving (Show)

type Int32_t = Int32

type Uint32_t = Int32

type Int64_t = Int64

type Uint64_t = Int64

NFS4_FHSIZE = 128
NFS4_VERIFIER_SIZE = 8
NFS4_OTHER_SIZE = 12
NFS4_OPAQUE_LIMIT = 1024
NFS4_SESSIONID_SIZE = 16
NFS4_INT64_MAX = 9223372036854775807
NFS4_UINT64_MAX = 18446744073709551615
NFS4_INT32_MAX = 2147483647
NFS4_UINT32_MAX = 4294967295
NFS4_MAXFILELEN = 18446744073709551615
NFS4_MAXFILEOFF = 18446744073709551614
data Nfs_ftype4 = NF4REG | NF4DIR | NF4BLK | NF4CHR | NF4LNK | NF4SOCK | NF4FIFO | NF4ATTRDIR | NF4NAMEDATTR deriving (Eq, Show)
instance Enum Nfs_ftype4 where
  fromEnum NF4REG = 1
  fromEnum NF4DIR = 2
  fromEnum NF4BLK = 3
  fromEnum NF4CHR = 4
  fromEnum NF4LNK = 5
  fromEnum NF4SOCK = 6
  fromEnum NF4FIFO = 7
  fromEnum NF4ATTRDIR = 8
  fromEnum NF4NAMEDATTR = 9
  toEnum 1 = NF4REG
  toEnum 2 = NF4DIR
  toEnum 3 = NF4BLK
  toEnum 4 = NF4CHR
  toEnum 5 = NF4LNK
  toEnum 6 = NF4SOCK
  toEnum 7 = NF4FIFO
  toEnum 8 = NF4ATTRDIR
  toEnum 9 = NF4NAMEDATTR

data Nfsstat4 = NFS4_OK | NFS4ERR_PERM | NFS4ERR_NOENT | NFS4ERR_IO | NFS4ERR_NXIO | NFS4ERR_ACCESS | NFS4ERR_EXIST | NFS4ERR_XDEV | NFS4ERR_NOTDIR | NFS4ERR_ISDIR | NFS4ERR_INVAL | NFS4ERR_FBIG | NFS4ERR_NOSPC | NFS4ERR_ROFS | NFS4ERR_MLINK | NFS4ERR_NAMETOOLONG | NFS4ERR_NOTEMPTY | NFS4ERR_DQUOT | NFS4ERR_STALE | NFS4ERR_BADHANDLE | NFS4ERR_BAD_COOKIE | NFS4ERR_NOTSUPP | NFS4ERR_TOOSMALL | NFS4ERR_SERVERFAULT | NFS4ERR_BADTYPE | NFS4ERR_DELAY | NFS4ERR_SAME | NFS4ERR_DENIED | NFS4ERR_EXPIRED | NFS4ERR_LOCKED | NFS4ERR_GRACE | NFS4ERR_FHEXPIRED | NFS4ERR_SHARE_DENIED | NFS4ERR_WRONGSEC | NFS4ERR_CLID_INUSE | NFS4ERR_RESOURCE | NFS4ERR_MOVED | NFS4ERR_NOFILEHANDLE | NFS4ERR_MINOR_VERS_MISMATCH | NFS4ERR_STALE_CLIENTID | NFS4ERR_STALE_STATEID | NFS4ERR_OLD_STATEID | NFS4ERR_BAD_STATEID | NFS4ERR_BAD_SEQID | NFS4ERR_NOT_SAME | NFS4ERR_LOCK_RANGE | NFS4ERR_SYMLINK | NFS4ERR_RESTOREFH | NFS4ERR_LEASE_MOVED | NFS4ERR_ATTRNOTSUPP | NFS4ERR_NO_GRACE | NFS4ERR_RECLAIM_BAD | NFS4ERR_RECLAIM_CONFLICT | NFS4ERR_BADXDR | NFS4ERR_LOCKS_HELD | NFS4ERR_OPENMODE | NFS4ERR_BADOWNER | NFS4ERR_BADCHAR | NFS4ERR_BADNAME | NFS4ERR_BAD_RANGE | NFS4ERR_LOCK_NOTSUPP | NFS4ERR_OP_ILLEGAL | NFS4ERR_DEADLOCK | NFS4ERR_FILE_OPEN | NFS4ERR_ADMIN_REVOKED | NFS4ERR_CB_PATH_DOWN | NFS4ERR_BADIOMODE | NFS4ERR_BADLAYOUT | NFS4ERR_BAD_SESSION_DIGEST | NFS4ERR_BADSESSION | NFS4ERR_BADSLOT | NFS4ERR_COMPLETE_ALREADY | NFS4ERR_CONN_NOT_BOUND_TO_SESSION | NFS4ERR_DELEG_ALREADY_WANTED | NFS4ERR_BACK_CHAN_BUSY | NFS4ERR_LAYOUTTRYLATER | NFS4ERR_LAYOUTUNAVAILABLE | NFS4ERR_NOMATCHING_LAYOUT | NFS4ERR_RECALLCONFLICT | NFS4ERR_UNKNOWN_LAYOUTTYPE | NFS4ERR_SEQ_MISORDERED | NFS4ERR_SEQUENCE_POS | NFS4ERR_REQ_TOO_BIG | NFS4ERR_REP_TOO_BIG | NFS4ERR_REP_TOO_BIG_TO_CACHE | NFS4ERR_RETRY_UNCACHED_REP | NFS4ERR_UNSAFE_COMPOUND | NFS4ERR_TOO_MANY_OPS | NFS4ERR_OP_NOT_IN_SESSION | NFS4ERR_HASH_ALG_UNSUPP | NFS4ERR_CLIENTID_BUSY | NFS4ERR_PNFS_IO_HOLE | NFS4ERR_SEQ_FALSE_RETRY | NFS4ERR_BAD_HIGH_SLOT | NFS4ERR_DEADSESSION | NFS4ERR_ENCR_ALG_UNSUPP | NFS4ERR_PNFS_NO_LAYOUT | NFS4ERR_NOT_ONLY_OP | NFS4ERR_WRONG_CRED | NFS4ERR_WRONG_TYPE | NFS4ERR_DIRDELEG_UNAVAIL | NFS4ERR_REJECT_DELEG | NFS4ERR_RETURNCONFLICT | NFS4ERR_DELEG_REVOKED | NFS4ERR_PARTNER_NOTSUPP | NFS4ERR_PARTNER_NO_AUTH | NFS4ERR_UNION_NOTSUPP | NFS4ERR_OFFLOAD_DENIED | NFS4ERR_WRONG_LFS | NFS4ERR_BADLABEL | NFS4ERR_OFFLOAD_NO_REQS deriving (Eq, Show)
instance Enum Nfsstat4 where
  fromEnum NFS4_OK = 0
  fromEnum NFS4ERR_PERM = 1
  fromEnum NFS4ERR_NOENT = 2
  fromEnum NFS4ERR_IO = 5
  fromEnum NFS4ERR_NXIO = 6
  fromEnum NFS4ERR_ACCESS = 13
  fromEnum NFS4ERR_EXIST = 17
  fromEnum NFS4ERR_XDEV = 18
  fromEnum NFS4ERR_NOTDIR = 20
  fromEnum NFS4ERR_ISDIR = 21
  fromEnum NFS4ERR_INVAL = 22
  fromEnum NFS4ERR_FBIG = 27
  fromEnum NFS4ERR_NOSPC = 28
  fromEnum NFS4ERR_ROFS = 30
  fromEnum NFS4ERR_MLINK = 31
  fromEnum NFS4ERR_NAMETOOLONG = 63
  fromEnum NFS4ERR_NOTEMPTY = 66
  fromEnum NFS4ERR_DQUOT = 69
  fromEnum NFS4ERR_STALE = 70
  fromEnum NFS4ERR_BADHANDLE = 10001
  fromEnum NFS4ERR_BAD_COOKIE = 10003
  fromEnum NFS4ERR_NOTSUPP = 10004
  fromEnum NFS4ERR_TOOSMALL = 10005
  fromEnum NFS4ERR_SERVERFAULT = 10006
  fromEnum NFS4ERR_BADTYPE = 10007
  fromEnum NFS4ERR_DELAY = 10008
  fromEnum NFS4ERR_SAME = 10009
  fromEnum NFS4ERR_DENIED = 10010
  fromEnum NFS4ERR_EXPIRED = 10011
  fromEnum NFS4ERR_LOCKED = 10012
  fromEnum NFS4ERR_GRACE = 10013
  fromEnum NFS4ERR_FHEXPIRED = 10014
  fromEnum NFS4ERR_SHARE_DENIED = 10015
  fromEnum NFS4ERR_WRONGSEC = 10016
  fromEnum NFS4ERR_CLID_INUSE = 10017
  fromEnum NFS4ERR_RESOURCE = 10018
  fromEnum NFS4ERR_MOVED = 10019
  fromEnum NFS4ERR_NOFILEHANDLE = 10020
  fromEnum NFS4ERR_MINOR_VERS_MISMATCH = 10021
  fromEnum NFS4ERR_STALE_CLIENTID = 10022
  fromEnum NFS4ERR_STALE_STATEID = 10023
  fromEnum NFS4ERR_OLD_STATEID = 10024
  fromEnum NFS4ERR_BAD_STATEID = 10025
  fromEnum NFS4ERR_BAD_SEQID = 10026
  fromEnum NFS4ERR_NOT_SAME = 10027
  fromEnum NFS4ERR_LOCK_RANGE = 10028
  fromEnum NFS4ERR_SYMLINK = 10029
  fromEnum NFS4ERR_RESTOREFH = 10030
  fromEnum NFS4ERR_LEASE_MOVED = 10031
  fromEnum NFS4ERR_ATTRNOTSUPP = 10032
  fromEnum NFS4ERR_NO_GRACE = 10033
  fromEnum NFS4ERR_RECLAIM_BAD = 10034
  fromEnum NFS4ERR_RECLAIM_CONFLICT = 10035
  fromEnum NFS4ERR_BADXDR = 10036
  fromEnum NFS4ERR_LOCKS_HELD = 10037
  fromEnum NFS4ERR_OPENMODE = 10038
  fromEnum NFS4ERR_BADOWNER = 10039
  fromEnum NFS4ERR_BADCHAR = 10040
  fromEnum NFS4ERR_BADNAME = 10041
  fromEnum NFS4ERR_BAD_RANGE = 10042
  fromEnum NFS4ERR_LOCK_NOTSUPP = 10043
  fromEnum NFS4ERR_OP_ILLEGAL = 10044
  fromEnum NFS4ERR_DEADLOCK = 10045
  fromEnum NFS4ERR_FILE_OPEN = 10046
  fromEnum NFS4ERR_ADMIN_REVOKED = 10047
  fromEnum NFS4ERR_CB_PATH_DOWN = 10048
  fromEnum NFS4ERR_BADIOMODE = 10049
  fromEnum NFS4ERR_BADLAYOUT = 10050
  fromEnum NFS4ERR_BAD_SESSION_DIGEST = 10051
  fromEnum NFS4ERR_BADSESSION = 10052
  fromEnum NFS4ERR_BADSLOT = 10053
  fromEnum NFS4ERR_COMPLETE_ALREADY = 10054
  fromEnum NFS4ERR_CONN_NOT_BOUND_TO_SESSION = 10055
  fromEnum NFS4ERR_DELEG_ALREADY_WANTED = 10056
  fromEnum NFS4ERR_BACK_CHAN_BUSY = 10057
  fromEnum NFS4ERR_LAYOUTTRYLATER = 10058
  fromEnum NFS4ERR_LAYOUTUNAVAILABLE = 10059
  fromEnum NFS4ERR_NOMATCHING_LAYOUT = 10060
  fromEnum NFS4ERR_RECALLCONFLICT = 10061
  fromEnum NFS4ERR_UNKNOWN_LAYOUTTYPE = 10062
  fromEnum NFS4ERR_SEQ_MISORDERED = 10063
  fromEnum NFS4ERR_SEQUENCE_POS = 10064
  fromEnum NFS4ERR_REQ_TOO_BIG = 10065
  fromEnum NFS4ERR_REP_TOO_BIG = 10066
  fromEnum NFS4ERR_REP_TOO_BIG_TO_CACHE = 10067
  fromEnum NFS4ERR_RETRY_UNCACHED_REP = 10068
  fromEnum NFS4ERR_UNSAFE_COMPOUND = 10069
  fromEnum NFS4ERR_TOO_MANY_OPS = 10070
  fromEnum NFS4ERR_OP_NOT_IN_SESSION = 10071
  fromEnum NFS4ERR_HASH_ALG_UNSUPP = 10072
  fromEnum NFS4ERR_CLIENTID_BUSY = 10074
  fromEnum NFS4ERR_PNFS_IO_HOLE = 10075
  fromEnum NFS4ERR_SEQ_FALSE_RETRY = 10076
  fromEnum NFS4ERR_BAD_HIGH_SLOT = 10077
  fromEnum NFS4ERR_DEADSESSION = 10078
  fromEnum NFS4ERR_ENCR_ALG_UNSUPP = 10079
  fromEnum NFS4ERR_PNFS_NO_LAYOUT = 10080
  fromEnum NFS4ERR_NOT_ONLY_OP = 10081
  fromEnum NFS4ERR_WRONG_CRED = 10082
  fromEnum NFS4ERR_WRONG_TYPE = 10083
  fromEnum NFS4ERR_DIRDELEG_UNAVAIL = 10084
  fromEnum NFS4ERR_REJECT_DELEG = 10085
  fromEnum NFS4ERR_RETURNCONFLICT = 10086
  fromEnum NFS4ERR_DELEG_REVOKED = 10087
  fromEnum NFS4ERR_PARTNER_NOTSUPP = 10088
  fromEnum NFS4ERR_PARTNER_NO_AUTH = 10089
  fromEnum NFS4ERR_UNION_NOTSUPP = 10090
  fromEnum NFS4ERR_OFFLOAD_DENIED = 10091
  fromEnum NFS4ERR_WRONG_LFS = 10092
  fromEnum NFS4ERR_BADLABEL = 10093
  fromEnum NFS4ERR_OFFLOAD_NO_REQS = 10094
  toEnum 0 = NFS4_OK
  toEnum 1 = NFS4ERR_PERM
  toEnum 2 = NFS4ERR_NOENT
  toEnum 5 = NFS4ERR_IO
  toEnum 6 = NFS4ERR_NXIO
  toEnum 13 = NFS4ERR_ACCESS
  toEnum 17 = NFS4ERR_EXIST
  toEnum 18 = NFS4ERR_XDEV
  toEnum 20 = NFS4ERR_NOTDIR
  toEnum 21 = NFS4ERR_ISDIR
  toEnum 22 = NFS4ERR_INVAL
  toEnum 27 = NFS4ERR_FBIG
  toEnum 28 = NFS4ERR_NOSPC
  toEnum 30 = NFS4ERR_ROFS
  toEnum 31 = NFS4ERR_MLINK
  toEnum 63 = NFS4ERR_NAMETOOLONG
  toEnum 66 = NFS4ERR_NOTEMPTY
  toEnum 69 = NFS4ERR_DQUOT
  toEnum 70 = NFS4ERR_STALE
  toEnum 10001 = NFS4ERR_BADHANDLE
  toEnum 10003 = NFS4ERR_BAD_COOKIE
  toEnum 10004 = NFS4ERR_NOTSUPP
  toEnum 10005 = NFS4ERR_TOOSMALL
  toEnum 10006 = NFS4ERR_SERVERFAULT
  toEnum 10007 = NFS4ERR_BADTYPE
  toEnum 10008 = NFS4ERR_DELAY
  toEnum 10009 = NFS4ERR_SAME
  toEnum 10010 = NFS4ERR_DENIED
  toEnum 10011 = NFS4ERR_EXPIRED
  toEnum 10012 = NFS4ERR_LOCKED
  toEnum 10013 = NFS4ERR_GRACE
  toEnum 10014 = NFS4ERR_FHEXPIRED
  toEnum 10015 = NFS4ERR_SHARE_DENIED
  toEnum 10016 = NFS4ERR_WRONGSEC
  toEnum 10017 = NFS4ERR_CLID_INUSE
  toEnum 10018 = NFS4ERR_RESOURCE
  toEnum 10019 = NFS4ERR_MOVED
  toEnum 10020 = NFS4ERR_NOFILEHANDLE
  toEnum 10021 = NFS4ERR_MINOR_VERS_MISMATCH
  toEnum 10022 = NFS4ERR_STALE_CLIENTID
  toEnum 10023 = NFS4ERR_STALE_STATEID
  toEnum 10024 = NFS4ERR_OLD_STATEID
  toEnum 10025 = NFS4ERR_BAD_STATEID
  toEnum 10026 = NFS4ERR_BAD_SEQID
  toEnum 10027 = NFS4ERR_NOT_SAME
  toEnum 10028 = NFS4ERR_LOCK_RANGE
  toEnum 10029 = NFS4ERR_SYMLINK
  toEnum 10030 = NFS4ERR_RESTOREFH
  toEnum 10031 = NFS4ERR_LEASE_MOVED
  toEnum 10032 = NFS4ERR_ATTRNOTSUPP
  toEnum 10033 = NFS4ERR_NO_GRACE
  toEnum 10034 = NFS4ERR_RECLAIM_BAD
  toEnum 10035 = NFS4ERR_RECLAIM_CONFLICT
  toEnum 10036 = NFS4ERR_BADXDR
  toEnum 10037 = NFS4ERR_LOCKS_HELD
  toEnum 10038 = NFS4ERR_OPENMODE
  toEnum 10039 = NFS4ERR_BADOWNER
  toEnum 10040 = NFS4ERR_BADCHAR
  toEnum 10041 = NFS4ERR_BADNAME
  toEnum 10042 = NFS4ERR_BAD_RANGE
  toEnum 10043 = NFS4ERR_LOCK_NOTSUPP
  toEnum 10044 = NFS4ERR_OP_ILLEGAL
  toEnum 10045 = NFS4ERR_DEADLOCK
  toEnum 10046 = NFS4ERR_FILE_OPEN
  toEnum 10047 = NFS4ERR_ADMIN_REVOKED
  toEnum 10048 = NFS4ERR_CB_PATH_DOWN
  toEnum 10049 = NFS4ERR_BADIOMODE
  toEnum 10050 = NFS4ERR_BADLAYOUT
  toEnum 10051 = NFS4ERR_BAD_SESSION_DIGEST
  toEnum 10052 = NFS4ERR_BADSESSION
  toEnum 10053 = NFS4ERR_BADSLOT
  toEnum 10054 = NFS4ERR_COMPLETE_ALREADY
  toEnum 10055 = NFS4ERR_CONN_NOT_BOUND_TO_SESSION
  toEnum 10056 = NFS4ERR_DELEG_ALREADY_WANTED
  toEnum 10057 = NFS4ERR_BACK_CHAN_BUSY
  toEnum 10058 = NFS4ERR_LAYOUTTRYLATER
  toEnum 10059 = NFS4ERR_LAYOUTUNAVAILABLE
  toEnum 10060 = NFS4ERR_NOMATCHING_LAYOUT
  toEnum 10061 = NFS4ERR_RECALLCONFLICT
  toEnum 10062 = NFS4ERR_UNKNOWN_LAYOUTTYPE
  toEnum 10063 = NFS4ERR_SEQ_MISORDERED
  toEnum 10064 = NFS4ERR_SEQUENCE_POS
  toEnum 10065 = NFS4ERR_REQ_TOO_BIG
  toEnum 10066 = NFS4ERR_REP_TOO_BIG
  toEnum 10067 = NFS4ERR_REP_TOO_BIG_TO_CACHE
  toEnum 10068 = NFS4ERR_RETRY_UNCACHED_REP
  toEnum 10069 = NFS4ERR_UNSAFE_COMPOUND
  toEnum 10070 = NFS4ERR_TOO_MANY_OPS
  toEnum 10071 = NFS4ERR_OP_NOT_IN_SESSION
  toEnum 10072 = NFS4ERR_HASH_ALG_UNSUPP
  toEnum 10074 = NFS4ERR_CLIENTID_BUSY
  toEnum 10075 = NFS4ERR_PNFS_IO_HOLE
  toEnum 10076 = NFS4ERR_SEQ_FALSE_RETRY
  toEnum 10077 = NFS4ERR_BAD_HIGH_SLOT
  toEnum 10078 = NFS4ERR_DEADSESSION
  toEnum 10079 = NFS4ERR_ENCR_ALG_UNSUPP
  toEnum 10080 = NFS4ERR_PNFS_NO_LAYOUT
  toEnum 10081 = NFS4ERR_NOT_ONLY_OP
  toEnum 10082 = NFS4ERR_WRONG_CRED
  toEnum 10083 = NFS4ERR_WRONG_TYPE
  toEnum 10084 = NFS4ERR_DIRDELEG_UNAVAIL
  toEnum 10085 = NFS4ERR_REJECT_DELEG
  toEnum 10086 = NFS4ERR_RETURNCONFLICT
  toEnum 10087 = NFS4ERR_DELEG_REVOKED
  toEnum 10088 = NFS4ERR_PARTNER_NOTSUPP
  toEnum 10089 = NFS4ERR_PARTNER_NO_AUTH
  toEnum 10090 = NFS4ERR_UNION_NOTSUPP
  toEnum 10091 = NFS4ERR_OFFLOAD_DENIED
  toEnum 10092 = NFS4ERR_WRONG_LFS
  toEnum 10093 = NFS4ERR_BADLABEL
  toEnum 10094 = NFS4ERR_OFFLOAD_NO_REQS

type Attrlist4 = String

type Bitmap4 = [Uint32_t]

type Changeid4 = Uint64_t

type Clientid4 = Uint64_t

type Count4 = Uint32_t

type Length4 = Uint64_t

type Mode4 = Uint32_t

type Nfs_cookie4 = Uint64_t

type Nfs_fh4 = String

type Offset4 = Uint64_t

type Qop4 = Uint32_t

type Sec_oid4 = String

type Sequenceid4 = Uint32_t

type Seqid4 = Uint32_t

type Sessionid4 = String

type Slotid4 = Uint32_t

type Utf8string = String

type Utf8str_cis = Utf8string

type Utf8str_cs = Utf8string

type Utf8str_mixed = Utf8string

type Component4 = Utf8str_cs

type Linktext4 = String

type Ascii_REQUIRED4 = Utf8string

type Pathname4 = [Component4]

type Verifier4 = String

type Secret4 = String

type Policy4 = Uint32_t

data Nfstime4 = Nfstime4
  { seconds :: Int64_t
  , nseconds :: Uint32_t
  } deriving (Show)

data Time_how4 = SET_TO_SERVER_TIME4 | SET_TO_CLIENT_TIME4 deriving (Eq, Show)
instance Enum Time_how4 where
  fromEnum SET_TO_SERVER_TIME4 = 0
  fromEnum SET_TO_CLIENT_TIME4 = 1
  toEnum 0 = SET_TO_SERVER_TIME4
  toEnum 1 = SET_TO_CLIENT_TIME4

data Settime4 = Settime4
  { set_it :: Time_how4
  , time :: Maybe Nfstime4
  } deriving (Show)

type Nfs_lease4 = Uint32_t

data Fsid4 = Fsid4
  { major :: Uint64_t
  , minor :: Uint64_t
  } deriving (Show)

data Change_policy4 = Change_policy4
  { cp_major :: Uint64_t
  , cp_minor :: Uint64_t
  } deriving (Show)

data Fs_location4 = Fs_location4
  { server :: [Utf8str_cis]
  , rootpath :: Pathname4
  } deriving (Show)

data Fs_locations4 = Fs_locations4
  { fs_root :: Pathname4
  , locations :: [Fs_location4]
  } deriving (Show)

ACL4_SUPPORT_ALLOW_ACL = 1
ACL4_SUPPORT_DENY_ACL = 2
ACL4_SUPPORT_AUDIT_ACL = 4
ACL4_SUPPORT_ALARM_ACL = 8
type Acetype4 = Uint32_t

ACE4_ACCESS_ALLOWED_ACE_TYPE = 0
ACE4_ACCESS_DENIED_ACE_TYPE = 1
ACE4_SYSTEM_AUDIT_ACE_TYPE = 2
ACE4_SYSTEM_ALARM_ACE_TYPE = 3
type Aceflag4 = Uint32_t

ACE4_FILE_INHERIT_ACE = 1
ACE4_DIRECTORY_INHERIT_ACE = 2
ACE4_NO_PROPAGATE_INHERIT_ACE = 4
ACE4_INHERIT_ONLY_ACE = 8
ACE4_SUCCESSFUL_ACCESS_ACE_FLAG = 16
ACE4_FAILED_ACCESS_ACE_FLAG = 32
ACE4_IDENTIFIER_GROUP = 64
ACE4_INHERITED_ACE = 128
type Acemask4 = Uint32_t

ACE4_READ_DATA = 1
ACE4_LIST_DIRECTORY = 1
ACE4_WRITE_DATA = 2
ACE4_ADD_FILE = 2
ACE4_APPEND_DATA = 4
ACE4_ADD_SUBDIRECTORY = 4
ACE4_READ_NAMED_ATTRS = 8
ACE4_WRITE_NAMED_ATTRS = 16
ACE4_EXECUTE = 32
ACE4_DELETE_CHILD = 64
ACE4_READ_ATTRIBUTES = 128
ACE4_WRITE_ATTRIBUTES = 256
ACE4_WRITE_RETENTION = 512
ACE4_WRITE_RETENTION_HOLD = 1024
ACE4_DELETE = 65536
ACE4_READ_ACL = 131072
ACE4_WRITE_ACL = 262144
ACE4_WRITE_OWNER = 524288
ACE4_SYNCHRONIZE = 1048576
ACE4_GENERIC_READ = 1179777
ACE4_GENERIC_WRITE = 1442054
ACE4_GENERIC_EXECUTE = 1179808
data Nfsace4 = Nfsace4
  { typex :: Acetype4
  , flag :: Aceflag4
  , access_mask :: Acemask4
  , who :: Utf8str_mixed
  } deriving (Show)

type Aclflag4 = Uint32_t

ACL4_AUTO_INHERIT = 1
ACL4_PROTECTED = 2
ACL4_DEFAULTED = 4
data Nfsacl41 = Nfsacl41
  { na41_flag :: Aclflag4
  , na41_aces :: [Nfsace4]
  } deriving (Show)

MODE4_SUID = 2048
MODE4_SGID = 1024
MODE4_SVTX = 512
MODE4_RUSR = 256
MODE4_WUSR = 128
MODE4_XUSR = 64
MODE4_RGRP = 32
MODE4_WGRP = 16
MODE4_XGRP = 8
MODE4_ROTH = 4
MODE4_WOTH = 2
MODE4_XOTH = 1
data Mode_masked4 = Mode_masked4
  { mm_value_to_set :: Mode4
  , mm_mask_bits :: Mode4
  } deriving (Show)

data Specdata4 = Specdata4
  { specdata1 :: Uint32_t
  , specdata2 :: Uint32_t
  } deriving (Show)

FH4_PERSISTENT = 0
FH4_NOEXPIRE_WITH_OPEN = 1
FH4_VOLATILE_ANY = 2
FH4_VOL_MIGRATION = 4
FH4_VOL_RENAME = 8
data Netaddr4 = Netaddr4
  { na_r_netid :: String
  , na_r_addr :: String
  } deriving (Show)

data Nfs_impl_id4 = Nfs_impl_id4
  { nii_domain :: Utf8str_cis
  , nii_name :: Utf8str_cs
  , nii_date :: Nfstime4
  } deriving (Show)

data Stateid4 = Stateid4
  { seqid :: Uint32_t
  , other :: String
  } deriving (Show)

data Layouttype4 = LAYOUT4_NFSV4_1_FILES | LAYOUT4_OSD2_OBJECTS | LAYOUT4_BLOCK_VOLUME | LAYOUT4_FLEX_FILES | LAYOUT4_SCSI deriving (Eq, Show)
instance Enum Layouttype4 where
  fromEnum LAYOUT4_NFSV4_1_FILES = 1
  fromEnum LAYOUT4_OSD2_OBJECTS = 2
  fromEnum LAYOUT4_BLOCK_VOLUME = 3
  fromEnum LAYOUT4_FLEX_FILES = 4
  fromEnum LAYOUT4_SCSI = 5
  toEnum 1 = LAYOUT4_NFSV4_1_FILES
  toEnum 2 = LAYOUT4_OSD2_OBJECTS
  toEnum 3 = LAYOUT4_BLOCK_VOLUME
  toEnum 4 = LAYOUT4_FLEX_FILES
  toEnum 5 = LAYOUT4_SCSI

data Layout_content4 = Layout_content4
  { loc_type :: Layouttype4
  , loc_body :: String
  } deriving (Show)

data Layouthint4 = Layouthint4
  { loh_type :: Layouttype4
  , loh_body :: String
  } deriving (Show)

data Layoutiomode4 = LAYOUTIOMODE4_READ | LAYOUTIOMODE4_RW | LAYOUTIOMODE4_ANY deriving (Eq, Show)
instance Enum Layoutiomode4 where
  fromEnum LAYOUTIOMODE4_READ = 1
  fromEnum LAYOUTIOMODE4_RW = 2
  fromEnum LAYOUTIOMODE4_ANY = 3
  toEnum 1 = LAYOUTIOMODE4_READ
  toEnum 2 = LAYOUTIOMODE4_RW
  toEnum 3 = LAYOUTIOMODE4_ANY

data Layout4 = Layout4
  { lo_offset :: Offset4
  , lo_length :: Length4
  , lo_iomode :: Layoutiomode4
  , lo_content :: Layout_content4
  } deriving (Show)

NFS4_DEVICEID4_SIZE = 16
type Deviceid4 = String

data Device_addr4 = Device_addr4
  { da_layout_type :: Layouttype4
  , da_addr_body :: String
  } deriving (Show)

data Layoutupdate4 = Layoutupdate4
  { lou_type :: Layouttype4
  , lou_body :: String
  } deriving (Show)

LAYOUT4_RET_REC_FILE = 1
LAYOUT4_RET_REC_FSID = 2
LAYOUT4_RET_REC_ALL = 3
data Layoutreturn_type4 = LAYOUTRETURN4_FILE | LAYOUTRETURN4_FSID | LAYOUTRETURN4_ALL deriving (Eq, Show)
instance Enum Layoutreturn_type4 where
  fromEnum LAYOUTRETURN4_FILE = LAYOUT4_RET_REC_FILE
  fromEnum LAYOUTRETURN4_FSID = LAYOUT4_RET_REC_FSID
  fromEnum LAYOUTRETURN4_ALL = LAYOUT4_RET_REC_ALL
  toEnum LAYOUT4_RET_REC_FILE = LAYOUTRETURN4_FILE
  toEnum LAYOUT4_RET_REC_FSID = LAYOUTRETURN4_FSID
  toEnum LAYOUT4_RET_REC_ALL = LAYOUTRETURN4_ALL

data Layoutreturn_file4 = Layoutreturn_file4
  { lrf_offset :: Offset4
  , lrf_length :: Length4
  , lrf_stateid :: Stateid4
  , lrf_body :: String
  } deriving (Show)

data Layoutreturn4 = Layoutreturn4
  { lr_returntype :: Layoutreturn_type4
  , lr_layout :: Maybe Layoutreturn_file4
  } deriving (Show)

data Fs4_status_type = STATUS4_FIXED | STATUS4_UPDATED | STATUS4_VERSIONED | STATUS4_WRITABLE | STATUS4_REFERRAL deriving (Eq, Show)
instance Enum Fs4_status_type where
  fromEnum STATUS4_FIXED = 1
  fromEnum STATUS4_UPDATED = 2
  fromEnum STATUS4_VERSIONED = 3
  fromEnum STATUS4_WRITABLE = 4
  fromEnum STATUS4_REFERRAL = 5
  toEnum 1 = STATUS4_FIXED
  toEnum 2 = STATUS4_UPDATED
  toEnum 3 = STATUS4_VERSIONED
  toEnum 4 = STATUS4_WRITABLE
  toEnum 5 = STATUS4_REFERRAL

data Fs4_status = Fs4_status
  { fss_absent :: Bool
  , fss_type :: Fs4_status_type
  , fss_source :: Utf8str_cs
  , fss_current :: Utf8str_cs
  , fss_age :: Int32_t
  , fss_version :: Nfstime4
  } deriving (Show)

TH4_READ_SIZE = 0
TH4_WRITE_SIZE = 1
TH4_READ_IOSIZE = 2
TH4_WRITE_IOSIZE = 3
type Threshold4_read_size = Length4

type Threshold4_write_size = Length4

type Threshold4_read_iosize = Length4

type Threshold4_write_iosize = Length4

data Threshold_item4 = Threshold_item4
  { thi_layout_type :: Layouttype4
  , thi_hintset :: Bitmap4
  , thi_hintlist :: String
  } deriving (Show)

data Mdsthreshold4 = Mdsthreshold4
  { mth_hints :: [Threshold_item4]
  } deriving (Show)

RET4_DURATION_INFINITE = 18446744073709551615
data Retention_get4 = Retention_get4
  { rg_duration :: Uint64_t
  , rg_begin_time :: [Nfstime4]
  } deriving (Show)

data Retention_set4 = Retention_set4
  { rs_enable :: Bool
  , rs_duration :: [Uint64_t]
  } deriving (Show)

FSCHARSET_CAP4_CONTAINS_NON_UTF8 = 1
FSCHARSET_CAP4_ALLOWS_ONLY_UTF8 = 2
type Fs_charset_cap4 = Uint32_t

data Netloc_type4 = NL4_NAME | NL4_URL | NL4_NETADDR deriving (Eq, Show)
instance Enum Netloc_type4 where
  fromEnum NL4_NAME = 1
  fromEnum NL4_URL = 2
  fromEnum NL4_NETADDR = 3
  toEnum 1 = NL4_NAME
  toEnum 2 = NL4_URL
  toEnum 3 = NL4_NETADDR

data Netloc4 = Netloc4
  { nl_type :: Netloc_type4
  , nl_name :: Maybe Utf8str_cis
  , nl_url :: Maybe Utf8str_cis
  , nl_addr :: Maybe Netaddr4
  } deriving (Show)

data Change_attr_type4 = NFS4_CHANGE_TYPE_IS_MONOTONIC_INCR | NFS4_CHANGE_TYPE_IS_VERSION_COUNTER | NFS4_CHANGE_TYPE_IS_VERSION_COUNTER_NOPNFS | NFS4_CHANGE_TYPE_IS_TIME_METADATA | NFS4_CHANGE_TYPE_IS_UNDEFINED deriving (Eq, Show)
instance Enum Change_attr_type4 where
  fromEnum NFS4_CHANGE_TYPE_IS_MONOTONIC_INCR = 0
  fromEnum NFS4_CHANGE_TYPE_IS_VERSION_COUNTER = 1
  fromEnum NFS4_CHANGE_TYPE_IS_VERSION_COUNTER_NOPNFS = 2
  fromEnum NFS4_CHANGE_TYPE_IS_TIME_METADATA = 3
  fromEnum NFS4_CHANGE_TYPE_IS_UNDEFINED = 4
  toEnum 0 = NFS4_CHANGE_TYPE_IS_MONOTONIC_INCR
  toEnum 1 = NFS4_CHANGE_TYPE_IS_VERSION_COUNTER
  toEnum 2 = NFS4_CHANGE_TYPE_IS_VERSION_COUNTER_NOPNFS
  toEnum 3 = NFS4_CHANGE_TYPE_IS_TIME_METADATA
  toEnum 4 = NFS4_CHANGE_TYPE_IS_UNDEFINED

data Labelformat_spec4 = Labelformat_spec4
  { lfs_lfs :: Policy4
  , lfs_pi :: Policy4
  } deriving (Show)

data Sec_label4 = Sec_label4
  { slai_lfs :: Labelformat_spec4
  , slai_data :: String
  } deriving (Show)

data Copy_from_auth_priv = Copy_from_auth_priv
  { cfap_shared_secret :: Secret4
  , cfap_destination :: Netloc4
  , cfap_username :: Utf8str_mixed
  } deriving (Show)

data Copy_to_auth_priv = Copy_to_auth_priv
  { ctap_shared_secret :: Secret4
  , ctap_source :: [Netloc4]
  , ctap_username :: Utf8str_mixed
  } deriving (Show)

data Copy_confirm_auth_priv = Copy_confirm_auth_priv
  { ccap_shared_secret_mic :: String
  , ccap_username :: Utf8str_mixed
  } deriving (Show)

data App_data_block4 = App_data_block4
  { adb_offset :: Offset4
  , adb_block_size :: Length4
  , adb_block_count :: Length4
  , adb_reloff_blocknum :: Length4
  , adb_block_num :: Count4
  , adb_reloff_pattern :: Length4
  , adb_pattern :: String
  } deriving (Show)

data Data4 = Data4
  { d_offset :: Offset4
  , d_data :: String
  } deriving (Show)

data Data_info4 = Data_info4
  { di_offset :: Offset4
  , di_length :: Length4
  } deriving (Show)

data Data_content4 = NFS4_CONTENT_DATA | NFS4_CONTENT_HOLE deriving (Eq, Show)
instance Enum Data_content4 where
  fromEnum NFS4_CONTENT_DATA = 0
  fromEnum NFS4_CONTENT_HOLE = 1
  toEnum 0 = NFS4_CONTENT_DATA
  toEnum 1 = NFS4_CONTENT_HOLE

data Stable_how4 = UNSTABLE4 | DATA_SYNC4 | FILE_SYNC4 deriving (Eq, Show)
instance Enum Stable_how4 where
  fromEnum UNSTABLE4 = 0
  fromEnum DATA_SYNC4 = 1
  fromEnum FILE_SYNC4 = 2
  toEnum 0 = UNSTABLE4
  toEnum 1 = DATA_SYNC4
  toEnum 2 = FILE_SYNC4

data Write_response4 = Write_response4
  { wr_callback_id :: [Stateid4]
  , wr_count :: Length4
  , wr_committed :: Stable_how4
  , wr_writeverf :: Verifier4
  } deriving (Show)

type Fattr4_supported_attrs = Bitmap4

type Fattr4_type = Nfs_ftype4

type Fattr4_fh_expire_type = Uint32_t

type Fattr4_change = Changeid4

type Fattr4_size = Uint64_t

type Fattr4_link_support = Bool

type Fattr4_symlink_support = Bool

type Fattr4_named_attr = Bool

type Fattr4_fsid = Fsid4

type Fattr4_unique_handles = Bool

type Fattr4_lease_time = Nfs_lease4

type Fattr4_rdattr_error = Nfsstat4

type Fattr4_acl = [Nfsace4]

type Fattr4_aclsupport = Uint32_t

type Fattr4_archive = Bool

type Fattr4_cansettime = Bool

type Fattr4_case_insensitive = Bool

type Fattr4_case_preserving = Bool

type Fattr4_chown_restricted = Bool

type Fattr4_fileid = Uint64_t

type Fattr4_files_avail = Uint64_t

type Fattr4_filehandle = Nfs_fh4

type Fattr4_files_free = Uint64_t

type Fattr4_files_total = Uint64_t

type Fattr4_fs_locations = Fs_locations4

type Fattr4_hidden = Bool

type Fattr4_homogeneous = Bool

type Fattr4_maxfilesize = Uint64_t

type Fattr4_maxlink = Uint32_t

type Fattr4_maxname = Uint32_t

type Fattr4_maxread = Uint64_t

type Fattr4_maxwrite = Uint64_t

type Fattr4_mimetype = Ascii_REQUIRED4

type Fattr4_mode = Mode4

type Fattr4_mode_set_masked = Mode_masked4

type Fattr4_mounted_on_fileid = Uint64_t

type Fattr4_no_trunc = Bool

type Fattr4_numlinks = Uint32_t

type Fattr4_owner = Utf8str_mixed

type Fattr4_owner_group = Utf8str_mixed

type Fattr4_quota_avail_hard = Uint64_t

type Fattr4_quota_avail_soft = Uint64_t

type Fattr4_quota_used = Uint64_t

type Fattr4_rawdev = Specdata4

type Fattr4_space_avail = Uint64_t

type Fattr4_space_free = Uint64_t

type Fattr4_space_total = Uint64_t

type Fattr4_space_used = Uint64_t

type Fattr4_system = Bool

type Fattr4_time_access = Nfstime4

type Fattr4_time_access_set = Settime4

type Fattr4_time_backup = Nfstime4

type Fattr4_time_create = Nfstime4

type Fattr4_time_delta = Nfstime4

type Fattr4_time_metadata = Nfstime4

type Fattr4_time_modify = Nfstime4

type Fattr4_time_modify_set = Settime4

type Fattr4_suppattr_exclcreat = Bitmap4

type Fattr4_dir_notif_delay = Nfstime4

type Fattr4_dirent_notif_delay = Nfstime4

type Fattr4_fs_layout_types = [Layouttype4]

type Fattr4_fs_status = Fs4_status

type Fattr4_fs_charset_cap = Fs_charset_cap4

type Fattr4_layout_alignment = Uint32_t

type Fattr4_layout_blksize = Uint32_t

type Fattr4_layout_hint = Layouthint4

type Fattr4_layout_types = [Layouttype4]

type Fattr4_mdsthreshold = Mdsthreshold4

type Fattr4_retention_get = Retention_get4

type Fattr4_retention_set = Retention_set4

type Fattr4_retentevt_get = Retention_get4

type Fattr4_retentevt_set = Retention_set4

type Fattr4_retention_hold = Uint64_t

type Fattr4_dacl = Nfsacl41

type Fattr4_sacl = Nfsacl41

type Fattr4_change_policy = Change_policy4

type Fattr4_space_freed = Uint64_t

type Fattr4_change_attr_type = Change_attr_type4

type Fattr4_sec_label = Sec_label4

type Fattr4_clone_blksize = Uint32_t

FATTR4_SUPPORTED_ATTRS = 0
FATTR4_TYPE = 1
FATTR4_FH_EXPIRE_TYPE = 2
FATTR4_CHANGE = 3
FATTR4_SIZE = 4
FATTR4_LINK_SUPPORT = 5
FATTR4_SYMLINK_SUPPORT = 6
FATTR4_NAMED_ATTR = 7
FATTR4_FSID = 8
FATTR4_UNIQUE_HANDLES = 9
FATTR4_LEASE_TIME = 10
FATTR4_RDATTR_ERROR = 11
FATTR4_FILEHANDLE = 19
FATTR4_SUPPATTR_EXCLCREAT = 75
FATTR4_ACL = 12
FATTR4_ACLSUPPORT = 13
FATTR4_ARCHIVE = 14
FATTR4_CANSETTIME = 15
FATTR4_CASE_INSENSITIVE = 16
FATTR4_CASE_PRESERVING = 17
FATTR4_CHOWN_RESTRICTED = 18
FATTR4_FILEID = 20
FATTR4_FILES_AVAIL = 21
FATTR4_FILES_FREE = 22
FATTR4_FILES_TOTAL = 23
FATTR4_FS_LOCATIONS = 24
FATTR4_HIDDEN = 25
FATTR4_HOMOGENEOUS = 26
FATTR4_MAXFILESIZE = 27
FATTR4_MAXLINK = 28
FATTR4_MAXNAME = 29
FATTR4_MAXREAD = 30
FATTR4_MAXWRITE = 31
FATTR4_MIMETYPE = 32
FATTR4_MODE = 33
FATTR4_NO_TRUNC = 34
FATTR4_NUMLINKS = 35
FATTR4_OWNER = 36
FATTR4_OWNER_GROUP = 37
FATTR4_QUOTA_AVAIL_HARD = 38
FATTR4_QUOTA_AVAIL_SOFT = 39
FATTR4_QUOTA_USED = 40
FATTR4_RAWDEV = 41
FATTR4_SPACE_AVAIL = 42
FATTR4_SPACE_FREE = 43
FATTR4_SPACE_TOTAL = 44
FATTR4_SPACE_USED = 45
FATTR4_SYSTEM = 46
FATTR4_TIME_ACCESS = 47
FATTR4_TIME_ACCESS_SET = 48
FATTR4_TIME_BACKUP = 49
FATTR4_TIME_CREATE = 50
FATTR4_TIME_DELTA = 51
FATTR4_TIME_METADATA = 52
FATTR4_TIME_MODIFY = 53
FATTR4_TIME_MODIFY_SET = 54
FATTR4_MOUNTED_ON_FILEID = 55
FATTR4_DIR_NOTIF_DELAY = 56
FATTR4_DIRENT_NOTIF_DELAY = 57
FATTR4_DACL = 58
FATTR4_SACL = 59
FATTR4_CHANGE_POLICY = 60
FATTR4_FS_STATUS = 61
FATTR4_FS_LAYOUT_TYPES = 62
FATTR4_LAYOUT_HINT = 63
FATTR4_LAYOUT_TYPES = 64
FATTR4_LAYOUT_BLKSIZE = 65
FATTR4_LAYOUT_ALIGNMENT = 66
FATTR4_FS_LOCATIONS_INFO = 67
FATTR4_MDSTHRESHOLD = 68
FATTR4_RETENTION_GET = 69
FATTR4_RETENTION_SET = 70
FATTR4_RETENTEVT_GET = 71
FATTR4_RETENTEVT_SET = 72
FATTR4_RETENTION_HOLD = 73
FATTR4_MODE_SET_MASKED = 74
FATTR4_FS_CHARSET_CAP = 76
FATTR4_CLONE_BLKSIZE = 77
FATTR4_SPACE_FREED = 78
FATTR4_CHANGE_ATTR_TYPE = 79
FATTR4_SEC_LABEL = 80
data Fattr4 = Fattr4
  { attrmask :: Bitmap4
  , attr_vals :: Attrlist4
  } deriving (Show)

data Change_info4 = Change_info4
  { atomic :: Bool
  , before :: Changeid4
  , after :: Changeid4
  } deriving (Show)

type Clientaddr4 = Netaddr4

data Cb_client4 = Cb_client4
  { cb_program :: Uint32_t
  , cb_location :: Netaddr4
  } deriving (Show)

data Nfs_client_id4 = Nfs_client_id4
  { verifier :: Verifier4
  , id :: String
  } deriving (Show)

data Client_owner4 = Client_owner4
  { co_verifier :: Verifier4
  , co_ownerid :: String
  } deriving (Show)

data Server_owner4 = Server_owner4
  { so_minor_id :: Uint64_t
  , so_major_id :: String
  } deriving (Show)

data State_owner4 = State_owner4
  { clientid :: Clientid4
  , owner :: String
  } deriving (Show)

type Open_owner4 = State_owner4

type Lock_owner4 = State_owner4

data Nfs_lock_type4 = READ_LT | WRITE_LT | READW_LT | WRITEW_LT deriving (Eq, Show)
instance Enum Nfs_lock_type4 where
  fromEnum READ_LT = 1
  fromEnum WRITE_LT = 2
  fromEnum READW_LT = 3
  fromEnum WRITEW_LT = 4
  toEnum 1 = READ_LT
  toEnum 2 = WRITE_LT
  toEnum 3 = READW_LT
  toEnum 4 = WRITEW_LT

data Ssv_subkey4 = SSV4_SUBKEY_MIC_I2T | SSV4_SUBKEY_MIC_T2I | SSV4_SUBKEY_SEAL_I2T | SSV4_SUBKEY_SEAL_T2I deriving (Eq, Show)
instance Enum Ssv_subkey4 where
  fromEnum SSV4_SUBKEY_MIC_I2T = 1
  fromEnum SSV4_SUBKEY_MIC_T2I = 2
  fromEnum SSV4_SUBKEY_SEAL_I2T = 3
  fromEnum SSV4_SUBKEY_SEAL_T2I = 4
  toEnum 1 = SSV4_SUBKEY_MIC_I2T
  toEnum 2 = SSV4_SUBKEY_MIC_T2I
  toEnum 3 = SSV4_SUBKEY_SEAL_I2T
  toEnum 4 = SSV4_SUBKEY_SEAL_T2I

data Ssv_mic_plain_tkn4 = Ssv_mic_plain_tkn4
  { smpt_ssv_seq :: Uint32_t
  , smpt_orig_plain :: String
  } deriving (Show)

data Ssv_mic_tkn4 = Ssv_mic_tkn4
  { smt_ssv_seq :: Uint32_t
  , smt_hmac :: String
  } deriving (Show)

data Ssv_seal_plain_tkn4 = Ssv_seal_plain_tkn4
  { sspt_confounder :: String
  , sspt_ssv_seq :: Uint32_t
  , sspt_orig_plain :: String
  , sspt_pad :: String
  } deriving (Show)

data Ssv_seal_cipher_tkn4 = Ssv_seal_cipher_tkn4
  { ssct_ssv_seq :: Uint32_t
  , ssct_iv :: String
  , ssct_encr_data :: String
  , ssct_hmac :: String
  } deriving (Show)

data Fs_locations_server4 = Fs_locations_server4
  { fls_currency :: Int32_t
  , fls_info :: String
  , fls_server :: Utf8str_cis
  } deriving (Show)

FSLI4BX_GFLAGS = 0
FSLI4BX_TFLAGS = 1
FSLI4BX_CLSIMUL = 2
FSLI4BX_CLHANDLE = 3
FSLI4BX_CLFILEID = 4
FSLI4BX_CLWRITEVER = 5
FSLI4BX_CLCHANGE = 6
FSLI4BX_CLREADDIR = 7
FSLI4BX_READRANK = 8
FSLI4BX_WRITERANK = 9
FSLI4BX_READORDER = 10
FSLI4BX_WRITEORDER = 11
FSLI4GF_WRITABLE = 1
FSLI4GF_CUR_REQ = 2
FSLI4GF_ABSENT = 4
FSLI4GF_GOING = 8
FSLI4GF_SPLIT = 16
FSLI4TF_RDMA = 1
data Fs_locations_item4 = Fs_locations_item4
  { fli_entries :: [Fs_locations_server4]
  , fli_rootpath :: Pathname4
  } deriving (Show)

data Fs_locations_info4 = Fs_locations_info4
  { fli_flags :: Uint32_t
  , fli_valid_for :: Int32_t
  , fli_fs_root :: Pathname4
  , fli_items :: [Fs_locations_item4]
  } deriving (Show)

FSLI4IF_VAR_SUB = 1
type Fattr4_fs_locations_info = Fs_locations_info4

NFL4_UFLG_MASK = 63
NFL4_UFLG_DENSE = 1
NFL4_UFLG_COMMIT_THRU_MDS = 2
NFL42_UFLG_IO_ADVISE_THRU_MDS = 4
NFL4_UFLG_STRIPE_UNIT_SIZE_MASK = 4294967232
type Nfl_util4 = Uint32_t

data Filelayout_hint_care4 = NFLH4_CARE_DENSE | NFLH4_CARE_COMMIT_THRU_MDS | NFL42_CARE_IO_ADVISE_THRU_MDS | NFLH4_CARE_STRIPE_UNIT_SIZE | NFLH4_CARE_STRIPE_COUNT deriving (Eq, Show)
instance Enum Filelayout_hint_care4 where
  fromEnum NFLH4_CARE_DENSE = NFL4_UFLG_DENSE
  fromEnum NFLH4_CARE_COMMIT_THRU_MDS = NFL4_UFLG_COMMIT_THRU_MDS
  fromEnum NFL42_CARE_IO_ADVISE_THRU_MDS = NFL42_UFLG_IO_ADVISE_THRU_MDS
  fromEnum NFLH4_CARE_STRIPE_UNIT_SIZE = 64
  fromEnum NFLH4_CARE_STRIPE_COUNT = 128
  toEnum NFL4_UFLG_DENSE = NFLH4_CARE_DENSE
  toEnum NFL4_UFLG_COMMIT_THRU_MDS = NFLH4_CARE_COMMIT_THRU_MDS
  toEnum NFL42_UFLG_IO_ADVISE_THRU_MDS = NFL42_CARE_IO_ADVISE_THRU_MDS
  toEnum 64 = NFLH4_CARE_STRIPE_UNIT_SIZE
  toEnum 128 = NFLH4_CARE_STRIPE_COUNT

data Nfsv4_1_file_layouthint4 = Nfsv4_1_file_layouthint4
  { nflh_care :: Uint32_t
  , nflh_util :: Nfl_util4
  , nflh_stripe_count :: Count4
  } deriving (Show)

type Multipath_list4 = [Netaddr4]

data Nfsv4_1_file_layout_ds_addr4 = Nfsv4_1_file_layout_ds_addr4
  { nflda_stripe_indices :: [Uint32_t]
  , nflda_multipath_ds_list :: [Multipath_list4]
  } deriving (Show)

data Nfsv4_1_file_layout4 = Nfsv4_1_file_layout4
  { nfl_deviceid :: Deviceid4
  , nfl_util :: Nfl_util4
  , nfl_first_stripe_index :: Uint32_t
  , nfl_pattern_offset :: Offset4
  , nfl_fh_list :: [Nfs_fh4]
  } deriving (Show)

data Nfs_opnum4 = OP_ACCESS | OP_CLOSE | OP_COMMIT | OP_CREATE | OP_DELEGPURGE | OP_DELEGRETURN | OP_GETATTR | OP_GETFH | OP_LINK | OP_LOCK | OP_LOCKT | OP_LOCKU | OP_LOOKUP | OP_LOOKUPP | OP_NVERIFY | OP_OPEN | OP_OPENATTR | OP_OPEN_CONFIRM | OP_OPEN_DOWNGRADE | OP_PUTFH | OP_PUTPUBFH | OP_PUTROOTFH | OP_READ | OP_READDIR | OP_READLINK | OP_REMOVE | OP_RENAME | OP_RENEW | OP_RESTOREFH | OP_SAVEFH | OP_SECINFO | OP_SETATTR | OP_SETCLIENTID | OP_SETCLIENTID_CONFIRM | OP_VERIFY | OP_WRITE | OP_RELEASE_LOCKOWNER | OP_BACKCHANNEL_CTL | OP_BIND_CONN_TO_SESSION | OP_EXCHANGE_ID | OP_CREATE_SESSION | OP_DESTROY_SESSION | OP_FREE_STATEID | OP_GET_DIR_DELEGATION | OP_GETDEVICEINFO | OP_GETDEVICELIST | OP_LAYOUTCOMMIT | OP_LAYOUTGET | OP_LAYOUTRETURN | OP_SECINFO_NO_NAME | OP_SEQUENCE | OP_SET_SSV | OP_TEST_STATEID | OP_WANT_DELEGATION | OP_DESTROY_CLIENTID | OP_RECLAIM_COMPLETE | OP_ALLOCATE | OP_COPY | OP_COPY_NOTIFY | OP_DEALLOCATE | OP_IO_ADVISE | OP_LAYOUTERROR | OP_LAYOUTSTATS | OP_OFFLOAD_CANCEL | OP_OFFLOAD_STATUS | OP_READ_PLUS | OP_SEEK | OP_WRITE_SAME | OP_CLONE | OP_ILLEGAL deriving (Eq, Show)
instance Enum Nfs_opnum4 where
  fromEnum OP_ACCESS = 3
  fromEnum OP_CLOSE = 4
  fromEnum OP_COMMIT = 5
  fromEnum OP_CREATE = 6
  fromEnum OP_DELEGPURGE = 7
  fromEnum OP_DELEGRETURN = 8
  fromEnum OP_GETATTR = 9
  fromEnum OP_GETFH = 10
  fromEnum OP_LINK = 11
  fromEnum OP_LOCK = 12
  fromEnum OP_LOCKT = 13
  fromEnum OP_LOCKU = 14
  fromEnum OP_LOOKUP = 15
  fromEnum OP_LOOKUPP = 16
  fromEnum OP_NVERIFY = 17
  fromEnum OP_OPEN = 18
  fromEnum OP_OPENATTR = 19
  fromEnum OP_OPEN_CONFIRM = 20
  fromEnum OP_OPEN_DOWNGRADE = 21
  fromEnum OP_PUTFH = 22
  fromEnum OP_PUTPUBFH = 23
  fromEnum OP_PUTROOTFH = 24
  fromEnum OP_READ = 25
  fromEnum OP_READDIR = 26
  fromEnum OP_READLINK = 27
  fromEnum OP_REMOVE = 28
  fromEnum OP_RENAME = 29
  fromEnum OP_RENEW = 30
  fromEnum OP_RESTOREFH = 31
  fromEnum OP_SAVEFH = 32
  fromEnum OP_SECINFO = 33
  fromEnum OP_SETATTR = 34
  fromEnum OP_SETCLIENTID = 35
  fromEnum OP_SETCLIENTID_CONFIRM = 36
  fromEnum OP_VERIFY = 37
  fromEnum OP_WRITE = 38
  fromEnum OP_RELEASE_LOCKOWNER = 39
  fromEnum OP_BACKCHANNEL_CTL = 40
  fromEnum OP_BIND_CONN_TO_SESSION = 41
  fromEnum OP_EXCHANGE_ID = 42
  fromEnum OP_CREATE_SESSION = 43
  fromEnum OP_DESTROY_SESSION = 44
  fromEnum OP_FREE_STATEID = 45
  fromEnum OP_GET_DIR_DELEGATION = 46
  fromEnum OP_GETDEVICEINFO = 47
  fromEnum OP_GETDEVICELIST = 48
  fromEnum OP_LAYOUTCOMMIT = 49
  fromEnum OP_LAYOUTGET = 50
  fromEnum OP_LAYOUTRETURN = 51
  fromEnum OP_SECINFO_NO_NAME = 52
  fromEnum OP_SEQUENCE = 53
  fromEnum OP_SET_SSV = 54
  fromEnum OP_TEST_STATEID = 55
  fromEnum OP_WANT_DELEGATION = 56
  fromEnum OP_DESTROY_CLIENTID = 57
  fromEnum OP_RECLAIM_COMPLETE = 58
  fromEnum OP_ALLOCATE = 59
  fromEnum OP_COPY = 60
  fromEnum OP_COPY_NOTIFY = 61
  fromEnum OP_DEALLOCATE = 62
  fromEnum OP_IO_ADVISE = 63
  fromEnum OP_LAYOUTERROR = 64
  fromEnum OP_LAYOUTSTATS = 65
  fromEnum OP_OFFLOAD_CANCEL = 66
  fromEnum OP_OFFLOAD_STATUS = 67
  fromEnum OP_READ_PLUS = 68
  fromEnum OP_SEEK = 69
  fromEnum OP_WRITE_SAME = 70
  fromEnum OP_CLONE = 71
  fromEnum OP_ILLEGAL = 10044
  toEnum 3 = OP_ACCESS
  toEnum 4 = OP_CLOSE
  toEnum 5 = OP_COMMIT
  toEnum 6 = OP_CREATE
  toEnum 7 = OP_DELEGPURGE
  toEnum 8 = OP_DELEGRETURN
  toEnum 9 = OP_GETATTR
  toEnum 10 = OP_GETFH
  toEnum 11 = OP_LINK
  toEnum 12 = OP_LOCK
  toEnum 13 = OP_LOCKT
  toEnum 14 = OP_LOCKU
  toEnum 15 = OP_LOOKUP
  toEnum 16 = OP_LOOKUPP
  toEnum 17 = OP_NVERIFY
  toEnum 18 = OP_OPEN
  toEnum 19 = OP_OPENATTR
  toEnum 20 = OP_OPEN_CONFIRM
  toEnum 21 = OP_OPEN_DOWNGRADE
  toEnum 22 = OP_PUTFH
  toEnum 23 = OP_PUTPUBFH
  toEnum 24 = OP_PUTROOTFH
  toEnum 25 = OP_READ
  toEnum 26 = OP_READDIR
  toEnum 27 = OP_READLINK
  toEnum 28 = OP_REMOVE
  toEnum 29 = OP_RENAME
  toEnum 30 = OP_RENEW
  toEnum 31 = OP_RESTOREFH
  toEnum 32 = OP_SAVEFH
  toEnum 33 = OP_SECINFO
  toEnum 34 = OP_SETATTR
  toEnum 35 = OP_SETCLIENTID
  toEnum 36 = OP_SETCLIENTID_CONFIRM
  toEnum 37 = OP_VERIFY
  toEnum 38 = OP_WRITE
  toEnum 39 = OP_RELEASE_LOCKOWNER
  toEnum 40 = OP_BACKCHANNEL_CTL
  toEnum 41 = OP_BIND_CONN_TO_SESSION
  toEnum 42 = OP_EXCHANGE_ID
  toEnum 43 = OP_CREATE_SESSION
  toEnum 44 = OP_DESTROY_SESSION
  toEnum 45 = OP_FREE_STATEID
  toEnum 46 = OP_GET_DIR_DELEGATION
  toEnum 47 = OP_GETDEVICEINFO
  toEnum 48 = OP_GETDEVICELIST
  toEnum 49 = OP_LAYOUTCOMMIT
  toEnum 50 = OP_LAYOUTGET
  toEnum 51 = OP_LAYOUTRETURN
  toEnum 52 = OP_SECINFO_NO_NAME
  toEnum 53 = OP_SEQUENCE
  toEnum 54 = OP_SET_SSV
  toEnum 55 = OP_TEST_STATEID
  toEnum 56 = OP_WANT_DELEGATION
  toEnum 57 = OP_DESTROY_CLIENTID
  toEnum 58 = OP_RECLAIM_COMPLETE
  toEnum 59 = OP_ALLOCATE
  toEnum 60 = OP_COPY
  toEnum 61 = OP_COPY_NOTIFY
  toEnum 62 = OP_DEALLOCATE
  toEnum 63 = OP_IO_ADVISE
  toEnum 64 = OP_LAYOUTERROR
  toEnum 65 = OP_LAYOUTSTATS
  toEnum 66 = OP_OFFLOAD_CANCEL
  toEnum 67 = OP_OFFLOAD_STATUS
  toEnum 68 = OP_READ_PLUS
  toEnum 69 = OP_SEEK
  toEnum 70 = OP_WRITE_SAME
  toEnum 71 = OP_CLONE
  toEnum 10044 = OP_ILLEGAL

ACCESS4_READ = 1
ACCESS4_LOOKUP = 2
ACCESS4_MODIFY = 4
ACCESS4_EXTEND = 8
ACCESS4_DELETE = 16
ACCESS4_EXECUTE = 32
data ACCESS4args = ACCESS4args
  { access :: Uint32_t
  } deriving (Show)

data ACCESS4resok = ACCESS4resok
  { supported :: Uint32_t
  , access :: Uint32_t
  } deriving (Show)

data ACCESS4res = ACCESS4res
  { status :: Nfsstat4
  , resok4 :: Maybe ACCESS4resok
  } deriving (Show)

data CLONE4args = CLONE4args
  { cl_src_stateid :: Stateid4
  , cl_dst_stateid :: Stateid4
  , cl_src_offset :: Offset4
  , cl_dst_offset :: Offset4
  , cl_count :: Length4
  } deriving (Show)

data CLONE4res = CLONE4res
  { cl_status :: Nfsstat4
  } deriving (Show)

data CLOSE4args = CLOSE4args
  { seqid :: Seqid4
  , open_stateid :: Stateid4
  } deriving (Show)

data CLOSE4res = CLOSE4res
  { status :: Nfsstat4
  , open_stateid :: Maybe Stateid4
  } deriving (Show)

data COMMIT4args = COMMIT4args
  { offset :: Offset4
  , count :: Count4
  } deriving (Show)

data COMMIT4resok = COMMIT4resok
  { writeverf :: Verifier4
  } deriving (Show)

data COMMIT4res = COMMIT4res
  { status :: Nfsstat4
  , resok4 :: Maybe COMMIT4resok
  } deriving (Show)

data Createtype4 = Createtype4
  { typec :: Nfs_ftype4
  , linkdata :: Maybe Linktext4
  , devdata :: Maybe Specdata4
  } deriving (Show)

data CREATE4args = CREATE4args
  { objtype :: Createtype4
  , objname :: Component4
  , createattrs :: Fattr4
  } deriving (Show)

data CREATE4resok = CREATE4resok
  { cinfo :: Change_info4
  , attrset :: Bitmap4
  } deriving (Show)

data CREATE4res = CREATE4res
  { status :: Nfsstat4
  , resok4 :: Maybe CREATE4resok
  } deriving (Show)

data DELEGPURGE4args = DELEGPURGE4args
  { clientid :: Clientid4
  } deriving (Show)

data DELEGPURGE4res = DELEGPURGE4res
  { status :: Nfsstat4
  } deriving (Show)

data DELEGRETURN4args = DELEGRETURN4args
  { deleg_stateid :: Stateid4
  } deriving (Show)

data DELEGRETURN4res = DELEGRETURN4res
  { status :: Nfsstat4
  } deriving (Show)

data GETATTR4args = GETATTR4args
  { attr_request :: Bitmap4
  } deriving (Show)

data GETATTR4resok = GETATTR4resok
  { obj_attributes :: Fattr4
  } deriving (Show)

data GETATTR4res = GETATTR4res
  { status :: Nfsstat4
  , resok4 :: Maybe GETATTR4resok
  } deriving (Show)

data GETFH4resok = GETFH4resok
  { object :: Nfs_fh4
  } deriving (Show)

data GETFH4res = GETFH4res
  { status :: Nfsstat4
  , resok4 :: Maybe GETFH4resok
  } deriving (Show)

data LINK4args = LINK4args
  { newname :: Component4
  } deriving (Show)

data LINK4resok = LINK4resok
  { cinfo :: Change_info4
  } deriving (Show)

data LINK4res = LINK4res
  { status :: Nfsstat4
  , resok4 :: Maybe LINK4resok
  } deriving (Show)

data Open_to_lock_owner4 = Open_to_lock_owner4
  { open_seqid :: Seqid4
  , open_stateid :: Stateid4
  , lock_seqid :: Seqid4
  , lock_owner :: Lock_owner4
  } deriving (Show)

data Exist_lock_owner4 = Exist_lock_owner4
  { lock_stateid :: Stateid4
  , lock_seqid :: Seqid4
  } deriving (Show)

data Locker4 = Locker4
  { new_lock_owner
  , open_owner :: Maybe Open_to_lock_owner4
  , lock_owner :: Maybe Exist_lock_owner4
  } deriving (Show)

data LOCK4args = LOCK4args
  { locktype :: Nfs_lock_type4
  , reclaim :: Bool
  , offset :: Offset4
  , length :: Length4
  , locker :: Locker4
  } deriving (Show)

data LOCK4denied = LOCK4denied
  { offset :: Offset4
  , length :: Length4
  , locktype :: Nfs_lock_type4
  , owner :: Lock_owner4
  } deriving (Show)

data LOCK4resok = LOCK4resok
  { lock_stateid :: Stateid4
  } deriving (Show)

data LOCK4res = LOCK4res
  { status :: Nfsstat4
  , resok4 :: Maybe LOCK4resok
  , denied :: Maybe LOCK4denied
  } deriving (Show)

data LOCKT4args = LOCKT4args
  { locktype :: Nfs_lock_type4
  , offset :: Offset4
  , length :: Length4
  , owner :: Lock_owner4
  } deriving (Show)

data LOCKT4res = LOCKT4res
  { status :: Nfsstat4
  , denied :: Maybe LOCK4denied
  } deriving (Show)

data LOCKU4args = LOCKU4args
  { locktype :: Nfs_lock_type4
  , seqid :: Seqid4
  , lock_stateid :: Stateid4
  , offset :: Offset4
  , length :: Length4
  } deriving (Show)

data LOCKU4res = LOCKU4res
  { status :: Nfsstat4
  , lock_stateid :: Maybe Stateid4
  } deriving (Show)

data LOOKUP4args = LOOKUP4args
  { objname :: Component4
  } deriving (Show)

data LOOKUP4res = LOOKUP4res
  { status :: Nfsstat4
  } deriving (Show)

data LOOKUPP4res = LOOKUPP4res
  { status :: Nfsstat4
  } deriving (Show)

data NVERIFY4args = NVERIFY4args
  { obj_attributes :: Fattr4
  } deriving (Show)

data NVERIFY4res = NVERIFY4res
  { status :: Nfsstat4
  } deriving (Show)

data Createmode4 = UNCHECKED4 | GUARDED4 | EXCLUSIVE4 | EXCLUSIVE4_1 deriving (Eq, Show)
instance Enum Createmode4 where
  fromEnum UNCHECKED4 = 0
  fromEnum GUARDED4 = 1
  fromEnum EXCLUSIVE4 = 2
  fromEnum EXCLUSIVE4_1 = 3
  toEnum 0 = UNCHECKED4
  toEnum 1 = GUARDED4
  toEnum 2 = EXCLUSIVE4
  toEnum 3 = EXCLUSIVE4_1

data Creatverfattr = Creatverfattr
  { cva_verf :: Verifier4
  , cva_attrs :: Fattr4
  } deriving (Show)

data Createhow4 = Createhow4
  { mode :: Createmode4
  , createattrs :: Maybe Fattr4
  , createverf :: Maybe Verifier4
  , ch_createboth :: Maybe Creatverfattr
  } deriving (Show)

data Opentype4 = OPEN4_NOCREATE | OPEN4_CREATE deriving (Eq, Show)
instance Enum Opentype4 where
  fromEnum OPEN4_NOCREATE = 0
  fromEnum OPEN4_CREATE = 1
  toEnum 0 = OPEN4_NOCREATE
  toEnum 1 = OPEN4_CREATE

data Openflag4 = Openflag4
  { opentype :: Opentype4
  , how :: Maybe Createhow4
  } deriving (Show)

data Limit_by4 = NFS_LIMIT_SIZE | NFS_LIMIT_BLOCKS deriving (Eq, Show)
instance Enum Limit_by4 where
  fromEnum NFS_LIMIT_SIZE = 1
  fromEnum NFS_LIMIT_BLOCKS = 2
  toEnum 1 = NFS_LIMIT_SIZE
  toEnum 2 = NFS_LIMIT_BLOCKS

data Nfs_modified_limit4 = Nfs_modified_limit4
  { num_blocks :: Uint32_t
  , bytes_per_block :: Uint32_t
  } deriving (Show)

data Nfs_space_limit4 = Nfs_space_limit4
  { limitby :: Limit_by4
  , filesize :: Maybe Uint64_t
  , mod_blocks :: Maybe Nfs_modified_limit4
  } deriving (Show)

OPEN4_SHARE_ACCESS_READ = 1
OPEN4_SHARE_ACCESS_WRITE = 2
OPEN4_SHARE_ACCESS_BOTH = 3
OPEN4_SHARE_DENY_NONE = 0
OPEN4_SHARE_DENY_READ = 1
OPEN4_SHARE_DENY_WRITE = 2
OPEN4_SHARE_DENY_BOTH = 3
OPEN4_SHARE_ACCESS_WANT_DELEG_MASK = 65280
OPEN4_SHARE_ACCESS_WANT_NO_PREFERENCE = 0
OPEN4_SHARE_ACCESS_WANT_READ_DELEG = 256
OPEN4_SHARE_ACCESS_WANT_WRITE_DELEG = 512
OPEN4_SHARE_ACCESS_WANT_ANY_DELEG = 768
OPEN4_SHARE_ACCESS_WANT_NO_DELEG = 1024
OPEN4_SHARE_ACCESS_WANT_CANCEL = 1280
OPEN4_SHARE_ACCESS_WANT_SIGNAL_DELEG_WHEN_RESRC_AVAIL = 65536
OPEN4_SHARE_ACCESS_WANT_PUSH_DELEG_WHEN_UNCONTENDED = 131072
data Open_delegation_type4 = OPEN_DELEGATE_NONE | OPEN_DELEGATE_READ | OPEN_DELEGATE_WRITE | OPEN_DELEGATE_NONE_EXT deriving (Eq, Show)
instance Enum Open_delegation_type4 where
  fromEnum OPEN_DELEGATE_NONE = 0
  fromEnum OPEN_DELEGATE_READ = 1
  fromEnum OPEN_DELEGATE_WRITE = 2
  fromEnum OPEN_DELEGATE_NONE_EXT = 3
  toEnum 0 = OPEN_DELEGATE_NONE
  toEnum 1 = OPEN_DELEGATE_READ
  toEnum 2 = OPEN_DELEGATE_WRITE
  toEnum 3 = OPEN_DELEGATE_NONE_EXT

data Open_claim_type4 = CLAIM_NULL | CLAIM_PREVIOUS | CLAIM_DELEGATE_CUR | CLAIM_DELEGATE_PREV | CLAIM_FH | CLAIM_DELEG_CUR_FH | CLAIM_DELEG_PREV_FH deriving (Eq, Show)
instance Enum Open_claim_type4 where
  fromEnum CLAIM_NULL = 0
  fromEnum CLAIM_PREVIOUS = 1
  fromEnum CLAIM_DELEGATE_CUR = 2
  fromEnum CLAIM_DELEGATE_PREV = 3
  fromEnum CLAIM_FH = 4
  fromEnum CLAIM_DELEG_CUR_FH = 5
  fromEnum CLAIM_DELEG_PREV_FH = 6
  toEnum 0 = CLAIM_NULL
  toEnum 1 = CLAIM_PREVIOUS
  toEnum 2 = CLAIM_DELEGATE_CUR
  toEnum 3 = CLAIM_DELEGATE_PREV
  toEnum 4 = CLAIM_FH
  toEnum 5 = CLAIM_DELEG_CUR_FH
  toEnum 6 = CLAIM_DELEG_PREV_FH

data Open_claim_delegate_cur4 = Open_claim_delegate_cur4
  { delegate_stateid :: Stateid4
  , file :: Component4
  } deriving (Show)

data Open_claim4 = Open_claim4
  { claim :: Open_claim_type4
  , file :: Maybe Component4
  , delegate_type :: Maybe Open_delegation_type4
  , delegate_cur_info :: Maybe Open_claim_delegate_cur4
  , file_delegate_prev :: Maybe Component4
  , oc_delegate_stateid :: Maybe Stateid4
  } deriving (Show)

data OPEN4args = OPEN4args
  { seqid :: Seqid4
  , share_access :: Uint32_t
  , share_deny :: Uint32_t
  , owner :: Open_owner4
  , openhow :: Openflag4
  , claim :: Open_claim4
  } deriving (Show)

data Open_read_delegation4 = Open_read_delegation4
  { stateid :: Stateid4
  , recall :: Bool
  , permissions :: Nfsace4
  } deriving (Show)

data Open_write_delegation4 = Open_write_delegation4
  { stateid :: Stateid4
  , recall :: Bool
  , space_limit :: Nfs_space_limit4
  , permissions :: Nfsace4
  } deriving (Show)

data Why_no_delegation4 = WND4_NOT_WANTED | WND4_CONTENTION | WND4_RESOURCE | WND4_NOT_SUPP_FTYPE | WND4_WRITE_DELEG_NOT_SUPP_FTYPE | WND4_NOT_SUPP_UPGRADE | WND4_NOT_SUPP_DOWNGRADE | WND4_CANCELLED | WND4_IS_DIR deriving (Eq, Show)
instance Enum Why_no_delegation4 where
  fromEnum WND4_NOT_WANTED = 0
  fromEnum WND4_CONTENTION = 1
  fromEnum WND4_RESOURCE = 2
  fromEnum WND4_NOT_SUPP_FTYPE = 3
  fromEnum WND4_WRITE_DELEG_NOT_SUPP_FTYPE = 4
  fromEnum WND4_NOT_SUPP_UPGRADE = 5
  fromEnum WND4_NOT_SUPP_DOWNGRADE = 6
  fromEnum WND4_CANCELLED = 7
  fromEnum WND4_IS_DIR = 8
  toEnum 0 = WND4_NOT_WANTED
  toEnum 1 = WND4_CONTENTION
  toEnum 2 = WND4_RESOURCE
  toEnum 3 = WND4_NOT_SUPP_FTYPE
  toEnum 4 = WND4_WRITE_DELEG_NOT_SUPP_FTYPE
  toEnum 5 = WND4_NOT_SUPP_UPGRADE
  toEnum 6 = WND4_NOT_SUPP_DOWNGRADE
  toEnum 7 = WND4_CANCELLED
  toEnum 8 = WND4_IS_DIR

data Open_none_delegation4 = Open_none_delegation4
  { ond_why :: Why_no_delegation4
  , ond_server_will_push_deleg :: Maybe Bool
  , ond_server_will_signal_avail :: Maybe Bool
  } deriving (Show)

data Open_delegation4 = Open_delegation4
  { delegation_type :: Open_delegation_type4
  , read :: Maybe Open_read_delegation4
  , write :: Maybe Open_write_delegation4
  , od_whynone :: Maybe Open_none_delegation4
  } deriving (Show)

OPEN4_RESULT_CONFIRM = 2
OPEN4_RESULT_LOCKTYPE_POSIX = 4
OPEN4_RESULT_PRESERVE_UNLINKED = 8
OPEN4_RESULT_MAY_NOTIFY_LOCK = 32
data OPEN4resok = OPEN4resok
  { stateid :: Stateid4
  , cinfo :: Change_info4
  , rflags :: Uint32_t
  , attrset :: Bitmap4
  , delegation :: Open_delegation4
  } deriving (Show)

data OPEN4res = OPEN4res
  { status :: Nfsstat4
  , resok4 :: Maybe OPEN4resok
  } deriving (Show)

data OPENATTR4args = OPENATTR4args
  { createdir :: Bool
  } deriving (Show)

data OPENATTR4res = OPENATTR4res
  { status :: Nfsstat4
  } deriving (Show)

data OPEN_CONFIRM4args = OPEN_CONFIRM4args
  { open_stateid :: Stateid4
  , seqid :: Seqid4
  } deriving (Show)

data OPEN_CONFIRM4resok = OPEN_CONFIRM4resok
  { open_stateid :: Stateid4
  } deriving (Show)

data OPEN_CONFIRM4res = OPEN_CONFIRM4res
  { status :: Nfsstat4
  , resok4 :: Maybe OPEN_CONFIRM4resok
  } deriving (Show)

data OPEN_DOWNGRADE4args = OPEN_DOWNGRADE4args
  { open_stateid :: Stateid4
  , seqid :: Seqid4
  , share_access :: Uint32_t
  , share_deny :: Uint32_t
  } deriving (Show)

data OPEN_DOWNGRADE4resok = OPEN_DOWNGRADE4resok
  { open_stateid :: Stateid4
  } deriving (Show)

data OPEN_DOWNGRADE4res = OPEN_DOWNGRADE4res
  { status :: Nfsstat4
  , resok4 :: Maybe OPEN_DOWNGRADE4resok
  } deriving (Show)

data PUTFH4args = PUTFH4args
  { object :: Nfs_fh4
  } deriving (Show)

data PUTFH4res = PUTFH4res
  { status :: Nfsstat4
  } deriving (Show)

data PUTPUBFH4res = PUTPUBFH4res
  { status :: Nfsstat4
  } deriving (Show)

data PUTROOTFH4res = PUTROOTFH4res
  { status :: Nfsstat4
  } deriving (Show)

data READ4args = READ4args
  { stateid :: Stateid4
  , offset :: Offset4
  , count :: Count4
  } deriving (Show)

data READ4resok = READ4resok
  { eof :: Bool
  , datar :: String
  } deriving (Show)

data READ4res = READ4res
  { status :: Nfsstat4
  , resok4 :: Maybe READ4resok
  } deriving (Show)

data READDIR4args = READDIR4args
  { cookie :: Nfs_cookie4
  , cookieverf :: Verifier4
  , dircount :: Count4
  , maxcount :: Count4
  , attr_request :: Bitmap4
  } deriving (Show)

data Entry4 = Entry4
  { cookie :: Nfs_cookie4
  , name :: Component4
  , attrs :: Fattr4
  , nextentry :: Entry4  -- OPTIONAL
  } deriving (Show)

data Dirlist4 = Dirlist4
  { entries :: Entry4  -- OPTIONAL
  , eof :: Bool
  } deriving (Show)

data READDIR4resok = READDIR4resok
  { cookieverf :: Verifier4
  , reply :: Dirlist4
  } deriving (Show)

data READDIR4res = READDIR4res
  { status :: Nfsstat4
  , resok4 :: Maybe READDIR4resok
  } deriving (Show)

data READLINK4resok = READLINK4resok
  { link :: Linktext4
  } deriving (Show)

data READLINK4res = READLINK4res
  { status :: Nfsstat4
  , resok4 :: Maybe READLINK4resok
  } deriving (Show)

data REMOVE4args = REMOVE4args
  { target :: Component4
  } deriving (Show)

data REMOVE4resok = REMOVE4resok
  { cinfo :: Change_info4
  } deriving (Show)

data REMOVE4res = REMOVE4res
  { status :: Nfsstat4
  , resok4 :: Maybe REMOVE4resok
  } deriving (Show)

data RENAME4args = RENAME4args
  { oldname :: Component4
  , newname :: Component4
  } deriving (Show)

data RENAME4resok = RENAME4resok
  { source_cinfo :: Change_info4
  , target_cinfo :: Change_info4
  } deriving (Show)

data RENAME4res = RENAME4res
  { status :: Nfsstat4
  , resok4 :: Maybe RENAME4resok
  } deriving (Show)

data RENEW4args = RENEW4args
  { clientid :: Clientid4
  } deriving (Show)

data RENEW4res = RENEW4res
  { status :: Nfsstat4
  } deriving (Show)

data RESTOREFH4res = RESTOREFH4res
  { status :: Nfsstat4
  } deriving (Show)

data SAVEFH4res = SAVEFH4res
  { status :: Nfsstat4
  } deriving (Show)

data SECINFO4args = SECINFO4args
  { name :: Component4
  } deriving (Show)

data Rpc_gss_svc_t = RPC_GSS_SVC_NONE | RPC_GSS_SVC_INTEGRITY | RPC_GSS_SVC_PRIVACY deriving (Eq, Show)
instance Enum Rpc_gss_svc_t where
  fromEnum RPC_GSS_SVC_NONE = 1
  fromEnum RPC_GSS_SVC_INTEGRITY = 2
  fromEnum RPC_GSS_SVC_PRIVACY = 3
  toEnum 1 = RPC_GSS_SVC_NONE
  toEnum 2 = RPC_GSS_SVC_INTEGRITY
  toEnum 3 = RPC_GSS_SVC_PRIVACY

data Rpcsec_gss_info = Rpcsec_gss_info
  { oid :: Sec_oid4
  , qop :: Qop4
  , service :: Rpc_gss_svc_t
  } deriving (Show)

data Secinfo4 = Secinfo4
  { flavor
  , flavor_info :: Maybe Rpcsec_gss_info
  } deriving (Show)

type SECINFO4resok = [Secinfo4]

data SECINFO4res = SECINFO4res
  { status :: Nfsstat4
  , resok4 :: Maybe SECINFO4resok
  } deriving (Show)

data SETATTR4args = SETATTR4args
  { stateid :: Stateid4
  , obj_attributes :: Fattr4
  } deriving (Show)

data SETATTR4res = SETATTR4res
  { status :: Nfsstat4
  , attrsset :: Bitmap4
  } deriving (Show)

data SETCLIENTID4args = SETCLIENTID4args
  { client :: Nfs_client_id4
  , callback :: Cb_client4
  , callback_ident :: Uint32_t
  } deriving (Show)

data SETCLIENTID4resok = SETCLIENTID4resok
  { clientid :: Clientid4
  , setclientid_confirm :: Verifier4
  } deriving (Show)

data SETCLIENTID4res = SETCLIENTID4res
  { status :: Nfsstat4
  , resok4 :: Maybe SETCLIENTID4resok
  , client_using :: Maybe Clientaddr4
  } deriving (Show)

data SETCLIENTID_CONFIRM4args = SETCLIENTID_CONFIRM4args
  { clientid :: Clientid4
  , setclientid_confirm :: Verifier4
  } deriving (Show)

data SETCLIENTID_CONFIRM4res = SETCLIENTID_CONFIRM4res
  { status :: Nfsstat4
  } deriving (Show)

data VERIFY4args = VERIFY4args
  { obj_attributes :: Fattr4
  } deriving (Show)

data VERIFY4res = VERIFY4res
  { status :: Nfsstat4
  } deriving (Show)

data WRITE4args = WRITE4args
  { stateid :: Stateid4
  , offset :: Offset4
  , stable :: Stable_how4
  , dataw :: String
  } deriving (Show)

data WRITE4resok = WRITE4resok
  { count :: Count4
  , committed :: Stable_how4
  , writeverf :: Verifier4
  } deriving (Show)

data WRITE4res = WRITE4res
  { status :: Nfsstat4
  , resok4 :: Maybe WRITE4resok
  } deriving (Show)

data RELEASE_LOCKOWNER4args = RELEASE_LOCKOWNER4args
  { lock_owner :: Lock_owner4
  } deriving (Show)

data RELEASE_LOCKOWNER4res = RELEASE_LOCKOWNER4res
  { status :: Nfsstat4
  } deriving (Show)

data ILLEGAL4res = ILLEGAL4res
  { status :: Nfsstat4
  } deriving (Show)

type Gsshandle4_t = String

data Gss_cb_handles4 = Gss_cb_handles4
  { gcbp_service :: Rpc_gss_svc_t
  , gcbp_handle_from_server :: Gsshandle4_t
  , gcbp_handle_from_client :: Gsshandle4_t
  } deriving (Show)

data Callback_sec_parms4 = Callback_sec_parms4
  { cb_secflavor
  , cbsp_sys_cred :: Maybe Authsys_parms
  , cbsp_gss_handles :: Maybe Gss_cb_handles4
  } deriving (Show)

data BACKCHANNEL_CTL4args = BACKCHANNEL_CTL4args
  { bca_cb_program :: Uint32_t
  , bca_sec_parms :: [Callback_sec_parms4]
  } deriving (Show)

data BACKCHANNEL_CTL4res = BACKCHANNEL_CTL4res
  { bcr_status :: Nfsstat4
  } deriving (Show)

data Channel_dir_from_client4 = CDFC4_FORE | CDFC4_BACK | CDFC4_FORE_OR_BOTH | CDFC4_BACK_OR_BOTH deriving (Eq, Show)
instance Enum Channel_dir_from_client4 where
  fromEnum CDFC4_FORE = 1
  fromEnum CDFC4_BACK = 2
  fromEnum CDFC4_FORE_OR_BOTH = 3
  fromEnum CDFC4_BACK_OR_BOTH = 7
  toEnum 1 = CDFC4_FORE
  toEnum 2 = CDFC4_BACK
  toEnum 3 = CDFC4_FORE_OR_BOTH
  toEnum 7 = CDFC4_BACK_OR_BOTH

data BIND_CONN_TO_SESSION4args = BIND_CONN_TO_SESSION4args
  { bctsa_sessid :: Sessionid4
  , bctsa_dir :: Channel_dir_from_client4
  , bctsa_use_conn_in_rdma_mode :: Bool
  } deriving (Show)

data Channel_dir_from_server4 = CDFS4_FORE | CDFS4_BACK | CDFS4_BOTH deriving (Eq, Show)
instance Enum Channel_dir_from_server4 where
  fromEnum CDFS4_FORE = 1
  fromEnum CDFS4_BACK = 2
  fromEnum CDFS4_BOTH = 3
  toEnum 1 = CDFS4_FORE
  toEnum 2 = CDFS4_BACK
  toEnum 3 = CDFS4_BOTH

data BIND_CONN_TO_SESSION4resok = BIND_CONN_TO_SESSION4resok
  { bctsr_sessid :: Sessionid4
  , bctsr_dir :: Channel_dir_from_server4
  , bctsr_use_conn_in_rdma_mode :: Bool
  } deriving (Show)

data BIND_CONN_TO_SESSION4res = BIND_CONN_TO_SESSION4res
  { bctsr_status :: Nfsstat4
  , bctsr_resok4 :: Maybe BIND_CONN_TO_SESSION4resok
  } deriving (Show)

EXCHGID4_FLAG_SUPP_MOVED_REFER = 1
EXCHGID4_FLAG_SUPP_MOVED_MIGR = 2
EXCHGID4_FLAG_SUPP_FENCE_OPS = 4
EXCHGID4_FLAG_BIND_PRINC_STATEID = 256
EXCHGID4_FLAG_USE_NON_PNFS = 65536
EXCHGID4_FLAG_USE_PNFS_MDS = 131072
EXCHGID4_FLAG_USE_PNFS_DS = 262144
EXCHGID4_FLAG_MASK_PNFS = 458752
EXCHGID4_FLAG_UPD_CONFIRMED_REC_A = 1073741824
EXCHGID4_FLAG_CONFIRMED_R = 2147483648
data State_protect_ops4 = State_protect_ops4
  { spo_must_enforce :: Bitmap4
  , spo_must_allow :: Bitmap4
  } deriving (Show)

data Ssv_sp_parms4 = Ssv_sp_parms4
  { ssp_ops :: State_protect_ops4
  , ssp_hash_algs :: [Sec_oid4]
  , ssp_encr_algs :: [Sec_oid4]
  , ssp_window :: Uint32_t
  , ssp_num_gss_handles :: Uint32_t
  } deriving (Show)

data State_protect_how4 = SP4_NONE | SP4_MACH_CRED | SP4_SSV deriving (Eq, Show)
instance Enum State_protect_how4 where
  fromEnum SP4_NONE = 0
  fromEnum SP4_MACH_CRED = 1
  fromEnum SP4_SSV = 2
  toEnum 0 = SP4_NONE
  toEnum 1 = SP4_MACH_CRED
  toEnum 2 = SP4_SSV

data State_protect4_a = State_protect4_a
  { spa_how :: State_protect_how4
  , spa_mach_ops :: Maybe State_protect_ops4
  , spa_ssv_parms :: Maybe Ssv_sp_parms4
  } deriving (Show)

data EXCHANGE_ID4args = EXCHANGE_ID4args
  { eia_clientowner :: Client_owner4
  , eia_flags :: Uint32_t
  , eia_state_protect :: State_protect4_a
  , eia_client_impl_id :: [Nfs_impl_id4]
  } deriving (Show)

data Ssv_prot_info4 = Ssv_prot_info4
  { spi_ops :: State_protect_ops4
  , spi_hash_alg :: Uint32_t
  , spi_encr_alg :: Uint32_t
  , spi_ssv_len :: Uint32_t
  , spi_window :: Uint32_t
  , spi_handles :: [Gsshandle4_t]
  } deriving (Show)

data State_protect4_r = State_protect4_r
  { spr_how :: State_protect_how4
  , spr_mach_ops :: Maybe State_protect_ops4
  , spr_ssv_info :: Maybe Ssv_prot_info4
  } deriving (Show)

data EXCHANGE_ID4resok = EXCHANGE_ID4resok
  { eir_clientid :: Clientid4
  , eir_sequenceid :: Sequenceid4
  , eir_flags :: Uint32_t
  , eir_state_protect :: State_protect4_r
  , eir_server_owner :: Server_owner4
  , eir_server_scope :: String
  , eir_server_impl_id :: [Nfs_impl_id4]
  } deriving (Show)

data EXCHANGE_ID4res = EXCHANGE_ID4res
  { eir_status :: Nfsstat4
  , eir_resok4 :: Maybe EXCHANGE_ID4resok
  } deriving (Show)

data Channel_attrs4 = Channel_attrs4
  { ca_headerpadsize :: Count4
  , ca_maxrequestsize :: Count4
  , ca_maxresponsesize :: Count4
  , ca_maxresponsesize_cached :: Count4
  , ca_maxoperations :: Count4
  , ca_maxrequests :: Count4
  , ca_rdma_ird :: [Uint32_t]
  } deriving (Show)

CREATE_SESSION4_FLAG_PERSIST = 1
CREATE_SESSION4_FLAG_CONN_BACK_CHAN = 2
CREATE_SESSION4_FLAG_CONN_RDMA = 4
data CREATE_SESSION4args = CREATE_SESSION4args
  { csa_clientid :: Clientid4
  , csa_sequence :: Sequenceid4
  , csa_flags :: Uint32_t
  , csa_fore_chan_attrs :: Channel_attrs4
  , csa_back_chan_attrs :: Channel_attrs4
  , csa_cb_program :: Uint32_t
  , csa_sec_parms :: [Callback_sec_parms4]
  } deriving (Show)

data CREATE_SESSION4resok = CREATE_SESSION4resok
  { csr_sessionid :: Sessionid4
  , csr_sequence :: Sequenceid4
  , csr_flags :: Uint32_t
  , csr_fore_chan_attrs :: Channel_attrs4
  , csr_back_chan_attrs :: Channel_attrs4
  } deriving (Show)

data CREATE_SESSION4res = CREATE_SESSION4res
  { csr_status :: Nfsstat4
  , csr_resok4 :: Maybe CREATE_SESSION4resok
  } deriving (Show)

data DESTROY_SESSION4args = DESTROY_SESSION4args
  { dsa_sessionid :: Sessionid4
  } deriving (Show)

data DESTROY_SESSION4res = DESTROY_SESSION4res
  { dsr_status :: Nfsstat4
  } deriving (Show)

data FREE_STATEID4args = FREE_STATEID4args
  { fsa_stateid :: Stateid4
  } deriving (Show)

data FREE_STATEID4res = FREE_STATEID4res
  { fsr_status :: Nfsstat4
  } deriving (Show)

type Attr_notice4 = Nfstime4

data GET_DIR_DELEGATION4args = GET_DIR_DELEGATION4args
  { gdda_signal_deleg_avail :: Bool
  , gdda_notification_types :: Bitmap4
  , gdda_child_attr_delay :: Attr_notice4
  , gdda_dir_attr_delay :: Attr_notice4
  , gdda_child_attributes :: Bitmap4
  , gdda_dir_attributes :: Bitmap4
  } deriving (Show)

data GET_DIR_DELEGATION4resok = GET_DIR_DELEGATION4resok
  { gddr_cookieverf :: Verifier4
  , gddr_stateid :: Stateid4
  , gddr_notification :: Bitmap4
  , gddr_child_attributes :: Bitmap4
  , gddr_dir_attributes :: Bitmap4
  } deriving (Show)

data Gddrnf4_status = GDD4_OK | GDD4_UNAVAIL deriving (Eq, Show)
instance Enum Gddrnf4_status where
  fromEnum GDD4_OK = 0
  fromEnum GDD4_UNAVAIL = 1
  toEnum 0 = GDD4_OK
  toEnum 1 = GDD4_UNAVAIL

data GET_DIR_DELEGATION4res_non_fatal = GET_DIR_DELEGATION4res_non_fatal
  { gddrnf_status :: Gddrnf4_status
  , gddrnf_resok4 :: Maybe GET_DIR_DELEGATION4resok
  , gddrnf_will_signal_deleg_avail :: Maybe Bool
  } deriving (Show)

data GET_DIR_DELEGATION4res = GET_DIR_DELEGATION4res
  { gddr_status :: Nfsstat4
  , gddr_res_non_fatal4 :: Maybe GET_DIR_DELEGATION4res_non_fatal
  } deriving (Show)

data GETDEVICEINFO4args = GETDEVICEINFO4args
  { gdia_device_id :: Deviceid4
  , gdia_layout_type :: Layouttype4
  , gdia_maxcount :: Count4
  , gdia_notify_types :: Bitmap4
  } deriving (Show)

data GETDEVICEINFO4resok = GETDEVICEINFO4resok
  { gdir_device_addr :: Device_addr4
  , gdir_notification :: Bitmap4
  } deriving (Show)

data GETDEVICEINFO4res = GETDEVICEINFO4res
  { gdir_status :: Nfsstat4
  , gdir_resok4 :: Maybe GETDEVICEINFO4resok
  , gdir_mincount :: Maybe Count4
  } deriving (Show)

data GETDEVICELIST4args = GETDEVICELIST4args
  { gdla_layout_type :: Layouttype4
  , gdla_maxdevices :: Count4
  , gdla_cookie :: Nfs_cookie4
  , gdla_cookieverf :: Verifier4
  } deriving (Show)

data GETDEVICELIST4resok = GETDEVICELIST4resok
  { gdlr_cookie :: Nfs_cookie4
  , gdlr_cookieverf :: Verifier4
  , gdlr_deviceid_list :: [Deviceid4]
  , gdlr_eof :: Bool
  } deriving (Show)

data GETDEVICELIST4res = GETDEVICELIST4res
  { gdlr_status :: Nfsstat4
  , gdlr_resok4 :: Maybe GETDEVICELIST4resok
  } deriving (Show)

data Newtime4 = Newtime4
  { nt_timechanged
  , nt_time :: Maybe Nfstime4
  } deriving (Show)

data Newoffset4 = Newoffset4
  { no_newoffset
  , no_offset :: Maybe Offset4
  } deriving (Show)

data LAYOUTCOMMIT4args = LAYOUTCOMMIT4args
  { loca_offset :: Offset4
  , loca_length :: Length4
  , loca_reclaim :: Bool
  , loca_stateid :: Stateid4
  , loca_last_write_offset :: Newoffset4
  , loca_time_modify :: Newtime4
  , loca_layoutupdate :: Layoutupdate4
  } deriving (Show)

data Newsize4 = Newsize4
  { ns_sizechanged
  , ns_size :: Maybe Length4
  } deriving (Show)

data LAYOUTCOMMIT4resok = LAYOUTCOMMIT4resok
  { locr_newsize :: Newsize4
  } deriving (Show)

data LAYOUTCOMMIT4res = LAYOUTCOMMIT4res
  { locr_status :: Nfsstat4
  , locr_resok4 :: Maybe LAYOUTCOMMIT4resok
  } deriving (Show)

data LAYOUTGET4args = LAYOUTGET4args
  { loga_signal_layout_avail :: Bool
  , loga_layout_type :: Layouttype4
  , loga_iomode :: Layoutiomode4
  , loga_offset :: Offset4
  , loga_length :: Length4
  , loga_minlength :: Length4
  , loga_stateid :: Stateid4
  , loga_maxcount :: Count4
  } deriving (Show)

data LAYOUTGET4resok = LAYOUTGET4resok
  { logr_return_on_close :: Bool
  , logr_stateid :: Stateid4
  , logr_layout :: [Layout4]
  } deriving (Show)

data LAYOUTGET4res = LAYOUTGET4res
  { logr_status :: Nfsstat4
  , logr_resok4 :: Maybe LAYOUTGET4resok
  , logr_will_signal_layout_avail :: Maybe Bool
  } deriving (Show)

data LAYOUTRETURN4args = LAYOUTRETURN4args
  { lora_reclaim :: Bool
  , lora_layout_type :: Layouttype4
  , lora_iomode :: Layoutiomode4
  , lora_layoutreturn :: Layoutreturn4
  } deriving (Show)

data Layoutreturn_stateid = Layoutreturn_stateid
  { lrs_present
  , lrs_stateid :: Maybe Stateid4
  } deriving (Show)

data LAYOUTRETURN4res = LAYOUTRETURN4res
  { lorr_status :: Nfsstat4
  , lorr_stateid :: Maybe Layoutreturn_stateid
  } deriving (Show)

data Secinfo_style4 = SECINFO_STYLE4_CURRENT_FH | SECINFO_STYLE4_PARENT deriving (Eq, Show)
instance Enum Secinfo_style4 where
  fromEnum SECINFO_STYLE4_CURRENT_FH = 0
  fromEnum SECINFO_STYLE4_PARENT = 1
  toEnum 0 = SECINFO_STYLE4_CURRENT_FH
  toEnum 1 = SECINFO_STYLE4_PARENT

type SECINFO_NO_NAME4args = Secinfo_style4

type SECINFO_NO_NAME4res = SECINFO4res

data SEQUENCE4args = SEQUENCE4args
  { sa_sessionid :: Sessionid4
  , sa_sequenceid :: Sequenceid4
  , sa_slotid :: Slotid4
  , sa_highest_slotid :: Slotid4
  , sa_cachethis :: Bool
  } deriving (Show)

SEQ4_STATUS_CB_PATH_DOWN = 1
SEQ4_STATUS_CB_GSS_CONTEXTS_EXPIRING = 2
SEQ4_STATUS_CB_GSS_CONTEXTS_EXPIRED = 4
SEQ4_STATUS_EXPIRED_ALL_STATE_REVOKED = 8
SEQ4_STATUS_EXPIRED_SOME_STATE_REVOKED = 16
SEQ4_STATUS_ADMIN_STATE_REVOKED = 32
SEQ4_STATUS_RECALLABLE_STATE_REVOKED = 64
SEQ4_STATUS_LEASE_MOVED = 128
SEQ4_STATUS_RESTART_RECLAIM_NEEDED = 256
SEQ4_STATUS_CB_PATH_DOWN_SESSION = 512
SEQ4_STATUS_BACKCHANNEL_FAULT = 1024
SEQ4_STATUS_DEVID_CHANGED = 2048
SEQ4_STATUS_DEVID_DELETED = 4096
data SEQUENCE4resok = SEQUENCE4resok
  { sr_sessionid :: Sessionid4
  , sr_sequenceid :: Sequenceid4
  , sr_slotid :: Slotid4
  , sr_highest_slotid :: Slotid4
  , sr_target_highest_slotid :: Slotid4
  , sr_status_flags :: Uint32_t
  } deriving (Show)

data SEQUENCE4res = SEQUENCE4res
  { sr_status :: Nfsstat4
  , sr_resok4 :: Maybe SEQUENCE4resok
  } deriving (Show)

data Ssa_digest_input4 = Ssa_digest_input4
  { sdi_seqargs :: SEQUENCE4args
  } deriving (Show)

data SET_SSV4args = SET_SSV4args
  { ssa_ssv :: String
  , ssa_digest :: String
  } deriving (Show)

data Ssr_digest_input4 = Ssr_digest_input4
  { sdi_seqres :: SEQUENCE4res
  } deriving (Show)

data SET_SSV4resok = SET_SSV4resok
  { ssr_digest :: String
  } deriving (Show)

data SET_SSV4res = SET_SSV4res
  { ssr_status :: Nfsstat4
  , ssr_resok4 :: Maybe SET_SSV4resok
  } deriving (Show)

data TEST_STATEID4args = TEST_STATEID4args
  { ts_stateids :: [Stateid4]
  } deriving (Show)

data TEST_STATEID4resok = TEST_STATEID4resok
  { tsr_status_codes :: [Nfsstat4]
  } deriving (Show)

data TEST_STATEID4res = TEST_STATEID4res
  { tsr_status :: Nfsstat4
  , tsr_resok4 :: Maybe TEST_STATEID4resok
  } deriving (Show)

data Deleg_claim4 = Deleg_claim4
  { dc_claim :: Open_claim_type4
  , dc_delegate_type :: Maybe Open_delegation_type4
  } deriving (Show)

data WANT_DELEGATION4args = WANT_DELEGATION4args
  { wda_want :: Uint32_t
  , wda_claim :: Deleg_claim4
  } deriving (Show)

data WANT_DELEGATION4res = WANT_DELEGATION4res
  { wdr_status :: Nfsstat4
  , wdr_resok4 :: Maybe Open_delegation4
  } deriving (Show)

data DESTROY_CLIENTID4args = DESTROY_CLIENTID4args
  { dca_clientid :: Clientid4
  } deriving (Show)

data DESTROY_CLIENTID4res = DESTROY_CLIENTID4res
  { dcr_status :: Nfsstat4
  } deriving (Show)

data RECLAIM_COMPLETE4args = RECLAIM_COMPLETE4args
  { rca_one_fs :: Bool
  } deriving (Show)

data RECLAIM_COMPLETE4res = RECLAIM_COMPLETE4res
  { rcr_status :: Nfsstat4
  } deriving (Show)

data COPY4args = COPY4args
  { ca_src_stateid :: Stateid4
  , ca_dst_stateid :: Stateid4
  , ca_src_offset :: Offset4
  , ca_dst_offset :: Offset4
  , ca_count :: Length4
  , ca_consecutive :: Bool
  , ca_synchronous :: Bool
  , ca_source_server :: [Netloc4]
  } deriving (Show)

data Copy_requirements4 = Copy_requirements4
  { cr_consecutive :: Bool
  , cr_synchronous :: Bool
  } deriving (Show)

data COPY4resok = COPY4resok
  { cr_response :: Write_response4
  , cr_requirements :: Copy_requirements4
  } deriving (Show)

data COPY4res = COPY4res
  { cr_status :: Nfsstat4
  , cr_resok4 :: Maybe COPY4resok
  , cr_requirements :: Maybe Copy_requirements4
  } deriving (Show)

data COPY_NOTIFY4args = COPY_NOTIFY4args
  { cna_src_stateid :: Stateid4
  , cna_destination_server :: Netloc4
  } deriving (Show)

data COPY_NOTIFY4resok = COPY_NOTIFY4resok
  { cnr_lease_time :: Nfstime4
  , cnr_stateid :: Stateid4
  , cnr_source_server :: [Netloc4]
  } deriving (Show)

data COPY_NOTIFY4res = COPY_NOTIFY4res
  { cnr_status :: Nfsstat4
  , resok4 :: Maybe COPY_NOTIFY4resok
  } deriving (Show)

data OFFLOAD_CANCEL4args = OFFLOAD_CANCEL4args
  { oca_stateid :: Stateid4
  } deriving (Show)

data OFFLOAD_CANCEL4res = OFFLOAD_CANCEL4res
  { ocr_status :: Nfsstat4
  } deriving (Show)

data OFFLOAD_STATUS4args = OFFLOAD_STATUS4args
  { osa_stateid :: Stateid4
  } deriving (Show)

data OFFLOAD_STATUS4resok = OFFLOAD_STATUS4resok
  { osr_count :: Length4
  , osr_complete :: [Nfsstat4]
  } deriving (Show)

data OFFLOAD_STATUS4res = OFFLOAD_STATUS4res
  { osr_status :: Nfsstat4
  , osr_resok4 :: Maybe OFFLOAD_STATUS4resok
  } deriving (Show)

data ALLOCATE4args = ALLOCATE4args
  { aa_stateid :: Stateid4
  , aa_offset :: Offset4
  , aa_length :: Length4
  } deriving (Show)

data ALLOCATE4res = ALLOCATE4res
  { ar_status :: Nfsstat4
  } deriving (Show)

data DEALLOCATE4args = DEALLOCATE4args
  { da_stateid :: Stateid4
  , da_offset :: Offset4
  , da_length :: Length4
  } deriving (Show)

data DEALLOCATE4res = DEALLOCATE4res
  { dr_status :: Nfsstat4
  } deriving (Show)

data IO_ADVISE_type4 = IO_ADVISE4_NORMAL | IO_ADVISE4_SEQUENTIAL | IO_ADVISE4_SEQUENTIAL_BACKWARDS | IO_ADVISE4_RANDOM | IO_ADVISE4_WILLNEED | IO_ADVISE4_WILLNEED_OPPORTUNISTIC | IO_ADVISE4_DONTNEED | IO_ADVISE4_NOREUSE | IO_ADVISE4_READ | IO_ADVISE4_WRITE | IO_ADVISE4_INIT_PROXIMITY deriving (Eq, Show)
instance Enum IO_ADVISE_type4 where
  fromEnum IO_ADVISE4_NORMAL = 0
  fromEnum IO_ADVISE4_SEQUENTIAL = 1
  fromEnum IO_ADVISE4_SEQUENTIAL_BACKWARDS = 2
  fromEnum IO_ADVISE4_RANDOM = 3
  fromEnum IO_ADVISE4_WILLNEED = 4
  fromEnum IO_ADVISE4_WILLNEED_OPPORTUNISTIC = 5
  fromEnum IO_ADVISE4_DONTNEED = 6
  fromEnum IO_ADVISE4_NOREUSE = 7
  fromEnum IO_ADVISE4_READ = 8
  fromEnum IO_ADVISE4_WRITE = 9
  fromEnum IO_ADVISE4_INIT_PROXIMITY = 10
  toEnum 0 = IO_ADVISE4_NORMAL
  toEnum 1 = IO_ADVISE4_SEQUENTIAL
  toEnum 2 = IO_ADVISE4_SEQUENTIAL_BACKWARDS
  toEnum 3 = IO_ADVISE4_RANDOM
  toEnum 4 = IO_ADVISE4_WILLNEED
  toEnum 5 = IO_ADVISE4_WILLNEED_OPPORTUNISTIC
  toEnum 6 = IO_ADVISE4_DONTNEED
  toEnum 7 = IO_ADVISE4_NOREUSE
  toEnum 8 = IO_ADVISE4_READ
  toEnum 9 = IO_ADVISE4_WRITE
  toEnum 10 = IO_ADVISE4_INIT_PROXIMITY

data IO_ADVISE4args = IO_ADVISE4args
  { iaa_stateid :: Stateid4
  , iaa_offset :: Offset4
  , iaa_count :: Length4
  , iaa_hints :: Bitmap4
  } deriving (Show)

data IO_ADVISE4resok = IO_ADVISE4resok
  { ior_hints :: Bitmap4
  } deriving (Show)

data IO_ADVISE4res = IO_ADVISE4res
  { ior_status :: Nfsstat4
  , resok4 :: Maybe IO_ADVISE4resok
  } deriving (Show)

data Device_error4 = Device_error4
  { de_deviceid :: Deviceid4
  , de_status :: Nfsstat4
  , de_opnum :: Nfs_opnum4
  } deriving (Show)

data LAYOUTERROR4args = LAYOUTERROR4args
  { lea_offset :: Offset4
  , lea_length :: Length4
  , lea_stateid :: Stateid4
  , lea_errors :: [Device_error4]
  } deriving (Show)

data LAYOUTERROR4res = LAYOUTERROR4res
  { ler_status :: Nfsstat4
  } deriving (Show)

data Io_info4 = Io_info4
  { ii_count :: Uint64_t
  , ii_bytes :: Uint64_t
  } deriving (Show)

data LAYOUTSTATS4args = LAYOUTSTATS4args
  { lsa_offset :: Offset4
  , lsa_length :: Length4
  , lsa_stateid :: Stateid4
  , lsa_read :: Io_info4
  , lsa_write :: Io_info4
  , lsa_deviceid :: Deviceid4
  , lsa_layoutupdate :: Layoutupdate4
  } deriving (Show)

data LAYOUTSTATS4res = LAYOUTSTATS4res
  { lsr_status :: Nfsstat4
  } deriving (Show)

data READ_PLUS4args = READ_PLUS4args
  { rpa_stateid :: Stateid4
  , rpa_offset :: Offset4
  , rpa_count :: Count4
  } deriving (Show)

data Read_plus_content = Read_plus_content
  { rpc_content :: Data_content4
  , rpc_data :: Maybe Data4
  , rpc_hole :: Maybe Data_info4
  } deriving (Show)

data Read_plus_res4 = Read_plus_res4
  { rpr_eof :: Bool
  , rpr_contents :: [Read_plus_content]
  } deriving (Show)

data READ_PLUS4res = READ_PLUS4res
  { rp_status :: Nfsstat4
  , rp_resok4 :: Maybe Read_plus_res4
  } deriving (Show)

data SEEK4args = SEEK4args
  { sa_stateid :: Stateid4
  , sa_offset :: Offset4
  , sa_what :: Data_content4
  } deriving (Show)

data Seek_res4 = Seek_res4
  { sr_eof :: Bool
  , sr_offset :: Offset4
  } deriving (Show)

data SEEK4res = SEEK4res
  { sa_status :: Nfsstat4
  , resok4 :: Maybe Seek_res4
  } deriving (Show)

data WRITE_SAME4args = WRITE_SAME4args
  { wsa_stateid :: Stateid4
  , wsa_stable :: Stable_how4
  , wsa_adb :: App_data_block4
  } deriving (Show)

data WRITE_SAME4res = WRITE_SAME4res
  { wsr_status :: Nfsstat4
  , resok4 :: Maybe Write_response4
  } deriving (Show)

data Ff_device_versions4 = Ff_device_versions4
  { ffdv_version :: Uint32_t
  , ffdv_minorversion :: Uint32_t
  , ffdv_rsize :: Uint32_t
  , ffdv_wsize :: Uint32_t
  , ffdv_tightly_coupled :: Bool
  } deriving (Show)

data Ff_device_addr4 = Ff_device_addr4
  { ffda_netaddrs :: Multipath_list4
  , ffda_versions :: [Ff_device_versions4]
  } deriving (Show)

FF_FLAGS_NO_LAYOUTCOMMIT = 1
FF_FLAGS_NO_IO_THRU_MDS = 2
FF_FLAGS_NO_READ_IO = 4
type Ff_flags4 = Uint32_t

data Ff_data_server4 = Ff_data_server4
  { ffds_deviceid :: Deviceid4
  , ffds_efficiency :: Uint32_t
  , ffds_stateid :: Stateid4
  , ffds_fh_vers :: [Nfs_fh4]
  , ffds_user :: Fattr4_owner
  , ffds_group :: Fattr4_owner_group
  } deriving (Show)

data Ff_mirror4 = Ff_mirror4
  { ffm_data_servers :: [Ff_data_server4]
  } deriving (Show)

data Ff_layout4 = Ff_layout4
  { ffl_stripe_unit :: Length4
  , ffl_mirrors :: [Ff_mirror4]
  , ffl_flags :: Ff_flags4
  , ffl_stats_collect_hint :: Uint32_t
  } deriving (Show)

data Ff_ioerr4 = Ff_ioerr4
  { ffie_offset :: Offset4
  , ffie_length :: Length4
  , ffie_stateid :: Stateid4
  , ffie_errors :: [Device_error4]
  } deriving (Show)

data Ff_io_latency4 = Ff_io_latency4
  { ffil_ops_requested :: Uint64_t
  , ffil_bytes_requested :: Uint64_t
  , ffil_ops_completed :: Uint64_t
  , ffil_bytes_completed :: Uint64_t
  , ffil_bytes_not_delivered :: Uint64_t
  , ffil_total_busy_time :: Nfstime4
  , ffil_aggregate_completion_time :: Nfstime4
  } deriving (Show)

data Ff_layoutupdate4 = Ff_layoutupdate4
  { ffl_addr :: Netaddr4
  , ffl_fhandle :: Nfs_fh4
  , ffl_read :: Ff_io_latency4
  , ffl_write :: Ff_io_latency4
  , ffl_duration :: Nfstime4
  , ffl_local :: Bool
  } deriving (Show)

data Ff_iostats4 = Ff_iostats4
  { ffis_offset :: Offset4
  , ffis_length :: Length4
  , ffis_stateid :: Stateid4
  , ffis_read :: Io_info4
  , ffis_write :: Io_info4
  , ffis_deviceid :: Deviceid4
  , ffis_layoutupdate :: Ff_layoutupdate4
  } deriving (Show)

data Ff_layoutreturn4 = Ff_layoutreturn4
  { fflr_ioerr_report :: [Ff_ioerr4]
  , fflr_iostats_report :: [Ff_iostats4]
  } deriving (Show)

data Ff_mirrors_hint = Ff_mirrors_hint
  { ffmc_valid
  , ffmc_mirrors :: Maybe Uint32_t
  } deriving (Show)

data Ff_layouthint4 = Ff_layouthint4
  { fflh_mirrors_hint :: Ff_mirrors_hint
  } deriving (Show)

data Ff_cb_recall_any_mask = FF_RCA4_TYPE_MASK_READ | FF_RCA4_TYPE_MASK_RW deriving (Eq, Show)
instance Enum Ff_cb_recall_any_mask where
  fromEnum FF_RCA4_TYPE_MASK_READ = -2
  fromEnum FF_RCA4_TYPE_MASK_RW = -1
  toEnum -2 = FF_RCA4_TYPE_MASK_READ
  toEnum -1 = FF_RCA4_TYPE_MASK_RW

data Nfs_argop4 = Nfs_argop4
  { argop :: Nfs_opnum4
  , opaccess :: Maybe ACCESS4args
  , opclose :: Maybe CLOSE4args
  , opcommit :: Maybe COMMIT4args
  , opcreate :: Maybe CREATE4args
  , opdelegpurge :: Maybe DELEGPURGE4args
  , opdelegreturn :: Maybe DELEGRETURN4args
  , opgetattr :: Maybe GETATTR4args
  , oplink :: Maybe LINK4args
  , oplock :: Maybe LOCK4args
  , oplockt :: Maybe LOCKT4args
  , oplocku :: Maybe LOCKU4args
  , oplookup :: Maybe LOOKUP4args
  , opnverify :: Maybe NVERIFY4args
  , opopen :: Maybe OPEN4args
  , opopenattr :: Maybe OPENATTR4args
  , opopen_confirm :: Maybe OPEN_CONFIRM4args
  , opopen_downgrade :: Maybe OPEN_DOWNGRADE4args
  , opputfh :: Maybe PUTFH4args
  , opread :: Maybe READ4args
  , opreaddir :: Maybe READDIR4args
  , opremove :: Maybe REMOVE4args
  , oprename :: Maybe RENAME4args
  , oprenew :: Maybe RENEW4args
  , opsecinfo :: Maybe SECINFO4args
  , opsetattr :: Maybe SETATTR4args
  , opsetclientid :: Maybe SETCLIENTID4args
  , opsetclientid_confirm :: Maybe SETCLIENTID_CONFIRM4args
  , opverify :: Maybe VERIFY4args
  , opwrite :: Maybe WRITE4args
  , oprelease_lockowner :: Maybe RELEASE_LOCKOWNER4args
  , opbackchannel_ctl :: Maybe BACKCHANNEL_CTL4args
  , opbind_conn_to_session :: Maybe BIND_CONN_TO_SESSION4args
  , opexchange_id :: Maybe EXCHANGE_ID4args
  , opcreate_session :: Maybe CREATE_SESSION4args
  , opdestroy_session :: Maybe DESTROY_SESSION4args
  , opfree_stateid :: Maybe FREE_STATEID4args
  , opget_dir_delegation :: Maybe GET_DIR_DELEGATION4args
  , opgetdeviceinfo :: Maybe GETDEVICEINFO4args
  , opgetdevicelist :: Maybe GETDEVICELIST4args
  , oplayoutcommit :: Maybe LAYOUTCOMMIT4args
  , oplayoutget :: Maybe LAYOUTGET4args
  , oplayoutreturn :: Maybe LAYOUTRETURN4args
  , opsecinfo_no_name :: Maybe SECINFO_NO_NAME4args
  , opsequence :: Maybe SEQUENCE4args
  , opset_ssv :: Maybe SET_SSV4args
  , optest_stateid :: Maybe TEST_STATEID4args
  , opwant_delegation :: Maybe WANT_DELEGATION4args
  , opdestroy_clientid :: Maybe DESTROY_CLIENTID4args
  , opreclaim_complete :: Maybe RECLAIM_COMPLETE4args
  , opallocate :: Maybe ALLOCATE4args
  , opcopy :: Maybe COPY4args
  , opoffload_notify :: Maybe COPY_NOTIFY4args
  , opdeallocate :: Maybe DEALLOCATE4args
  , opio_advise :: Maybe IO_ADVISE4args
  , oplayouterror :: Maybe LAYOUTERROR4args
  , oplayoutstats :: Maybe LAYOUTSTATS4args
  , opoffload_cancel :: Maybe OFFLOAD_CANCEL4args
  , opoffload_status :: Maybe OFFLOAD_STATUS4args
  , opread_plus :: Maybe READ_PLUS4args
  , opseek :: Maybe SEEK4args
  , opwrite_same :: Maybe WRITE_SAME4args
  , opclone :: Maybe CLONE4args
  } deriving (Show)

data Nfs_resop4 = Nfs_resop4
  { resop :: Nfs_opnum4
  , opaccess :: Maybe ACCESS4res
  , opclose :: Maybe CLOSE4res
  , opcommit :: Maybe COMMIT4res
  , opcreate :: Maybe CREATE4res
  , opdelegpurge :: Maybe DELEGPURGE4res
  , opdelegreturn :: Maybe DELEGRETURN4res
  , opgetattr :: Maybe GETATTR4res
  , opgetfh :: Maybe GETFH4res
  , oplink :: Maybe LINK4res
  , oplock :: Maybe LOCK4res
  , oplockt :: Maybe LOCKT4res
  , oplocku :: Maybe LOCKU4res
  , oplookup :: Maybe LOOKUP4res
  , oplookupp :: Maybe LOOKUPP4res
  , opnverify :: Maybe NVERIFY4res
  , opopen :: Maybe OPEN4res
  , opopenattr :: Maybe OPENATTR4res
  , opopen_confirm :: Maybe OPEN_CONFIRM4res
  , opopen_downgrade :: Maybe OPEN_DOWNGRADE4res
  , opputfh :: Maybe PUTFH4res
  , opputpubfh :: Maybe PUTPUBFH4res
  , opputrootfh :: Maybe PUTROOTFH4res
  , opread :: Maybe READ4res
  , opreaddir :: Maybe READDIR4res
  , opreadlink :: Maybe READLINK4res
  , opremove :: Maybe REMOVE4res
  , oprename :: Maybe RENAME4res
  , oprenew :: Maybe RENEW4res
  , oprestorefh :: Maybe RESTOREFH4res
  , opsavefh :: Maybe SAVEFH4res
  , opsecinfo :: Maybe SECINFO4res
  , opsetattr :: Maybe SETATTR4res
  , opsetclientid :: Maybe SETCLIENTID4res
  , opsetclientid_confirm :: Maybe SETCLIENTID_CONFIRM4res
  , opverify :: Maybe VERIFY4res
  , opwrite :: Maybe WRITE4res
  , oprelease_lockowner :: Maybe RELEASE_LOCKOWNER4res
  , opbackchannel_ctl :: Maybe BACKCHANNEL_CTL4res
  , opbind_conn_to_session :: Maybe BIND_CONN_TO_SESSION4res
  , opexchange_id :: Maybe EXCHANGE_ID4res
  , opcreate_session :: Maybe CREATE_SESSION4res
  , opdestroy_session :: Maybe DESTROY_SESSION4res
  , opfree_stateid :: Maybe FREE_STATEID4res
  , opget_dir_delegation :: Maybe GET_DIR_DELEGATION4res
  , opgetdeviceinfo :: Maybe GETDEVICEINFO4res
  , opgetdevicelist :: Maybe GETDEVICELIST4res
  , oplayoutcommit :: Maybe LAYOUTCOMMIT4res
  , oplayoutget :: Maybe LAYOUTGET4res
  , oplayoutreturn :: Maybe LAYOUTRETURN4res
  , opsecinfo_no_name :: Maybe SECINFO_NO_NAME4res
  , opsequence :: Maybe SEQUENCE4res
  , opset_ssv :: Maybe SET_SSV4res
  , optest_stateid :: Maybe TEST_STATEID4res
  , opwant_delegation :: Maybe WANT_DELEGATION4res
  , opdestroy_clientid :: Maybe DESTROY_CLIENTID4res
  , opreclaim_complete :: Maybe RECLAIM_COMPLETE4res
  , opallocate :: Maybe ALLOCATE4res
  , opcopy :: Maybe COPY4res
  , opcopy_notify :: Maybe COPY_NOTIFY4res
  , opdeallocate :: Maybe DEALLOCATE4res
  , opio_advise :: Maybe IO_ADVISE4res
  , oplayouterror :: Maybe LAYOUTERROR4res
  , oplayputstats :: Maybe LAYOUTSTATS4res
  , opoffload_cancel :: Maybe OFFLOAD_CANCEL4res
  , opoffload_status :: Maybe OFFLOAD_STATUS4res
  , opread_plus :: Maybe READ_PLUS4res
  , opseek :: Maybe SEEK4res
  , opwrite_same :: Maybe WRITE_SAME4res
  , opclone :: Maybe CLONE4res
  , opillegal :: Maybe ILLEGAL4res
  } deriving (Show)

data COMPOUND4args = COMPOUND4args
  { tag :: Utf8str_cs
  , minorversion :: Uint32_t
  , argarray :: [Nfs_argop4]
  } deriving (Show)

data COMPOUND4res = COMPOUND4res
  { status :: Nfsstat4
  , tag :: Utf8str_cs
  , resarray :: [Nfs_resop4]
  } deriving (Show)

data CB_GETATTR4args = CB_GETATTR4args
  { fh :: Nfs_fh4
  , attr_request :: Bitmap4
  } deriving (Show)

data CB_GETATTR4resok = CB_GETATTR4resok
  { obj_attributes :: Fattr4
  } deriving (Show)

data CB_GETATTR4res = CB_GETATTR4res
  { status :: Nfsstat4
  , resok4 :: Maybe CB_GETATTR4resok
  } deriving (Show)

data CB_RECALL4args = CB_RECALL4args
  { stateid :: Stateid4
  , truncate :: Bool
  , fh :: Nfs_fh4
  } deriving (Show)

data CB_RECALL4res = CB_RECALL4res
  { status :: Nfsstat4
  } deriving (Show)

data CB_ILLEGAL4res = CB_ILLEGAL4res
  { status :: Nfsstat4
  } deriving (Show)

data Layoutrecall_type4 = LAYOUTRECALL4_FILE | LAYOUTRECALL4_FSID | LAYOUTRECALL4_ALL deriving (Eq, Show)
instance Enum Layoutrecall_type4 where
  fromEnum LAYOUTRECALL4_FILE = LAYOUT4_RET_REC_FILE
  fromEnum LAYOUTRECALL4_FSID = LAYOUT4_RET_REC_FSID
  fromEnum LAYOUTRECALL4_ALL = LAYOUT4_RET_REC_ALL
  toEnum LAYOUT4_RET_REC_FILE = LAYOUTRECALL4_FILE
  toEnum LAYOUT4_RET_REC_FSID = LAYOUTRECALL4_FSID
  toEnum LAYOUT4_RET_REC_ALL = LAYOUTRECALL4_ALL

data Layoutrecall_file4 = Layoutrecall_file4
  { lor_fh :: Nfs_fh4
  , lor_offset :: Offset4
  , lor_length :: Length4
  , lor_stateid :: Stateid4
  } deriving (Show)

data Layoutrecall4 = Layoutrecall4
  { lor_recalltype :: Layoutrecall_type4
  , lor_layout :: Maybe Layoutrecall_file4
  , lor_fsid :: Maybe Fsid4
  } deriving (Show)

data CB_LAYOUTRECALL4args = CB_LAYOUTRECALL4args
  { clora_type :: Layouttype4
  , clora_iomode :: Layoutiomode4
  , clora_changed :: Bool
  , clora_recall :: Layoutrecall4
  } deriving (Show)

data CB_LAYOUTRECALL4res = CB_LAYOUTRECALL4res
  { clorr_status :: Nfsstat4
  } deriving (Show)

data Notify_type4 = NOTIFY4_CHANGE_CHILD_ATTRS | NOTIFY4_CHANGE_DIR_ATTRS | NOTIFY4_REMOVE_ENTRY | NOTIFY4_ADD_ENTRY | NOTIFY4_RENAME_ENTRY | NOTIFY4_CHANGE_COOKIE_VERIFIER deriving (Eq, Show)
instance Enum Notify_type4 where
  fromEnum NOTIFY4_CHANGE_CHILD_ATTRS = 0
  fromEnum NOTIFY4_CHANGE_DIR_ATTRS = 1
  fromEnum NOTIFY4_REMOVE_ENTRY = 2
  fromEnum NOTIFY4_ADD_ENTRY = 3
  fromEnum NOTIFY4_RENAME_ENTRY = 4
  fromEnum NOTIFY4_CHANGE_COOKIE_VERIFIER = 5
  toEnum 0 = NOTIFY4_CHANGE_CHILD_ATTRS
  toEnum 1 = NOTIFY4_CHANGE_DIR_ATTRS
  toEnum 2 = NOTIFY4_REMOVE_ENTRY
  toEnum 3 = NOTIFY4_ADD_ENTRY
  toEnum 4 = NOTIFY4_RENAME_ENTRY
  toEnum 5 = NOTIFY4_CHANGE_COOKIE_VERIFIER

data Notify_entry4 = Notify_entry4
  { ne_file :: Component4
  , ne_attrs :: Fattr4
  } deriving (Show)

data Prev_entry4 = Prev_entry4
  { pe_prev_entry :: Notify_entry4
  , pe_prev_entry_cookie :: Nfs_cookie4
  } deriving (Show)

data Notify_remove4 = Notify_remove4
  { nrm_old_entry :: Notify_entry4
  , nrm_old_entry_cookie :: Nfs_cookie4
  } deriving (Show)

data Notify_add4 = Notify_add4
  { nad_old_entry :: [Notify_remove4]
  , nad_new_entry :: Notify_entry4
  , nad_new_entry_cookie :: [Nfs_cookie4]
  , nad_prev_entry :: [Prev_entry4]
  , nad_last_entry :: Bool
  } deriving (Show)

data Notify_attr4 = Notify_attr4
  { na_changed_entry :: Notify_entry4
  } deriving (Show)

data Notify_rename4 = Notify_rename4
  { nrn_old_entry :: Notify_remove4
  , nrn_new_entry :: Notify_add4
  } deriving (Show)

data Notify_verifier4 = Notify_verifier4
  { nv_old_cookieverf :: Verifier4
  , nv_new_cookieverf :: Verifier4
  } deriving (Show)

type Notifylist4 = String

data Notify4 = Notify4
  { notify_mask :: Bitmap4
  , notify_vals :: Notifylist4
  } deriving (Show)

data CB_NOTIFY4args = CB_NOTIFY4args
  { cna_stateid :: Stateid4
  , cna_fh :: Nfs_fh4
  , cna_changes :: [Notify4]
  } deriving (Show)

data CB_NOTIFY4res = CB_NOTIFY4res
  { cnr_status :: Nfsstat4
  } deriving (Show)

data CB_PUSH_DELEG4args = CB_PUSH_DELEG4args
  { cpda_fh :: Nfs_fh4
  , cpda_delegation :: Open_delegation4
  } deriving (Show)

data CB_PUSH_DELEG4res = CB_PUSH_DELEG4res
  { cpdr_status :: Nfsstat4
  } deriving (Show)

RCA4_TYPE_MASK_RDATA_DLG = 0
RCA4_TYPE_MASK_WDATA_DLG = 1
RCA4_TYPE_MASK_DIR_DLG = 2
RCA4_TYPE_MASK_FILE_LAYOUT = 3
RCA4_TYPE_MASK_BLK_LAYOUT = 4
RCA4_TYPE_MASK_OBJ_LAYOUT_MIN = 8
RCA4_TYPE_MASK_OBJ_LAYOUT_MAX = 9
RCA4_TYPE_MASK_OTHER_LAYOUT_MIN = 12
RCA4_TYPE_MASK_OTHER_LAYOUT_MAX = 15
data CB_RECALL_ANY4args = CB_RECALL_ANY4args
  { craa_objects_to_keep :: Uint32_t
  , craa_type_mask :: Bitmap4
  } deriving (Show)

data CB_RECALL_ANY4res = CB_RECALL_ANY4res
  { crar_status :: Nfsstat4
  } deriving (Show)

type CB_RECALLABLE_OBJ_AVAIL4args = CB_RECALL_ANY4args

data CB_RECALLABLE_OBJ_AVAIL4res = CB_RECALLABLE_OBJ_AVAIL4res
  { croa_status :: Nfsstat4
  } deriving (Show)

data CB_RECALL_SLOT4args = CB_RECALL_SLOT4args
  { rsa_target_highest_slotid :: Slotid4
  } deriving (Show)

data CB_RECALL_SLOT4res = CB_RECALL_SLOT4res
  { rsr_status :: Nfsstat4
  } deriving (Show)

data Referring_call4 = Referring_call4
  { rc_sequenceid :: Sequenceid4
  , rc_slotid :: Slotid4
  } deriving (Show)

data Referring_call_list4 = Referring_call_list4
  { rcl_sessionid :: Sessionid4
  , rcl_referring_calls :: [Referring_call4]
  } deriving (Show)

data CB_SEQUENCE4args = CB_SEQUENCE4args
  { csa_sessionid :: Sessionid4
  , csa_sequenceid :: Sequenceid4
  , csa_slotid :: Slotid4
  , csa_highest_slotid :: Slotid4
  , csa_cachethis :: Bool
  , csa_referring_call_lists :: [Referring_call_list4]
  } deriving (Show)

data CB_SEQUENCE4resok = CB_SEQUENCE4resok
  { csr_sessionid :: Sessionid4
  , csr_sequenceid :: Sequenceid4
  , csr_slotid :: Slotid4
  , csr_highest_slotid :: Slotid4
  , csr_target_highest_slotid :: Slotid4
  } deriving (Show)

data CB_SEQUENCE4res = CB_SEQUENCE4res
  { csr_status :: Nfsstat4
  , csr_resok4 :: Maybe CB_SEQUENCE4resok
  } deriving (Show)

data CB_WANTS_CANCELLED4args = CB_WANTS_CANCELLED4args
  { cwca_contended_wants_cancelled :: Bool
  , cwca_resourced_wants_cancelled :: Bool
  } deriving (Show)

data CB_WANTS_CANCELLED4res = CB_WANTS_CANCELLED4res
  { cwcr_status :: Nfsstat4
  } deriving (Show)

data CB_NOTIFY_LOCK4args = CB_NOTIFY_LOCK4args
  { cnla_fh :: Nfs_fh4
  , cnla_lock_owner :: Lock_owner4
  } deriving (Show)

data CB_NOTIFY_LOCK4res = CB_NOTIFY_LOCK4res
  { cnlr_status :: Nfsstat4
  } deriving (Show)

data Notify_deviceid_type4 = NOTIFY_DEVICEID4_CHANGE | NOTIFY_DEVICEID4_DELETE deriving (Eq, Show)
instance Enum Notify_deviceid_type4 where
  fromEnum NOTIFY_DEVICEID4_CHANGE = 1
  fromEnum NOTIFY_DEVICEID4_DELETE = 2
  toEnum 1 = NOTIFY_DEVICEID4_CHANGE
  toEnum 2 = NOTIFY_DEVICEID4_DELETE

data Notify_deviceid_delete4 = Notify_deviceid_delete4
  { ndd_layouttype :: Layouttype4
  , ndd_deviceid :: Deviceid4
  } deriving (Show)

data Notify_deviceid_change4 = Notify_deviceid_change4
  { ndc_layouttype :: Layouttype4
  , ndc_deviceid :: Deviceid4
  , ndc_immediate :: Bool
  } deriving (Show)

data CB_NOTIFY_DEVICEID4args = CB_NOTIFY_DEVICEID4args
  { cnda_changes :: [Notify4]
  } deriving (Show)

data CB_NOTIFY_DEVICEID4res = CB_NOTIFY_DEVICEID4res
  { cndr_status :: Nfsstat4
  } deriving (Show)

data Offload_info4 = Offload_info4
  { coa_status :: Nfsstat4
  , coa_resok4 :: Maybe Write_response4
  , coa_bytes_copied :: Maybe Length4
  } deriving (Show)

data CB_OFFLOAD4args = CB_OFFLOAD4args
  { coa_fh :: Nfs_fh4
  , coa_stateid :: Stateid4
  , coa_offload_info :: Offload_info4
  } deriving (Show)

data CB_OFFLOAD4res = CB_OFFLOAD4res
  { cor_status :: Nfsstat4
  } deriving (Show)

data Nfs_cb_opnum4 = OP_CB_GETATTR | OP_CB_RECALL | OP_CB_LAYOUTRECALL | OP_CB_NOTIFY | OP_CB_PUSH_DELEG | OP_CB_RECALL_ANY | OP_CB_RECALLABLE_OBJ_AVAIL | OP_CB_RECALL_SLOT | OP_CB_SEQUENCE | OP_CB_WANTS_CANCELLED | OP_CB_NOTIFY_LOCK | OP_CB_NOTIFY_DEVICEID | OP_CB_OFFLOAD | OP_CB_ILLEGAL deriving (Eq, Show)
instance Enum Nfs_cb_opnum4 where
  fromEnum OP_CB_GETATTR = 3
  fromEnum OP_CB_RECALL = 4
  fromEnum OP_CB_LAYOUTRECALL = 5
  fromEnum OP_CB_NOTIFY = 6
  fromEnum OP_CB_PUSH_DELEG = 7
  fromEnum OP_CB_RECALL_ANY = 8
  fromEnum OP_CB_RECALLABLE_OBJ_AVAIL = 9
  fromEnum OP_CB_RECALL_SLOT = 10
  fromEnum OP_CB_SEQUENCE = 11
  fromEnum OP_CB_WANTS_CANCELLED = 12
  fromEnum OP_CB_NOTIFY_LOCK = 13
  fromEnum OP_CB_NOTIFY_DEVICEID = 14
  fromEnum OP_CB_OFFLOAD = 15
  fromEnum OP_CB_ILLEGAL = 10044
  toEnum 3 = OP_CB_GETATTR
  toEnum 4 = OP_CB_RECALL
  toEnum 5 = OP_CB_LAYOUTRECALL
  toEnum 6 = OP_CB_NOTIFY
  toEnum 7 = OP_CB_PUSH_DELEG
  toEnum 8 = OP_CB_RECALL_ANY
  toEnum 9 = OP_CB_RECALLABLE_OBJ_AVAIL
  toEnum 10 = OP_CB_RECALL_SLOT
  toEnum 11 = OP_CB_SEQUENCE
  toEnum 12 = OP_CB_WANTS_CANCELLED
  toEnum 13 = OP_CB_NOTIFY_LOCK
  toEnum 14 = OP_CB_NOTIFY_DEVICEID
  toEnum 15 = OP_CB_OFFLOAD
  toEnum 10044 = OP_CB_ILLEGAL

data Nfs_cb_argop4 = Nfs_cb_argop4
  { argop :: Nfs_opnum4
  , opcbgetattr :: Maybe CB_GETATTR4args
  , opcbrecall :: Maybe CB_RECALL4args
  , opcblayoutrecall :: Maybe CB_LAYOUTRECALL4args
  , opcbnotify :: Maybe CB_NOTIFY4args
  , opcbpush_deleg :: Maybe CB_PUSH_DELEG4args
  , opcbrecall_any :: Maybe CB_RECALL_ANY4args
  , opcbrecallable_obj_avail :: Maybe CB_RECALLABLE_OBJ_AVAIL4args
  , opcbrecall_slot :: Maybe CB_RECALL_SLOT4args
  , opcbsequence :: Maybe CB_SEQUENCE4args
  , opcbwants_cancelled :: Maybe CB_WANTS_CANCELLED4args
  , opcbnotify_lock :: Maybe CB_NOTIFY_LOCK4args
  , opcbnotify_deviceid :: Maybe CB_NOTIFY_DEVICEID4args
  , opcboffload :: Maybe CB_OFFLOAD4args
  } deriving (Show)

data Nfs_cb_resop4 = Nfs_cb_resop4
  { resop :: Nfs_opnum4
  , opcbgetattr :: Maybe CB_GETATTR4res
  , opcbrecall :: Maybe CB_RECALL4res
  , opcblayoutrecall :: Maybe CB_LAYOUTRECALL4res
  , opcbnotify :: Maybe CB_NOTIFY4res
  , opcbpush_deleg :: Maybe CB_PUSH_DELEG4res
  , opcbrecall_any :: Maybe CB_RECALL_ANY4res
  , opcbrecallable_obj_avail :: Maybe CB_RECALLABLE_OBJ_AVAIL4res
  , opcbrecall_slot :: Maybe CB_RECALL_SLOT4res
  , opcbsequence :: Maybe CB_SEQUENCE4res
  , opcbwants_cancelled :: Maybe CB_WANTS_CANCELLED4res
  , opcbnotify_lock :: Maybe CB_NOTIFY_LOCK4res
  , opcbnotify_deviceid :: Maybe CB_NOTIFY_DEVICEID4res
  , opcboffload :: Maybe CB_OFFLOAD4res
  , opcbillegal :: Maybe CB_ILLEGAL4res
  } deriving (Show)

data CB_COMPOUND4args = CB_COMPOUND4args
  { tag :: Utf8str_cs
  , minorversion :: Uint32_t
  , callback_ident :: Uint32_t
  , argarray :: [Nfs_cb_argop4]
  } deriving (Show)

data CB_COMPOUND4res = CB_COMPOUND4res
  { status :: Nfsstat4
  , tag :: Utf8str_cs
  , resarray :: [Nfs_cb_resop4]
  } deriving (Show)

--------------------------
{-
stamp :: Int32
, machinename :: String
, uid :: Int32
, gid :: Int32
, gids :: [Int32]
-}

packAuthsys_parms :: Authsys_parms -> Put
packAuthsys_parms (Authsys_parms s m u g gs) = do
  putInt32le 0xaa
  putInt32le 0xbb
  putInt32le 0xcc
