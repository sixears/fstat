{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

-- we export the c'tor for FStat so that clients can make their own for
-- mock purposes
module FStat
  ( FileType(..), FStat( FStat )
  , access, dev_id, dev_major, dev_minor, ftype, group, inode, mkfstat, nlinks
  , owner, perms, sampleLStat0, size, specialDeviceID, status_change
  )
where

import Prelude  ( div, error, fromIntegral, rem )

-- base --------------------------------

import Data.Bits           ( (.&.) )
import Data.Bool           ( Bool( True ) )
import Data.Eq             ( Eq )
import Data.Function       ( ($) )
import Data.Word           ( Word8, Word16, Word64 )
import System.Posix.Types  ( CUid, CGid, DeviceID, FileID
                           , FileMode, GroupID, LinkCount, UserID )
import Text.Show           ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- text --------------------------------

import Data.Text  ( unlines )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time.Clock        ( UTCTime )
import Data.Time.Clock.POSIX  ( POSIXTime, posixSecondsToUTCTime )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus
                           , accessTimeHiRes, deviceID, fileGroup, fileID
                           , fileMode, fileOwner, fileSize, fileTypeModes
                           , intersectFileModes, isBlockDevice
                           , isCharacterDevice, isDirectory, isNamedPipe
                           , isRegularFile, isSocket, isSymbolicLink, linkCount
                           , modificationTimeHiRes, specialDeviceID
                           , statusChangeTimeHiRes
                           )

--------------------------------------------------------------------------------

data FileType = BlockDevice | CharacterDevice | NamedPipe | RegularFile
              | Directory   | SymbolicLink    | Socket
  deriving (Eq,Show)

fileType ∷ FileStatus → FileType
fileType (isBlockDevice     → True) = BlockDevice
fileType (isCharacterDevice → True) = CharacterDevice
fileType (isNamedPipe       → True) = NamedPipe
fileType (isRegularFile     → True) = RegularFile
fileType (isDirectory       → True) = Directory
fileType (isSymbolicLink    → True) = SymbolicLink
fileType (isSocket          → True) = Socket
fileType s = error $ [fmt|unknown file type: %w|]
                       (fileMode s `intersectFileModes` fileTypeModes)

data FStat = FStat { _ftype          ∷ FileType
                   , _dev_id         ∷ DeviceID
                   , _special_dev_id ∷ DeviceID
                   , _inode          ∷ FileID
                   , _fmode          ∷ FileMode
                   , _nlinks         ∷ LinkCount
                   , _owner          ∷ UserID
                   , _group          ∷ GroupID
                   , _size           ∷ Word64
                   , _access         ∷ POSIXTime
                   , _modification   ∷ POSIXTime
                   , _status_change  ∷ POSIXTime
                   }
  deriving (Eq,Show)

dev_id ∷ FStat → DeviceID
dev_id = _dev_id

ftype ∷ FStat → FileType
ftype = _ftype

inode ∷ FStat → FileID
inode = _inode

nlinks ∷ FStat → LinkCount
nlinks = _nlinks

special_dev_id ∷ FStat → DeviceID
special_dev_id = _special_dev_id

dev_major ∷ FStat → Word8
dev_major s = fromIntegral $ special_dev_id s `div` 256

dev_minor ∷ FStat → Word8
dev_minor s = fromIntegral $ special_dev_id s `rem` 256

size ∷ FStat → Word64
size = _size

perms ∷ FStat → Word16
perms s = fromIntegral $ _fmode s .&. 0o7777

owner ∷ FStat → CUid
owner = _owner

group ∷ FStat → CGid
group = _group

access ∷ FStat → UTCTime
access = posixSecondsToUTCTime ∘ _access

modification ∷ FStat → UTCTime
modification = posixSecondsToUTCTime ∘ _modification

status_change ∷ FStat → UTCTime
status_change = posixSecondsToUTCTime ∘ _status_change

{- | Rough 'n' ready stat output.  Feel free to improve this as time allows,
     it's just a textual representation, nothing should be parsing it. -}
instance Printable FStat where
  print s = P.text $
    unlines [ [fmt|%-15w\tSize: %d|] (ftype s) (size s)
            , [fmt|Maj/Min: %d,%d\tDevice: %w\tInode: %d\tLinks: %d|]
                 (dev_major s) (dev_minor s) (dev_id s) (inode s) (nlinks s)
            , [fmt|Access: %04o\tUid: %d\tGid: %d|]
                         (perms s) (owner s) (group s)
            , [fmt|Access: %Z|] (access s)
            , [fmt|Modify: %Z|] (modification s)
            , [fmt|Change: %Z|] (status_change s)
            ]

mkfstat ∷ FileStatus → FStat
mkfstat s = FStat { _ftype          = fileType s
                  , _dev_id         = deviceID s
                  , _special_dev_id = specialDeviceID s
                  , _inode          = fileID s
                  , _fmode          = fileMode s
                  , _nlinks         = linkCount s
                  , _owner          = fileOwner s
                  , _group          = fileGroup s
                  , _size           = fromIntegral $ fileSize s
                  , _access         = accessTimeHiRes s
                  , _modification   = modificationTimeHiRes s
                  , _status_change  = statusChangeTimeHiRes s
                  }

{-| An example FStat, for testing & mocking, etc.  -}
sampleLStat0 ∷ FStat
sampleLStat0 =
  FStat SymbolicLink 5 0 224 00120777 1 0 0 15 94694400 94694400 94694400


-- that's all, folks! ----------------------------------------------------------
