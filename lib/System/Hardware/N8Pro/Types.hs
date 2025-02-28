module System.Hardware.N8Pro.Types (
	module System.Hardware.N8Pro.Types,
	module Data.Int,
	module Data.Word,
	ByteString,
	ExitCode(..),
	UTCTime(..),
	) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Data.Time.Clock
import System.Exit

-- | There are many commands where the official implementation always issues a
-- 'GetStatus' afterwards. These commands are marked with \"S\".
data Command a where
	-- | Check if everything is still hunky-dory. edlink inserts this command
	-- after a bunch of other commands.
	GetStatus :: Command ExitCode
	GetMode :: Command N8Mode
	-- | Reset the N8 and enter 'Service' mode. You will have to reconnect
	-- after issuing this command (i.e. close and re-open the 'Handle'), and it
	-- may take a bit for the N8 to boot up and start accepting connections
	-- again.
	Reset :: Command ()
	-- TODO: the N8 doesn't seem to send any bytes back when we make this request
	GetVDC :: Command VDC
	-- TODO: have I broken the N8? this isn't returning bytes, even though it
	-- used to. maybe popping the battery will help?
	GetTime :: Command UTCTime
	-- TODO: what's the real supported range?
	-- | Only years in the range [2000, 2100) are supported. (This is checked,
	-- and will result in a 'ParseError'.)
	SetTime :: UTCTime -> Command ()
	-- TODO: this seems to be very spotty; e.g. reading from addrFlashMenu
	-- works up to size 12 and then it starts returning chunks smaller than
	-- requested; addrFlashCore works up to size 63; probably others are odd
	-- too, though I didn't test them
	-- | See also 'addrFlashMenu', 'addrFlashFPGA', and 'addrFlashCore'.
	ReadFlash :: Range -> Command ByteString
	-- TODO: completely untested, because I'm a scaredy cat
	-- | S
	WriteFlash :: Address -> ByteString -> Command ()
	-- TODO: seems to always return 0 bytes??
	ReadMemory :: Range -> Command ByteString
	-- TODO: the rest of these are untested
	-- TODO: WriteMemory :: Address -> ByteString -> Command ()
	-- | S
	SetMemory :: Range -> Word8 -> Command ()
	TestMemory :: Range -> Word8 -> Command Bool
	ChecksumMemory :: Range -> Command Checksum
	-- TODO: CMD_FPG_*
	CalibrateClock :: UTCTime -> CalibrationQuery -> Command Int32
	UpdateExecution :: Address -> Checksum -> Command ()
	-- | S
	InitDisk :: Command ()
	-- | The 'Word8' is how many 512-byte blocks to read.
	ReadDisk :: Address -> Word8 -> Command ByteString
	-- | S
	OpenDirectory :: ByteString -> Command ()
	-- | The 'Word16' is the maximum file name length you're willing to accept.
	ReadDirectory :: Word16 -> Command FileInfo
	-- | S The 'Word8' affects sorting, though it's not documented how.
	-- Presumably a 'Bool' would suffice, but I'm not certain.
	LoadDirectory :: Word8 -> ByteString -> Command ()
	SizeDirectory :: Command Word16
	-- | S See also 'modeDefault' etc.
	OpenFile :: FileMode -> ByteString -> Command ()
	-- | S
	ReadFile :: Length -> Command ByteString
	-- | S
	WriteFile :: ByteString -> Command ()
	-- | S
	CloseFile :: Command ()
	-- | S
	SetFilePointer :: Address -> Command ()
	InfoFile :: ByteString -> Command FileInfo
	ChecksumFile :: Length -> Command Checksum
	-- note to self for when we start cataloguing/documenting which commands
	-- should be followed by a GetStatus: this one already has the following
	-- GetStatus baked in, so we can interpret error code 8 specially
	-- | Returns whether the directory already existed.
	MakeDirectory :: ByteString -> Command Bool
	-- | S
	DeleteFile :: ByteString -> Command ()
	-- note to self for when we start cataloguing/documenting which commands
	-- should be followed by a GetStatus: this one already has the following
	-- GetStatus baked in, so we can interpret error code 0x88 specially
	-- | Returns whether the "current core matches the recovery copy", whatever
	-- that means. The official implementation always uses @'ReadFlash'
	-- ('Range' addrFlashCore 4)@ (interpreted as little-endian) as the
	-- checksum.
	RecoverUSB :: Checksum -> Command Bool
	-- | Reset the N8 and enter 'Application' mode. You will have to reconnect
	-- after issuing this command (i.e. close and re-open the 'Handle'), and it
	-- may take a bit for the N8 to boot up and start accepting connections
	-- again.
	RunApplication :: Command ()

deriving instance Eq (Command a)
deriving instance Ord (Command a)
deriving instance Show (Command a)

data Packet = Packet { fixed :: ByteString, variable :: ByteString }
	deriving (Eq, Ord, Read, Show)

data Parser a = Yield a | Failure String | Receive Int (ByteString -> Parser a)
	deriving Functor

instance Applicative Parser where
	pure = Yield
	(<*>) = ap

instance Monad Parser where
	m >>= f = case m of
		Yield a -> f a
		Failure err -> Failure err
		Receive n f' -> Receive n (f' >=> f)

newtype TransmissionError = TransmissionError { variableSizeTransmitted :: Int }
	deriving (Eq, Ord, Read, Show)

data ParseError = ParseError
	{ errorDescription :: String
	, chunksReceived :: [ByteString]
	} deriving (Eq, Ord, Read, Show)

data CommandError a = CommandError
	{ problemCommand :: Command a
	, problemPacket :: Packet
	, details :: Either TransmissionError ParseError
	} deriving (Eq, Ord, Show)

type Address = Word32
type Length = Word32
type Checksum = Word32 -- ^ CRC
type FileMode = Word8

data Range = Range { base :: Address , size :: Length }
	deriving (Eq, Ord, Read, Show)

-- | 'Service' mode means the N8 is powered only via USB; I guess 'Application'
-- mode therefore probably means it's in an NES.
data N8Mode = Service | Application deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | I haven't the faintest idea. Maybe a battery indicator?
data VDC = VDC { v50, v25, v12, vbt :: Word16 } deriving (Eq, Ord, Read, Show)

data CalibrationQuery
	= SetTimeAndAbortCalibration
	| StartCalibration
	| FinishCalibration
	| GetCalibrationValue
	| GetCalibrationEstimate
	| GetTimeDeviation -- ^ in ms
	| SetCalibrationValueToYear
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

data FileInfo = FileInfo
	{ fiSize :: Word32
	, fiDate :: Word16
	, fiTime :: Word16
	, fiAttributes :: Word8
	, fiName :: ByteString
	} deriving (Eq, Ord, Read, Show)
