{-# LANGUAGE OverloadedStrings #-}
module System.Hardware.N8Pro (
	module System.Hardware.N8Pro,
	module System.Hardware.N8Pro.Types,
	module System.Hardware.N8Pro.IncompletePatterns,
	) where

import System.Hardware.N8Pro.IncompletePatterns
import System.Hardware.N8Pro.Types

import Data.Bifunctor
import Data.Bits
import Data.ByteString.Builder (Builder)
import Data.Time.Calendar
import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS

openPort :: FilePath -> IO Handle
openPort fp = do
	h <- openBinaryFile fp ReadWriteMode
	h <$ hSetBuffering h NoBuffering

hCommand :: Handle -> Command a -> IO (Either (CommandError a) a)
hCommand h cmd = do
	BS.hPut h (fixed pkt)
	hPutVariable h (variable pkt) >>= \case
		Just n -> transmissionError n
		Nothing -> first parseError <$> hParse h (fromBytes cmd)
	where
	pkt = toPacket cmd
	cmdError dets = CommandError
		{ problemCommand = cmd
		, problemPacket = pkt
		, details = dets
		}
	transmissionError n = pure . Left . cmdError $ Left TransmissionError
		{ variableSizeTransmitted = n
		}
	parseError (err, chunks) = cmdError $ Right ParseError
		{ errorDescription = err
		, chunksReceived = chunks
		}

toPacket :: Command a -> Packet
toPacket = \case
	GetStatus -> getStatusPkt
	GetMode -> getModePkt
	Reset -> extendedResetPkt
	GetVDC -> getVDCPkt
	GetTime -> getTimePkt
	SetTime t -> extendPacket setTimePkt t
	ReadFlash range -> extendPacket readFlashPkt range
	WriteFlash addr bs -> (extendPacket writeFlashPkt addr) { variable = bs }
	ReadMemory range -> extendPacket readMemoryPkt (range, 0 :: Word8)
	SetMemory range w -> extendPacket setMemoryPkt (range, [w, 0])
	TestMemory range w -> extendPacket testMemoryPkt (range, [w, 0])
	ChecksumMemory range -> extendPacket checksumMemoryPkt (range, 0 :: Word32, 0 :: Word8)
	CalibrateClock t arg -> extendPacket calibrateClockPkt (t, arg)
	UpdateExecution addr crc -> extendPacket updateExecutionPkt (addr, crc, 0 :: Word8)
	InitDisk -> initDiskPkt
	ReadDisk addr blocks -> extendPacket readDiskPkt (addr, fromIntegral blocks :: Word32)
	OpenDirectory path -> extendPacket openDirectoryPkt path
	ReadDirectory maxSize0 -> extendPacket readDirectoryPkt maxSize
		where maxSize = max (maxSize0-1) maxSize0
	LoadDirectory sorted path -> extendPacket loadDirectoryPkt (sorted, path)
	SizeDirectory -> sizeDirectoryPkt
	OpenFile mode path -> extendPacket openFilePkt (mode, path)
	ReadFile sz -> extendPacket readFilePkt sz
	WriteFile bs -> writeFilePkt { variable = bs }
	CloseFile -> closeFilePkt
	SetFilePointer addr -> extendPacket setFilePointerPkt addr
	InfoFile path -> extendPacket infoFilePkt path
	ChecksumFile sz -> extendPacket checksumFilePkt [sz, 0]
	MakeDirectory bs -> extendPacket makeDirectoryPkt (bs, OnlyFixed getStatusPkt)
	DeleteFile bs -> extendPacket deleteFilePkt bs
	RecoverUSB crc -> extendPacket recoverUSBPkt (addrFlashCore, crc, OnlyFixed getStatusPkt)
	RunApplication -> runApplicationPkt

-- TODO: is the block buffering actually needed?
-- | 'Just' indicates failure, with how much got sent before the failure was detected
hPutVariable :: Handle -> ByteString -> IO (Maybe Int)
hPutVariable h bs0 = do
	BS.hPut h . u32Bytes . fromIntegral $ BS.length bs0
	hSetBuffering h (BlockBuffering (Just blockSize))
	go 0 bs0 <* hSetBuffering h NoBuffering
	where
	blockSize = 1024
	go n bs = case BS.splitAt blockSize bs of
		("", _) -> pure Nothing
		(b, e) -> do
			resp <- BS.hGet h 1
			case BS.unpack resp of
				[0] -> do
					BS.hPut h b
					go (n + blockSize) e
				_ -> pure (Just n)

hParse :: Handle -> Parser a -> IO (Either (String, [ByteString]) a)
hParse h = go [] where
	go chunks = \case
		Yield a -> pure (Right a)
		Failure err -> pure (Left (err, reverse chunks))
		-- TODO: if n is large, should we set up buffering temporarily?
		Receive n k -> do
			bs <- BS.hGet h n
			go (bs:chunks) (k bs)

class N8Encode a where n8Encode :: a -> Builder
newtype OnlyFixed = OnlyFixed Packet deriving (Eq, Ord, Read, Show)

instance N8Encode Word8 where n8Encode = BS.word8
instance N8Encode Word16 where n8Encode = BS.word16LE
instance N8Encode Word32 where n8Encode = BS.word32LE
instance N8Encode ByteString where n8Encode = (n8Encode . (fromIntegral :: Int -> Word16) . BS.length) <> BS.byteString
instance N8Encode Range where n8Encode = (n8Encode . base) <> (n8Encode . size)
instance N8Encode OnlyFixed where n8Encode (OnlyFixed pkt) = BS.byteString (fixed pkt)
instance (N8Encode a, N8Encode b) => N8Encode (a, b) where n8Encode = (n8Encode . fst) <> (n8Encode . snd)
instance (N8Encode a, N8Encode b, N8Encode c) => N8Encode (a, b, c) where n8Encode (a, b, c) = n8Encode a <> n8Encode b <> n8Encode c
instance N8Encode a => N8Encode [a] where n8Encode = foldMap n8Encode

instance N8Encode UTCTime where
	n8Encode (UTCTime (YearMonthDay y mo d) hms) = foldMap (n8Encode @Word8 . bcd) [fromInteger (y - 2000), mo, d, h, mi, s] where
		(s, mh) = round hms `quotRem` 60
		(mi, h) = mh `quotRem` 60
		bcd n = let (q, r) = n `quotRem` 10 in fromIntegral (shiftL q 4 .|. r)

instance N8Encode CalibrationQuery where
	n8Encode = n8Encode @Word8 . \case
		SetTimeAndAbortCalibration -> 0
		StartCalibration -> 1
		FinishCalibration -> 2
		GetCalibrationValue -> 3
		GetCalibrationEstimate -> 4
		GetTimeDeviation -> 5
		SetCalibrationValueToYear -> 6

extendPacket :: N8Encode a => Packet -> a -> Packet
extendPacket pkt a = pkt { fixed = BS.toStrict . BS.toLazyByteString $ BS.byteString (fixed pkt) <> n8Encode a }

extendedResetPkt :: Packet
extendedResetPkt = extendPacket resetPkt (0 :: Word8)

u32Bytes :: Word32 -> ByteString
u32Bytes = BS.pack . take 4 . map fromIntegral . iterate (`shiftR` 8)
