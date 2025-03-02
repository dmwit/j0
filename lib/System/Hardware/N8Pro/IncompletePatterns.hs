{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Hardware.N8Pro.IncompletePatterns where

import System.Hardware.N8Pro.Types

import Data.Bits
import Data.Functor
import Data.Time.Calendar
import Text.Printf

import qualified Data.ByteString as BS

fromBytes :: Command a -> Parser a
fromBytes = \case
	GetStatus{} -> Receive 2 \bs -> case BS.unpack bs of
		[0x00, 0xa5] -> Yield ExitSuccess
		[err , 0xa5] -> Yield (ExitFailure (fromIntegral err))
		[_   , byte] -> Failure $ printf "Expected response byte 0xa5, but got 0x%02x" byte
	GetMode{} -> Receive 1 \bs -> Yield if BS.head bs == 0xa1 then Service else Application
	Reset{} -> ok
	GetVDC{} -> u16sParser 4 <&> \[val50, val25, val12, valbt] -> VDC { v50 = val50, v25 = val25, v12 = val12, vbt = valbt }
	GetTime{} -> Receive 6 \bs -> let
		bcd :: Num a => Int -> a
		bcd n = let byte = bs `BS.index` n in fromIntegral (10 * (byte `shiftR` 4) + (byte .&. 0xf))
		in Yield UTCTime
			{ utctDay = YearMonthDay (2000 + bcd 0) (bcd 1) (bcd 2)
			, utctDayTime = ((bcd 3*60) + bcd 4*60) + bcd 5
			}
	SetTime t -> either Failure (const ok) (yearInRange t)
	ReadFlash range -> receiveWord32 (size range)
	WriteFlash{} -> ok
	ReadMemory range -> receiveWord32 (size range)
	WriteMemory{} -> ok
	SetMemory{} -> ok
	TestMemory{} -> Receive 1 \bs -> Yield (bs /= "\0")
	ChecksumMemory{} -> u32Parser
	CalibrateClock{} -> fromIntegral <$> u32Parser
	UpdateExecution{} -> ok
	InitDisk{} -> ok
	ReadDisk _ blocks -> receiveBlocksOfSize 512 "read disk" (512 * fromIntegral blocks)
	OpenDirectory{} -> ok
	ReadDirectory{} -> fileInfoParser "read directory"
	LoadDirectory{} -> ok
	SizeDirectory{} -> u16Parser
	OpenFile{} -> ok
	ReadFile sz -> receiveBlocksOfSize 4096 "read file" sz
	WriteFile{} -> ok
	CloseFile{} -> ok
	SetFilePointer{} -> ok
	InfoFile{} -> fileInfoParser "info file"
	ChecksumFile{} -> statusParser "checksum file" >> u32Parser
	MakeDirectory{} -> fromBytes GetStatus >>= \case
		ExitSuccess -> Yield False
		ExitFailure 8 -> Yield True
		ExitFailure n -> Failure $ printf "unknown directory creation error 0x%02x" n
	DeleteFile{} -> ok
	RecoverUSB{} -> fromBytes GetStatus >>= \case
		ExitSuccess -> Yield False
		ExitFailure 0x88 -> Yield True
		ExitFailure n -> Failure $ printf "unknown recovery error 0x%02x" n
	RunApplication{} -> ok
	where
	ok = Yield ()

undefinedPkt ,   getStatusPkt, getModePkt, resetPkt, getVDCPkt, getTimePkt, setTimePkt, readFlashPkt, writeFlashPkt, readMemoryPkt, writeMemoryPkt, setMemoryPkt, testMemoryPkt, checksumMemoryPkt, calibrateClockPkt, updateExecutionPkt, initDiskPkt, readDiskPkt, openDirectoryPkt, readDirectoryPkt, loadDirectoryPkt, sizeDirectoryPkt, openFilePkt, readFilePkt, writeFilePkt, closeFilePkt, setFilePointerPkt, infoFilePkt, checksumFilePkt, makeDirectoryPkt, deleteFilePkt, recoverUSBPkt, runApplicationPkt :: Packet
[undefinedPkt,   getStatusPkt, getModePkt, resetPkt, getVDCPkt, getTimePkt, setTimePkt, readFlashPkt, writeFlashPkt, readMemoryPkt, writeMemoryPkt, setMemoryPkt, testMemoryPkt, checksumMemoryPkt, calibrateClockPkt, updateExecutionPkt, initDiskPkt, readDiskPkt, openDirectoryPkt, readDirectoryPkt, loadDirectoryPkt, sizeDirectoryPkt, openFilePkt, readFilePkt, writeFilePkt, closeFilePkt, setFilePointerPkt, infoFilePkt, checksumFilePkt, makeDirectoryPkt, deleteFilePkt, recoverUSBPkt, runApplicationPkt] = map commandPacket
 [  undefined,           0x10,       0x11,     0x12,      0x13,       0x14,       0x15,         0x16,          0x17,          0x19,           0x1a,         0x1b,          0x1c,              0x1d,              0x21,               0x28,        0xc0,        0xc1,             0xc2,             0xc3,             0xc4,             0xc5,        0xc9,        0xca,         0xcc,         0xce,              0xcf,        0xd0,            0xd1,             0xd2,          0xd3,          0xf0,              0xf1]

addrUndefined ,   addrPRG,   addrCHR,  addrSRAM, addrConfig,   addrSSR,  addrFIFO, addrFlashMenu, addrFlashFPGA, addrFlashCore,        addrMenuPRG,        addrMenuCHR :: Address
[addrUndefined,   addrPRG,   addrCHR,  addrSRAM, addrConfig,   addrSSR,  addrFIFO, addrFlashMenu, addrFlashFPGA, addrFlashCore,        addrMenuPRG,        addrMenuCHR] =
 [   undefined, 0x0000000, 0x0800000, 0x1000000,  0x1800000, 0x1802000, 0x1810000,     0x0000000,     0x0040000,     0x0080000, addrPRG + 0x7e0000, addrCHR + 0x7e0000]

szUndefined ,    szPRG,    szCHR,  szSRAM, szConfig, szSSR, szFIFO :: LengthN8
[szUndefined,    szPRG,    szCHR,  szSRAM, szConfig, szSSR, szFIFO] =
 [ undefined, 0x800000, 0x800000, 0x40000,     0x30, 0x100,    0x1]

rangeUndefined , rangePRG, rangeCHR, rangeSRAM, rangeConfig, rangeSSR, rangeFIFO :: Range
[rangeUndefined, rangePRG, rangeCHR, rangeSRAM, rangeConfig, rangeSSR, rangeFIFO] = zipWith Range
 [addrUndefined,  addrPRG,  addrCHR,  addrSRAM,  addrConfig,  addrSSR,  addrFIFO]
 [  szUndefined,    szPRG,    szCHR,    szSRAM,    szConfig,    szSSR,    szFIFO]

modeUndefined , modeDefault, modeRead, modeWrite, modeCreateNew, modeCreateAlways, modeOpenAlways, modeOpenAppend :: FileMode
[modeUndefined, modeDefault, modeRead, modeWrite, modeCreateNew, modeCreateAlways, modeOpenAlways, modeOpenAppend] =
 [   undefined,        0x00,     0x01,      0x02,          0x04,             0x08,           0x10,           0x30]

-- no incomplete patterns after this comment

-- | unsafe because there's no overflow checking at all
unsafeNumFromBS :: (Bits a, Num a) => ByteString -> a
unsafeNumFromBS = BS.foldr' (\w n -> fromIntegral w .|. shiftL n 8) 0

u16sParser :: Int -> Parser [Word16]
u16sParser n = Receive (n * 2) (Yield . map unsafeNumFromBS . take n . iterate (BS.drop 2))

u8Parser :: Parser Word8
u8Parser = Receive 1 (Yield . unsafeNumFromBS)

u16Parser :: Parser Word16
u16Parser = Receive 2 (Yield . unsafeNumFromBS)

u32Parser :: Parser Word32
u32Parser = Receive 4 (Yield . unsafeNumFromBS)

receiveWord32 :: Word32 -> Parser ByteString
receiveWord32 = go [] where
	go bss 0 = Yield (BS.concat (reverse bss))
	go bss n = Receive nI \bs -> go (bs:bss) (n - fromIntegral nI)
		where nI = fromIntegral (min n (bit 21))

receiveBlocksOfSize :: Word32 -> String -> Word32 -> Parser ByteString
receiveBlocksOfSize block err = go [] where
	go bss 0 = Yield (BS.concat (reverse bss))
	go bss n_ = statusParser err >> Receive nI \bs -> go (bs:bss) (n_ - n) where
		n = min n_ block
		nI = fromIntegral n

statusParser :: String -> Parser ()
statusParser err = Receive 1 \resp -> if resp == "\0" then Yield () else Failure err

fileInfoParser :: String -> Parser FileInfo
fileInfoParser err = do
	statusParser err
	pure FileInfo
		<*> u32Parser
		<*> u16Parser
		<*> u16Parser
		<*> u8Parser
		<*> byteStringParser

byteStringParser :: Parser ByteString
byteStringParser = u16Parser >>= \len -> Receive (fromIntegral len) Yield

commandHeader :: Word8 -> ByteString
commandHeader w = BS.pack [43, 212, w, complement w]

commandPacket :: Word8 -> Packet
commandPacket w = Packet
	{ fixed = commandHeader w
	, variable = Nothing
	}

-- TODO: is this really the right range?
yearInRange :: UTCTime -> Either String UTCTime
yearInRange t
	| y < 2000 = Left $ "Invalid year " ++ show y ++ " (must be 2000 or later)"
	| y >= 2100 = Left $ "Invalid year " ++ show y ++ " (must be before 2100)"
	| otherwise = Right t
	where YearMonthDay y _ _ = utctDay t
