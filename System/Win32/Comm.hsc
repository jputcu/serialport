{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Win32.Comm where

import System.Win32.Types
import Data.Bits
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import System.Hardware.Serialport.Types

#include <windows.h>


-- | If this member is TRUE, binary mode is enabled. Windows does not support nonbinary mode transfers,
--   so this member must be TRUE.
fBinary :: DWORD
fBinary = 0x00000001


-- | If this member is TRUE, parity checking is performed and errors are reported.
fParity :: DWORD
fParity = 0x00000002


-- Preceding bits: (fBinary)1 + (fParity)1 + (fOutxCtrFlow)1 + (fOutxDsrFlow)1
fDtrEnable :: DWORD
fDtrEnable = (#const DTR_CONTROL_ENABLE) `shift` 4


-- Preciding bits:(fBinary)1 + (fParity)1 + (fOutxCtrFlow)1 + (fOutxDsrFlow)1 + (fDtrControl)2 + (fDsrSensitivity)1 +
--                (fTXContinueOnXoff)1 + (fOutX)1 + (fInx)1 + (fErrorChar)1 + (fNull)1
fRtsEnable :: DWORD
fRtsEnable = (#const RTS_CONTROL_ENABLE) `shift` 12


instance Storable SerialPortSettings where
  sizeOf _ = #{size DCB}
  alignment = sizeOf
  poke buf settings = do
    #{poke DCB, DCBlength} buf (sizeOf settings)
    #{poke DCB, BaudRate} buf (fromIntegral (commSpeedToBaudRate (commSpeed settings)) :: DWORD)
    pokeByteOff buf 8 (fBinary .|. fDtrEnable .|. fRtsEnable )
    #{poke DCB, wReserved} buf (0 :: WORD)
    #{poke DCB, XonLim} buf (2048 :: WORD)
    #{poke DCB, XoffLim} buf (512 :: WORD)
    #{poke DCB, ByteSize} buf (bitsPerWord settings :: BYTE)
    #{poke DCB, Parity} buf (case parity settings of
                               NoParity -> #const NOPARITY
                               Odd      -> #const ODDPARITY
                               Even     -> #const EVENPARITY
                               :: BYTE)
    #{poke DCB, StopBits} buf (case stopb settings of
                               One -> #const ONESTOPBIT
                               Two -> #const TWOSTOPBITS
                               :: BYTE)
    #{poke DCB, wReserved1} buf (0 :: WORD)
  peek buf = do
    _dCBlength <- #{peek DCB, DCBlength} buf :: IO DWORD
    _commSpeed <- do _baud <- #{peek DCB, BaudRate} buf :: IO DWORD
                     return $ baudRateToCommSpeed (fromIntegral _baud)
    _fsettings <- peekByteOff buf 8 :: IO DWORD
    _byteSize <- #{peek DCB, ByteSize} buf :: IO BYTE
    _parity <- do _par <- #{peek DCB, Parity} buf :: IO BYTE
                  case _par of
                    #{const NOPARITY}    -> return NoParity
                    #{const ODDPARITY}   -> return Odd
                    #{const EVENPARITY}  -> return Even
                    #{const MARKPARITY}  -> fail "unsupported markparity"
                    #{const SPACEPARITY} -> fail "unsupported spaceparity"
                    _                    -> fail $ "unsupported parity" ++ show _par
    _stopBits <- do _stopb <- #{peek DCB, StopBits} buf :: IO BYTE
                    case _stopb of
                      #{const ONESTOPBIT}   -> return One
                      #{const ONE5STOPBITS} -> fail "unsupported one5stopbits"
                      #{const TWOSTOPBITS}  -> return Two
                      _ -> fail "unexpected stop bit count"
    _XonLim <- #{peek DCB, XonLim} buf :: IO WORD
    _XoffLim <- #{peek DCB, XoffLim} buf :: IO WORD
    return SerialPortSettings {
                 commSpeed = _commSpeed,
                 bitsPerWord = _byteSize,
                 stopb = _stopBits,
                 parity = _parity,
                 flowControl = NoFlowControl,
                 timeout = 0
                 }


getCommState :: HANDLE -> IO SerialPortSettings
getCommState h =
  alloca (\dcbp -> do failIfFalse_ "getCommState" $ c_GetCommState h dcbp
                      peek dcbp )


--BOOL WINAPI GetCommState(
--  __in     HANDLE hFile,
--  __inout  LPDCB lpDCB
--);
foreign import stdcall unsafe "winbase.h GetCommState"
  c_GetCommState :: HANDLE -> Ptr SerialPortSettings -> IO BOOL


setCommState :: HANDLE -> SerialPortSettings -> IO ()
setCommState h settings =
  failIfFalse_ "setCommState" ( with settings (c_SetCommState h))


--BOOL WINAPI SetCommState(
--  __in  HANDLE hFile,
--  __in  LPDCB lpDCB
--);
foreign import stdcall unsafe "winbase.h SetCommState"
  c_SetCommState :: HANDLE -> Ptr SerialPortSettings -> IO BOOL



-- If an application sets ReadIntervalTimeout and ReadTotalTimeoutMultiplier to MAXDWORD and
-- sets ReadTotalTimeoutConstant to a value greater than zero and less than MAXDWORD, one of
-- the following occurs when the ReadFile function is called:
--
--   * If there are any bytes in the input buffer, ReadFile returns immediately with the bytes in the buffer.
--
--   * If there are no bytes in the input buffer, ReadFile waits until a byte arrives and then returns immediately.
--
--   * If no bytes arrive within the time specified by ReadTotalTimeoutConstant, ReadFile times out.
--
type LPCOMMTIMEOUTS = Ptr COMMTIMEOUTS
data COMMTIMEOUTS = COMMTIMEOUTS
    { readIntervalTimeout :: DWORD, -- in milliseconds
      readTotalTimeoutMultiplier :: DWORD,  -- in milliseconds
      readTotalTimeoutConstant :: DWORD,  -- in milliseconds
      writeTotalTimeoutMultiplier :: DWORD,  -- in milliseconds
      writeTotalTimeoutConstant :: DWORD } -- in milliseconds
      deriving Show


instance Storable COMMTIMEOUTS where
  sizeOf _ = #{size COMMTIMEOUTS}
  alignment = sizeOf
  poke buf ct = do
    #{poke COMMTIMEOUTS, ReadIntervalTimeout} buf (readIntervalTimeout ct)
    #{poke COMMTIMEOUTS, ReadTotalTimeoutMultiplier} buf ( readTotalTimeoutMultiplier ct)
    #{poke COMMTIMEOUTS, ReadTotalTimeoutConstant} buf ( readTotalTimeoutConstant ct)
    #{poke COMMTIMEOUTS, WriteTotalTimeoutMultiplier} buf ( writeTotalTimeoutMultiplier ct)
    #{poke COMMTIMEOUTS, WriteTotalTimeoutConstant} buf ( writeTotalTimeoutConstant ct)
  peek buf = do
    _readIntervalTimeout <- #{peek COMMTIMEOUTS, ReadIntervalTimeout} buf
    _readTotalTimeoutMultiplier <- #{peek COMMTIMEOUTS, ReadTotalTimeoutMultiplier } buf
    _readTotalTimeoutConstant  <- #{peek COMMTIMEOUTS, ReadTotalTimeoutConstant } buf
    _writeTotalTimeoutMultiplier <- #{peek COMMTIMEOUTS, WriteTotalTimeoutMultiplier } buf
    _writeTotalTimeoutConstant <- #{peek COMMTIMEOUTS, WriteTotalTimeoutConstant } buf
    return COMMTIMEOUTS { readIntervalTimeout = _readIntervalTimeout,
                          readTotalTimeoutMultiplier = _readTotalTimeoutMultiplier,
                          readTotalTimeoutConstant = _readTotalTimeoutConstant,
                          writeTotalTimeoutMultiplier = _writeTotalTimeoutMultiplier,
                          writeTotalTimeoutConstant = _writeTotalTimeoutConstant }


getCommTimeouts :: HANDLE -> IO COMMTIMEOUTS
getCommTimeouts h =
  alloca (\c -> do _ <- c_GetCommTimeouts h c
                   peek c )


-- getcommtimeouts
-- winbase.h -> BOOL WINAPI GetCommTimeouts(HANDLE, LPCOMMTIMEOUTS);
foreign import stdcall unsafe "winbase.h GetCommTimeouts"
  c_GetCommTimeouts :: HANDLE -> LPCOMMTIMEOUTS -> IO BOOL



-- |
--
-- On success it returns nonzero. On failure, the return value is zero and the GetLastError should be called.
--
setCommTimeouts :: HANDLE -> COMMTIMEOUTS -> IO ()
setCommTimeouts h ct =
  failIfFalse_ "setCommTimeouts" ( with ct (c_SetCommTimeouts h) )


-- setcommtimeouts
-- winbase.h -> BOOL WINAPI SetCommTimeouts(HANDLE, LPCOMMTIMEOUTS);
foreign import stdcall unsafe "winbase.h SetCommTimeouts"
  c_SetCommTimeouts :: HANDLE -> LPCOMMTIMEOUTS -> IO BOOL


clrDTR :: DWORD
clrDTR = #const CLRDTR

setDTR :: DWORD
setDTR = #const SETDTR

clrRTS :: DWORD
clrRTS = #const CLRRTS

setRTS :: DWORD
setRTS = #const SETRTS

--
--
foreign import stdcall unsafe "winbase.h EscapeCommFunction"
  c_EscapeCommFunction :: HANDLE -> DWORD -> IO BOOL


escapeCommFunction :: HANDLE -> DWORD -> IO ()
escapeCommFunction h t =
  failIfFalse_ "excapeCommFunction" ( c_EscapeCommFunction h t )


baudRateToCommSpeed :: Int -> CommSpeed
baudRateToCommSpeed baud =
    case baud of
      (#const CBR_110) -> CS110
      _                -> CS9600


commSpeedToBaudRate :: CommSpeed -> Int
commSpeedToBaudRate cs =
    case cs of
      CS110 -> (#const CBR_110)
      CS300 -> (#const CBR_300)
      CS600 -> (#const CBR_600)
      CS1200 -> (#const CBR_1200)
      CS2400 -> (#const CBR_2400)
      CS4800 -> (#const CBR_4800)
      CS9600 -> (#const CBR_9600)
      CS19200 -> (#const CBR_19200)
      CS38400 -> (#const CBR_38400)
      CS57600 -> (#const CBR_57600)
      CS115200 -> (#const CBR_115200)