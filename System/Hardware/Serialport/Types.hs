{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Types where

import Data.Word
#if defined(mingw32_HOST_OS)
import System.Win32.Types (HANDLE)
#else
import System.Posix.Types (Fd)
#endif


-- | Supported baudrates
data CommSpeed
  = CS110
  | CS300
  | CS600
  | CS1200
  | CS2400
  | CS4800
  | CS9600
  | CS19200
  | CS38400
  | CS57600
  | CS115200
  deriving (Show)


data StopBits = One | Two
data Parity = Even | Odd | NoParity
data FlowControl = Software | NoFlowControl

data SerialPortSettings = SerialPortSettings {
                      commSpeed    :: CommSpeed,   -- ^ baudrate
                      bitsPerWord  :: Word8,       -- ^ Number of bits in a word
                      stopb        :: StopBits,    -- ^ Number of stop bits
                      parity       :: Parity,      -- ^ Type of parity
                      flowControl  :: FlowControl, -- ^ Type of flowcontrol
                      timeout      :: Int          -- ^ Timeout when receiving a char in tenth of seconds
                  }


data SerialPort = SerialPort {
#if defined(mingw32_HOST_OS)
                      handle :: HANDLE,
#else
                      fd :: Fd,
#endif
                      newSettings :: SerialPortSettings
                  }


-- | Most commonly used configuration
--
--  - 9600 baud
--
--  - 8 data bits
--
--  - 1 stop bit
--
--  - no parity
--
--  - no flow control
--
--  - 0.1 millisecond receive timeout
--
defaultSerialSettings :: SerialPortSettings
defaultSerialSettings =
  SerialPortSettings CS9600 8 One NoParity NoFlowControl 1

