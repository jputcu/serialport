{-# LANGUAGE CPP #-}

-- |This module provides the serial port interface.
--
-- > import System.Hardware.Serialport
-- > let port = "COM3"          -- Windows
-- > let port = "/dev/ttyUSB0"  -- Linux
-- > s <- openSerial port defaultSerialSettings { commSpeed = CS2400 }
-- > sendString s "AT\r"
-- > recvString s >>= print
-- > closeSerial s
--

module System.Hardware.Serialport (
  -- * Types
   CommSpeed(..)
  ,StopBits(..)
  ,Parity(..)
  ,FlowControl(..)
  ,SerialPort
  -- * Configure port
  -- | You don't need the get or set functions, they are used by openSerial
  ,SerialPortSettings(..)
  ,defaultSerialSettings
  ,setSerialSettings
  ,getSerialSettings
  -- * Serial methods
  -- ** Device
  ,openSerial
  ,closeSerial
  -- ** Sending & receiving
  ,sendChar
  ,sendString
  ,recvChar
  ,recvString
  ,send
  ,recv
  ,recvRetry
  -- ** Line control
  ,setDTR
  ,setRTS
  ) where

#if defined(mingw32_HOST_OS)
import System.Hardware.Serialport.Windows
#else
import System.Hardware.Serialport.Posix
#endif
import System.Hardware.Serialport.Types
