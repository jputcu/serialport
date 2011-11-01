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
  ,withSerial
  -- ** Sending & receiving
  ,sendChar
  ,sendString
  ,recvChar
  ,recvString
  ,flush
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

import qualified Control.Exception as Ex

-- |Safer device function, so you don't forget to close the device
withSerial :: String -> SerialPortSettings -> ( SerialPort -> IO a ) -> IO a
withSerial dev settings = Ex.bracket (openSerial dev settings) closeSerial
