{-# LANGUAGE CPP #-}

-- |This module provides the serial port interface.
--
-- > import System.Hardware.Serialport
-- > s <- openSerial "/dev/ttyUSB0" defaultSerialSettings
-- > sendChar s 'A'
-- > Just resp <- recvChar s
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
  -- * Serial methods 
  ,openSerial
  ,sendChar
  ,recvChar
  ,closeSerial
  ,setDTR
  ,setRTS
  ,setSerialSettings
  ,getSerialSettings
  ) where

#if defined(mingw32_HOST_OS)
import System.Hardware.Serialport.Windows
#else
import System.Hardware.Serialport.Posix
#endif
import System.Hardware.Serialport.Types
