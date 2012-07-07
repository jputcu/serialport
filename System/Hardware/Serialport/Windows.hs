{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Windows where

import Data.Bits
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as BU
import qualified System.Win32.Comm as Comm
import System.Win32.Types
import System.Win32.File
import Foreign.Marshal.Alloc
import System.Hardware.Serialport.Types
import Control.Monad


-- | Open and configure a serial port
openSerial :: String      -- ^ Serial port, such as @COM5@ or @CNCA0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
  h <- createFile ("\\\\.\\" ++ dev) access_mode share_mode security_attr create_mode file_attr template_file
  let serial_port = SerialPort h defaultSerialSettings
  return =<< setSerialSettings serial_port settings
  where
    access_mode = gENERIC_READ .|. gENERIC_WRITE
    share_mode = fILE_SHARE_NONE
    security_attr = Nothing
    create_mode = oPEN_EXISTING
    file_attr = fILE_ATTRIBUTE_NORMAL -- .|. fILE_FLAG_OVERLAPPED
    template_file = Nothing


-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO B.ByteString
recv (SerialPort h _) n =
  allocaBytes n $ \p -> do
    recv_cnt <- win32_ReadFile h p count overlapped
    B.packCStringLen (p, fromIntegral recv_cnt)
  where
    count = fromIntegral n
    overlapped = Nothing


-- |Send bytes
send :: SerialPort
        -> B.ByteString
        -> IO Int          -- ^ Number of bytes actually sent
send (SerialPort h _) msg =
  BU.unsafeUseAsCString msg $ \p ->
    fromIntegral `fmap` win32_WriteFile h p count overlapped
  where
    count = fromIntegral $ B.length msg
    overlapped = Nothing


-- |Flush buffers
flush :: SerialPort -> IO ()
flush s@(SerialPort h _) =
  flushFileBuffers h
  >> consumeIncomingChars
  where
    consumeIncomingChars = do
      ch <- recv s 1
      unless (ch == B.empty) consumeIncomingChars


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial = closeHandle . handle


-- |Set the Data Terminal Ready level
setDTR :: SerialPort -> Bool -> IO ()
setDTR (SerialPort h _ ) True =  Comm.escapeCommFunction h Comm.setDTR
setDTR (SerialPort h _ ) False =  Comm.escapeCommFunction h Comm.clrDTR


-- |Set the Ready to send level
setRTS :: SerialPort -> Bool -> IO ()
setRTS (SerialPort h _ ) True = Comm.escapeCommFunction h Comm.setRTS
setRTS (SerialPort h _ ) False = Comm.escapeCommFunction h Comm.clrRTS


-- |Configure the serial port
setSerialSettings :: SerialPort           -- ^ The currently opened serial port
                  -> SerialPortSettings   -- ^ The new settings
                  -> IO SerialPort        -- ^ New serial port
setSerialSettings (SerialPort h _) new_settings = do
  let timeout = case block new_settings of
                     Block t   -> t
                     NonBlock  -> error "System.Hardware.Serialport.Windows: Non-blocking mode unsupported on Windows"
      ct = Comm.COMMTIMEOUTS {
                    Comm.readIntervalTimeout = maxBound :: DWORD,
                    Comm.readTotalTimeoutMultiplier = maxBound :: DWORD,
                    Comm.readTotalTimeoutConstant = fromIntegral (timeout new_settings) * 100,
                    Comm.writeTotalTimeoutMultiplier = 0,
                    Comm.writeTotalTimeoutConstant = 0 }
  Comm.setCommTimeouts h ct

  Comm.setCommState h new_settings

  return (SerialPort h new_settings)


-- |Get configuration from serial port
getSerialSettings :: SerialPort -> SerialPortSettings
getSerialSettings = portSettings
