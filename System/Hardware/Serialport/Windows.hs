{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Windows where

import Data.Bits
import qualified System.Win32.Comm as Comm
import System.Win32.Types
import System.Win32.File
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.Hardware.Serialport.Types


-- | Open and configure a serial port
openSerial :: String      -- ^ The filename of the serial port, such as @COM5@ or @\/\/.\/CNCA0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
  h <- createFile dev access_mode share_mode security_attr create_mode file_attr template_file
  let serial_port = SerialPort h defaultSerialSettings
  return =<< setSerialSettings serial_port settings
  where
    access_mode = gENERIC_READ .|. gENERIC_WRITE
    share_mode = fILE_SHARE_NONE
    security_attr = Nothing
    create_mode = oPEN_EXISTING
    file_attr = fILE_ATTRIBUTE_NORMAL -- .|. fILE_FLAG_OVERLAPPED
    template_file = Nothing


-- |Possibly receive a character unless the timeout given in openSerial is exceeded.
recvChar :: SerialPort -> IO (Maybe Char)
recvChar (SerialPort h _) =
  allocaBytes 1 $ \ p_n -> do
    received <- win32_ReadFile h p_n count overlapped
    if received == 0
      then return Nothing
      else do c <- peek p_n :: IO CChar
              return $ Just $ castCCharToChar c
  where
    count = 1
    overlapped = Nothing


-- |Receive a string
recvString :: SerialPort -> IO String
recvString (SerialPort h _) =
  allocaBytes 128 $ \ p_n -> do
    recv_cnt <- win32_ReadFile h p_n count overlapped
    peekCStringLen (p_n, fromIntegral recv_cnt)
  where
    count = 128
    overlapped = Nothing


-- |Send a character
sendChar :: SerialPort -> Char -> IO ()
sendChar (SerialPort h _) s =
  with s (\ p_s -> do _ <- win32_WriteFile h p_s count overlapped
                      return () )
  where
    count = 1
    overlapped = Nothing


-- |Send a string
sendString :: SerialPort -> String -> IO ()
sendString (SerialPort h _) s =
  withCString s (\ p_s -> do _ <- win32_WriteFile h p_s (fromIntegral count) overlapped
                             return () )
  where
    count = length s
    overlapped = Nothing


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial (SerialPort h _) =
  closeHandle h


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
  let ct = Comm.COMMTIMEOUTS {
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
getSerialSettings (SerialPort _ settings) = settings
