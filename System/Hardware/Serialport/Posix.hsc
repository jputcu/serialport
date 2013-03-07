{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable  #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Posix where

import qualified Data.ByteString.Char8 as B
import qualified Control.Exception as Ex
import System.Posix.IO
import System.Posix.Types
import System.Posix.Terminal
import System.Hardware.Serialport.Types
import Foreign (Ptr, castPtr, alloca, peek, with)
import Foreign.C
import GHC.IO.Handle
import GHC.IO.Device
import GHC.IO.BufferedIO
import Data.Typeable
import GHC.IO.Buffer
import GHC.IO.Encoding
import Control.Monad (void)
import Data.Bits


data SerialPort = SerialPort {
                      fd :: Fd,
                      portSettings :: SerialPortSettings
                  }
                  deriving (Typeable)


instance RawIO SerialPort where
  read (SerialPort fd' _) ptr n = return . fromIntegral =<< fdReadBuf fd' ptr (fromIntegral n)
  readNonBlocking _ _ _ = error "readNonBlocking not implemented"
  write (SerialPort fd' _) ptr n = void (fdWriteBuf fd' ptr (fromIntegral n))
  writeNonBlocking _ _ _ = error "writenonblocking not implemented"


instance IODevice SerialPort where
  ready _ _ _ = return True
  close = closeSerial
  isTerminal _ = return False
  isSeekable _ = return False
  seek _ _ _ = return ()
  tell _ = return 0
  getSize _ = return 0
  setSize _ _ = return ()
  setEcho _ _ = return ()
  getEcho _ = return False
  setRaw _ _ = return ()
  devType _ = return Stream


instance BufferedIO SerialPort where
  newBuffer _ = newByteBuffer 100
  fillReadBuffer = readBuf
  fillReadBuffer0 = readBufNonBlocking
  flushWriteBuffer = writeBuf
  flushWriteBuffer0 = writeBufNonBlocking


-- |Open and configure a serial port returning a standard Handle
hOpenSerial :: FilePath
           -> SerialPortSettings
           -> IO Handle
hOpenSerial dev settings = do
  ser <- openSerial dev settings
  h <- mkDuplexHandle ser dev Nothing noNewlineTranslation
  hSetBuffering h NoBuffering
  return h


-- |Open and configure a serial port
openSerial :: FilePath            -- ^ Serial port, such as @\/dev\/ttyS0@ or @\/dev\/ttyUSB0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
  fd' <- openFd dev ReadWrite Nothing defaultFileFlags { noctty = True }
  let serial_port = SerialPort fd' defaultSerialSettings
  return =<< setSerialSettings serial_port settings


-- |Use specific encoding for an action and restore old encoding afterwards
withEncoding :: TextEncoding -> IO a -> IO a
#if MIN_VERSION_base(4,5,0)
withEncoding encoding fun = do
  cur_enc <- getForeignEncoding
  setForeignEncoding encoding
  result <- fun
  setForeignEncoding cur_enc
  return result
#else
withEncoding _ fun = fun
#endif


-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO B.ByteString
recv (SerialPort fd' _) n = do
  result <- withEncoding char8 $ Ex.try $ fdRead fd' count :: IO (Either IOError (String, ByteCount))
  case result of
     Right (str, _) -> return $ B.pack str
     Left _         -> return B.empty
  where
    count = fromIntegral n


-- |Send bytes
send :: SerialPort
        -> B.ByteString
        -> IO Int          -- ^ Number of bytes actually sent
send (SerialPort fd' _ ) msg = do
  ret <- withEncoding char8 (fdWrite fd' (B.unpack msg))
  return $ fromIntegral ret


-- |Flush buffers
flush :: SerialPort -> IO ()
flush (SerialPort fd' _) =
  discardData fd' BothQueues


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial = closeFd . fd


#include <sys/ioctl.h>

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

cIoctl' :: Fd -> Int -> Ptr d -> IO ()
cIoctl' f req =
  throwErrnoIfMinus1_ "ioctl" .
     c_ioctl (fromIntegral f) (fromIntegral req) . castPtr


getTIOCM :: Fd -> IO Int
getTIOCM fd' =
  alloca $ \p -> cIoctl' fd' #{const TIOCMGET} p >> peek p


setTIOCM :: Fd -> Int -> IO ()
setTIOCM fd' val =
  with val $ cIoctl' fd' #{const TIOCMSET}


-- |Set the Data Terminal Ready level
setDTR :: SerialPort -> Bool -> IO ()
setDTR (SerialPort fd' _) set = do
  current <- getTIOCM fd'
  setTIOCM fd' $ if set
                   then current .|. #{const TIOCM_DTR}
                   else current .&. complement #{const TIOCM_DTR}


-- |Set the Ready to send level
setRTS :: SerialPort -> Bool -> IO ()
setRTS (SerialPort fd' _) set = do
  current <- getTIOCM fd'
  setTIOCM fd' $ if set
                   then current .|. #{const TIOCM_RTS}
                   else current .&. complement #{const TIOCM_RTS}


-- |Configure the serial port
setSerialSettings :: SerialPort           -- ^ The currently opened serial port
                  -> SerialPortSettings   -- ^ The new settings
                  -> IO SerialPort        -- ^ New serial port
setSerialSettings (SerialPort fd' _) new_settings = do
  termOpts <- getTerminalAttributes fd'
  let termOpts' = configureSettings termOpts new_settings
  setTerminalAttributes fd' termOpts' Immediately
  return (SerialPort fd' new_settings)


-- |Get configuration from serial port
getSerialSettings :: SerialPort -> SerialPortSettings
getSerialSettings = portSettings


withParity :: TerminalAttributes -> Parity -> TerminalAttributes
withParity termOpts Even =
    termOpts `withMode` EnableParity
             `withoutMode` OddParity
withParity termOpts Odd =
    termOpts `withMode` EnableParity
             `withMode` OddParity
withParity termOpts NoParity =
    termOpts `withoutMode` EnableParity


withFlowControl :: TerminalAttributes -> FlowControl -> TerminalAttributes
withFlowControl termOpts NoFlowControl =
    termOpts `withoutMode` StartStopInput
             `withoutMode` StartStopOutput
withFlowControl termOpts Software =
    termOpts `withMode` StartStopInput
             `withMode` StartStopOutput


withStopBits :: TerminalAttributes -> StopBits -> TerminalAttributes
withStopBits termOpts One =
    termOpts `withoutMode` TwoStopBits
withStopBits termOpts Two =
    termOpts `withMode` TwoStopBits


configureSettings :: TerminalAttributes -> SerialPortSettings -> TerminalAttributes
configureSettings termOpts settings =
    termOpts `withInputSpeed` commSpeedToBaudRate (commSpeed settings)
             `withOutputSpeed` commSpeedToBaudRate (commSpeed settings)
             `withBits` fromIntegral (bitsPerWord settings)
             `withStopBits` stopb settings
             `withParity` parity settings
             `withFlowControl` flowControl settings
             `withoutMode` EnableEcho
             `withoutMode` EchoErase
             `withoutMode` EchoKill
             `withoutMode` ProcessInput
             `withoutMode` ProcessOutput
             `withoutMode` MapCRtoLF
             `withoutMode` EchoLF
             `withoutMode` HangupOnClose
             `withoutMode` KeyboardInterrupts
             `withoutMode` ExtendedFunctions
             `withMode` LocalMode
             `withMode` ReadEnable
             `withTime` timeout settings
             `withMinInput` 0


commSpeedToBaudRate :: CommSpeed -> BaudRate
commSpeedToBaudRate speed =
    case speed of
      CS110 -> B110
      CS300 -> B300
      CS600 -> B600
      CS1200 -> B1200
      CS2400 -> B2400
      CS4800 -> B4800
      CS9600 -> B9600
      CS19200 -> B19200
      CS38400 -> B38400
      CS57600 -> B57600
      CS115200 -> B115200


