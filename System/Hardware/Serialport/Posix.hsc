{-# LANGUAGE ForeignFunctionInterface, ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Posix where
import Data.ByteString (ByteString)
import System.IO.Error
import System.Posix.IO
import qualified System.Posix.IO.ByteString as BS
import System.Posix.Types
import System.Posix.Terminal
import System.Hardware.Serialport.Types
import Foreign
import Foreign.C

-- |Open and configure a serial port
openSerial :: FilePath            -- ^ The filename of the serial port, such as @\/dev\/ttyS0@ or @\/dev\/ttyUSB0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
  fd' <- openFd dev ReadWrite Nothing defaultFileFlags { noctty = True }
  let serial_port = SerialPort fd' defaultSerialSettings
  return =<< setSerialSettings serial_port settings

-- |@recv port n@ receives up to @n@ bytes over a serial port.
recv :: SerialPort -> Int -> IO (Maybe ByteString)
recv (SerialPort fd' _) (fromIntegral -> n) = 
  either (const Nothing) Just `fmap` try (BS.fdRead fd' n)

-- |@recvRetry port numRetries n@ tries up to @numRetries@ times to
-- read a total of @n@ bytes over a serial port.
recvRetry :: SerialPort -> Int -> Int -> IO (Maybe ByteString)
recvRetry (SerialPort fd' _) retries (fromIntegral -> n) = 
  either (const Nothing) Just `fmap` try (BS.fdReads retry 0 fd' n)
  where retry n' tryNum
          | n' == n = Nothing
          | tryNum >= retries = Nothing
          | otherwise = Just $! tryNum + 1 

-- |Send bytes over a serial port. Returns the number of bytes
-- actually sent.
send :: SerialPort -> ByteString -> IO Int
send (SerialPort fd' _) = fmap fromIntegral . BS.fdWrite fd'

-- |Possibly receive a character unless the timeout given in openSerial is exceeded.
recvChar :: SerialPort -> IO (Maybe Char)
recvChar (SerialPort fd' _) = do
  result <- try $ fdRead fd' 1
  return $ case result of
             Right (str, _) -> Just $ head str
             Left _         -> Nothing


-- |Receive a string
recvString :: SerialPort -> IO String
recvString (SerialPort fd' _) = do
  result <- try $ fdRead fd' 128
  return $ case result of
             Right (str, _) -> str
             Left _         -> ""


-- |Send a character
sendChar :: SerialPort -> Char -> IO ()
sendChar (SerialPort fd' _ ) c =
  fdWrite fd' [c] >> return ()


-- |Send a string
sendString :: SerialPort -> String -> IO ()
sendString (SerialPort fd' _) s =
  fdWrite fd' s >> return ()


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial (SerialPort fd' _ ) =
  closeFd fd'


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
getSerialSettings (SerialPort _ settings) = settings


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
    termOpts `withInputSpeed` (commSpeedToBaudRate (commSpeed settings))
             `withOutputSpeed` (commSpeedToBaudRate (commSpeed settings))
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
             --`withoutMode` HangupOnClose
             `withMode` HangupOnClose
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


