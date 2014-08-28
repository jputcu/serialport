import System.Environment
import System.Hardware.Serialport
import System.IO
import qualified Data.ByteString.Char8 as B
import Test.HUnit
import System.Exit
import Control.Concurrent



configureComm :: SerialPort -> CommSpeed -> IO ()
configureComm h cs = do
  let control_char = case cs of
        CS1200   -> "a"
        CS2400   -> "b"
        CS4800   -> "c"
        CS9600   -> "d"
        CS19200  -> "e"
        CS57600  -> "f"
        CS115200 -> "g"
        _        -> error "commSpeed not supported"
  send h $ B.pack control_char
  assertEqual "configure serial port" control_char . B.unpack =<< recv h 100



sendRange :: SerialPort -> IO ()
sendRange s =
  mapM_ sendAndRecv ['\x00'..'\xff']
  where
    sendAndRecv :: Char -> IO ()
    sendAndRecv c = do
      send s $ B.pack [c]
      assertEqual "byte mismatch" [c] . B.unpack =<< recv s 1



testSerialport :: CommSpeed -> String -> SerialPort -> Test
testSerialport cs test_port control = TestCase $ do
  configureComm control cs
  withSerial test_port defaultSerialSettings { commSpeed = cs } sendRange
  assertEqual "test ok" "ok\r\n" . B.unpack =<< recv control 100



sendRangeH :: Handle -> IO ()
sendRangeH h =
  mapM_ sendAndRecv ['\x00'..'\xff']
  where
    sendAndRecv :: Char -> IO ()
    sendAndRecv c = do
      hPutChar h c
      assertEqual "byte mismatch" c =<< hGetChar h



testHandle :: CommSpeed -> String -> SerialPort -> Test
testHandle cs test_port control = TestCase $ do
  configureComm control cs
  h <- hOpenSerial test_port defaultSerialSettings { commSpeed = cs }
  sendRangeH h
  hClose h
  assertEqual "test ok" "ok\r\n" . B.unpack =<< recv control 100


testDelay :: String -> SerialPort -> Test
testDelay test_port control = TestCase $ do
  s <- openSerial test_port defaultSerialSettings { timeout = 6 }
  let control_char = 'h'
  send control $ B.pack [control_char]
  assertEqual "configure serial port" [control_char] . B.unpack =<< recv control 100

  send s $ B.pack "a"
  assertEqual "immediatly" "aA" . B.unpack =<< recv s 10

  send s $ B.pack "b"
  assertEqual "delay 50 ms" "bB" . B.unpack =<< recv s 10

  send s $ B.pack "c"
  assertEqual "delay 150 ms" "c" . B.unpack =<< recv s 10
  assertEqual "second try" "C" . B.unpack =<< recv s 10

  closeSerial s



tests :: String -> SerialPort -> Test
tests test_port control = TestList $ map (\(descr,fun) -> TestLabel descr (fun test_port control)) testCases
  where
    testCases = [
      ("b1200 Serialport",  testSerialport CS1200),
      ("b2400 Serialport",  testSerialport CS2400),
      ("b4800 Serialport",  testSerialport CS4800),
      ("b9600 Serialport",  testSerialport CS9600),
      ("b19200 Serialport", testSerialport CS19200),
      ("b57600 Serialport", testSerialport CS57600),
      ("b115200 Serialport",testSerialport CS115200),
      ("b9600 Handle",      testHandle CS9600)
      --("test delay",        testDelay)
      ]


main :: IO ExitCode
main = do
  [control_port, test_port] <- getArgs
  cnts <- withSerial control_port defaultSerialSettings (runTestTT . tests test_port)
  if failures cnts == 0
    then exitSuccess
    else exitFailure

