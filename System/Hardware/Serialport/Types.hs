{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Types where


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
  deriving (Show, Eq, Bounded)


-- | Number of data bits in a byte
data ByteSize
  = FiveBitsByte
  | SixBitsByte
  | SevenBitsByte
  | EightBitsByte
  deriving (Show, Eq, Bounded)


instance Enum ByteSize where
  toEnum 5 = FiveBitsByte
  toEnum 6 = SixBitsByte
  toEnum 7 = SevenBitsByte
  toEnum 8 = EightBitsByte
  toEnum _ = error "unsupported byte size"
  fromEnum FiveBitsByte = 5
  fromEnum SixBitsByte = 6
  fromEnum SevenBitsByte = 7
  fromEnum EightBitsByte = 8


data StopBits = One | Two deriving (Show, Eq, Bounded)
data Parity = Even | Odd | NoParity deriving (Show, Eq)
data FlowControl = Software | NoFlowControl deriving (Show, Eq)

data SerialPortSettings = SerialPortSettings {
                      commSpeed    :: CommSpeed,   -- ^ baudrate
                      byteSize     :: ByteSize,    -- ^ Number of data bits in a byte
                      stopb        :: StopBits,    -- ^ Number of stop bits
                      parity       :: Parity,      -- ^ Type of parity
                      flowControl  :: FlowControl, -- ^ Type of flowcontrol
                      timeout      :: Int          -- ^ Timeout when receiving a char in tenth of seconds
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
--  - 0.1 second receive timeout
--
defaultSerialSettings :: SerialPortSettings
defaultSerialSettings =
  SerialPortSettings CS9600 EightBitsByte One NoParity NoFlowControl 1

