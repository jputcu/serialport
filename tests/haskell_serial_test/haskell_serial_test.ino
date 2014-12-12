static int test_running;
static uint8_t expected_char;

static const uint8_t lowest_expected_char = 0x00;
static const uint8_t highest_expected_char = 0xff;

// the setup routine runs once when you press reset:
void setup()
{
  // initialize the digital pin as an output.
  Serial.begin(9600);
  pinMode(13, OUTPUT);
}



void ConsumeAllSerial()
{
  while( Serial.available() )
    Serial.read();
}



// the loop routine runs over and over again forever:
void loop()
{
  if( Serial.available() )
  {
    int i = Serial.read();
    long baudRate = 0;
    switch (i)
    {
      case 'a':
        baudRate = 1200;
        break;
      case 'b':
        baudRate = 2400;
        break;
      case 'c':
        baudRate = 4800;
        break;
      case 'd':
      case 'h':
        baudRate = 9600;
        break;
      case 'e':
        baudRate = 19200;
        break;
      case 'f':
        baudRate = 57600;
        break;
      case 'g':
        baudRate = 115200;
        break;
      default:
        return;
    }

    Serial.write(i);
    Serial1.end();
    Serial1.begin(baudRate);
    ConsumeAllSerial();

    if (i == 'h')
    {
      test_running = 2;

    }
    else
    {
      test_running = 1;
      expected_char = lowest_expected_char;
    }
  }

  if( test_running == 1 )
  {
    while( Serial1.available() )
    {
      uint8_t i = Serial1.read() & 0xff;
      if( i == expected_char)
      {
        Serial1.write(expected_char);
        if( expected_char == highest_expected_char )
        {
          // test successfull
          Serial.println("ok");
          Serial1.flush();
          Serial1.end();
          test_running = 0;
        }
        else
          expected_char++;
      }
      else
      {
        Serial.print("expected char('");
        Serial.print(expected_char, HEX);
        Serial.print("') got ('");
        Serial.print(i, HEX);
        Serial.println("') test failed");
      }
    }
  }
  else if( test_running == 2 )
  {
    while( Serial1.available() )
    {
      uint8_t i = Serial1.read() & 0xff;
      switch( i )
      {
        case 'a':
          Serial1.write('a');
          Serial1.write('A');
          break;
        case 'b':
          Serial1.write('b');
          delay(50);
          Serial1.write('B');
          break;
        case 'c':
          Serial1.write('c');
          delay(150);
          Serial1.write('C');
          break;
        case 'd':
          Serial1.write('d');
          delay(200);
          Serial1.write('D');
          break;
        case 'e':
          // 50 ms delay
          delay(500);
          Serial1.write('E');
          break;
        default:
          test_running = 0;
          break;
      }
    }
  }
}

