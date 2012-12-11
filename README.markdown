Objectives
----------
* Cross platform: at least Linux, Windows and Mac OS.

Tests
-----
Arduino Leonardo + FTDI breakout board

Upload arduino code

Configure cabal to build the tests: cabal configure --enable-tests.

Build: cabal build

Run the tests: cabal test --test-options="/dev/ttyACM0 /dev/ttyUSB0"
