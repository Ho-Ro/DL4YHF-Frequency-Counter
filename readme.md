# About This Repo

This repo contains the original public domain source code for Wolfgang Buescher's (DL4YHF)
PIC based frequency counter in the directory [DL4YHF](DL4YHF) as a reference.

## counter_DL4YHF.asm
I have modified the source code [counter_DL4YHF.asm](counter_DL4YHF.asm) slightly so that it can be
built with [GNU gpasm](https://gputils.sourceforge.io/) under Linux and have added a Makefile.

To build just type `make`. The resulting `counter_DL4YHF.hex` file is identical
to Wolfgang's original version `DL4YHF/counter2.hex`. This can be tested with `make compare`.

## counter.asm
A 2nd variant [counter.asm](counter.asm) differs in three details:
1. Underflow is shown with the zero in the rightmost (5th) digit.
2. Overflow is shown with the E in the 1st digit.
3. Gate time is 1 s for frequencies < 101760 Hz -> display resolution 1 Hz up to 99999 Hz.
   This change was inspired by [TheHWcave](https://github.com/TheHWcave/PIC-freq.counter-modification).

This looks better on 5-digit units and is easier to recognise at first glance.

## counter_hires_event.asm
A 3rd variant [counter_hires_event.asm](counter_hires_event.asm) that is heavily based on the good work of
[TheHWcave](https://github.com/TheHWcave/PIC-freq.counter-modification)
provides a lot improvements:

* 1 Hz resolution up to 99999 Hz (range < 101760 Hz).
* Hi-res (two-decimals) mode with 10 mHz resolution up to 255.99 Hz.
* Toggle three-decimals mode with 1 mHz resolution up to 60.999 Hz with key press.
This allows to measure the mains frequencies quite
* Removed RPM measurement.
* Enter event counting mode when key pressed at startup.
* Rewrote "display_freq" to a more consistent layout:

````
 -------------------------
 |           |  DISPLAY  |
 | Frequency | Freq mode |
 |-----------|-----------|
 | < 1 Hz    |        0  |
 | 1 Hz      |    1„000„ |  Two Hz-dots are flashing (three-digits mode)
 | 10 Hz     |   10„000„ |  Two Hz-dots are flashing (three-digits mode)
 | 1 Hz      |     1„00„ |  Two Hz-dots are flashing
 | 10 Hz     |    10„00„ |  Two Hz-dots are flashing
 | 100 Hz    |   100„00„ |  Two Hz-dots are flashing
 | 255.99 Hz |   255„99„ |  Two Hz-dots are flashing
 | 256 Hz    |    0„256  |  One kHz-dot is flashing
 | 1000 Hz   |    1„000  |  One kHz-dot is flashing
 | 10.00 KHz |   10„000  |  One kHz-dot is flashing
 | 100.0 KHz |   100„00  |  One kHz-dot is flashing
 | 1.000 MHz |   1.0000  |  One MHz-dot is steady
 | 10.00 MHz |   10.000  |  One MHz-dot is steady
 -------------------------
 '.': steady display dot
 '„': flashing display dot
````

The flashing dots change their state with the measurement rate
### Three-digits mode
Frequencies < 61 Hz can optionally be displayed with three decimal digits e.g. "50.123."
To switch between two- and three-digit mode, press the key until the mode changes.
The high-resolution mode is cancelled when the frequency rises above 61 Hz.
The 61 Hz is a compromise between the conversion time and the possibility
of measuring the mains frequencies 50 Hz or 60 Hz precisely. A measurement of the mains frequency
shows almost exact matching with the values available online from https://www.netzfrequenzmessung.de/.

If there is no signal at all, a single zero is displayed in the 5th digit.

### Event counting mode
To enter the event counting mode, press the button during power-up
until the message "Count" is shown.
Events are shown with leading zeros and without dots, e.g. "01234".
Pressing the button at any time resets the counter back to zero.

## License
The work (except [Wolfgang's original code](DL4YHF) that is in the public domain) is released under GPL v3.

Follwing is the content from original `readme.txt` from Wolfgang's [source code archive](https://www.qsl.net/dl4yhf/freq_counter/freq_counter.zip).


Simple frequency counter with a PIC microcontroller
---------------------------------------------------

by Wolfgang Buescher, DL4YHF

Features:
 - 4 or 5 LED digits
 - automatic range switching
 - input range 1 Hz ... 50 MHz (maybe a bit more)
 - uses a cheap PIC16F628
 - clocked with 4 or 20 MHz crystal
 - for common anode or common cathode display
 - optional power-saving mode


There are different firmware variants!
----------------------------------------------

Different firmware variants are available,
`counter1.hex` to `counter3.hex`, all assembled
from the same sourcecode `counter.asm`,
using Microchip's MPLAB with MPASM and TWO
project files (`FreqCnt1.mcp` and `FreqCnt2.mcp`).

What's the difference between `counter1.hex`, `counter2.hex`,
and `counter3.hex`?

`counter1.hex` is the firmware for the first prototype,
where PIC and display are on the same (bread-)board.
It is considered to be the ult board, with as low
power consumption as possible, but with limited
resolution at HF (256 Hz resolution at 50 MHz input).

We later decided to put the PIC and the LED display
on two separate boards, to save space on the front
panel. Some display output pins were swapped to make
the electrical wiring easier. For the second variant,
use the firmware `counter2.hex`. The PIC consumes
about 3 mA more for this variant because of the
20 MHz clock. The resolution is 64 Hz at 50 MHz input.
`counter2.hex` (like `counter1.hex`) drives common cathode displays.
`counter2.hex` is also used in the DL-QRP-AG's dipper,
and in the digital frequency display for Miss Mosquita.

`counter3.hex` uses the same pins as `counter2.hex`, but the control
outputs are inverted to drive common anode display.
In the circuit, use PNP instead of NPN for T1 to drive
the 5th digit of a common anode display, furthermore
connect D1..D4 with reverse polarity, and connect D4
to Vsupp instead of GND.


The following table shows the differences


  Function   |    counter1      |    counter2/3
-------------|------------------|---------------------
  1st digit  | PA3 = PIC pin  2 | PA3 = PIC pin  2
  2nd digit  | PA0 = PIC pin 17 | PA2 = PIC pin  1
  3rd digit  | PA2 = PIC pin  1 | PA0 = PIC pin 17
  4th digit  | PA1 = PIC pin 18 | PA1 = PIC pin 18
  5th digit  | NAND-combination | NAND-combination
  Segment A  | PB0 = PIC pin  6 | PB6 = PIC pin 12
  Segment B  | PB1 = PIC pin  7 | PB7 = PIC pin 13
  Segment C  | PB5 = PIC pin 11 | PB2 = PIC pin  8
  Segment D  | PB3 = PIC pin  9 | PB0 = PIC pin  6
  Segment E  | PB2 = PIC pin  8 | PB3 = PIC pin  9
  Segment F  | PB6 = PIC pin 12 | PB4 = PIC pin 10
  Segment G  | PB7 = PIC pin 13 | PB5 = PIC pin 11
  Segment DP | PB4 = PIC pin 10 | PB1 = PIC pin  7

     AAAA
    F    B
    F    B
     GGGG
    E    C
    E    C
     DDDD   DP


Notes:
 - The PIC used here is a PIC16F628 (18 pins), but
   there are a number of pin compatible other PICs
   out there which may also be used (with some
   firmware modifications). Some are even cheaper..
 - `counter1.hex` as well as `counter2.hex` are "layout-
   optimized" for a display by Kingbright, SC39-11SRWA.
 - Use a "LOW-POWER"/"High-efficiency" or "superbright"
   display! Don't expect a 20-year old display from
   the junkbox to give an impressive display with
   a few milliamperes per digit..


Apart from this, both variants have exactly the same
functionality (they are different VARIANTS, but both
firmwares will always have the same VERSION).


The circuit diagram, breadboard layout,
and description of the PIC frequency counter is at:

    www.qsl.net/dl4yhf/freq_counter


Good luck and happy homebrewing,

  Wolfgang ("Wolf") DL4YHF
