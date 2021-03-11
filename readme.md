# About This Repo

This repo contains the original public domain source code for Wolfgang Buescher's (DL4YHF)
PIC based frequency counter in the directory [DL4YHF](DL4YHF) as a reference.

## 1. counter_DL4YHF.asm
I have modified the source code [counter_DL4YHF.asm](counter_DL4YHF.asm) slightly so that it can be
built with [GNU gpasm](https://gputils.sourceforge.io/) under Linux and have added a [Makefile](Makefile).

To build just type `make`. The resulting `counter_DL4YHF.hex` file is identical
to Wolfgang's original version `DL4YHF/counter2.hex`. This can be tested with `make compare`.

## 2. counter.asm
A 2nd variant [counter.asm](counter.asm) differs in three details:
1. Underflow is shown with the zero in the rightmost (5th) digit.
2. Overflow is shown with the E in the 1st digit.
3. Gate time is 1 s for frequencies < 101760 Hz -> display resolution 1 Hz up to 99999 Hz.
   This change was inspired by [TheHWcave](https://github.com/TheHWcave/PIC-freq.counter-modification).

This looks better on 5-digit units and is easier to recognise at first glance.

## 3. counter_hires_event.asm
A 3rd variant [counter_hires_event.asm](counter_hires_event.asm) that is heavily based on the good work of
[TheHWcave](https://github.com/TheHWcave/PIC-freq.counter-modification)
provides a lot improvements:

* Measuremant range > 100 MHz (out of PIC prescaler spec but may work)
* 1 Hz resolution up to 99999 Hz (range < 100800 Hz).
* Round the displayed value for frequencies > 99999 Hz.
* Hi-res (two-decimals) mode with 10 mHz resolution up to 255.99 Hz.
* Toggle three-decimals mode with 1 mHz resolution up to 60.999 Hz with key press.
This allows to measure the mains frequencies very precisely.
* Zoom into the 5 lowest digits while measuring frequencies up to 3.2 MHz, this allows to
calibrate the counter exactly, apply exact 1 MHz, e.g. from a GPDSO and adjust to 00000.
* Removed RPM measurement.
* Enter event counting mode when key pressed at startup.
* Rewrote "display_freq" to show a more consistent layout:

````
 --------------------------
 |            |  DISPLAY  |
 | Frequency  | Freq mode |
 |------------|-----------|
 | < 1 Hz     |        0  |
 | 1 Hz       |    1„000„ |  Two Hz-dots are flashing (three-digits mode)
 | 10 Hz      |   10„000„ |  Two Hz-dots are flashing (three-digits mode)
 | 1 Hz       |     1„00„ |  Two Hz-dots are flashing (normal mode)
 | 10 Hz      |    10„00„ |  Two Hz-dots are flashing (normal mode)
 | 100 Hz     |   100„00„ |  Two Hz-dots are flashing (normal mode)
 | 255.99 Hz  |   255„99„ |  Two Hz-dots are flashing (normal mode)
 | 256 Hz     |    0„256  |  One kHz-dot is flashing
 | 1000 Hz    |    1„000  |  One kHz-dot is flashing
 | 10.000 KHz |   10„000  |  One kHz-dot is flashing
 | 100.00 KHz |   100„00  |  One kHz-dot is flashing
 | 1.0000 MHz |   1.0000  |  One MHz-dot is steady
 | 10.000 MHz |   10.000  |  One MHz-dot is steady
 | 100.00 MHz |   100.00  |  One MHz-dot is steady
 --------------------------
 '.': steady display dot
 '„': flashing display dot
````

The flashing dots change their state with every measurement loop

If there is no signal at all, a single zero is displayed in the 5th digit.

### Three-digits mode
Frequencies < 61 Hz can optionally be displayed with three decimal digits e.g. `50„123„`,
where `„` symbolises the flashing dots.
To switch between two- and three-digits mode, press the key until the mode changes.
The high-resolution mode is cancelled when the frequency rises above 61 Hz.
The 61 Hz is a compromise between the conversion time and the possibility
of measuring typical mains frequencies 50 Hz or 60 Hz precisely.
A test measurement of the European mains frequency shows almost exact matching
with the realtime values available online from https://www.netzfrequenzmessung.de/.

### Calibration "zoom"
In the frequency range 100 .. 3200 kHz the display can be "zoomed" into a 1 Hz resolution
by pressing the button. This selects temporarily a measurement range of 1 s gate time
without prescaler, yielding 1 Hz resolution,
e.g.: a signal of exact 1012345 Hz normally shows `1.0123` with a steady dot, meaning 1.0123 MHz.
Pressing the button changes the display to the 1 Hz resolution mode (that shows only the 5 low digits),
giving `1„2„345`, where `„` symbolises the alternately flashing dots.

This mode is intended to calibrate the quartz oscillator circuit; apply an exact 1 MHz signal
(e.g. from a GPSDO) and adjust the variable capacitor until the display shows `0„0„000`.
This will give you short term accuracy down to 1ppm.

### Event counting mode
To enter the event counting mode, press the button during power-up
until the message "Count" is shown.
Events are shown with leading zeros and without dots, e.g. `01234`.
Pressing the button at any time resets the counter back to zero.

## License
The work (except [Wolfgang's original code](DL4YHF) that is in the public domain) is released under GPL v3.

-----

## Original [readme.txt](DL4YHF/readme.txt) from Wolfgang's [source code archive](https://www.qsl.net/dl4yhf/freq_counter/freq_counter.zip):

````
Simple frequency counter with a PIC microcontroller
---------------------------------------------------

by Wolfgang Buescher, DL4YHF

This directory contains the sourcecode (*.asm)
and the assembled firmware (*.hex) for
DL4YHF's simple frequency counter.

Features:
 - 4 or 5 LED digits
 - automatic range switching
 - input range 1 Hz ... 50 MHz (maybe a bit more)
 - uses a cheap PIC16F628
 - clocked with 4 or 20 MHz crystal
 - for common anode or common cathode display
 - optional power-saving mode


There are different firmware variants !
----------------------------------------------

Different firmware variants are available,
  counter1.hex  to counter3.hex, all assembled
  from the same sourcecode counter.asm,
  using Microchip's MPLAB with MPASM and TWO
  project files (FreqCnt1.mcp and FreqCnt2.mcp).
Unfortunately I could not convince MPLAB to pro-
duce two different hex files, so I had to rename
them manually.

What's the difference between COUNTER1, COUNTER2,
and COUNTER3 ?

COUNTER1.HEX is the firmware for the first prototype,
where PIC and display are on the same (bread-)board.
It is considered to be the ult board, with as low
power consumption as possible, but with limited
resolution at HF (256 Hz resolution at 50 MHz input).

We later decided to put the PIC and the LED display
on two separate boards, to save space on the front
panel. Some display output pins were swapped to make
the electrical wiring easier. For the second variant,
use the firmware COUNTER2.HEX . The PIC consumes
about 3 mA more for this variant because of the
20 MHz clock. The resolution is 64 Hz at 50 MHz input.
COUNTER2 (like COUNTER1) drives COMMON CATHODE displays.
'COUNTER2.HEX' is also used in the DL-QRP-AG's dipper,
and in the digital frequency display for Miss Mosquita.

COUNTER3 uses the same pins as COUNTER2, but the control
outputs are inverted to drive COMMON ANODE display.
In the circuit, use PNP instead of NPN for T1 to drive
the 5th digit of a COMMON ANODE display, furthermore
connect D1..D4 with reverse polarity, and connect D4
to Vsupp instead of GND.


The following table shows the differences
(use a simple text editor with a fixed pitch font
 like courier new to view this file !)


 Function    |    COUNTER1      |     COUNTER2
------------------------------------------------------
  1st digit  | PA3 = PIC pin  2 | PA3 = PIC pin  2
  2nd digit  | PA0 = PIC pin 17 | PA2 = PIC pin  1
  3rd digit  | PA2 = PIC pin  1 | PA0 = PIC pin 17
  4th digit  | PA1 = PIC pin 18 | PA1 = PIC pin 18
  5th digit  | NAND-combination | NAND-combination
  Segment a  | PB0 = PIC pin  6 | PB6 = PIC pin 12
  Segment b  | PB1 = PIC pin  7 | PB7 = PIC pin 13
  Segment c  | PB5 = PIC pin 11 | PB2 = PIC pin  8
  Segment d  | PB3 = PIC pin  9 | PB0 = PIC pin  6
  Segment e  | PB2 = PIC pin  8 | PB3 = PIC pin  9
  Segment f  | PB6 = PIC pin 12 | PB4 = PIC pin 10
  Segment g  | PB7 = PIC pin 13 | PB5 = PIC pin 11
  Segment DP | PB4 = PIC pin 10 | PB1 = PIC pin  7


Notes:
 - The PIC used here is a PIC16F628 (18 pins), but
   there are a number of pin compatible other PICs
   out there which may also be used (with some
   firmware modifications). Some are even cheaper..
 - COUNTER1 as well as COUNTER2 are "layout-
   optimized" for a display by Kingbright, SC39-11SRWA .
 - Use a "LOW-POWER"/"High-efficiency" or "superbright"
   display ! Don't expect a 20-year old display from
   the junkbox to give an impressive display with
   a few milliamperes per digit..


Apart from this, both variants have exactly the same
functionality (they are different VARIANTS, but both
firmwares will always have the same VERSION).


  .---.

The circuit diagram, breadboard layout,
and description of the PIC frequency counter is at:

   c:\myhome\freq_counter ,
or
   www.qsl.net/dl4yhf/freq_counter .



Good luck and happy homebrewing,

  Wolfgang ("Wolf") DL4YHF .

````
