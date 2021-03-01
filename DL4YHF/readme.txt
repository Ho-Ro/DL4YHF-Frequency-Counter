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
