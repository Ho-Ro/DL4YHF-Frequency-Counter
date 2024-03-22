; SPDX-License-Identifier: GPL-3.0-or-later

;**************************************************************************
; FILE:      counter_hires_event.asm                                      *
; CONTENTS:  Simple low-cost digital frequency meter using a PIC 16F628A  *
; ORIGIN:    Wolfgang Buescher, DL4YHF                                    *
;            (based on a work by James Hutchby, MadLab, 1996)             *
;            This software works only with hardware version COUNTER2:     *
;            PIC16F628A clocked with 20 MHz, 5 digits common cathode LEDs *
;            The hardware is available as a kit from china:               *
;            "Crystal Oscillator Frequency Counter Tester" for ~10€ or $  *
;                                                                         *
; REVISIONS: (latest entry first)                                         *
; 2024-03-22 - Ho-Ro:                                                     *
;              Store 2 or 3 digit resolution option (<60 Hz) in EEPROM,   *
;              change with push button during low frequency measurement   *
; 2024-03-20 - Ho-Ro:                                                     *
;              Refactoring, move macros into own include file,            *
;              exchange steady/blinking dots: Hz, kHz steady, MHz blink   *
; 2024-03-19 - Ho-Ro:                                                     *
;              Store function (frequency measurement / event counter)     *
;              in EEPROM, change with push button during power up         *
; 2024-03-17 - Ho-Ro:                                                     *
;              Event counter fixed.                                       *
; 2023-01-27 - Ho-Ro:                                                     *
;              Event counter currently not working correctly -> disabled  *
; 2021-03-27 - Ho-Ro:                                                     *
;              Measure 256.0 Hz .. 999.9 Hz with 1 decimal                *
; 2021-03-11 - Ho-Ro:                                                     *
;              Increase display range to nnn MHz (measure up to 120 MHz)  *
; 2021-03-05 - Ho-Ro:                                                     *
;              Round values for ranges nnn kHz, n MHz and nn MHz          *
;              instead of cutting lower digits                            *
; 2021-02-28 - Ho-Ro:                                                     *
;              Added TheHWcode's commit f16a1b0 (conv. range fix, bugfix) *
;              Calibration support: apply exact 1 MHz (e.g. from GPDSO),  *
;              press button and adjust to 00000 (last digit = 1Hz = 1ppm) *
; 2021-02-26 - Ho-Ro:                                                     *
;              1 Hz resolution up to 99999 Hz (range < 101760 Hz)         *
;              hi-res (two decimals) up to 255.99 Hz                      *
;              toggle three-decimals mode up to 60.999 Hz with key press  *
;              rewrote "DisplayFreq" to a more consistent layout          *
;              removed RPM measurement                                    *
;              enter event counting mode when key pressed at startup      *
; 2019-06-28 - TheHWcave: added the capability to select counter mode     *
; 2019-05-02 - TheHWcave: added the capability to select RPM measurement  *
;              using 1 pulse, 2, 3 or 4 pulses per revolution, which      *
;              allows measuring 2 bladed, 3 or 4 bladed fans or propellers*
;              and extended the range for RPM display. Frequency range is *
;              now up to 200 Hz in period measurement. In RPM mode above  *
;              255 Hz the normal frequency is used for conversion up to   *
;              around 92100 RPM in all 3 modes                            *
; 2019-04-18 - TheHWcave: major change: removed all variants except #2 and*
;              the programming and frequency offset bits. Instead I added *
;              period measurement for frequencies below 100 Hz which are  *
;              now shown with 2 decimal points. Pressing the button       *
;              toggles between RPM and frequency format (for frequencies  *
;              below 100 Hz / below 9999 RPM. The last setting is stored  *
;              in EEPROM and becomes the default at power-up              *
; 2018-03-23 - TheHWcave: Changed to always use 5 digits in               *
;              DISPLAY_VARIANT_2 and _3 and also use 1Hz resolution       *
;              (Range 1) for Range 2 because with 5 digits it makes sense *
;              to use the best resolution for as long as possible         *
; 2006-05-31 - Added the 'power-save' option which temporarily puts the   *
;              PIC to sleep (with only the watchdog-oscillator running)   *
; 2006-05-15 - New entry in the preconfigured frequency table for 4-MHz   *
;              IF filters (like "Miss Mosquita" [Moskita] by DK1HE)       *
; 2005-08-24 - Cured a bug in the COMMON ANODE decimal point setting.     *
;              (the "^0xFF" for the AND-mask was missing in macro 'conv') *
; 2005-03-21 - Added a few conditionals to use the same sourcecode        *
;              to drive a COMMON ANODE display ( DISPLAY_VARIANT_3 )      *
; 2004-03-14 - Fixed a range-switching bug around 8 MHz.                  *
;            - Support TWO different display variants now,                *
;              optimized for different board layouts, and different clock *
;              frequencies (4 MHz for variant 1,  20 MHz for variant 2).  *
; 2004-03-05 - Added the feature to add or subtract a frequency offset.   *
; 2004-02-18 - Migration to a PIC16F628 with 4 MHz crystal (el Cheapo)    *
;            - Changed the LED patterns '6' and '9' because they looked   *
;              like 'b' and 'q' in the old counter version.               *
;            - Added the auto-ranging feature                             *
;            - Stepped from 24-bit to 32-bit integer arithmetic, to be    *
;              able to count 50 MHz with 1-second gate time,              *
;              or (at least) adjust ANY result for the ANY prescaler      *
;              division ratio, which may give pretty large numbers.       *
;            - A PIC16F628 worked up to 63 MHz with this firmware.        *
;**************************************************************************

; Source code is suitable for gpasm under Debian Linux.

 PROCESSOR 16F628A

 ; set radix for constants, system default is hex
 ; hex  0x20
 ; dec  D'128' or .128
 ; bin  B'10101010'

 RADIX DEC              ; use decimal notation as default - bye-bye (ugly) leading dot

 INCLUDE <p16f628a.inc> ; processor specific definitions

 INCLUDE "macros.inc"   ; 16 and 32 bit arithmetic macros

 #DEFINE DEBUG 0        ; DEBUG=1 for simulation, DEBUG=0 for real hardware


;**************************************************************************
;                                                                         *
; Summary                                                                 *
;                                                                         *
;**************************************************************************

; The software functions as a frequency meter with an input signal
; range of 1 Hz to ~ 100 MHz and with an short term accuracy of +/- 1Hz
; if the oscillator crystal is properly trimmed.

; Signal pulses are counted over a fixed time interval of 1/4 second to
; 1 second (gate time). High frequency pulses are counted over 1/4 s
; to make the meter more responsive with no loss of displayed accuracy.

; Pulses are counted using Timer 0 of the PIC,
; which is set to increment on rising edges on the TMR0 pin. The 8-bit
; hardware register is extended by software into a 32-bit pulse counter.
; If timer 0 rolls over (msb 1 -> 0) between successive polls then the
; high two bytes of the pulse counter are incremented.

; Timer 0 is unable to count more than one pulse per instruction cycle
; (per 4 clock cycles) so the prescaler is used at frequencies above
; 1MHz (4MHz clock / 4) and also to ensure that pulses are not lost
; between polls of timer 0 (which would happen if more than 128 pulses were
; received). Fortunately the prescaler is an asynchronous counter
; which works up to a few ten MHz (sometimes as far as 60 MHz).

; Timing is based on a software loop of known execution period. The loop
; time is 20 us which gives integer counts to time 1 s  and 1/4 s.
; During this timing loop, the multiplexed LED display is updated.
;
; To enable accurate low frequency measurenents, the timing loop also measures
; the period of the signal and accumulates these values over 1 second. This is
; later converted to a frequency value for frequencies < 1000 Hz.
;
; The frequency in binary is converted to decimal using a powers-of-ten
; lookup table. The binary powers of ten are repeatedly subtracted from
; the frequency to determine the individual decimal digits. The decimal
; digits are stored at the 9 bytes at 'digits'. Leading zeroes are then
; suppressed and the 5 significant digits are converted to LED data
; for the 7-segment displays using a lookup table.

; The signal frequency is displayed on five 7-segment displays.
; The displays are multiplexed which means that only one display is enabled
; at any one time. The variable 'disp_index' contains the index of the currently
; enabled display. Each display is enabled in turn at a sufficient frequency
; that no flicker is discernable. A prescaler ('disp_timer') is used
; to set the multiplexing frequency to a few hundred Hz.

; The display shows the signal frequency in Hz, kHz or MHz
; according to the following table:

; -------------------------
; |           |  DISPLAY  |
; | Frequency | Freq mode |
; |-----------|-----------|
; | < 1 Hz    |        0  |
; | 1 Hz      |    1.000. |  Two Hz-dots are steady (three-digits mode)
; | 10 Hz     |   10.000. |  Two Hz-dots are steady (three-digits mode)
; | 1 Hz      |     1.00. |  Two Hz-dots are steady
; | 10 Hz     |    10.00. |  Two Hz-dots are steady
; | 100 Hz    |   100.00. |  Two Hz-dots are steady
; | 255.99 Hz |   255.99. |  Two Hz-dots are steady
; | 256 Hz    |    256.0. |  T.o Hz-dots are steady
; | 999.9 Hz  |    999.9. |  Two Hz-dots are steady
; | 1000 Hz   |    1.000  |  One kHz-dot is steady
; | 10.00 KHz |   10.000  |  One kHz-dot is steady
; | 100.0 KHz |   100.00  |  One kHz-dot is steady
; | 1.000 MHz |   1„0000  |  One MHz-dot is flashing
; | 10.00 MHz |   10„000  |  One MHz-dot is flashing
; | 100.0 MHz |   100„00  |  One MHz-dot is flashing
; -------------------------
; '.': steady display dot
; '„': flashing display dot
; The flashing dots change their state with the measurement rate
;
; Three-digits mode
; Frequencies < 61 Hz can optionally be displayed with three decimal places,
; e.g. "50.123", whereby the measurement rate decreases.
; To switch between two- and three-digit mode, press the key until the mode changes.
; The selection is stored in EEPROM and active until changed with button press.
; The 61 Hz is a compromise between the computing time, which increases with
; the measurement frequency, and the possibility of precisely measuring the
; global mains frequencies of 50 Hz and 60 Hz.
; If there is no signal at all, a single zero is displayed in the 5th digit.
;
; Frequency zoom
; Holding down the button at frequencies between 100 kHz and 3.2 MHz
; switches to 1 s sample time and shows the 5 lowest digits with all
; dots blinking, yielding a dispolay resolution of 1 Hz. This allows
; to adjust the counter - apply an exact frequency of 1 MHz, e.g. from
; a GPSDO and adjust the counter until the display shows "00000".
; This gives a (short-term) accuracy of 1ppm.
;
; Event counting mode
; To enter the event counting mode, hold down the button during power-up
; until the message "Count" is shown. The setting is stored in EEPROM,
; the device will now start up as an event counter. To switch back to
; frequency counter hold down the button at startup until "FrEQ" is shown.
; Events are shown with leading zeros and without dots, e.g. "01234".
; Pressing the button at any time resets the counter back to zero.


;**************************************************************************
;                                                                         *
; PIC config definitions                                                  *
;                                                                         *
;**************************************************************************


; '__CONFIG' directive is used to embed configuration data within .asm file.
; The lables following the directive are located in the respective .inc file.
; See respective data sheet for additional information on configuration word.
;
; Since 2006-05-28, the watchdog must be ENABLED in the config word
; because of its wakeup-from-sleep function (see 'Sleep100ms').
; EX(16F84:)     __CONFIG   _CP_OFF & _WDT_ON & _PWRTE_ON & _RC_OSC
; Ho-Ro: sleep is not used anymore, WDT can be switched off

; display variants 2+3 : clocked with 20 MHz (needs "HS" oscillator)
    __CONFIG   _CP_OFF & _WDT_ON & _PWRTE_ON & _HS_OSC & _LVP_OFF & _BODEN_OFF & _MCLRE_OFF
;    __CONFIG   _CP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC & _LVP_OFF & _BODEN_OFF & _MCLRE_OFF


; '__IDLOCS' directive may be used to set the 4 * 4(?!?) ID Location Bits.
; These shall be placed in the HEX file at addresses 0x2000...0x2003.
;   __IDLOCS H'1234'



;**************************************************************************
;                                                                         *
; Port assignments                                                        *
;                                                                         *
;**************************************************************************

PORT_A_IO       equ  b'0000'            ; port A I/O mode (all output)
PORT_B_IO       equ  b'00000000'        ; port B I/O mode (all output)

LEDS_PORT       equ  PORTB              ; 7-segment LEDs port

ENABLE_PORT     equ  PORTA              ; display enable port

#define PUSH_BUTTON  PORTA,5            ; digital input signal, active LOW
                                        ; switch mode, select special function,
                                        ; reset event counter


;**************************************************************************
;                                                                         *
; Constants and timings                                                   *
;                                                                         *
;**************************************************************************

; display variant 2 (the chinese clone) is clocked with 20 MHz (higher resolution)
CLOCK     equ  20000000

; microseconds per timing loop

; clocked with 20 MHz
  ; 20 microseconds is possible with 20-MHz-Crystal,
  ; Make sure all gate times can be divided by this interval without remainder :
  ; 1    second / 20 us = 50000  (ok)
  ; 1/2  second / 20 us = 25000  (ok)
  ; 1/4  second / 20 us = 12500  (ok)
  ; 1/8  second / 20 us =  6250  (ok)
  ; 1/16 second / 20 us =  3125  (ok)
;
TIME      equ  20


; Clock cycles per timing loop.  See subroutine CountPulses.
;  Usually CYCLES=400 (for 20 MHz crystal, 20 usec - loop)
CYCLES    equ  TIME*CLOCK/1000000

GATE_TIME_LOOPS equ  CLOCK/CYCLES       ; number of gate-time loops for ONE SECOND gate time

LAMPTEST_LOOPS  equ  CLOCK/(2*CYCLES)   ; number of loops for a 0.5 SECOND lamp test after power-on

COUNTMODE_LOOPS  equ  CLOCK/(10*CYCLES) ; number of delay loops for display "count" (0.1 sec)

ONESECOND       equ 50000               ; 1 second in 20 us units
PER2FREQ1       equ ONESECOND * 10      ; convert periods into HZ with 1 decimal point
PER2FREQ2       equ ONESECOND * 100     ; convert periods into HZ with 2 decimal points
PER2FREQ3       equ ONESECOND * 1000    ; convert periods into HZ with 3 decimal points


;**************************************************************************
;                                                                         *
; File register usage                                                     *
;                                                                         *
;**************************************************************************


; RAM memory (general purpose registers, unfortunately not the same for PIC16F84 & PIC16F628)
;   in PIC16F628: RAM from 0x20..0x7F   (96 bytes, 0x20.. only accessable in Bank0)
;                          0xA0..0xEF   (another 80 bytes in Bank1)
;                          0x120..0x14F (another 48 bytes in Bank2)
;   0x0F0..0x0FF, 0x170..0x17F , 0x1F0..0x1FF are mapped to 0x70..0x7F (same in all banks)
;   So use 0x70..0x7F for context saving in the PIC16F628 and forget 0x0F0.. 0xNNN !
;
;  Note on the 32-bit integer arithmetics as used in this code:
;   - They begin with MOST SIGNIFICANT BYTE in memory, but...
;   - Every byte location has its own label here, which makes debugging
;     with Microchip's simulator much easier (point the mouse on the name
;     of a variable to see what I mean !)
;
;
tens_index      equ  0x27       ; index into the powers-of-ten table
divi            equ  0x28       ; power of ten (32 bits)
divi_hi         equ  0x28       ; same as 'divi' : HIGH byte
divi_mh         equ  0x29       ; MEDIUM HIGH byte
divi_ml         equ  0x2A       ; MEDIUM LOW  byte
divi_lo         equ  0x2B       ; LOW byte

timer0_old      equ  0x2C       ; previous reading from timer0 register

gatecnt         equ  0x2D       ; 16-bit counter (msb first)
gatecnt_hi      equ  0x2D       ; same location, msb
gatecnt_lo      equ  0x2E       ; lsb

bTemp           equ  0x2F       ; temporary 8-bit register,
                                ; may be overwritten in ALL subroutines

freq            equ  0x30       ; frequency in binary (32 bits)....
freq_hi         equ  0x30       ; same location, begins with HIGH byte
freq_mh         equ  0x31       ; ... medium high byte
freq_ml         equ  0x32       ; ... medium low byte
freq_lo         equ  0x33       ; ... low byte


freq2           equ  0x34       ; frequency too,
freq2_hi        equ  0x34       ; same location, begins with HIGH byte
freq2_mh        equ  0x35       ; ... medium high byte
freq2_ml        equ  0x36       ; ... medium low byte
freq2_lo        equ  0x37       ; ... low byte


pstart_hi       equ  0x38       ; holds the gatecnt at start of period measurement (high byte)
pstart_lo       equ  0x39       ; ... low byte

t0dark          equ  0x3A       ; counter mode only: value tmr0 increased during dark time (outside CountPulses)
t0last          equ  0x3B       ; counter mode only: value of tmr0 at the end of CountPulses


menu_index      equ  0x3C       ; menu item for programming mode
menu_timer      equ  0x3D       ; used to detect how long a key was pressed

digits          equ  0x40       ; frequency as decimal digits (9 bytes)...
digit_0         equ  0x40       ; same location as MOST SIGNIFICANT digit, 100-MHz
digit_1         equ  0x41       ; usually the 10-MHz-digit
digit_2         equ  0x42       ; usually the 1-MHz-digit
digit_3         equ  0x43       ; usually the 100-kHz-digit
digit_4         equ  0x44       ; usually the 10-kHz-digit
digit_5         equ  0x45       ; usually the 1-kHz-digit
digit_6         equ  0x46       ; usually the 100-Hz-digit
digit_7         equ  0x47       ; usually the 10-Hz-digit
digit_8         equ  0x48       ; usually the 1-Hz-digit
digit_9         equ  0x49       ; must contain a blank character (or trailing zero)

display0        equ  0x4A       ; display #0 data
display1        equ  0x4B       ; display #1 data
display2        equ  0x4C       ; display #2 data
display3        equ  0x4D       ; display #3 data
display4        equ  0x4E       ; display #4 data

disp_index      equ  0x4F       ; index of the enabled display (0 to 4 for 5-digit display)
disp_timer      equ  0x50       ; display multiplex timer (5 bits)

adjust_shifts   equ  0x51       ; count of 'left shifts' to compensate prescaler+gate time

blinker         equ  0x52       ; prescaler for the flashing 1 MHz-dot

period_waste    equ  0x53       ; stores the number of cycle*4 to waste to make up the correct total
                                ; ... number of instructions in the CountPulses loop
pcnt            equ  0x54       ; number of periods measured
period_hi       equ  0x55       ; accumulated period in 20us increments (high byte)
period_lo       equ  0x56       ; ... low byte
pdiv_mh         equ  0x57       ; used to store the final division result (bits 16..23)
pdiv_ml         equ  0x58       ; ... bits 8..15
pdiv_lo         equ  0x59       ; ... bits 0..7
options         equ  0x5A       ; persistent options, stored in EEPROM
                                ; 7 6 5 4 3 2 1 0
                                ; - - - - - - x 1    = EVENT: count events, period measuring off
                                ; - - - - - - 1 x    = MILLI: switch to mHz resolution below 61 Hz
#define EVENT options, 0        ; 0 = measure frequency, 1 = count events
#define MILLI options, 1        ; 0 = resolution 10 mHz, 1 = resolution 1 mHz
#define EVENT_MASK  b'00000001'
#define MILLI_MASK  b'00000010'
#define OPTION_MASK b'00000011' ; mask device options
modebits        equ  0x5B       ; special device modes
                                ; 7 6 5 4 3 2 1 0
                                ; 0 x x x - - - -    = period measuring off (normal freq disp)
                                ; 1 0 0 0 - - - -    = PMODE: measure/show 10mHz
                                ; 1 1 0 0 - - - -    = PMODE + DEC_3: measure/show 1 mHz
                                ; 1 0 1 0 - - - -    = PMODE + DEC_1: measure/show 100 mHz
                                ; 0 0 0 1 - - - -    = FZOOM
#define PMODE modebits, 7       ; 0 = count signal edges, 1 = measure signal period
#define DEC_3 modebits, 6       ; 0 = normal, 1 = resolution 1 mHz
#define DEC_1 modebits, 5       ; 0 = normal, 1 = resolution 100 mHz
#define FZOOM modebits, 4       ; 0 = normal, 1 = measure 1 second and display lowest 5 digits
#define PMODE_MASK b'10000000'
#define DEC_3_MASK b'01000000'
#define DEC_1_MASK b'00100000'
#define FZOOM_MASK b'00010000'
#define MODES_MASK b'11110000'  ; all possible special modes


#define EEPROM_ADR_OPTIONS 0    ; EEPROM location for "options" (flags)

 org (0x2100+EEPROM_ADR_OPTIONS)
    de      .0                  ; "options" (flags), cleared by default



;**************************************************************************
;                                                                         *
; Program start                                                           *
;                                                                         *
;**************************************************************************

  ORG   0x000                   ; processor reset vector
        goto    MainInit        ; go to beginning of program
; (begin of ROM is too precious to waste for ordinary code, see below...)



;**************************************************************************
;                                                                         *
; Lookup tables                                                           *
;    Must be at the start of the code memory to avoid crossing pages !!   *
;                                                                         *
;**************************************************************************

;--------------------------------------------------------------------------
; 7-segment LED data table
;--------------------------------------------------------------------------

    ; Index 0..9 used for decimal numbers, all other indices defined below :
CHAR_A  equ     10              ; Letters A..F = HEX digits, index 10..15
CHAR_b  equ     11              ;
CHAR_C  equ     12              ;
CHAR_d  equ     13              ;
CHAR_E  equ     14              ;
CHAR_F  equ     15              ;
CHAR_o  equ     16              ; Other letters used in "Count"
CHAR_u  equ     17              ;
CHAR_n  equ     18              ;
CHAR_t  equ     19              ;
CHAR_r  equ     20              ; Other letters used in "FrEQ"
CHAR_Q  equ     21              ;
BLANK   equ     22              ; blank display
TEST    equ     23              ; power-on display test

DPPOINT_BIT equ 1               ; decimal point bit (same for all digits)
#define _A      0x40            ; bitmask for segment A , etc ..
#define _B      0x80
#define _C      0x04
#define _D      0x01
#define _E      0x08
#define _F      0x10
#define _G      0x20
#define _DP     0x02

BLANK_PATTERN equ b'00000000'   ; blank display pattern (7-segment code)


;-----------------------------------------------------------------------------
;  Table to convert a decimal digit or a special character into 7-segment-code
;   Note: In DL4YHF's PIC counter, all digits have the same segment connections,
;         so we do not need individual conversion tables for all segments.
;
;  AAAA
; F    B
; F    B
;  GGGG
; E    C
; E    C
;  DDDD   DP
;
;-----------------------------------------------------------------------------
Digit2SevenSeg:
        addwf PCL,f  ; caution: this is 'PCL' only, not 'PC'. Beware of page borders.
        ; A = 0, B = 1, C = 5, D = 3, E = 2, F = 6, G = 7, DP = 4

#define SSEG_XORMASK 0x00  ; for COMMON CATHODE: No bitwise EXOR to the pattern
        ; digits 0..9
        retlw (_A+_B+_C+_D+_E+_F   )^SSEG_XORMASK ; ABCDEF. = '0'    ( # 0  )
        retlw (   _B+_C            )^SSEG_XORMASK ; .BC.... = '1'    ( # 1  )
        retlw (_A+_B   +_D+_E   +_G)^SSEG_XORMASK ; AB.DE.G = '2'    ( # 2  )
        retlw (_A+_B+_C+_D      +_G)^SSEG_XORMASK ; ABCD..G = '3'    ( # 3  )
        retlw (   _B+_C      +_F+_G)^SSEG_XORMASK ; .BC..FG = '4'    ( # 4  )
        retlw (_A   +_C+_D   +_F+_G)^SSEG_XORMASK ; A.CD.FG = '5'    ( # 5  )
        retlw (_A   +_C+_D+_E+_F+_G)^SSEG_XORMASK ; A.CDEFG = '6'    ( # 6  )
        retlw (_A+_B+_C            )^SSEG_XORMASK ; ABC.... = '7'    ( # 7  )
        retlw (_A+_B+_C+_D+_E+_F+_G)^SSEG_XORMASK ; ABCDEFG = '8'    ( # 8  )
        retlw (_A+_B+_C+_D   +_F+_G)^SSEG_XORMASK ; ABCD.FG = '9'    ( # 9  )
        ; hexdigits A..F
        retlw (_A+_B+_C   +_E+_F+_G)^SSEG_XORMASK ; ABC.EFG = 'A'    ( # 10 )
        retlw (      _C+_D+_E+_F+_G)^SSEG_XORMASK ; ..CDEFG = 'b'    ( # 11 )
        retlw (_A+      _D+_E+_F   )^SSEG_XORMASK ; ...DE.G = 'C'    ( # 12 )
        retlw (   _B+_C+_D+_E   +_G)^SSEG_XORMASK ; .BCDE.G = 'd'    ( # 13 )
        retlw (_A      +_D+_E+_F+_G)^SSEG_XORMASK ; A..DEFG = 'E'    ( # 14 )
        retlw (_A         +_E+_F+_G)^SSEG_XORMASK ; A...EFG = 'F'    ( # 15 )
        ; A few more letters for text "count"
        retlw (      _C+_D+_E   +_G)^SSEG_XORMASK ; ..CDE.G = 'o'    ( # 16 )
        retlw (      _C+_D+_E      )^SSEG_XORMASK ; ..CDE.. = 'u'    ( # 17 )
        retlw (      _C   +_E   +_G)^SSEG_XORMASK ; ABC.EF. = 'n'    ( # 18 )
        retlw (         _D+_E+_F+_G)^SSEG_XORMASK ; ...DEFG = 't'    ( # 19 )
        retlw (           +_E   +_G)^SSEG_XORMASK ; ABC.EFG = 'r'    ( # 20 )
        retlw (_A+_B+_C+_D+_E+_F+_DP)^SSEG_XORMASK; ABC.EFG+DP = 'Q' ( # 21 )
        ; blank and test pattern
        retlw (BLANK_PATTERN       )^SSEG_XORMASK ; ....... = ' '    ( # 22 )
        retlw (b'11111111'         )^SSEG_XORMASK ; all segments on  ( # 23 )


;--------------------------------------------------------------------------
; Table to control which 7-segment display is enabled. Displays are usually
; COMMON CATHODE (variants 1+2) so pulled low to enable.
; For DISP_VARIANT=3 (COMMON ANODE), the digit-driving pattern is inverted.
; Input:   W = 0 means the MOST SIGNIFICANT DIGIT (the leftmost one), etc.
; Result:  VALUE to be written to ENABLE_PORT to activate the digit
;--------------------------------------------------------------------------
;
Digit2MuxValue:         ;
        addwf PCL, f    ; caution: this is 'PCL' only, not 'PC'
        ; Note: If the program counter is affected, a command requires two instruction cycles (=8 osc cycles)

; muliplexer values (5 digits, COMMON CATHODE) :
        retlw b'11110111'       ; 1st most significant digit is on  PA3 (!)
        retlw b'11111011'       ; 2nd next less significant dig. on PA2 (!)
        retlw b'11111110'       ; 3rd next less significant dig. on PA0 (!!)
        retlw b'11111101'       ; 4th next less significant dig. on PA1 (!!)
        retlw b'11111111'       ; 5th least significant digit = NOT (PA3+PA2+PA1+PA0)


;--------------------------------------------------------------------------
; Powers-of-ten table (32 bits, most significant byte first)
;   Based on an idea by James Hutchby (MadLab, 1996).
;   Modified for 32-bit arithmetic by Wolfgang Buescher (2004).
;--------------------------------------------------------------------------
;
TensTable:
        addwf   PCL, f
        CQUAD   100000000       ; 100 million is sufficient for the counter itself
        CQUAD   10000000
        CQUAD   1000000
        CQUAD   100000
        CQUAD   10000
        CQUAD   1000
        CQUAD   100
        CQUAD   10
        CQUAD   1

TensTableSize   equ 9*4


;**************************************************************************
;
; main entry point
;
;**************************************************************************
;
MainInit:
        movlw   PORT_A_IO               ; initialise port A
        errorlevel -302 ; Turn off banking message for the next few instructions..
        bsf     STATUS, RP0             ;! setting RP0 enables access to TRIS regs
        movwf   PORTA                   ;! looks like PORTA but is in fact TRISA
        bcf     STATUS, RP0             ;! clearing RP0 enables access to PORTs
        clrf    PORTA

        movlw   PORT_B_IO               ; initialise port B
        bsf     STATUS, RP0             ;! setting RP0 enables access to TRIS regs
        movwf   PORTB                   ;! looks like PORTB but is in fact TRISB
        bcf     STATUS, RP0             ;! clearing RP0 enables access to PORTs
        errorlevel +302 ; Enable banking message again
        clrf    PORTB

        clrf    disp_index              ; initialise display index and
        clrf    disp_timer              ; display multiplex timer

        movlw   BLANK                   ; blank character as dummy ...
        movwf   digit_8                 ; for the lowest frequency display range
IF (DEBUG==1)
        movlw   TEST                    ; test all LED segments
        call    ConvChar0
        movlw   TEST
        call    ConvChar1
        movlw   TEST
        call    ConvChar2
        movlw   TEST
        call    ConvChar3
        movlw   TEST
        call    ConvChar4

        ; Do a LAMP TEST for half a second, including all decimal points :
        MOVLx16 LAMPTEST_LOOPS, gatecnt
        call    CountPulses             ; some delay to show the test pattern
ENDIF
        movlw   PSC_DIV_BY_256          ; let the prescaler divide by 256 while testing..
        call    SetPrescaler            ; safely write <W> into option register

        clrf    modebits                ; set default mode (frequency counter)

        movlw  options                  ; destination address for reading from EEPROM..
        movwf  FSR                      ;
        movlw  EEPROM_ADR_OPTIONS       ; load EEPROM-internal offset of "options"-byte
        call   EEPROM_ReadByte          ; read single byte from EEPROM: options := EEEPROM[W]

        ; Blank the display until 1st measurement is available :
        call  ClearDisplay

        ; TheHWCave:  We use the options variable which has been restored
        ;             from EEPROM earlier, to store whether RPM or frequency
        ;             should be displayed
        movlw OPTION_MASK               ; restore only the valid options
        andwf options, f
SwitchMode:
        call  ShowMode
        movlw (LAMPTEST_LOOPS)>>8       ; high byte for 0.5 second lamp test
        movwf gatecnt_hi
        movlw (LAMPTEST_LOOPS)&0xff     ; low byte for 0.5 second lamp test
        movwf gatecnt_lo
        call  CountPulses
        clrf  bTemp                     ; marker
        btfsc PUSH_BUTTON               ; check the switch
        goto  SaveMode                  ; not pressed: save and go ...
        bsf   bTemp, 0
        movlw EVENT_MASK                ; pressed: next setting
        xorwf options, f                ; change mode
        goto  SwitchMode
SaveMode:
        btfss bTemp, 0                  ; button was not pressed and released
        goto  MainLoop                  ; .. do not write EEPROM
        movlw OPTION_MASK               ; save only the options
        andwf options, f
        movlw options                   ; .. and store in EEPROM
        movwf FSR
        movlw EEPROM_ADR_OPTIONS        ; load EEPROM-internal offset of "options"-byte
        call  SaveInEEPROM


;--------------------------------------------------------------------------
;
; main loop :  Preparation, auto ranging, measurement, conversion, display
;
;--------------------------------------------------------------------------
;
MainLoop:
        clrf    freq2_hi                ; clear counter (for counter mode)
        clrf    freq2_mh                ; bits 23..16
        clrf    freq2_ml                ; bits 15..8
        clrf    freq2_lo                ; bits  7..0
        clrf    t0dark
        clrf    t0last
        ; re-initialise ports
        ; ex: tris  PORTA;   tris  PORTB
        errorlevel -302 ; Turn off banking message for the next few instructions..
        bsf     STATUS, RP0             ;! setting RP0 enables access to TRIS regs
        movlw   PORT_A_IO               ;!
        movwf   PORTA                   ;! looks like PORTA but is in fact TRISA
        movlw   PORT_B_IO               ;!
        movwf   PORTB                   ;! looks like PORTB but is in fact TRISB
        bcf     STATUS, RP0             ;! clearing RP0 enables access to PORTs
        clrwdt                          ; configure TMR0... but clear watchdog timer first
        movlw   b'100000'               ; value for OPTION reg: edge - low-to-high transition,
                                        ;  + prescaler assigned to Timer 0, 1:2
        bsf     STATUS, RP0             ;! setting RP0 enables access to OPTION reg
        ; option register is in bank1. i know. thanks for the warning.
        movwf   OPTION_REG              ;! ex: "option" command (yucc)
        bcf     STATUS, RP0             ;! clearing RP0 for normal register access
        errorlevel +302 ; Enable banking message again

        ; check for event counting already here?
        btfsc   EVENT                   ; if in event mode
        goto    EventCount              ; .. enter event counting

        ; First do a 'range-detection measurement' to find
        ; a suitable prescaler ratio. Worst-case-estimation:
        ; 50 MHz at the input of the async TIMER 0 prescaler
        ; requires a prescaler ratio of 64 because
        ; the synchron counter in TIMER 0 accepts a maximum
        ; frequency of f_osc / 4, here: max. 1 MHz.
        ; The theoretic maximum frequency is 64 MHz then, which
        ; was almost reached when tested with a PIC 16F628.
        ; The range-detection interval is somewhere near 1/25 seconds (see RANGE_DET_LOOPS),
        ; so frequencies below 25*64 = 1600 Hz are not detectable at this step.
RANGE_DET_LOOPS equ  CLOCK/(25*CYCLES)   ; number of gate-time loops to detect the MEASURING RANGE
                                         ; (which is required to find a good prescaler value)
        MOVLx16 RANGE_DET_LOOPS, gatecnt ; RANGE DETECTION loop counter
        movlw   PSC_DIV_BY_64            ; let the prescaler divide by 64 while testing..
        call    SetPrescaler             ; safely write <W> into option register

        call    CountPulses              ; count pulses for the range detection interval (1/25 sec)
        ; The result will be placed in freq_lo,freq_ml,freq_mh,freq_hi (32 bit)
        ; but the max count at 64 MHz input, 1/25 sec gate time, and prescaler=64 will be:
        ;   64MHz / (25 * 64) = 40000 pulses, so only 16 bits in the counter
        ;  are required here (call them "testcount", f_in = testcount * 25*64).
        ; The frequency resolution of this coarse measurement is 64*25 Hz = 1.6 kHz.
        ; (for that reason it's not suited for "wake-up from power-save on frequency-change")


        ; Load the default (soft-)counters for the GATE TIME.
        ; Most measuring ranges use a 1/2 second gate time !
        MOVLx16 GATE_TIME_LOOPS/2, gatecnt  ; gate time


        ; Increment the "blinker" once every 0.25 seconds.
        ; If the gate time is longer, flashing will be slower -> showing sample rate.
        incf    blinker, f

        ; Look at the range-detection count ("testcount")
        ; and decide which measuring range to use, beginning with the highest frequency range.
        ;
        ; Even if PIC clocked with 20MHz, keep the input of TIMER0 below 4(!) MHz.
        ; Also hi and low time of the input signal must be > 2 * Tosc + 20 ns = 120 ns
        ;
        ; TheHWcave 2021-02-27:
        ; "The v2 version fixes a problem discovered by a user (Haim)
        ; of Wolf's original firmware when measuring frequencies in the range 1 .. 4 MHz.
        ; It is also present in my changed firmware. Below 4MHz is the highest frequency range
        ; the PIC measures without pre-scaler. If you measure signals with a duty cycle;
        ; significantly different from 50% in that range, the pulse width can drop;
        ; below the 120ns required by the PIC. This causes the PIC to show fluctuating values
        ; and it is quite obvious when it happens.
        ; It is more likely to happen if no pre-amp is used but can happen even with a pre-amp.
        ; The fix is to use the pre-scaler earlier (above 983KHz) because that reduces
        ; the required pulse-width significantly. This reduces the measurement resolution
        ; from 4Hz to 8Hz in the range from 1..4MHz but is no issue because we can anyway
        ; only see the top 5 digits (display resolution 100Hz)."
        ; Ranges for 20 MHz CRYSTAL (Loop time = 1/25 s)
        ; Range  testcount       f_in     presc. gate_time   display  resol. last digit step
        ; (1)      0..63      ..  100 kHz   1    1   second  XX„XXX    1 Hz  1
        ; (2)     64..255     ..  408 kHz   2    1   second  XXX„XX    2 Hz  1
        ; (2)    256..511     ..  817 KHz   2    1   second  X.XXXX    2 Hz  1
        ; (2)    512..1023    ..  1.6 MHz   2    1   second  X.XXXX    2 Hz  1
        ; (4)   1024..2047    ..  3.2 MHz   2    1/2 second  X.XXXX    4 Hz  1
        ; (8)   2048..4095    ..  6.5 MHz   4    1/2 second  X.XXXX    8 Hz  1
        ; (16)  4096..8191    ..   13 MHz   8    1/2 second  X.XXXX   16 Hz  1
        ; (32)  8192..16383   ..   26 MHz  16    1/2 second  X.XXXX   32 Hz  1
        ; (64) 16384..32767   ..   52 MHz  32    1/2 second  XX.XXX   64 Hz  1
        ; (64) 32768..40000   ..   64 MHz  32    1/2 second  XX.XXX   64 Hz  1
        ;
        movf    freq_ml, w      ; first look at bits 15..8 of the 'test count' result
        andlw   b'11000000'     ; any of bits 15..14 set (>=16384, 26 MHz..) -> no Z flag -> range 64
        btfss   STATUS, Z       ; skip next instruction if ZERO-flag set (!)
        goto    Range64         ; far jump                          ->  range 64
        btfsc   freq_ml, 5      ; bit 13 set (>=8192, 13 MHz..)     ->  range 32
        goto    Range32
        btfsc   freq_ml, 4      ; bit 12 set (>=4096, 6.5 MHZ..)    ->  range 16
        goto    Range16
        btfsc   freq_ml, 3      ; bit 11 set (>=2048, 3.2 MHz..)    ->  range 8
        goto    Range8
        btfsc   freq_ml, 2      ; bit 10 set (>=1024, 1.6 MHz..)    ->  range 4 (with "zoom" option)
        goto    Range4
        btfsc   freq_ml, 1      ; bit 9 set (>=512, 817 kHz..)      ->  range 2 (with "zoom" option)
        goto    Range2
        btfsc   freq_ml, 0      ; bit 8 set (>=256, 408 kHz)        ->  range 2 (with "zoom" option)
        goto    Range2
        movf    freq_lo, w      ; now look at bits 7..0 only ..
        sublw   62              ; subtract #62 - W register -> C=0 if result negative
        btfss   STATUS, C       ; skip next instruction if C=1 (#62-W >= 0)
        goto    Range2          ; freq > 100 kHz                    ->  range 2 (with "zoom" option)
        goto    Range1          ; .. else:                          ->  range 1

Range1_zoom:
	; come her with a frequency of 100 kHz .. 3.2 MHz
        ; measure one second and show the 5 low digits with 1Hz resolution
        ; this allows e.g. to calibrate the quartz oscillator circuit:
        ; apply exact 1 or 2 MHz - e.g. from GPSDO - and adjust the display to 00000
        ; max input frequency w/o prescaler is 4 MHz (t_hi = t_lo > 120 ns)
        bsf     FZOOM       ; use "display_calibrate" later to show low 1 Hz res

Range1:
        ; async prescaler off, 1 second gate time for low frequencies
        call    PrescalerOff    ; turn hardware prescaler off
        ; Load the GATE TIMER (as count of loops) for this measuring range.
        MOVLx16 GATE_TIME_LOOPS,gatecnt    ; 1 second gate time
        ; Load the count of "left shifts" to compensate gate time + prescaler :
        movlw   0   ; no need to multiply with prescaler 1:1 and 1-sec gate time
        goto    GoMeasure

Range2:
        btfss   PUSH_BUTTON             ; if "zoom" switch is low (pressed) ..
        goto    Range1_zoom             ; .. calibration zoom
        ; async prescaler /2 , gate time = 1 second
        movlw   PSC_DIV_BY_2            ; let the prescaler divide by 2 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        ; Load the GATE TIMER (as count of loops) for this measuring range.
        MOVLx16 GATE_TIME_LOOPS, gatecnt ; 1 second gate time
        ; Load the count of "left shifts" to compensate gate time + prescaler :
        movlw   1  ; multiply by 2 (=2^1) later to compensate prescaling (1/2)
        goto    GoMeasure

Range4:
        btfss   PUSH_BUTTON             ; if switch is low (pressed) ..
        goto    Range1_zoom             ; .. calibration zoom
        ; async prescaler /2 , gate time = default (1/2 second)
        movlw   PSC_DIV_BY_2            ; let the prescaler divide by 2 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        ; Load the GATE TIMER (as count of loops) for this measuring range.
        ; MOVLx16 GATE_TIME_LOOPS/.2, gatecnt ; 1/2 second gate time
        ; Load the count of "left shifts" to compensate gate time + prescaler :
        movlw   2   ; multiply by 4 (=2^2) later to compensate prescaling (1/2) * gate time (1/2 s)
        goto    GoMeasure

Range8:
        ; async prescaler /4, gate time = default (1/2 second)
        movlw   PSC_DIV_BY_4            ; let the prescaler divide by 4 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        movlw   3   ; multiply by 8 (=2^3) later to compensate prescaling (1/4) * gate time (1/2 s)
        goto    GoMeasure

Range16:
        ; async prescaler /8, gate time = default (1/2 second)
        movlw   PSC_DIV_BY_8            ; let the prescaler divide by 8 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        movlw   4   ; multiply by 16 (=2^4) later to compensate prescaling (1/8) * gate time (1/2 s)
        goto    GoMeasure

Range32:
        ; async prescaler /16, gate time = default (1/2 second)
        movlw   PSC_DIV_BY_16            ; let the prescaler divide by 16 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        movlw   5   ; multiply by 32 (=2^5) later to compensate prescaling (1/16) * gate time (1/2 s)
        goto    GoMeasure

Range64:
        ; async prescaler /32, gate time = default (1/2 second)
        movlw   PSC_DIV_BY_32           ; let the prescaler divide by 32 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        movlw   6   ; multiply by 64 (=2^6) later to compensate prescaling (1/32) * gate time (1/2 s)

GoMeasure:
        movwf   adjust_shifts           ; save the number of "arithmetic left shifts" for later
        ;btfsc   EVENT                   ; if in event mode
        ;goto    EventCount              ; .. enter event counting - so late?

        ; measure frequency
        call    CountPulses    ; count pulses for 1, 1/2, 1/4, or 1/8 s
        ; Result in freq_lo,freq_ml,freq_mh,freq_hi (32 bit) now,
        ; NOT adjusted for the gate-time or prescaler division ratio yet.

PrepDisp: ; Prepare the frequency (32-bit 'unadjusted' integer) for display:
        ; Multiply freq by 2^adjust_shifts to adjust for the prescaling
        ; and the timing period.  The result will be a frequency in HERTZ, 32-bit integer.
        ; Note: the adjustment factor may be ONE which means no shift at all.
        tstf    adjust_shifts
        BZ      NoAdjust
Adjust: clrc
        rlf     freq_lo, f
        rlf     freq_ml, f
        rlf     freq_mh, f
        rlf     freq_hi, f
        decfsz  adjust_shifts, f
        goto    Adjust

NoAdjust: ; Check the result against under- and overflow.
        ; (There should be none if the frequency didn't change too rapidly
        ;  between the range-detection and the actual measurement )
        movf    freq_hi, w      ; underflow (freq = 0) ?
        iorwf   freq_mh, w
        iorwf   freq_ml, w
        iorwf   freq_lo, w
        BZ      FreqUnderflow  ; show "    0"

        btfsc   freq_hi, 7      ; if overflow (freq > 7fffffffh) ?
        goto    FreqOverflow    ; show "E    "

        MOVLx32 999, freq2
        SUBx32  freq, freq2     ; C = ( freq < 1000 )
        BNC     F_Hirange       ; frequencies >= 1000 Hz go to extended range check
        ;  frequency  is < 1000 Hz.
P_Zero: movf    pcnt, f         ; zero periods? This could happen at very low
        btfss   STATUS, Z       ; .. freqencies < 2Hz.
        goto    P_Conv          ; .. we must avoid dividing by zero
        movlw   1               ; .. by preset to 1 x 1s period = 1Hz
        movwf   pcnt
        movlw   (ONESECOND>>8) & 0xff
        movwf   period_hi
        movlw   ONESECOND & 0xff
        movwf   period_lo

        ; TheHWcave:  This section adjusts the conversion factor for the number
        ;             of periods we have measured. When we later divide by the
        ;             sum of the measured periods, we effectively average
        ;             across the number of periods
        ;
        ;             conversion = conversion * number_of_periods (pcnt)
        ;
        ;             the multiplication is done by repeated adding and it gets
        ;             big, so 32-bit mode is needed
        ;
        ;
P_Conv:                         ; come here if f < 1000 Hz
        iorwf   freq_ml,f       ; if > 255 Hz?
        BNZ     P_Conv_1        ; convert with 1 decimal digit
        bcf     DEC_1
        movlw   62              ; limit to < 62 Hz
        subwf   freq_lo, w      ; .. to examine utility frequencies 50 & 60 Hz
        BC      P_Conv_2        ; skip if f >= 62 Hz

        btfsc   PUSH_BUTTON     ; check the switch
        goto    P_Conv_NoBut

        movlw   MILLI_MASK
        xorwf   options, f      ; .. if pressed toggle MILLI mode
        movlw   OPTION_MASK     ; save only the options
        andwf   options, f
        movlw   options         ; .. and store in EEPROM
        movwf   FSR
        movlw   EEPROM_ADR_OPTIONS ; load EEPROM-internal offset of "options"-byte
        call    SaveInEEPROM


P_Conv_NoBut

        btfss   MILLI           ; check if MILLI mode
        goto    P_Conv_2        ; .. not set: goto normal 2 decimal mode
        bsf     DEC_3           ; set mode bit

P_Conv_3
        ; calculate frequency with 3 decimal digits
        MOVLx32 PER2FREQ3, freq2 ; conversion for resolution 1 mHz
        goto    P_Conv_end

P_Conv_2
        ; calculate frequency with 2 decimals digits
        bcf     DEC_3           ; exit very low mode
        MOVLx32 PER2FREQ2, freq2 ; conversion for resolution 10 mHz
        goto    P_Conv_end

P_Conv_1
        ; calculate frequency with 1 decimals digit
        bcf     DEC_3           ; exit very low mode
        bsf     DEC_1           ; set medium low mode
        MOVLx32 PER2FREQ1, freq2 ; conversion for resolution 100 mHz

P_Conv_end:
        clrf    freq_hi         ; we re-use the frequency buffer for the 32-bit result
        clrf    freq_mh         ; .. but need to clear it first
        clrf    freq_ml
        clrf    freq_lo

P_Mul:  ; freq := pcnt * freq2 (8bit * 32bit -> 32bit)
        ADDx32  freq2, freq ; freq = freq + freq2
        decf    pcnt, f
        btfss   STATUS, Z
        goto    P_Mul
        ; P_Mul end

        ; TheHWcave:  This section divides the adjusted conversion factor "freq" by the
        ;             sum of the periods "period". This is a 32-bit divided by 16-bit
        ;             operation using repeated 32-bit subtraction. The result is
        ;             a 24-bit number "freq" which is the desired frequency (in millihertz).
        ;
        ;             This calculation takes so much time that the watchdog timer
        ;             would trigger and because the display multiplexing is stopped
        ;             the last digit would be extremly bright and destroy the display
        ;             I tried turning the display off, which works but causes a very
        ;             irritating blinking display. So .. there is no choice, we have
        ;             to keep multiplexing the display during the division which
        ;             makes the calculation even slower..

        clrf    freq2_hi
        clrf    freq2_mh
        movf    period_hi, w
        movwf   freq2_ml
        movf    period_lo, w
        movwf   freq2_lo
        clrf    pdiv_mh
        clrf    pdiv_ml
        clrf    pdiv_lo
P_Div:
        call    RefreshDisplay
        SUBx32  freq2, freq     ; freq = freq - freq2
        btfss   STATUS, C       ;
        goto    P_DivEnd        ; C = 0 -> ready
        incf    pdiv_lo, f      ; incx24
        btfsc   STATUS, Z
        incf    pdiv_ml, f
        btfsc   STATUS, Z
        incf    pdiv_mh, f
        goto    P_Div
P_DivEnd:
        clrf  freq_hi           ; copy the result back into the freq_xx variable
        movf    pdiv_mh, w
        movwf   freq_mh
        movf    pdiv_ml, w
        movwf   freq_ml
        movf    pdiv_lo, w
        movwf   freq_lo
        bsf     PMODE           ; switch to display format XXX.XX Hz
        ; Div end

F_Hirange:
        call    CvtAndDisplayFreq ; Convert <freq> into BCD and show it on the display
        goto    MainLoop        ; end of main loop


;--------------------------------------------------------------------------
; frequency underflow in MainLoop (frequency < 1Hz)
;--------------------------------------------------------------------------
;
FreqUnderflow:
        movlw   BLANK           ; display underflow as "    0"
        call    ConvChar0
        movlw   BLANK
        call    ConvChar1
        movlw   BLANK
        call    ConvChar2
        movlw   BLANK
        call    ConvChar3
        movlw   0
        call    ConvChar4

        goto    MainLoop


;--------------------------------------------------------------------------
; frequency overflow in MainLoop (frequency > XX MHz)
;--------------------------------------------------------------------------
;
FreqOverflow:
        movlw   CHAR_E          ; display overflow as "E    "
        call    ConvChar0
        movlw   BLANK
        call    ConvChar1
        movlw   BLANK
        call    ConvChar2
        movlw   BLANK
        call    ConvChar3
        movlw   BLANK
        call    ConvChar4

        goto    MainLoop        ; end of main loop



;--------------------------------------------------------------------------
;
; we are in event counter mode
;
;--------------------------------------------------------------------------
;
EventCount:
        ; Load the GATE TIMER (as count of loops) for this measuring range.
        MOVLx16 GATE_TIME_LOOPS/10, gatecnt ; 1/10 second gate time
        call    CountPulses                 ; count pulses for 1/10s
        ; Result in freq_lo,freq_ml,freq_mh,freq_hi (32 bit) now
        ADDx32  freq, freq2 ; freq2 = freq2 + freq
        ;
        ; add any counts that happened during "dark time"
        movf    t0dark, w
        movwf   freq_lo
        clrf    freq_ml
        clrf    freq_mh
        clrf    freq_hi
        ADDx32  freq, freq2     ; freq2 = freq2 + freq
        ; copy result back into freq for display
        movf    freq2_hi, w
        movwf   freq_hi
        movf    freq2_mh, w
        movwf   freq_mh
        movf    freq2_ml, w
        movwf   freq_ml
        movf    freq2_lo, w
        movwf   freq_lo
        ; check for overflow 100000  = 0x0186A0
        ; if so, reset counter
        movf    freq_mh, w
        sublw   0x01
        BNC     cntreset        ; >= 0x02xxxx = must reset
        BNZ     cntvalid        ; <= 0x00xxxx = ok
        movf    freq_ml, w
        sublw   0x86
        BNC     cntreset        ; >= 0x0187xx = must reset
        BNZ     cntvalid        ; <= 0x0185xx = ok
        movf    freq_lo, w
        sublw   0x9F
        BNC     cntreset        ; >= 0x0186A0 = must reset
cntvalid:
        call    CvtAndDisplayFreq

        btfsc   PUSH_BUTTON     ; check the switch
        goto    EventCount      ; not pressed: keep counting...
cntreset:
        clrf    freq2_hi        ; reset counter
        clrf    freq2_mh        ; bits 23..16
        clrf    freq2_ml        ; bits 15..8
        clrf    freq2_lo        ; bits  7..0
        clrf    t0dark
        clrf    t0last
        goto    EventCount


;--------------------------------------------------------------------------
; Count pulses, fed with the number of loop iterations for the gate time.
;           WHILE counting, the multiplexed LED display is updated.
;           Watchdog is fed in this loop!
; Input:    Count of gate-time-loops in 'gatecnt_hi'+'gatecnt_lo' (16 bit).
; Returns:  The number of pulses in 20us increments
;           The number of periods measured (in pcnt)               (added TheHWcave )
;           the sum of all measured periods in 20us increments     (added TheHWcave )
;           in t0dark the number of pulses tmr0 counted outside this routine (added TheHWcave )
;           in t0last the last value of tmr0 when leaving this routine (added TheHWcave )
;--------------------------------------------------------------------------
;
CountPulses:
        clrf    freq_hi         ; clear pulse counter (bits 31..24)
        clrf    freq_mh         ; bits 23..16
        clrf    freq_ml         ; bits 15..8
        clrf    freq_lo         ; bits  7..0

        clrf    period_hi       ; clear period accumulator bits 8..15
        clrf    period_lo       ; bits 0..7
        clrf    pcnt            ; clear number of periods measured
        bcf     PMODE           ; turn period conversion off by default
                                ; until we are sure the input frequency
                                ; is low enough for this to make sense

        clrf    timer0_old      ; 'old' value of timer0 to detect toggling MSB
        ;
        ; in counter mode, check if TMR0 moved since the last time and record
        ; the difference in t0dark
        btfss   EVENT           ; ifnot EVENT
        goto    count0          ; then proceed as normal
        ;
        ; calculate the number of pulses missed while being ouside this routine
        ; store the result in t0dark
        movf    TMR0, w
        movwf   t0dark
        movf    t0last, w
        subwf   t0dark,f
count0:
        clrf    TMR0            ; timer register (PIC's hardware timer, 8 bit)
        nop                     ; 2 instruction cycle delay
        nop                     ; after writing to TMR0  (MPLAB-SIM: set breakpoint + clear stopwatch here)


; --------------- start of critial timing loop >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; The following timing loop must take a well-defined time in total per
; iteration, usually 20 microseconds, which can be precisely achieved
; with a 20-MHz-crystal.
; This gives a basic delay for the frequency counter's gate time.
;    The frequency at the input of TIMER 0 (not the prescaler)
;    can not exceed f_crystal / 4,
;    and every HIGH->LOW transition of bit7 in TIMER0 must be polled here.
;  This is safe because ..
;    With a 20-MHz-crystal, Timer0 can count up to 4 MHz input,
;    MSB toggles every (128/4MHz) = 32 us, polled every 20us -> ok.

;  The numbers in square brackets below are the INSTRUCTION NUMBER within the loop.
;  (not the count of oscillator cycles for a single command, which is always 4).
;  These values can be checked with the "Stopwatch" function in MPLAB-SIM.
;  The goal is to let this loop take EXACTLY <TIME> microseconds (50us or 20us).

count1  movf    disp_index, w   ; [1] get the current digit number (disp_index = 0..4)
        call    Digit2MuxValue  ; [2,3,4,5,6,7] display (6 commands including call+retlw)
        movwf   bTemp           ; [8] save the bit pattern for the multiplexer port
        movlw   display0        ; [9]  get the LED display data for the current digit...
        addwf   disp_index, w   ; [10] add current digit number to address of LED data
        movwf   FSR             ; [11] move address into the PIC's poor 'data pointer'
        movf    INDF, w         ; [12] w := *(FSR) use indirection register to read from table
        movwf   LEDS_PORT       ; [13] set the LED segments
        movf    bTemp, w        ; [14] get the mupliplexer pattern (hurry, hurry !)
        movwf   ENABLE_PORT     ; [15] set the LED multiplexer

        incf    disp_timer, f   ; [16] increment display-multiplex timer
        btfsc   disp_timer, 6   ; [17] (6-bit prescaler)
        incf    disp_index, f   ; [18] next display if rolled over
        bcf     disp_timer, 6   ; [19] limit disp_timer to 6 bits (!)
        movf    disp_index, w   ; [20] limit display index to  0...4
        sublw   4               ; [21] subtract #4 - W register -> C=0(!) if result negative (W>4)
        btfss   STATUS, C       ; [22] skip next instruction if C=1 (#4-W >= 0)
        clrf    disp_index      ; [23] if C=0 (disp_index>4) then disp_index=0

; the following fragments of code always take the same number of clock
; cycles to execute, irrespective of whether the skips take place or not.
; Here still in 'CountPulses'.

        movf    TMR0, w         ; [24] read least significant byte of
        movwf   freq_lo         ; [25]  pulse counter (bits 7..0)


        movlw   1               ; [26] determine if timer 0 has rolled
        btfss   timer0_old, 7   ; [27] over (rolled over if msb was
        clrw                    ; [28] previously set and now isn't)
        btfsc   freq_lo, 7      ; [29]
        clrw                    ; [30]

        addwf   freq_ml, f      ; [31] increment high bytes of pulse counter
        skpnc                   ; [32] if low byte rolled over
        incf    freq_mh, f      ; [33] (mh = "medium high byte" of counter)
                                ; NOTE: we are not modifying freq_hi here!
                                ;       Bits 31..24 may be used later when multiplying with some factor
                                ;       (2^n) to compensate for the ASYNCHRON PRESCALER!

        btfsc   freq_mh, 7      ; [34] overflow (freq > 7fffffh)?
        goto    count3          ; [35+1] stop if yes

        nop                     ; [36]
        nop                     ; [37]
        ;movfw   freq_lo        ; [36] save previous value from timer 0
        ;movwf   timer0_old     ; [37]

        tstf    gatecnt_lo      ; [38] check inner gate-time counter, LOW byte
        skpnz                   ; [39] only decrement h-byte if l-byte zero
        decf    gatecnt_hi, f   ; [40]  decrement gate-time counter, HIGH byte
        decf    gatecnt_lo, f   ; [41] always decrement gate-time counter, LOW byte


        ; 100 instruction cycles per loop
        ; (f_xtal=20 MHz, t_loop=20us, t_instr=4/20MHz=0.2us)
        ;
        clrwdt                  ; [42] (ex: nop, but since 2006-05-28 the dog must be fed!)
        ;

        ; TheHWcave:	Measure period(s) in 20 uS units
        ;               The original software aready kept a copy of the timer0 content
        ;               at the previous loop, so we can use this to detect when the counter
        ;               has changed. Because we are only interested in very low frequencies
        ;               we can safely detect a single increment and there will be lots of loops
        ;               with no change at all. The loop count (gatecnt) is counting down
        ;               from 50000 (=1 second) and each count represents 20 us
        ;
        ;	        The idea is that if freq_lo changes to 1 we take copy of the gatecnt in pstart
        ;	        and for every subsequent frequency change, we calculate the difference between the current
        ;               gatecnt and pstart which is the accumulated elapsed time (period) in 20 us units
        ;               units. We also keep a counter pcnt to allow calculating the average period later
        ;               for better resolution
        ;
        ;               Note 1: We can only start measuring after the frequency counter value has reached
        ;               1 because the time from 0 to 1 is undetermined (the external signal is asynchronous)
        ;               This means we need at least 1 Hz for this to produce period measurements.
        ;
        ;		Note 2: This method could create a pcnt that can get too high for the subsequent 32-bit maths
        ;	        Therefore there is a check to stop accumulating periods and incrementing pcnt if
        ;               80 (in frequency mode) or 140 (in RPM) mode is reached
        ;
        ;
        ;
        movlw   10              ; [43] prepare to add 10x4 =40 wasted instructions later 50+40=90
        movwf   period_waste    ; [44]
        movf    freq_lo, w      ; [45]
        xorwf   timer0_old, w   ; [46]    XOR of old and new counter.
        btfsc   STATUS, Z       ; [47]    zero means no change
        goto    P_done          ; [48+1]
        ;
        movlw   1               ; [49]
        subwf   freq_lo, w      ; [50]
        btfss   STATUS, Z       ; [51]
        goto    P_Measure       ; [52+1]

        ; first transition: take a copy of the current gatecnt
        movf    gatecnt_lo, w   ; [53]
        movwf   pstart_lo       ; [54]
        movf    gatecnt_hi, w   ; [55]
        movwf   pstart_hi       ; [56]
        movlw   7               ; [57] prepare to add 7x4 =28 wasted instructions later 62+28=90
        movwf   period_waste    ; [58]
        nop                     ; [59]
        goto    P_done          ; [60+1]

        ; subsequent transitions: (2, 3, 4..) calculate the accumulated elapsed time and copy to period
P_Measure:
        movlw   6               ; [54] prepare to add 6x4 =24 wasted instructions later 66+24=90
        movwf   period_waste    ; [55]
        ;
        ; check if pcnt has reached maximum. If yes, don't add amy further periods
        ;
        nop                     ; [56]
        nop ;movfw   modebits   ; [57]   are we in RPM or frequency mode?
        nop ;andlw   0x07       ; [58]
        nop ;btfss   STATUS, Z  ; [59]
        nop ;movlw   .60        ; [60]           RPM: pre-load W with 60   freq: W is already 0
        movlw   255 ;addlw .80  ; [61]   add 80: RPM  W is 60+80=140       freq: W is 0+80 = 80
        subwf   pcnt, w         ; [62] W = pcnt - W
        btfsc   STATUS, C       ; [63]
        goto    P_done          ; [64+1]  stop if pcnt >= 80 (freq) or >= 140 (rpm)


        ;  period = pstart - gatecnt  (period=pstart; period=period-gatecnt)
        ;
        nop                     ; [65]
        movlw   2               ; [66] prepare to add 2x4 =8 wasted instructions later 82+8=90
        movwf   period_waste    ; [67]
        movf    pstart_hi, w    ; [68] period = pstart
        movwf   period_hi       ; [69] "
        movf    pstart_lo, w    ; [70] "
        movwf   period_lo       ; [71] "
        bsf     STATUS, C       ; [72] ; 16-bit subtraction with carry
        movf    gatecnt_lo, w   ; [73]
        btfss   STATUS, C       ; [74]
        incfsz  gatecnt_lo, w   ; [75]
        subwf   period_lo, f    ; [76]
        movf    gatecnt_hi, w   ; [77]
        btfss   STATUS, C       ; [78]
        incfsz  gatecnt_hi, w   ; [79]
        subwf   period_hi, f    ; [80]
        ;
        incf    pcnt, f         ; [81]


P_done: movf    period_waste, w ; [50, 62, 66, 82  ]
WasteT1:addlw   0xFF            ; [51,    .. ]
        btfss   STATUS, Z       ; [52,    .. ]      eats 4(!) INSTRUCTION CYCLES per loop
        goto    WasteT1         ; [53+1 , .. ]
                        ; Check this with MPLAB-SIM: here, after loop. below should always be [90]
        movf    freq_lo, w      ; [90] save previous value from timer 0
        movwf   timer0_old      ; [91]

        nop                     ; [92]
        nop                     ; [93]
        nop                     ; [94]
        nop                     ; [95]
        movf    gatecnt_hi, w   ; [96] counter = 0 ?
        iorwf   gatecnt_lo, w   ; [97]
        skpz                    ; [98]
        goto    count1          ; [99,50]  goto always takes TWO instruction cycles


; <<<<<<<<<<<<<<<<<<<<<<<< end of timing loop -----------------------------


        movf    TMR0, w         ; get final value from timer 0
        movwf   freq_lo
        movwf   t0last          ; save it for counter mode
        movlw   1               ; determine if timer 0 has rolled
        btfss   timer0_old, 7   ; over (rolled over if msb was
        clrw                    ; previously set and now isn't)
        btfsc   freq_lo, 7
        clrw

        addwf   freq_ml, f      ; increment high bytes of pulse
        skpnc                   ; counter if low byte rolled
        incf    freq_mh, f      ; over

count3  retlw   0
; end of routine 'CountPulses'.   Result now in   freq_lo..freq_hi.



;--------------------------------------------------------------------------
;
; Convert *FSR (32 bit) into BCD and show it on the display.
;  Input :  INDF = *FSR, 32-bit integer.
;  Bad side effect : CONTENTS OF <freq> will be lost!
;  Currently unused, but I keep it for the future
;
;--------------------------------------------------------------------------
;
IF 0
ShowInt32_FSR   ; Convert <*FSR> (32 bit integer) to 8 BCD-digits ...
        movf   INDF, w          ; W   := *FSR   , load LOW byte
        incf   FSR , f          ; FSR := FSR + 1
        movwf  freq             ; freq.hi := W
        movf   INDF, w          ; W   := *FSR   , load MIDDLE LOW byte
        incf   FSR , f          ; FSR := FSR + 1
        movwf  freq+1           ; freq.mh := W
        movf   INDF, w          ; W   := *FSR   , load MIDDLE HIGH byte
        incf   FSR , f          ; FSR := FSR + 1
        movwf  freq+2           ; freq.ml := W
        movf   INDF, w          ; W   := *FSR   , load HIGH byte
        incf   FSR , f          ; FSR := FSR + 1
        movwf  freq+3           ; freq.lo := W
        ; continue with CvtAndDisplayFreq !
ENDIF

;--------------------------------------------------------------------------
;
; Convert <freq> into BCD and show it on the display.
;  Input :  freq, 32-bit integer.  CONTENTS OF <freq> will be lost!
;
;--------------------------------------------------------------------------
;
CvtAndDisplayFreq  ; Convert <freq>(32 bit integer) to 9 BCD-digits ...
        ;
        movf    modebits, w     ; check special modes
        andlw   MODES_MASK      ; any of bits 7654...0 set -> no Z flag -> NoRound
        btfss   STATUS, Z       ; skip next instruction if standard freq counter mode
        goto    NoRound         ; .. else do not round the value
        ;
        ; put rounding code here
        ; add 5 to the digit right below the lowest visible digit
        ;
        ;   if ( freq  >= 10000000 )    // shows nn.nnn (MHz), lowest digit is 1000 Hz
        ;       freq += 500;            // .. -> add 500 Hz
        ;   else if ( freq >= 1000000 ) // shows n.nnnn (MHz), lowest digit is 100 Hz
        ;       freq += 50;             // .. -> add 50 Hz
        ;   else if ( freq >= 100000 )  // shows as nnn.nn (kHz), lowest digit is 10 Hz
        ;       freq += 5;              // .. -> add 5 Hz
        ;
        MOVLx32 9999999, freq2
        SUBx32  freq, freq2     ; freq2 = freq2 - freq; // C set when freq <= freq2
        bc      check1M         ; C = ( freq < 10M ) goto next check 1M
        clrf    freq2_hi
        clrf    freq2_mh
        movlw   (500 >> 8) & 0xff
        movwf   freq2_ml
        movlw   500 & 0xff
        movwf   freq2_lo
        ADDx32  freq2,freq      ; freq += 500;
        goto    NoRound

check1M:
        MOVLx32 999999, freq2
        SUBx32  freq, freq2     ; C = ( freq < 1M )
        bc      check100k       ; goto next check 100k
        clrf    freq2_hi
        clrf    freq2_mh
        clrf    freq2_ml
        movlw   50
        movwf   freq2_lo
        ADDx32  freq2, freq     ; freq += 50;
        goto    NoRound

check100k:
        MOVLx32 99999, freq2
        SUBx32  freq, freq2     ; C = ( freq < 100k )
        bc      NoRound         ; goto end
        clrf    freq2_hi
        clrf    freq2_mh
        clrf    freq2_ml
        movlw   5
        movwf   freq2_lo
        ADDx32  freq2, freq     ; freq += 5;

NoRound:
        clrf    tens_index      ; initialise the table index

        movlw   digits          ; initialise the indirection register
        movwf   FSR             ; ( FSR="pointer"; *FSR=INDF)

conv1   ; Loop for ALL POWERS OF TEN in the lookup table..
        clrwdt                  ; feed the watchdog (may stay a bit longer)
        movf    tens_index, w   ; fetch the next power of ten
        call    TensTable       ; (32 bits) from the lookup table
        movwf   divi+0          ; and store in divi
        incf    tens_index, f   ; this was the HIGH byte

        movf    tens_index, w
        call    TensTable
        movwf   divi+1
        incf    tens_index, f   ; this was the MIDDLE-HIGH byte

        movf    tens_index, w
        call    TensTable
        movwf   divi+2
        incf    tens_index, f   ; this was the MIDDLE-LOW byte

        movf    tens_index, w
        call    TensTable
        movwf   divi+3
        incf    tens_index, f   ; and this was the LOW-byte of a power of ten

        ; ex: clrf 0  ; clear the decimal digit .. but address ZERO is called 'INDF' these days !
        clrf    INDF            ; *FSR = 0

conv2   ; Loop to repeatedly subtract divi from freq (32-bit subtract)
        ;         until underflow while incrementing the decimal digit.
        SUBx32  divi, freq      ; freq := freq - divi  (with divi = 10 power N)
        BNC     conv3           ;
        incf    INDF, f         ;    The RESULT will be written back to freq,
        goto    conv2           ;    in other words 'freq' will be lost !

conv3   ADDx32  divi, freq      ; freq := freq+divi;  ready for next digit
        incf    FSR, f          ; step to next decimal digit
        movlw   TensTableSize   ; 9 x 4-byte entries in TensTable
        subwf   tens_index, w
        BNZ     conv1           ; loop until end of table

;--------------------------------------------------------------------------
; displays the frequency in decimal
;--------------------------------------------------------------------------
;
; Display the decimal digits according to the following rules
; input: decimal values in digit_0 (leftmost) .. digit_8 (rightmost); digit_9 = 0
;
; PMODE == 1 + DEC_3 == 1: very low freq period measurements with 3 decimals
; 012345678
; 00000Abcd =>  A.bcd. ' 1.000' to ' 9.999' Hz     1 Hz =  steady points at digit 5 and 8
; 0000ABcde => AB.cde. '10.000' to '60.999' Hz    10 Hz =  steady points at digit 5 and 8
;
; PMODE == 1: low freq period measurements with 2 decimals
; 012345678
; 000000Abc =>   A.bc. '  1.00' to '  9.99' Hz     1 Hz =  steady points at digit 6 and 8
; 00000ABcd =>  AB.cd. ' 10.00' to ' 99.99' Hz    10 Hz =  steady points at digit 6 and 8
; 0000ABCde => ABC.de. '100.00' to '249.99' Hz   100 Hz =  steady points at digit 6 and 8
;
; PMODE == 1 + DEC_1 == 1: freq period measurements with 1 decimal
; 012345678
; 00000ABCd => ABC.d. ' 250.0' to ' 999.9' Hz  250 Hz =  steady points at digit 7 and 8
;
; PMODE = 0: freq counter measurements
; 012345678
; 000000ABC =>  0.ABC  ' 0.250' to ' 0.999' KHz     kHz = steady point at digit 5 - still valid?
; 00000ABCD =>  A.BCD  ' 1.000' to ' 9.999' KHz    1kHz = steady point at digit 5
; 0000ABCDE => AB.CDE  '10.000' to '99.999' KHz   10kHz = steady point at digit 5
; 000ABCDEF => ABC.DE  '100.00' to '999.99' KHz  100KHz = steady point at digit 5
; 00ABCDEFG => A:BCDE  '1.0000' to '9.9999' Mhz    1MHz = blinking point at digit 2
; 0ABCDEFGH => AB:CDE  '10.000' to '99.999' Mhz   10MHz = blinking point at digit 2
; ABCDEFGHI => ABC:DE  '100.00' to '999.99' Mhz  100MHz = blinking point at digit 2
;
; PMODE = 0 + FZOOM: freq counter zoom on 5 lowest digits
; 012345678
; XXXXABCDE => A:B:C:D:E:  5 blinking points at all digit
;
; PMODE = 0 + EVENT: event counter mode
; 012345678
; XXXXABCDE => ABCDE  5 digits with leading zeros

; . = steady point
; : = blinking point

;
DisplayFreq:
        btfss   PMODE           ; ifnot period measurement mode
        goto    display_f_int   ; .. go to normal display
        btfsc   DEC_3           ; if very low frequency
        goto    display_f_3dec  ; .. go to display with 3 decimals
        btfsc   DEC_1           ; if medium low frequency
        goto    display_f_1dec  ; .. go to display with 1 decimals

        ; else display frequencies with two decimals up to "999.99 Hz" (theoretical):
display_f_2dec:
        movlw   digit_4         ; find the first significant digit starting from d4 ..
        movwf   FSR             ; .. by stepping over leading zeroes
        tstf    INDF            ; INDF = *(FSR) in "C" syntax, FSR points to 'digits'
        BNZ     d_f2d1          ; 100-Hz-digit non-zero, show frequency in Hz [XXX.xx]
        movlw   BLANK
        movwf   INDF            ; else blank the leading zero
        incf    FSR, f          ; check next digit
        tstf    INDF
        BNZ     d_f2d1          ; 10-Hz-digit non-zero, show frequency in Hz [ XX.xx]
        movwf   INDF            ; else blank also this leading zero
                                ; and show frequency in Hz [  X.xx]
d_f2d1: bsf     digit_6, 7      ; set two steady points around the 2 decimals ..
        bsf     digit_8, 7      ; .. indicating Hz
        goto    display_5_lowest ; and show the result right aligned

        ; Display routine for frequencies with one decimal up to "9999.9 Hz" (theoretical):
display_f_1dec:
        movlw   digit_4         ; find the first significant digit starting from d4 ..
        movwf   FSR             ; .. by stepping over leading zero
        tstf    INDF            ; INDF = *(FSR) in "C" syntax, FSR points to 'digits'
        BNZ     d_f1d1          ; 1000-Hz-digit non-zero, show frequency in Hz [XXXX.x]
        movlw   BLANK
        movwf   INDF            ; else blank the leading zero
                                ; and show frequency in Hz [ XXX.x]
d_f1d1: bsf     digit_7, 7      ; set two steady points around the decimal ..
        bsf     digit_8, 7      ; .. indicating Hz
        goto    display         ; and show the result right aligned

        ; Display routine for very low frequencies with three decimals up to "99.999 Hz" (theoretical):
display_f_3dec:
        movlw   digit_4         ; find the first significant digit starting from d4 ..
        movwf   FSR             ; .. by stepping over leading zeroes
        tstf    INDF            ; INDF = *(FSR) in "C" syntax, FSR points to 'digits'
        BNZ     d_f3d1          ; 10-Hz-digit non-zero, show frequency in Hz [XX.xxx]
        movlw   BLANK
        movwf   INDF            ; else blank the leading zero
                                ; and show frequency in Hz [ X.xxx]
d_f3d1: bsf     digit_5, 7      ; set two steady points around the 3 decimals ..
        bsf     digit_8, 7      ; .. indicating Hz
        goto    display         ; and show the result right aligned

        ; Display routine for integer frequencies from 1 Hz up to 999.99 MHz (theoretical):
        ; (do NOT insert the decimal point yet,
        ;   it would disturb the blanking of LEADING zeroes )
display_f_int:
        bcf     DEC_3           ; leave very low mode
        btfsc   EVENT           ; if event mode
        goto    display_5_lowest ; .. show 5 rightmost digits without point
        btfsc   FZOOM           ; if calibrate mode
        goto    display_calibrate ; .. show 5 rightmost digits with 2 points alternating
        movlw   digits          ; else find the first significant digit ..
        movwf   FSR             ; .. by stepping over leading zeroes
        tstf    INDF            ; INDF = *(FSR) in "C" syntax, FSR points to 'digits'
        BNZ     displ_d0        ; 100-MHz-digit non-zero, show frequency in MHz [XXX:XX]
        incf    FSR, f          ; otherwise skip 1st digit (the 100-MHz place)
        tstf    INDF            ; INDF = *(FSR) in "C" syntax, FSR points to 'digits'
        BNZ     displ_d1        ; 10-MHz-digit non-zero, show frequency in MHz [XX:XXX]
        incf    FSR, f          ; otherwise skip 2nd digit (the 10-MHz place)
        tstf    INDF
        BNZ     displ_d2        ; 1-MHz-digit non-zero, show frequency in MHz [X:XXXX]
        incf    FSR, f          ; otherwise skip 3rd digit (the 1-MHz place)
        tstf    INDF
        BNZ     displ_d3        ; 100-kHz-digit non-zero, show frequency in kHz [XXX.XX]
        incf    FSR, f          ; otherwise skip 4th digit (the 100-kHz place)
        tstf    INDF
        BNZ     displ_d4        ; 10-kHz-digit non-zero, show frequency in kHz [XX.XXX]
                                ; otherwise show the frequency in kHz [ X.XXX]
displ_d5:       ; 1000 Hz .. 9999 Hz, FSR points at digit_3 (10-kHz-digit)
        bsf     digit_5, 7      ; set one decimal point indicating kHz
        movlw   BLANK           ; show right aligned as [ X.XXX]
        goto    display_w

displ_d4:       ;  10000 ..  99999
displ_d3:       ; 100000 .. 999999
        ; left aligned with STEADY POINT to indicate the kilohertz-digit
        bsf     digit_5, 7      ; set one decimal point indicating kHz
        goto    display

displ_d2:
displ_d1:
displ_d0:
        ; left aligned with BLINKING POINT at megahertz-digit
        btfsc   blinker, 0      ; check the blink flag (bit 0) for the MHz-point
        bsf     digit_2, 7      ; blink the decimal point indicating MHz
        goto    display

display_calibrate:              ; cal. mode: blink all dots
        bcf     FZOOM           ; clear FZOOM mode
        btfss   blinker, 0      ; if blink flag clear
        goto    display_5_lowest
                                ; if blink flag set
        bsf     digit_4, 7      ; .. set topmost dot
        bsf     digit_5, 7      ; .. set 2nd dot
        bsf     digit_6, 7      ; .. set 3rd dot
        bsf     digit_7, 7      ; .. set 4th dot
        bsf     digit_8, 7      ; .. set 5th dot

display_5_lowest:               ; show 5 lowest digits
        movlw   digit_4         ; digits 45678
        movwf   FSR
display:  ; Show the FIVE digits beginning at INDF = *(FSR) on the LED display...
        movf    INDF, w         ; convert the five digits to LED data
display_w:                      ; come here with 1st char in w, e.g. BLANK
        call    ConvChar0       ; first visible digit
        incf    FSR, f          ; increment pointer to next digit
        movf    INDF, w         ; w = *(FSR)
        call    ConvChar1       ; second visible digit
        incf    FSR, f
        movf    INDF, w
        call    ConvChar2       ; third visible digit
        incf    FSR, f
        movf    INDF, w
        call    ConvChar3       ; fourth visible digit
        incf    FSR, f
        movf    INDF, w
        goto    ConvChar4       ; convert fifth visible digit AND RETURN
; end of routine "CvtAndDisplayFreq"


;--------------------------------------------------------------------------
;
; Display the current mode (either "FrEQ" or "Count")
;
;--------------------------------------------------------------------------

ShowMode:
        movfw modebits          ; get current mode
        andlw OPTION_MASK        ; just the function selection
        btfss STATUS, Z         ; if not zero
        goto sm_count           ; .. it is count
sm_freq:
        movlw CHAR_F            ; set to "FrEQ"
        call ConvChar0
        movlw CHAR_r
        call ConvChar1
        movlw CHAR_E
        call ConvChar2
        movlw CHAR_Q
        call ConvChar3
        movlw BLANK
        call ConvChar4
        retlw 0
sm_count:
        movlw CHAR_C            ; set to "Count"
        call ConvChar0
        movlw CHAR_o
        call ConvChar1
        movlw CHAR_u
        call ConvChar2
        movlw CHAR_n
        call ConvChar3
        movlw CHAR_t
        call ConvChar4
        retlw 0


RefreshDisplay:
        ; TheHWCave: This is a straight copy from the timing loop
        ; except the watchdog timer is included as well
        ;
        movf    disp_index, w   ; [1] get the current digit number (disp_index = 0..4)
        call    Digit2MuxValue  ; [2,3,4,5,6,7] display (6 commands including call+retlw)
        movwf   bTemp           ; [8] save the bit pattern for the multiplexer port
        movlw   display0        ; [9]  get the LED display data for the current digit...
        addwf   disp_index, w   ; [10] add current digit number to address of LED data
        movwf   FSR             ; [11] move address into the PIC's poor 'data pointer'
        movf    INDF, w         ; [12] w := *(FSR) use indirection register to read from table
        movwf   LEDS_PORT       ; [13] set the LED segments
        movf    bTemp, w        ; [14] get the mupliplexer pattern (hurry, hurry !)
        movwf   ENABLE_PORT     ; [15] set the LED multiplexer

        incf    disp_timer, f   ; [16] increment display-multiplex timer
        btfsc   disp_timer, 6   ; [17] (6-bit prescaler)
        incf    disp_index, f   ; [18] next display if rolled over
        bcf     disp_timer, 6   ; [19] limit disp_timer to 6 bits (!)
        movf    disp_index, w   ; [20] limit display index to  0...4
        sublw   4               ; [21] subtract #4 - W register -> C=0(!) if result negative (W>4)
        btfss   STATUS, C       ; [22] skip next instruction if C=1 (#4-W >= 0)
        clrf    disp_index      ; [23] if C=0 (disp_index>4) then disp_index=0
        clrwdt                  ; feed the dog
        retlw   0


;**************************************************************************
;                                                                         *
; Procedures                                                              *
;                                                                         *
;**************************************************************************


;--------------------------------------------------------------------------
;  Configure the prescaler for TIMER 0 in the PIC's OPTION register.
;--------------------------------------------------------------------------
;
; Description of the OPTION register, from the PIC16F628 data sheet:
; bit 7: RBPU: PORTB Pull-up Enable bit
;        1 = PORTB pull-ups are disabled
;        0 = PORTB pull-ups are enabled by individual port latch values
; bit 6: INTEDG: Interrupt Edge Select bit
;        1 = Interrupt on rising edge of RB0/INT pin
;        0 = Interrupt on falling edge of RB0/INT pin
; bit 5: T0CS: TMR0 Clock Source Select bit
;        1 = Transition on RA4/T0CKI pin
;        0 = Internal instruction cycle clock (CLKOUT)
; bit 4: T0SE: TMR0 Source Edge Select bit
;        1 = Increment on high-to-low transition on RA4/T0CKI pin
;        0 = Increment on low-to-high transition on RA4/T0CKI pin
; bit 3: PSA: Prescaler Assignment bit
;        1 = Prescaler is assigned to the WDT
;        0 = Prescaler is assigned to the Timer0 module
; bit 2-0: PS2:PS0: Prescaler Rate Select bits, here shown for TMR0 :
;     000  = 1 : 2
; ... 111  = 1 : 256
;        Note: to count EVERY pulse (1 : 1) with TMR0, the prescaler
;              must be assigned to the WATCHDOG TIMER (WDT) !
; Some examples (for the OPTION register, parameter in W for SetPrescaler):
PSC_DIV_BY_2   equ  b'00100000' ; let prescaler divide TMR0 by two
PSC_DIV_BY_4   equ  b'00100001' ; let prescaler divide TMR0 by   4
PSC_DIV_BY_8   equ  b'00100010' ; let prescaler divide TMR0 by   8
PSC_DIV_BY_16  equ  b'00100011' ; let prescaler divide TMR0 by  16
PSC_DIV_BY_32  equ  b'00100100' ; let prescaler divide TMR0 by  32
PSC_DIV_BY_64  equ  b'00100101' ; let prescaler divide TMR0 by  64
PSC_DIV_BY_128 equ  b'00100110' ; let prescaler divide TMR0 by 128
PSC_DIV_BY_256 equ  b'00100111' ; let prescaler divide TMR0 by 256

SetPrescaler:  ; copy W into OPTION register, avoid watchdog trouble
        clrwdt     ; recommended by Microchip ("switching prescaler assignment")
        errorlevel -302         ; Turn off banking message for the next few instructions..
        bsf   STATUS, RP0       ;! setting RP0 enables access to OPTION reg
                                ; option register is in bank1. i know. thanks for the warning.
        movwf OPTION_REG        ;! ex: "option" command (yucc)
        bcf   STATUS, RP0       ;! clearing RP0 for normal register access
        errorlevel +302         ; Enable banking message again
        retlw 0


PrescalerOff:  ; turn the prescaler for TMR0 "off"
             ; (actually done by assigning the prescaler to the watchdog timer)
        clrwdt                  ; clear watchdog timer
        clrf  TMR0              ; clear timer 0 AND PRESCALER(!)
        errorlevel -302         ; Turn off banking message for the next few instructions..
        bsf   STATUS, RP0       ;! setting RP0 enables access to OPTION reg
                                ; option register is in bank1. i know. thanks for the warning.
        movlw b'00100111'       ;! recommended by Microchip when
                                ;! changing prescaler assignment from TMR0 to WDT
        movwf OPTION_REG        ;! ex: "option" command (yucc)
        clrwdt                  ;! clear watchdog again
        movlw b'00101111'       ;! bit 3 set means PS assigned to WDT now
        movwf OPTION_REG        ;! ex: "option" command (yucc)
        bcf   STATUS, RP0       ;! clearing RP0 for normal register access
        errorlevel +302         ; Enable banking message again
        retlw 0


;--------------------------------------------------------------------------
; Convert a character into LEDs data for the 7-segment displays, fed with
; the character in w.  Bit 7 set means 'decimal point AFTER this digit'.
;--------------------------------------------------------------------------
;
CONV    macro   display         ; macro for duplicate code
        movwf   display         ; save decimal point bit (msb)
        andlw   0x7f            ; mask bit
        call    Digit2SevenSeg  ; convert digit into 7-segment-code via table
        btfsc   display,7       ; check bit 7 = decimal point ?

        iorlw   1<<DPPOINT_BIT  ; include decimal point if bit 7 set (bitwise OR)

        movwf   display         ; set display data register
        endm

ConvChar0:     ; display digit #0  (leftmost, or MOST SIGNIFICANT digit)
        CONV    display0
        retlw   0

ConvChar1:     ; display #1
        CONV    display1
        retlw   0

ConvChar2:     ; display #2
        CONV    display2
        retlw   0

ConvChar3:     ; display #3
        CONV    display3
        retlw   0

ConvChar4:     ; display #4  (rightmost, or LEAST SIGNIFICANT digit, "ones")
        CONV    display4
        retlw   0


;--------------------------------------------------------------------------
; Fill the 5-digit display latch with blank characters
;--------------------------------------------------------------------------
ClearDisplay:
        movlw   BLANK_PATTERN
        movwf   display0
        movwf   display1
        movwf   display2
        movwf   display3
        movwf   display4
        retlw   0


;--------------------------------------------------------------------------
; Save a single Byte in the PIC's Data-EEPROM.
;  Input parameters:
;    INDF = *FSR    contains byte to be written (was once EEDATA)
;    w              contains EEPROM address offset (i.e. "destination index")
;
;--------------------------------------------------------------------------
        ; write to EEPROM data memory as explained in the 16F628 data sheet.
        ; EEDATA and EEADR must have been set before calling this subroutine
        ; (optimized for the keyer-state-machine).
        ; CAUTION : What the lousy datasheet DS40300B wont tell you:
        ;           The example given there for the 16F628 is WRONG !
        ;           All EEPROM regs are in BANK1 for the 16F628.
        ;           In the PIC16F84, some were in BANK0 others in BANK1..
        ; In the PIC16F628, things are much different... all EEPROM regs are in BANK1 !
SaveInEEPROM:    ; save "INDF" = *FSR   in EEPROM[<w>]
         bcf     INTCON, GIE    ; disable INTs
         errorlevel -302        ; Turn off banking message for the next few instructions..
         bsf     STATUS, RP0    ;!; Bank1 for "EEADR" access, PIC16F628 ONLY (not F84)
         movwf   EEADR          ;!; write into EEPROM address register (BANK1 !!)
         bcf     STATUS, RP0    ;!; Bank0 to read "bStorageData"
         movfw   INDF           ; ; w := *FSR (read source data from BANK 0)
         bsf     STATUS, RP0    ;!; Bank1 for "EEDATA" access, PIC16F628 ONLY (not F84)
         movwf   EEDATA         ;!; EEDATA(in BANK1) := w  (BANK1; F628 only, NOT F84 !!!)
         bsf     EECON1, WREN   ;!; set WRite ENable
         bcf     INTCON, GIE    ;!; Is this REALLY required as in DS40300B Example 13-2 ?
         movlw   055h           ;!;
         movwf   EECON2         ;!; write 55h
         movlw   0AAh           ;!;
         movwf   EECON2         ;!; write AAh
         bsf     EECON1, WR     ;!; set WR bit, begin write
         ; wait until write access to the EEPROM is complete.
SaveEW:  btfsc   EECON1, WR     ;!; WR is cleared after completion of write
         goto    SaveEW         ;!; WR=1, write access not finished yet
         ; Arrived here: the EEPROM write is ready
         bcf     EECON1, WREN   ;!; disable further WRites
         bcf     STATUS, RP0    ;!; Bank0 for normal access
         errorlevel +302 ; Enable banking message again
   ;     bsf     INTCON, GIE    ; enable INTs ? NOT IN THIS APPLICATION !
         retlw   0  ; end SaveInEEPROM


;--------------------------------------------------------------------------
; Read a single Byte from the PIC's Data-EEPROM.
;  Input parameters:
;    w    contains EEPROM address offset (i.e. "source index")
;         will *NOT* be modified to simplify block-read .
;    FSR  points to the memory location where the byte shall be placed.
;
;  Result:
;     INDF = *FSR  returns the read byte
;--------------------------------------------------------------------------
        ; Caution: EEDATA and EEADR have been moved from Bank0(16F84) to Bank1(16F628)
        ;          and the example from the datasheet telling you to switch to
        ;          bank0 to access EEDATA is rubbish (DS40300B page 93 example 13-1).
EEPROM_ReadByte:    ; read ONE byte from the PIC's data EEPROM
        movwf   bTemp           ; save W
        bcf     INTCON, GIE     ; disable INTs
        errorlevel -302         ; Turn off banking message for the next few instructions..
        bsf     STATUS, RP0     ; Bank1 for ***ALL*** EEPROM registers in 16F628 (!)
        movwf   EEADR           ;! write into EEPROM address register
        bsf     EECON1, RD      ;! set "Read"-Flag for EEPROM
                                ;   why is EECON1.RD not cleared in MPLAB-sim ?!?
        movf    EEDATA, w       ;! read byte from EEPROM latch
        bcf     STATUS, RP0     ;! normal access to Bank0
        errorlevel +302         ; Enable banking message again
    ;   bsf     INTCON, GIE     ; re-enable interrupts ? NOT IN THIS APPLICATION !
        movwf   INDF            ; place result in *FSR
        movfw   bTemp           ; restore W
        return                  ; back to caller
 ; end EEPROM_ReadByte


        END   ; directive 'end of program'


