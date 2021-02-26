;**************************************************************************
; FILE:      counter_hires_event.asm                                      *
; CONTENTS:  Simple low-cost digital frequency meter using a PIC 16F628A  *
; AUTHOR:    Wolfgang Buescher, DL4YHF                                    *
;            (based on a work by James Hutchby, MadLab, 1996)             *
; REVISIONS: (latest entry first)                                         *
;                                                                         *
; 2021-02-26 - Ho-Ro:                                                     *
;              1 Hz resolution up to 99999 Hz (range < 101760 Hz)         *
;              hi-res (two decimals) up to 255.99 Hz                      *
;              toggle three-decimals mode up to 60.999 Hz with key press  *
;              rewrote "display_freq" to a more consistent layout         *
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
; 2005-08-24 - Cured a bug in the COMMON ANODE decimal point setting .    *
;              (the "^0xFF" for the AND-mask was missing in macro 'conv') *
; 2005-03-21 - Added a few conditionals to use the same sourcecode        *
;              to drive a COMMON ANODE display ( DISPLAY_VARIANT_3 )      *
; 2004-03-14 - Fixed a range-switching bug around 8 MHz .                 *
;            - Support TWO different display variants now,                *
;              optimized for different board layouts, and different clock *
;              frequencies (4 MHz for variant 1,  20 MHz for variant 2).  *
; 2004-03-05 - Added the feature to add or subtract a frequency offset .  *
; 2004-02-18 - Migration to a PIC16F628 with 4 MHz crystal (el Cheapo)    *
;            - Changed the LED patterns '6' and '9' because they looked   *
;              like 'b' and 'q' in the old counter version .              *
;            - Added the auto-ranging feature                             *
;            - Stepped from 24-bit to 32-bit integer arithmetic, to be    *
;              able to count 50 MHz with 1-second gate time,              *
;              or (at least) adjust ANY result for the ANY prescaler      *
;              division ratio, which may give pretty large numbers .      *
;            - A PIC16F628 worked up to 63 MHz with this firmware .       *
;**************************************************************************

; Source code is suitable for gpasm under Debian Linux.

 list P=16F628A
 #include <p16f628a.inc> ; processor specific definitions
 #define DEBUG 0         ; DEBUG=1 for simulation, DEBUG=0 for real hardware


;**************************************************************************
;                                                                         *
; Summary                                                                 *
;                                                                         *
;**************************************************************************

; The software functions as a frequency meter with an input signal
; range of 1 Hz to ~ 50 MHz and with an accuracy of +/- 1Hz
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
; later converted to frequency for frequencies < 256 Hz.
;
; The frequency in binary is converted to decimal using a powers-of-ten
; lookup table. The binary powers of ten are repeatedly subtracted from
; the frequency to determine the individual decimal digits. The decimal
; digits are stored at the 8 bytes at 'digits'. Leading zeroes are then
; suppressed and the 5 significant digits are converted to LED data
; for the 7-segment displays using a lookup table.

; The signal frequency is displayed on five 7-segment displays.
; The displays are multiplexed which means that only one display is enabled
; at any one time. The variable 'disp_index' contains the index of the currently
; enabled display. Each display is enabled in turn at a sufficient frequency
; that no flicker is discernable. A prescaler ('disp_timer') is used
; to set the multiplexing frequency to a few hundred Hz.

; The display shows the signal frequency in Hz, KHz or MHz
; according to the following table:

; -------------------------
; |           |  DISPLAY  |
; | Frequency | Freq mode |
; |-----------|-----------|
; | < 1 Hz    |        0  |
; | 1 Hz      |    1„000„ |  Two Hz-dots are flashing (three-digits mode)
; | 10 Hz     |   10„000„ |  Two Hz-dots are flashing (three-digits mode)
; | 1 Hz      |     1„00„ |  Two Hz-dots are flashing
; | 10 Hz     |    10„00„ |  Two Hz-dots are flashing
; | 100 Hz    |   100„00„ |  Two Hz-dots are flashing
; | 255.99 Hz |   255„99„ |  Two Hz-dots are flashing
; | 256 Hz    |    0„256  |  One kHz-dot is flashing
; | 1000 Hz   |    1„000  |  One kHz-dot is flashing
; | 10.00 KHz |   10„000  |  One kHz-dot is flashing
; | 100.0 KHz |   100„00  |  One kHz-dot is flashing
; | 1.000 MHz |   1.0000  |  One MHz-dot is steady
; | 10.00 MHz |   10.000  |  One MHz-dot is steady
; -------------------------
; '.': steady display dot
; '„': flashing display dot
; The flashing dots change their state with the measurement rate
; Three-digits mode
; Frequencies < 61 Hz can optionally be displayed with three decimal digits.
; e.g. "50.123."
; To switch between two- and three-digit mode, press the key until the mode changes.
; The high-resolution mode is cancelled when the frequency rises above 61 Hz.
; The 61 Hz is a compromise between the conversion time and the possibility
; of measuring the mains frequencies 50 Hz or 60 Hz precisely.
;
; If there is no signal at all, a single zero is displayed in the 5th digit.
;
; Event counting mode
; To enter the event counting mode, press the button during power-up
; until the message "Count" is shown.
; Events are shown with leading zeros and without dots, e.g. "01234"
; Pressing the button at any time resets the counter back to zero.


;**************************************************************************
;                                                                         *
; PIC config definitions                                                  *
;                                                                         *
;**************************************************************************


; '__CONFIG' directive is used to embed configuration data within .asm file.
; The lables following the directive are located in the respective .inc file.
; See respective data sheet for additional information on configuration word.
; Since 2006-05-28, the watchdog must be ENABLED in the config word
;       because of its wakeup-from-sleep function (see 'Sleep100ms').
; EX(16F84:)     __CONFIG   _CP_OFF & _WDT_ON & _PWRTE_ON & _RC_OSC


; display variants 2+3 : clocked with 20 MHz (needs "HS" oscillator)
    __CONFIG   _CP_OFF & _WDT_ON & _PWRTE_ON & _HS_OSC & _LVP_OFF & _BODEN_OFF & _MCLRE_OFF


; '__IDLOCS' directive may be used to set the 4 * 4(?!?) ID Location Bits.
; These shall be placed in the HEX file at addresses 0x2000...0x2003 .
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

#define IOP_SWITCH  PORTA,5             ; digital input signal
                                        ; LOW enters event mode and resets event counter


;**************************************************************************
;                                                                         *
; Constants and timings                                                   *
;                                                                         *
;**************************************************************************

; display variant 2 (the chinese clone) is clocked with 20 MHz (higher resolution)
CLOCK     equ  .20000000

; microseconds per timing loop

; clocked with 20 MHz
  ; 20 microseconds is possible with 20-MHz-Crystal,
  ; Make sure all gate times can be divided by this interval without remainder :
  ; 1   second / 20 us = 50000  (ok)
  ; 1/4 second / 20 us = 12500  (ok)
  ; 1/8 second / 20 us =  6250  (ok)
TIME      equ  .20


; Clock cycles per timing loop.  See subroutine count_pulses .
;  Usually CYCLES=200 (for 4 MHz crystal,  50 usec - loop)
;              or 400 (for 20 MHz crystal, 20 usec - loop)
CYCLES    equ  TIME*CLOCK/.1000000

GATE_TIME_LOOPS equ  CLOCK/CYCLES       ;  number of gate-time loops for ONE SECOND gate time

LAMPTEST_LOOPS  equ  CLOCK/(.2*CYCLES)  ; number of loops for a 0.5 SECOND lamp test after power-on

COUNTMODE_LOOPS  equ  CLOCK/(.1*CYCLES) ; number of delay loops for display "count" (0.1 sec)

ONESECOND       equ .50000              ; 1 second in 20 us units
PER2FREQ2       equ ONESECOND * .100    ; convert periods into HZ with 2 decimal points
PER2FREQ3       equ ONESECOND * .1000   ; convert periods into HZ with 3 decimal points


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

gatecnt_hi      equ  0x2D       ; 16-bit counter (msb first)
gatecnt_lo      equ  0x2E       ; 16-bit counter (lsb last)

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

t0dark          equ  0x3A       ; counter mode only: value tmr0 increased during dark time (outside count_pulses)
t0last          equ  0x3B       ; counter mode only: value of tmr0 at the end of count_pulses


menu_index      equ  0x3C       ; menu item for programming mode
menu_timer      equ  0x3D       ; used to detect how long a key was pressed

digits          equ  0x40       ; frequency as decimal digits (8 bytes)...
digit_0         equ  0x40       ; same location as MOST SIGNIFICANT digit, 10-MHz
digit_1         equ  0x41       ; usually the 1-MHz-digit
digit_2         equ  0x42       ; usually the 100-kHz-digit
digit_3         equ  0x43       ; usually the 10-kHz-digit
digit_4         equ  0x44       ; usually the 1-kHz-digit
digit_5         equ  0x45       ; usually the 100-Hz-digit
digit_6         equ  0x46       ; usually the 10-Hz-digit
digit_7         equ  0x47       ; usually the 1-Hz-digit
digit_8         equ  0x48       ; must contain a blank character (or trailing zero)

display0        equ  0x49       ; display #0 data
display1        equ  0x4A       ; display #1 data
display2        equ  0x4B       ; display #2 data
display3        equ  0x4C       ; display #3 data
display4        equ  0x4D       ; display #4 data

disp_index      equ  0x4E       ; index of the enabled display (0 to 4 for 5-digit display)
disp_timer      equ  0x4F       ; display multiplex timer (5 bits)

adjust_shifts   equ  0x50       ; count of 'left shifts' to compensate prescaler+gate time

blinker         equ  0x51       ; prescaler for the flashing 1-kHz-dot

period_waste    equ  0x52       ; stores the number of cycle*4 to waste to make up the correct total
                                ; ... number of instructions in the count_pulses loop
pcnt            equ  0x53       ; number of periods measured
period_hi       equ  0x54       ; accumulated period in 20us increments (high byte)
period_lo       equ  0x55       ; ... low byte
pdiv_mh         equ  0x56       ; used to store the final division result (bits 16..23)
pdiv_ml         equ  0x57       ; ... bits 8..15
pdiv_lo         equ  0x58       ; ... bits 0..7
pmodeflag       equ  0x59       ; 7 6 5 4 3 2 1 0
                                ; 0 0 0 - - - - -    = period measuring off (normal freq disp)
                                ; 1 0 0 - - - - -    = PMODE_ON: measure 10mHz
                                ; 1 1 0 - - - - -    = PMODE_ON + VERY_LOW: measure 1 mHz
                                ; 0 0 1 - - - - -    = EVENT_ON: count events, period measuring off
#define PMODE_ON pmodeflag,7    ; 0 = off, 1 = on
#define VERY_LOW pmodeflag,6    ; 0 = normal, 1 = measure 1e-3 Hz
#define EVENT_ON pmodeflag,5    ; 0 = off, 1 = on
#define VERY_LOW_MASK b'01000000'
;**************************************************************************
;                                                                         *
; Macros                                                                  *
;                                                                         *
;**************************************************************************

;--------------------------------------------------------------------------
; macros to implement lookup tables - these macros hide the PIC syntax
; used and make the source code more readable
;   (YHF: CAUTION - BUT THESE MACROS HIDE SOME VERY NASTY PITFALLS .
;         TABLE MUST NOT CROSS PAGE BORDER DUE TO 'ADDWF PCL, f' ! )
;--------------------------------------------------------------------------

cquad   macro value
        retlw value>>.24                ; high byte
        retlw (value>>.16)&0xFF         ; middle-high byte
        retlw (value>>8)&0xFF           ; middle-low  byte
        retlw value&0xFF                ; low byte
        endm

table   macro label                     ; define lookup table

label   addwf PCL,f  ; caution: this is 'PCL' only, cannot add to the full 'PC' in a PIC !
        endm


;--------------------------------------------------------------------------
; add with carry - adds the w register and the carry flag to the file
; register reg, returns the result in <reg> with the carry flag set if overflow
;--------------------------------------------------------------------------

addcwf  macro reg

        local add1,add2

        BNC add1                      ; branch if no carry set

        addwf reg , f                 ; add byte

        incf reg , f                  ; add carry
        skpnz
        setc

        goto add2

add1    addwf reg,f                   ; add byte

add2
        endm


;--------------------------------------------------------------------------
; subtract with "no-carry" - subtracts the w register and the no-carry flag
; from the file register reg, returns the result in reg with the no carry flag
; set if underflow
;--------------------------------------------------------------------------

subncwf macro reg

        local sub1,sub2

        BC sub1                       ; branch if carry set
        subwf reg, f                  ; subtract byte
        skpnz                         ; subtract no carry
        clrc
        decf reg , f
        goto sub2
sub1    subwf reg , f                 ; subtract byte
sub2
        endm


;--------------------------------------------------------------------------
; MACRO to perform 32-bit addition - adds the four bytes at op2 to the
; three bytes at op1 (most significant bytes first), returns the result in
; op1 with op2 unchanged and the carry flag set if overflow
;--------------------------------------------------------------------------

add32   macro op1,op2                 ; op1 := op1 + op2

        movfw op2+3                   ; add low byte        (bits 7...0)
        addwf op1+3,f

        movfw op2+2                   ; add middle-low byte (bits 15..8)
        addcwf op1+2

        movfw op2+1                   ; add middle-high byte (bits 23...16)
        addcwf op1+1

        movfw op2+0                   ; add high byte       (bits 31...24)
        addcwf op1+0

        endm


;--------------------------------------------------------------------------
; MACRO to perform 32-bit subtraction - subtracts the four bytes at op2
; from the four bytes at op1 (most significant bytes first), returns the
; result in op1 with op2 unchanged and the no carry flag set if underflow
;--------------------------------------------------------------------------

sub32   macro op1,op2                 ; op1 := op1 - op2

        movfw op2+3                   ; subtract low byte
        subwf op1+3 , f

        movfw op2+2                   ; subtract middle low byte
        subncwf op1+2

        movfw op2+1                   ; subtract middle high byte
        subncwf op1+1

        movfw op2+0                   ; subtract high byte
        subncwf op1+0

        endm


;--------------------------------------------------------------------------
; MACRO to negate a 32-bit value  (  op := 0 - op ) .
;--------------------------------------------------------------------------

neg32   macro op                      ; op1 := 0 - op2
        local neg_done
        comf  op,   f             ; invert all 8 bits in high byte
        comf  op+1, f             ; invert all 8 bits in middle high byte
        comf  op+2, f             ; invert all 8 bits in middle low byte
        comf  op+3, f             ; invert all 8 bits in low byte
        ; Note at this point 0x000000 would have turned into 0xFFFFFFF .
        ; Must add ONE to complete the TWO's COMPLIMENT calculation ( -0  = 0 ).
        ; Note that "incf" affects only the Z flag but not the C flag .
        incfsz op+3, f            ; increment low byte        (bits 7...0)
        goto   neg_done           ; if incremented result NOT zero, we're through !
        incfsz op+2, f            ; increment middle low byte (bits 15...8)
        goto   neg_done           ; if incremented result NOT zero, ...
        incfsz op+1, f            ; increment middle high byte (bits 23...16)
        goto   neg_done           ; if ...
        incfsz op+0, f            ; increment high byte       (bits 31...24)
        goto   neg_done           ;
neg_done
        endm


;--------------------------------------------------------------------------
; MACRO to perform 32-bit subtraction - subtracts the four bytes at op2
; from the four bytes at op1 (most significant bytes first), returns the
; result in op1 with op2 unchanged and the no carry flag set if underflow
;--------------------------------------------------------------------------

subx32  macro src,dst          ;dst := dst - src
        bsf STATUS, C
        movf src+3,w
        btfss STATUS, C
        incfsz src+3,w
        subwf dst+3,f
        movf src+2,w
        btfss STATUS, C
        incfsz src+2,w
        subwf dst+2,f
        movf src+1,w
        btfss STATUS, C
        incfsz src+1,w
        subwf dst+1,f
        movf src+0,w
        btfss STATUS, C
        incfsz src+0,w
        subwf dst+0,f
        endm

addx32  macro src,dst          ;dst := dst + src
        bcf STATUS, C
        movf src+3,w
        btfsc STATUS, C
        incfsz src+3,w
        addwf dst+3,f
        movf src+2,w
        btfsc STATUS, C
        incfsz src+2,w
        addwf dst+2,f
        movf src+1,w
        btfsc STATUS, C
        incfsz src+1,w
        addwf dst+1,f
        movf src+0,w
        btfsc STATUS, C
        incfsz src+0,w
        addwf dst+0,f
        endm


;**********************************************************************
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
CHAR_A  equ     .10             ; Letters A..F = HEX digits, index 10..15
CHAR_b  equ     .11             ;
CHAR_C  equ     .12             ;
CHAR_d  equ     .13             ;
CHAR_E  equ     .14             ;
CHAR_F  equ     .15             ;
CHAR_o  equ     .16             ; Other letters used in "Count"
CHAR_u  equ     .17             ;
CHAR_n  equ     .18             ;
CHAR_t  equ     .19             ;
BLANK   equ     .20             ; blank display
TEST    equ     .21             ; power-on display test

DPPOINT_BIT equ  1   ; decimal point bit (same for all digits)
#define _A      0x40     ; bitmask for segment A , etc ..
#define _B      0x80
#define _C      0x04
#define _D      0x01
#define _E      0x08
#define _F      0x10
#define _G      0x20
#define _DP     0x02

BLANK_PATTERN equ b'00000000'      ; blank display pattern (7-segment code)


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
        ; blank and test pattern
        retlw (BLANK_PATTERN       )^SSEG_XORMASK ; ....... = ' '    ( # 20 )
        retlw (b'11111111'         )^SSEG_XORMASK ; all segments on  ( # 21 )


;--------------------------------------------------------------------------
; Table to control which 7-segment display is enabled. Displays are usually
; COMMON CATHODE (variants 1+2) so pulled low to enable.
; For DISP_VARIANT=3 (COMMON ANODE), the digit-driving pattern is inverted.
; Input:   W = 0 means the MOST SIGNIFICANT DIGIT (the leftmost one), etc.
; Result:  VALUE to be written to ENABLE_PORT to activate the digit
;--------------------------------------------------------------------------
Digit2MuxValue:     ;
        addwf PCL,f  ; caution: this is 'PCL' only, not 'PC'
        ; Note: If the program counter is affected, a command requires to instruction cycles (=8 osc cycles)

; muliplexer values (5 digits, COMMON CATHODE) :
        retlw b'11110111'        ; most significant digit is on   PA3 (!)
        retlw b'11111011'        ; next less significant dig. on  PA2 (!!)
        retlw b'11111110'        ; next less significant dig. on  PA0 (!!)
        retlw b'11111101'        ; 4th (sometimes the last) digit PA1 (!)
        retlw b'11111111'        ; 5th (OPTIONAL) least significant digit = NOT (PA3+PA2+PA1+PA0)


;--------------------------------------------------------------------------
; Powers-of-ten table (32 bits, most significant byte first)
;   Based on an idea by James Hutchby (MadLab, 1996) .
;   Modified for 32-bit arithmetic by Wolfgang Buescher (2004).
;--------------------------------------------------------------------------
TensTable  addwf PCL,f
        cquad   .10000000  ; 10 million is sufficient for the counter itself
        cquad   .1000000
        cquad   .100000
        cquad   .10000
        cquad   .1000
        cquad   .100
        cquad   .10
        cquad   .1



;**************************************************************************
;                                                                         *
; Procedures                                                              *
;                                                                         *
;**************************************************************************


;--------------------------------------------------------------------------
;  Configure the prescaler for TIMER 0 in the PIC's OPTION register .
;--------------------------------------------------------------------------

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
PSC_DIV_BY_2   equ  b'00100000'   ; let prescaler divide TMR0 by two
PSC_DIV_BY_4   equ  b'00100001'   ; let prescaler divide TMR0 by   4
PSC_DIV_BY_8   equ  b'00100010'   ; let prescaler divide TMR0 by   8
PSC_DIV_BY_16  equ  b'00100011'   ; let prescaler divide TMR0 by  16
PSC_DIV_BY_32  equ  b'00100100'   ; let prescaler divide TMR0 by  32
PSC_DIV_BY_64  equ  b'00100101'   ; let prescaler divide TMR0 by  64
PSC_DIV_BY_128 equ  b'00100110'   ; let prescaler divide TMR0 by 128
PSC_DIV_BY_256 equ  b'00100111'   ; let prescaler divide TMR0 by 256

SetPrescaler:  ; copy W into OPTION register, avoid watchdog trouble
        clrwdt     ; recommended by Microchip ("switching prescaler assignment")
        errorlevel -302 ; Turn off banking message for the next few instructions..
        bsf   STATUS, RP0            ;! setting RP0 enables access to OPTION reg
             ; option register is in bank1. i know. thanks for the warning.
        movwf OPTION_REG             ;! ex: "option" command (yucc)
        bcf   STATUS, RP0            ;! clearing RP0 for normal register access
        errorlevel +302 ; Enable banking message again
        retlw 0


PrescalerOff:  ; turn the prescaler for TMR0 "off"
             ; (actually done by assigning the prescaler to the watchdog timer)
        clrwdt                        ; clear watchdog timer
        clrf  TMR0                    ; clear timer 0 AND PRESCALER(!)
        errorlevel -302 ; Turn off banking message for the next few instructions..
        bsf   STATUS, RP0            ;! setting RP0 enables access to OPTION reg
             ; option register is in bank1. i know. thanks for the warning.
        movlw b'00100111'            ;! recommended by Microchip when
                                     ;! changing prescaler assignment from TMR0 to WDT
        movwf OPTION_REG             ;! ex: "option" command (yucc)
        clrwdt                       ;! clear watchdog again
        movlw b'00101111'            ;! bit 3 set means PS assigned to WDT now
        movwf OPTION_REG             ;! ex: "option" command (yucc)
        bcf   STATUS, RP0            ;! clearing RP0 for normal register access
        errorlevel +302 ; Enable banking message again
        retlw 0


;--------------------------------------------------------------------------
; Power-saving subroutine: Puts the PIC to sleep for ROUGHLY 100 milliseconds .
;  - crystal oscillator turned OFF during this phase
;  - only the internal RC-oscillator for the watchdog keeps running
;  - expiration of watchdog during sleep does NOT reset the PIC,
;    only wakes it up again so normal operation may resume
;  - LED display will be off during this time
;--------------------------------------------------------------------------
Sleep150ms:  ; go to sleep for approx. 150 milliseconds, and then RETURN (no reset)
   ; Details on the PIC's watchdog timer (from PIC16F628 datasheet) :
   ; > The WDT has a nominal timeout period of 18 ms (with
   ; > no prescaler). The timeout periods vary with temperature,
   ; > VDD and process variations from part to part (see
   ; > DC specs).
   ; > The Watchdog Timer is a free running on-chip RC oscillator which does
   ; > not require any external components. This RC oscillator is separate
   ; > from the ER oscillator of the CLKIN pin. That means that the WDT will run,
   ; > even if the clock on the OSC1 and OSC2 pins of the device has been stopped,
   ; > for example, by execution of a SLEEP instruction.
   ; > During normal operation, a WDT timeout generates a device RESET.
   ; > If the device is in SLEEP mode, a WDT timeout causes the device to wake-up
   ; > and continue with normal operation.
   ; > The WDT can be permanently disabled by programming the configuration bit
   ; > WDTE as clear .
   ; In other words, to use the watchdog-timer for "temporary sleep" here ,
   ; it must be ENABLED in the configuration word when programming the PIC.
   ;  (because its not possible to turn it on via software if it's not on).
   ; But once the watchdog timer is ON, it must be FED periodically otherwise
   ; it will reset the PIC during normal operation !
   ; Here (in the frequency counter), the prescaler remains assigned to timer0
   ; so the watchdog interval is ~ 18 milliseconds (+/-, RC-oscillator) .
   ; > The CLRWDT and SLEEP instructions clear the WDT and the postscaler,
   ; > if assigned to the WDT, and prevent it from timing out and generating
   ; >  a device RESET. The TO bit in the STATUS register will be cleared upon
   ; > a Watchdog Timer timeout.
; display with COMMON CATHODE :
        movlw 0x00                    ; segment drivers LOW to turn off

        movwf LEDS_PORT               ; turn LED segments off
        ; Note: The global interrupt-enable flag (GIE) is off in this application !
        ; To avoid unintended wake-up on 'interrupt' (port level change),
        ; disable all interrupt-SOURCES: Clear T0IE,INTE,RBIE,PEIE too :
        clrf  INTCON                  ; disable all interrupts during SLEEP mode
        clrwdt                        ; clear watchdog timer
        clrf  TMR0                    ; clear timer 0 AND PRESCALER(!)
        errorlevel -302 ; Turn off banking message for the next few instructions..
        bsf   STATUS, RP0            ;! setting RP0 enables access to OPTION reg
             ; option register is in bank1. i know. thanks for the warning.
        movlw b'00101011'            ;! assign PS to WDT; divide by 8 FOR WDT(!)
        movwf OPTION_REG             ;! ex: "option" command (yucc)
        bcf   STATUS, RP0            ;! clearing RP0 for normal register access
        errorlevel +302 ; Enable banking message again
        sleep                         ; sleep for approx 18 ms (one watchdog interval)
        ; The SLEEP command clears the Watchdog Timer and stops the main oscillator.
        ; Only the internal watchdog timer keeps running.
        ; The WDT is is also cleared when the device wakes-up from SLEEP,
        ; regardless of the source of wake-up, so no need for 'clrwdt' here !
        nop    ; arrived here, slept for ~ 8 times 18 milliseconds
        return ; end  Sleep150ms



;--------------------------------------------------------------------------
; Convert a character into LEDs data for the 7-segment displays, fed with
; the character in w.  Bit 7 set means 'decimal point AFTER this digit' .
;--------------------------------------------------------------------------

conv    macro   display                 ; macro for duplicate code
        movwf   display                 ; save decimal point bit (msb)
        andlw   7fh                     ; mask bit
        call    Digit2SevenSeg          ; convert digit into 7-segment-code via table
        btfsc   display,7               ; check bit 7 = decimal point ?

        iorlw   1<<DPPOINT_BIT          ; include decimal point if bit 7 set (bitwise OR)

        movwf   display                 ; set display data register
        endm

conv_char0:     ; display digit #0  (leftmost, or MOST SIGNIFICANT digit)
        conv    display0
        retlw   0

conv_char1:     ; display #1
        conv    display1
        retlw   0

conv_char2:     ; display #2
        conv    display2
        retlw   0

conv_char3:     ; display #3
        conv    display3
        retlw   0

conv_char4:     ; display #4  (rightmost, or LEAST SIGNIFICANT digit, "ones")
        conv    display4
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
; Count pulses, fed with the number of loop iterations for the gate time .
;               WHILE counting, the multiplexed LED display is updated .
;               Watchdog is fed in this loop !
; Input:    Count of gate-time-loops in 'gatecnt_hi'+'gatecnt_lo' (16 bit).
; Returns:  The number of pulses in 20us increments
;           The number of periods measured (in pcnt)               (added TheHWcave )
;           the sum of all measured periods in 20us increments     (added TheHWcave )
;           in t0dark the number of pulses tmr0 counted outside this routine (added TheHWcave )
;           in t0last the last value of tmr0 when leaving this routine (added TheHWcave )
;--------------------------------------------------------------------------
count_pulses:
        clrf    freq_hi         ; clear pulse counter (bits 31..24)
        clrf    freq_mh         ; bits 23..16
        clrf    freq_ml         ; bits 15..8
        clrf    freq_lo         ; bits  7..0

        clrf    period_hi       ; clear period accumulator bits 8..15
        clrf    period_lo       ; bits 0..7
        clrf    pcnt            ; clear number of periods measured
        bcf     PMODE_ON        ; turn period conversion off by default
                                ; until we are sure the input frequency
                                ; is low enough for this to make sense


        clrf    timer0_old      ; 'old' value of timer0 to detect toggling MSB
        ;
        ; in counter mode, check if TMR0 moved since the last time and record
        ; the difference in t0dark
        btfss   EVENT_ON        ; ifnot EVENT_ON
        goto    count0          ; then proceed as normal
        ;
        ; calculate the number of pulses missed while being ouside this routine
        ; store the result in t0dark
        movfw   TMR0
        movwf   t0dark
        movfw   t0last
        subwf   t0dark,f
count0:
        clrf    TMR0            ; timer register (PIC's hardware timer, 8 bit)
        nop                     ; 2 instruction cycle delay
        nop                     ; after writing to TMR0  (MPLAB-SIM: set breakpoint + clear stopwatch here)


; --------------- start of critial timing loop >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; The following timing loop must take a well-defined time in total per
; iteration, usually 50 (or 20) microseconds, which can be precisely achieved
; with a 4-MHz-crystal (or 20 MHz for variant 2+3) .
; This gives a basic delay for the frequency counter's gate time .
;    The frequency at the input of TIMER 0 (not the prescaler)
;    can not exceed f_crystal / 4,
;    and every HIGH->LOW transition of bit7 in TIMER0 must be polled here.
;  This is safe because ..
;    Variant 1:  With a 4-MHz-crystal, Timer0 can count up to 1 MHz input,
;                MSB toggles every (128/1MHz) = 128 us, polled every 50us  -> ok.
;    Variant 2:  With a 20-MHz-crystal, Timer0 can count up to 4 (not 5?!) MHz input,
;                MSB toggles every (128/4MHz) = 32 us, polled every 20us -> ok.

;  The numbers in square brackets below are the INSTRUCTION NUMBER within the loop.
;  (not the count of oscillator cycles for a single command, which is always 4).
;  These values can be checked with the "Stopwatch" function in MPLAB-SIM.
;  The goal is to let this loop take EXACTLY <TIME> microseconds (50us or 20us).

count1  movfw   disp_index      ; [1] get the current digit number (disp_index = 0..4)
        call    Digit2MuxValue  ; [2,3,4,5,6,7] display (6 commands including call+retlw)
        movwf   bTemp           ; [8] save the bit pattern for the multiplexer port
        movlw   display0        ; [9]  get the LED display data for the current digit...
        addwf   disp_index,w    ; [10] add current digit number to address of LED data
        movwf   FSR             ; [11] move address into the PIC's poor 'data pointer'
        movfw   INDF            ; [12] w := *(FSR) use indirection register to read from table
        movwf   LEDS_PORT       ; [13] set the LED segments
        movfw   bTemp           ; [14] get the mupliplexer pattern (hurry, hurry !)
        movwf   ENABLE_PORT     ; [15] set the LED multiplexer

        incf    disp_timer,f    ; [16] increment display-multiplex timer
        btfsc   disp_timer,6    ; [17] (6-bit prescaler)
        incf    disp_index,f    ; [18] next display if rolled over
        bcf     disp_timer,6    ; [19] limit disp_timer to 6 bits (!)
        movfw   disp_index      ; [20] limit display index to  0...4
        sublw   .4              ; [21] subtract #4 - W register -> C=0(!) if result negative (W>4)
        btfss   STATUS,C        ; [22] skip next instruction if C=1 (#4-W >= 0)
        clrf    disp_index      ; [23] if C=0 (disp_index>4) then disp_index=0

; the following fragments of code always take the same number of clock
; cycles to execute, irrespective of whether the skips take place or not .
; Here still in 'count_pulses'.

        movfw   TMR0            ; [24] read least significant byte of
        movwf   freq_lo         ; [25]  pulse counter (bits 7..0)


        movlw   1               ; [26] determine if timer 0 has rolled
        btfss   timer0_old,7    ; [27] over (rolled over if msb was
        clrw                    ; [28] previously set and now isn't)
        btfsc   freq_lo,7       ; [29]
        clrw                    ; [30]

        addwf   freq_ml,f       ; [31] increment high bytes of pulse counter
        skpnc                   ; [32] if low byte rolled over
        incf    freq_mh,f       ; [33] (mh = "medium high byte" of counter)
                                ; NOTE: we are not modifying freq_hi here !
                                ;       Bits 31..24 may be used later when multiplying with some factor
                                ;       (2^n) to compensate for the ASYNCHRON PRESCALER !

        btfsc   freq_mh,7       ; [34] overflow (freq > 7fffffh) ?
        goto    count3          ; [35] branch if yes

        nop                     ; [36]
        nop                     ; [37]
        ;movfw   freq_lo        ; [36] save previous value from timer 0
        ;movwf   timer0_old     ; [37]

        tstf    gatecnt_lo      ; [38] check inner gate-time counter, LOW byte
        skpnz                   ; [39] only decrement h-byte if l-byte zero
        decf    gatecnt_hi,f    ; [40]  decrement gate-time counter, HIGH byte
        decf    gatecnt_lo,f    ; [41] always decrement gate-time counter, LOW byte


        ; 100 instruction cycles per loop
        ; (f_xtal=20 MHz, t_loop=20us, t_instr=4/20MHz=0.2us)
        ;
        clrwdt                  ; [42] (ex: nop, but since 2006-05-28 the dog must be fed !)
        ;

        ; TheHWcave:	Measure period(s) in 20 uS units
        ;               The original software aready kept a copy of the timer0 content
        ;               at the previous loop, so we can use this to detect when the counter
        ;               has changed. Because we are only interested in very low frequencies
        ;               we can safely detect a single increment and there will be lots of loops
        ;               with no change at all.  The loop count (gatecnt) is counting down
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
        movlw   .10             ; [43] prepare to add 10x4 =40 wasted instructions later 50+40=90
        movwf   period_waste    ; [44]
        movfw   freq_lo         ; [45]
        xorwf   timer0_old,w    ; [46]    XOR of old and new counter.
        btfsc   STATUS, Z       ; [47]    zero means no change
        goto    P_done          ; [48+1]
        ;
        movlw   .1              ; [49]
        subwf   freq_lo,w       ; [50]
        btfss   STATUS, Z       ; [51]
        goto    P_Measure       ; [52+1]

        ; first transition: take a copy of the current gatecnt
        movfw   gatecnt_lo      ; [53]
        movwf   pstart_lo       ; [54]
        movfw   gatecnt_hi      ; [55]
        movwf   pstart_hi       ; [56]
        movlw   .7              ; [57] prepare to add 7x4 =28 wasted instructions later 62+28=90
        movwf   period_waste    ; [58]
        nop                     ; [59]
        goto    P_done          ; [60+1]

        ; subsequent transitions: (2, 3, 4..) calculate the accumulated elapsed time and copy to period
P_Measure:
        movlw   .6              ; [54] prepare to add 6x4 =24 wasted instructions later 66+24=90
        movwf   period_waste    ; [55]
        ;
        ; check if pcnt has reached maximum. If yes, don't add amy further periods
        ;
        nop                     ; [56]
        nop ;movfw   pmodeflag       ; [57]   are we in RPM or frequency mode?
        nop ;andlw   0x07            ; [58]
        nop ;btfss   STATUS, Z       ; [59]
        nop ;movlw   .60             ; [60]           RPM: pre-load W with 60   freq: W is already 0
        addlw   .80             ; [61]   add 80: RPM  W is 60+80=140       freq: W is 0+80 = 80
        subwf   pcnt,w          ; [62]
        btfsc   STATUS, C       ; [63]
        goto    P_done          ; [64+1]  stop if pcnt >= 80 (freq) or >= 140 (rpm)


        ;  period = pstart - gatecnt  (period=pstart; period=period-pstart
        ;
        movlw   .2              ; [66] prepare to add 2x4 =8 wasted instructions later 82+8=90
        movwf   period_waste    ; [67]
        movfw   pstart_hi       ; [68]
        movwf   period_hi       ; [69]
        movfw   pstart_lo       ; [70]
        movwf   period_lo       ; [71]
        bsf     STATUS, C       ; [72] ; 16-bit subtraction with carry
        movfw   gatecnt_lo      ; [73]
        btfss   STATUS, C       ; [74]
        incfsz  gatecnt_lo,w    ; [75]
        subwf   period_lo,f     ; [76]
        movfw   gatecnt_hi      ; [77]
        btfss   STATUS, C       ; [78]
        incfsz  gatecnt_hi,w    ; [79]
        subwf   period_hi,f     ; [80]
        ;
        incf    pcnt,f          ; [81]


P_done: movf    period_waste,w  ; [50, 62, 66, 82  ]
WasteT1:addlw   0xFF            ; [51,    .. ]
        btfss   STATUS, Z       ; [52,    .. ]      eats 4(!) INSTRUCTION CYCLES per loop
        goto    WasteT1         ; [53+1 , .. ]
                        ; Check this with MPLAB-SIM: here, after loop. below should always be [90]
        movfw   freq_lo         ; [90] save previous value from timer 0
        movwf   timer0_old      ; [91]

        nop                     ; [92]
        nop                     ; [93]
        nop                     ; [94]
        nop                     ; [95]
        movfw   gatecnt_hi      ; [96] counter = 0 ?
        iorwf   gatecnt_lo,w    ; [97]
        skpz                    ; [98]
        goto    count1          ; [99,50]  goto always takes TWO instruction cycles


; <<<<<<<<<<<<<<<<<<<<<<<< end of timing loop -----------------------------


        movfw   TMR0            ; get final value from timer 0
        movwf   freq_lo
        movwf	t0last          ; save it for counter mode
        movlw   1               ; determine if timer 0 has rolled
        btfss   timer0_old,7    ; over (rolled over if msb was
        clrw                    ; previously set and now isn't)
        btfsc   freq_lo,7
        clrw

        addwf   freq_ml,f       ; increment high bytes of pulse
        skpnc                   ; counter if low byte rolled
        incf    freq_mh,f       ; over

count3  retlw   0

; end of routine 'count_pulses'.   Result now in   freq_lo..freq_hi.



;--------------------------------------------------------------------------
; Convert *FSR (32 bit) into BCD and show it on the display .
;  Input :  INDF = *FSR, 32-bit integer.
;  Bad side effect : CONTENTS OF <freq> will be lost !!
;--------------------------------------------------------------------------
ShowInt32_FSR   ; Convert <*FSR> (32 bit integer) to 8 BCD-digits ...
        movfw  INDF        ; W   := *FSR   , load LOW byte
        incf   FSR , f     ; FSR := FSR + 1
        movwf  freq        ; freq.hi := W
        movfw  INDF        ; W   := *FSR   , load MIDDLE LOW byte
        incf   FSR , f     ; FSR := FSR + 1
        movwf  freq+1      ; freq.mh := W
        movfw  INDF        ; W   := *FSR   , load MIDDLE HIGH byte
        incf   FSR , f     ; FSR := FSR + 1
        movwf  freq+2      ; freq.ml := W
        movfw  INDF        ; W   := *FSR   , load HIGH byte
        incf   FSR , f     ; FSR := FSR + 1
        movwf  freq+3      ; freq.lo := W
        ; continue with CvtAndDisplayFreq !

;--------------------------------------------------------------------------
; Convert <freq> into BCD and show it on the display .
;  Input :  freq, 32-bit integer.  CONTENTS OF <freq> will be lost !!
;--------------------------------------------------------------------------
CvtAndDisplayFreq  ; Convert <freq>(32 bit integer) to 8 BCD-digits ...
        clrf    tens_index              ; initialise the table index

        movlw   digits                  ; initialise the indirection register
        movwf   FSR                     ; ( FSR="pointer"; *FSR=INDF)

conv1   ; Loop for ALL POWERS OF TEN in the lookup table..
        clrwdt                          ; feed the watchdog (may stay a bit longer)
        movfw   tens_index              ; fetch the next power of ten
        call    TensTable               ; (32 bits) from the lookup table
        movwf   divi+0                  ; and store in divi
        incf    tens_index , f          ; this was the HIGH byte

        movfw   tens_index
        call    TensTable
        movwf   divi+1
        incf    tens_index , f          ; this was the MIDDLE-HIGH byte

        movfw   tens_index
        call    TensTable
        movwf   divi+2
        incf    tens_index , f          ; this was the MIDDLE-LOW byte

        movfw   tens_index
        call    TensTable
        movwf   divi+3
        incf    tens_index , f          ; and this was the LOW-byte of a power of ten

        ; ex: clrf 0  ; clear the decimal digit .. but address ZERO is called 'INDF' these days !
        clrf    INDF                    ; *FSR = 0

conv2   ; Loop to repeatedly subtract divi from freq (32-bit subtract)
        ;         until underflow while incrementing the decimal digit.
        sub32   freq,divi               ; freq := freq - divi  (with divi = 10 power N)
        BNC     conv3                   ;
        incf    INDF , f                ;    The RESULT will be written back to freq,
        goto    conv2                   ;    in other words 'freq' will be lost !

conv3   add32   freq,divi               ; freq := freq+divi;  ready for next digit
        incf    FSR , f                 ; step to next decimal digit
        movlw   8*4                     ; 8 x 4-byte entries in TensTable
        subwf   tens_index,w
        BNZ     conv1                   ; loop until end of table

;--------------------------------------------------------------------------
; displays the frequency in decimal
;--------------------------------------------------------------------------
;
; Display the decimal digits according to the following rules
; input: decimal values in digit_0 (leftmost) .. digit_7 (rightmost); digit_8 = 0
;
; PMODE_ON == 1 + VERY_LOW == 1: very low freq period measurements with 3 decimals
; 01234567
; 0000Abcd =>  A,bcd, ' 1.000' to ' 9.999' Hz  Hz =  blinking points at digit 4 and 7
; 000ABcde => AB,cde, '10.000' to '60.999' Hz  Hz =  blinking points at digit 4 and 7
;
; PMODE_ON == 1: low freq period measurements with 2 decimals
; 01234567
; 00000Abc =>   A,bc, '  1.00' to '  9.99' Hz  Hz =  blinking points at digit 5 and 7
; 0000ABcd =>  AB,cd, ' 10.00' to ' 99.99' Hz  Hz =  blinking points at digit 5 and 7
; 000ABCde => ABC,de, '100.00' to '249.99' Hz  Hz =  blinking points at digit 5 and 7
;
; PMODE_ON = 0: freq counter measurements
; 01234567
; 00000ABC =>  0,ABC  ' 0.250' to ' 0.999' KHz     kHz = blinking point at digit 4
; 0000ABCD =>  A,BCD  ' 1.000' to ' 9.999' KHz     kHz = blinking point at digit 4
; 000ABCDE => AB,CDE  '10.000' to '99.999' KHz     kHz = blinking point at digit 4
; 00ABCDEF => ABC,DE  '100.00' to '999.99' KHz  100KHz = blinking point at digit 4
; 0ABCDEFG => A.BCDE  '1.0000' to '9.9999' Mhz    1MHz = steady point at digit 1
; ABCDEFGH => AB.CDE  '10.000' to '99.999' Mhz   10MHz = steady point at digit 1
; , = blinking point
; . = steady point
;
display_freq:
        btfss   PMODE_ON                ; ifnot period measurement mode
        goto    display_f_int           ; .. go to normal display
        btfss   VERY_LOW                ; ifnot very low frequency
        goto    display_f_2dec          ; .. go to display with 2 decimals

        ; Display routine for very low frequencies with three decimals up to "99.999 Hz" (theoretical):
display_f_3dec:
        movlw   digit_3                 ; find the first significant digit starting from d3 ..
        movwf   FSR                     ; .. by stepping over leading zeroes
        tstf    INDF                    ; INDF = *(FSR) in "C" syntax, FSR points to 'digits'
        BNZ     d_f3d1                  ; 10-Hz-digit non-zero, show frequency in Hz [XX.xxx]
        movlw   BLANK
        movwf   INDF                    ; else blank the leading zero
                                        ; and show frequency in Hz [ X.xxx]
d_f3d1: btfss   blinker, 0              ; check the blink flag (bit 0) for the Hz-points
        goto    display
        bsf     digit_4, 7              ; set two blinking points around the decimals ..
        bsf     digit_7, 7              ; .. indicating Hz
        goto    display                 ; and show the result right aligned

        ; Display routine for frequencies with two decimals up to "999.99 Hz" (theoretical):
display_f_2dec:
        movlw   digit_3                 ; find the first significant digit starting from d3 ..
        movwf   FSR                     ; .. by stepping over leading zeroes
        tstf    INDF                    ; INDF = *(FSR) in "C" syntax, FSR points to 'digits'
        BNZ     d_f2d1                  ; 100-Hz-digit non-zero, show frequency in Hz [XXX.xx]
        movlw   BLANK
        movwf   INDF                    ; else blank the leading zero
        incf    FSR, f                  ; check next digit
        tstf    INDF
        BNZ     d_f2d1                   ; 10-Hz-digit non-zero, show frequency in Hz [ XX.xx]
        movwf   INDF                    ; else blank also this leading zero
                                        ; and show frequency in Hz [  X.xx]
d_f2d1: btfss   blinker, 0              ; ifnot blink flag (bit 0) for the Hz-points
        goto    display_5_lowest        ; .. show the result right aligned
        bsf     digit_5, 7              ; set two blinking points around the decimals ..
        bsf     digit_7, 7              ; .. indicating Hz
        goto    display_5_lowest        ; and show the result right aligned

        ; Display routine for integer frequencies from 1 Hz up to 99.999 MHz (theoretical):
        ; (do NOT insert the decimal point yet,
        ;   it would disturb the blanking of LEADING zeroes )
display_f_int:
        bcf     VERY_LOW                ; leave very low mode
        btfsc   EVENT_ON                ; if event mode
        goto    display_5_lowest        ; .. show 5 rightmost digits without point
        movlw   digits                  ; else find the first significant digit ..
        movwf   FSR                     ; .. by stepping over leading zeroes
        tstf    INDF                    ; INDF = *(FSR) in "C" syntax, FSR points to 'digits'
        BNZ     displ_d0                ; 10-MHz-digit non-zero, show frequency in MHz [XX.XXX]
        incf    FSR, f                  ; otherwise skip 1st digit (the 10-MHz place)
        tstf    INDF
        BNZ     displ_d1                ; 1-MHz-digit non-zero, show frequency in MHz [X.XXXX]
        incf    FSR, f                  ; otherwise skip 2nd digit (the 1-MHz place)
        tstf    INDF
        BNZ     displ_d2                ; 100-kHz-digit non-zero, show frequency in kHz [XXX:XX]
        incf    FSR  ,  f               ; otherwise skip 3rd digit (the 100-kHz place)
        tstf    INDF
        BNZ     displ_d3                ; 10-kHz-digit non-zero, show frequency in kHz [XX:XXX]
                                        ; otherwise show the frequency in kHz [ X:XXX] or [ 0:XXX]
displ_d4:       ; 250 Hz .. 9999 Hz, FSR points at digit_3 (10-kHz-digit)
        btfsc   blinker, 0              ; check the blink flag (bit 0) for the kHz-point
        bsf     digit_4, 7              ; blink the decimal point indicating kHz
        movlw   BLANK                   ; show right aligned as [ 0:XXX] or [ X:XXX]
        goto    display_w

displ_d3:       ;  10000 ..  99999
displ_d2:       ; 100000 .. 999999
        ; left aligned with BLINKING POINT to indicate the kilohertz-digit
        btfsc   blinker, 0              ; check the blink flag (bit 0) for the kHz-point
        bsf     digit_4, 7              ; blink the decimal point indicating kHz
        goto    display

displ_d1:
displ_d0:       ; left aligned with STEADY POINT at megahertz-digit
        bsf     digit_1, 7              ; set the decimal point indicating MHz
        goto    display

display_5_lowest:                       ; show 5 lowest digits
        movlw   digit_3
        movwf   FSR
display:  ; Show the FIVE digits beginning at INDF = *(FSR) on the LED display...
        movfw   INDF                    ; convert the five digits to LED data
display_w:                              ; come here with 1st char in w, e.g. BLANK
        call    conv_char0              ; first visible digit
        incf    FSR  ,  f               ; increment pointer to next digit
        movfw   INDF                    ; w = *(FSR)
        call    conv_char1              ; second visible digit
        incf    FSR  ,  f
        movfw   INDF
        call    conv_char2              ; third visible digit
        incf    FSR  ,  f
        movfw   INDF
        call    conv_char3              ; fourth visible digit
        incf    FSR  ,  f
        movfw   INDF
        goto    conv_char4              ; convert fifth visible digit AND RETURN
; end of routine "CvtAndDisplayFreq"


;--------------------------------------------------------------------------
; main entry point
;--------------------------------------------------------------------------

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

        movlw   TEST                    ; test all LED segments
        call    conv_char0
        movlw   TEST
        call    conv_char1
        movlw   TEST
        call    conv_char2
        movlw   TEST
        call    conv_char3
        movlw   TEST
        call    conv_char4

        movlw   PSC_DIV_BY_256          ; let the prescaler divide by 256 while testing..
        call    SetPrescaler            ; safely write <W> into option register

        ; Do a LAMP TEST for half a second, including all decimal points :
        movlw   (LAMPTEST_LOOPS)>>8     ; high byte for 0.5 second lamp test
        movwf   gatecnt_hi
        movlw   (LAMPTEST_LOOPS)&0ffh   ; low byte for 0.5 second lamp test
        movwf   gatecnt_lo
        call    count_pulses            ; some delay to show the test pattern

        clrf    pmodeflag               ; set default mode (frequency counter)
        btfsc   IOP_SWITCH              ; if switch is high (not pressed) ..
        goto    MainLoop                ; .. save and go
        bsf     EVENT_ON
        movlw   CHAR_C                  ; prepare text "count"
        call    conv_char0
        movlw   CHAR_o
        call    conv_char1
        movlw   CHAR_u
        call    conv_char2
        movlw   CHAR_n
        call    conv_char3
        movlw   CHAR_t
        call    conv_char4
sw_wait:
        ; show the text for 0.1 second
        movlw   (COUNTMODE_LOOPS)>>8    ; high byte for 0.1 second display
        movwf   gatecnt_hi
        movlw   (COUNTMODE_LOOPS)&0ffh  ; low byte for 0.1 second display
        movwf   gatecnt_lo
        call    count_pulses            ; some delay to show the test pattern
        ;
        btfss   IOP_SWITCH              ; if switch is still pressed ..
        goto    sw_wait                 ; .. wait

;--------------------------------------------------------------------------
; main loop :  Preparation, auto ranging, measurement, conversion, display
;--------------------------------------------------------------------------

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
        ;errorlevel +302 ; Enable banking message again

        ; First do a 'range-detection measurement' to find
        ; a suitable prescaler ratio. Worst-case-estimation:
        ; 50 MHz at the input of the async TIMER 0 prescaler
        ; requires a prescaler ratio of 64 because
        ; the synchron counter in TIMER 0 accepts a maximum
        ; frequency of f_osc / 4, here: max. 1 MHz.
        ; The theoretic maximum frequency is 64 MHz then, which
        ; was almost reached when tested with a PIC 16F628 .
        ; The range-detection interval is somewhere near 1/30 seconds (see RANGE_DET_LOOPS),
        ; so frequencies below 30*64 = 1920 Hz are not detectable at this step.
RANGE_DET_LOOPS equ  CLOCK/(.30*CYCLES) ; number of gate-time loops to detect the MEASURING RANGE
                                        ; (which is required to find a good prescaler value)
        movlw   (RANGE_DET_LOOPS)>>8    ; high byte for RANGE DETECTION loop counter
        movwf   gatecnt_hi
        movlw   (RANGE_DET_LOOPS)&0ffh  ; low byte for RANGE DETECTION loop counter
        movwf   gatecnt_lo
        movlw   PSC_DIV_BY_64           ; let the prescaler divide by 64 while testing..
        call    SetPrescaler            ; safely write <W> into option register

        call    count_pulses            ; count pulses for the range detection interval (1/16 sec)
        ; The result will be placed in freq_lo,freq_ml,freq_mh,freq_hi (32 bit)
        ; but the max count at 64 MHz input, 1/30 sec gate time, and prescaler=64 will be :
        ;   64MHz / (30 * 64) = 33333 pulses, so only 16 bits in the counter
        ;  are required here (call them "testcount", f_in = testcount * 30*64) .
        ; The frequency resolution of this coarse measurement is 64*16 Hz = roughly 1 kHz.
        ; (for that reason it's not suited for "wake-up from power-save on frequency-change")


        ; Load the default (soft-)counters for the GATE TIME.
        ; Most measuring ranges use a 1/4 second gate time !
        movlw   (GATE_TIME_LOOPS/4)>>8  ; high byte of gate time
        movwf   gatecnt_hi
        movlw   (GATE_TIME_LOOPS/4)&0ffh ; low byte of gate time
        movwf   gatecnt_lo


        ; Increment the "blinker" once every 0.25 seconds.
        ; If the gate time is longer, flashing will be slower -> showing sample rate.
        incf    blinker, f

        ; Look at the range-detection count ("testcount")
        ; and decide which measuring range to use, beginning with the highest frequency range.

        ; Ranges FOR  20 MHz CRYSTAL (draws more power, but gives better resolution at HF )
        ; Even if PIC clocked with 20MHz, keep the input of TIMER0 below 4(!) MHz.
        ; Rng  testcount    f_in            prescaler gate_time   display,   resolution
        ; (1)     0..6         0.. 11.5 kHz   1       1   second  X.XXXkHz,  0.001kHz (4 digits only)
        ; (2)     7..52         ..101.8 kHz   1       1   second  XX.XXXkHz, 0.001kHz (last digit steps by 1)
        ; (3)    53..2047       ..  3.9 MHz   1       1/4 second  X.XXXXMHz,    4 Hz  (last digit steps by 1)
        ; (4)  2048..4095       ..  7.9 MHz   2       1/4 second  X.XXXXMHz,    8 Hz  (last digit steps by 1)
        ; (5)  4096..8191      ... 15.7 MHz   4       1/4 second  X.XXXXMHz,   16 Hz  (last digit steps by 1)
        ; (6)  8192..16383     ... 31.4 MHz   8       1/4 second  X.XXXXMHz,   32 Hz  (last digit steps by 1 or 2)
        ; (7) 16384..33330     ... 63.9 MHz  16       1/4 second  XX.XXXMHz,   64 Hz  (last digit steps by 1)
        movfw   freq_ml         ; first look at bits 15..8 of the 'test count' result
        andlw   b'11000000'     ; any of bits 15..14 set (>=16384) -> no Z flag -> range 7
        btfss   STATUS,Z        ; skip next instruction if ZERO-flag set (!)
        goto    Range7          ; far jump to range 7
        btfsc   freq_ml,5       ; bit 13 set (>=8192) ->  range 6
        goto    Range6
        btfsc   freq_ml,4       ; bit 12 set (>=4096) ->  range 5
        goto    Range5
        btfsc   freq_ml,3       ; bit 11 set (>=2048) ->  range 4
        goto    Range4
        btfsc   freq_ml,2       ; bit 10 set (>=1024) ->  range 3
        goto    Range3
        btfsc   freq_ml,1       ; bit 9 set (>=512)   ->  range 3
        goto    Range3
        btfsc   freq_ml,0       ; bit 8 set (>=256) -> no Z flag  -> range 3
        goto    Range3
        movfw   freq_lo         ; now look at bits 7..0 only ..
        sublw   .52             ; subtract #52 - W register -> C=0 if result negative
        btfss   STATUS,C        ; skip next instruction if C=1 (#52-W >= 0)
        goto    Range3          ; freq > 100kHz -> also range 3

Range1: ; Range 1:  async prescaler off, 1 second gate time for very low frequencies  :
        call    PrescalerOff    ; turn hardware prescaler off
        ; Load the GATE TIMER (as count of loops) for this measuring range.
        movlw   (GATE_TIME_LOOPS)>>8    ; high byte for 1 second gate time
        movwf   gatecnt_hi
        movlw   (GATE_TIME_LOOPS)&0ffh  ; low byte for 1 second gate time
        movwf   gatecnt_lo
        ; Load the count of "left shifts" to compensate gate time + prescaler :
        movlw   0   ; no need to multiply with prescaler 1:1 and 1-sec gate time
        goto    GoMeasure

#if 0
Range2: ; Range 2:  async prescaler off, 1/2 second gate time for quite low frequencies (not used)
        call    PrescalerOff            ; turn hardware prescaler off
        ; Load the GATE TIMER (as count of loops) for this measuring range.
        movlw   (GATE_TIME_LOOPS/2)>>8  ; high byte for 1/2 second gate time
        movwf   gatecnt_hi
        movlw   (GATE_TIME_LOOPS/2)&0ffh ; low byte for 1/2 second gate time
        movwf   gatecnt_lo
        ; Load the count of "left shifts" to compensate gate time + prescaler :
        movlw   1   ; multiply by 2 (=2^1) later to compensate gate time (1/2 s)
        goto    GoMeasure
#endif

Range3: ; Range 3: async prescaler off, gate time = default (1/4 sec) :
        call    PrescalerOff             ; turn hardware prescaler off
        movlw   2   ; multiply by 4 (=2^2) later to compensate gate time (1/4 s)
        goto    GoMeasure

Range4: ; Range 4: prescaler divide by 2 , gate time = default (1/4 sec) :
        movlw   PSC_DIV_BY_2            ; let the prescaler divide by 2 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        movlw   3   ; multiply by 8 (=2^3) later to compensate prescaling (1:2) * gate time (1/4 s)
        goto    GoMeasure

Range5: ; Range 5: prescaler divide by 4 , gate time = default (1/4 sec) :
        movlw   PSC_DIV_BY_4            ; let the prescaler divide by 2 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        movlw   4   ; multiply by 16 (=2^4) later to compensate prescaling (1:4) * gate time (1/4 s)
        goto    GoMeasure

Range6: ; Range 6: prescaler divide by 8 , gate time = default (1/4 sec) :
        movlw   PSC_DIV_BY_8            ; let the prescaler divide by 2 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        movlw   5   ; multiply by 32 (=2^5) later to compensate prescaling (1:8) * gate time (1/4 s)
        goto    GoMeasure

Range7: ; Range 7: prescaler divide by 16 , gate time = default (1/4 sec) :
        movlw   PSC_DIV_BY_16           ; let the prescaler divide by 2 while MEASURING...
        call    SetPrescaler            ; safely write <W> into option register
        movlw   6   ; multiply by 64 (=2^6) later to compensate prescaling (1:16) * gate time (1/4 s)
        goto    GoMeasure



GoMeasure:
        movwf adjust_shifts             ; save the number of "arithmetic left shifts" for later
        btfss   EVENT_ON                ; ifnot in event mode
        goto    GoMeasure1              ; .. skip the counting stuff

MainLoopCnt:
        ; we are in counter mode
        ; Load the GATE TIMER (as count of loops) for this measuring range.
        movlw   (GATE_TIME_LOOPS/.10)>>8   ; high byte for 1/10 second gate time
        movwf   gatecnt_hi
        movlw   (GATE_TIME_LOOPS/.10)&0ffh ; low byte for 1/10 second gate time
        movwf   gatecnt_lo
        call    count_pulses               ; count pulses for 1/10s
        ; Result in freq_lo,freq_ml,freq_mh,freq_hi (32 bit) now
        addx32  freq,freq2 ; freq2 = freq2 + freq
        ;
        ; add any counts that happened during "dark time"
        movfw   t0dark
        movwf   freq_lo
        clrf    freq_ml
        clrf	freq_mh
        clrf	freq_hi
        addx32  freq,freq2 ; freq2 = freq2 + freq
        ; copy result back into freq for display
        movfw   freq2_hi
        movwf   freq_hi
        movfw   freq2_mh
        movwf   freq_mh
        movfw   freq2_ml
        movwf   freq_ml
        movfw   freq2_lo
        movwf   freq_lo
        ; check for overflow 100000  = 0x0186A0
        ; if so, reset counter
        movfw   freq_mh
        sublw   0x01
        BNC     cntreset  ; >= 0x02xxxx = must reset
        BNZ     cntvalid  ; <= 0x00xxxx = ok
        movfw   freq_ml
        sublw   0x86
        BNC     cntreset  ; >= 0x0187xx = must reset
        BNZ     cntvalid  ; <= 0x0185xx = ok
        movfw   freq_lo
        sublw   0x9F
        BNC     cntreset  ; >= 0x0186A0 = must reset
cntvalid:
        call    CvtAndDisplayFreq

        btfsc   IOP_SWITCH      ; check the switch
        goto    MainLoopCnt     ; not pressed: keep counting...
cntreset:
        clrf    freq2_hi        ; reset counter
        clrf    freq2_mh        ; bits 23..16
        clrf    freq2_ml        ; bits 15..8
        clrf    freq2_lo        ; bits  7..0
        clrf    t0dark
        clrf    t0last
        goto    MainLoopCnt

GoMeasure1:
        ; measure frequency or RPM
        call    count_pulses            ; count pulses for 1, 1/2, or 1/8 s .
        ; Result in freq_lo,freq_ml,freq_mh,freq_hi (32 bit) now,
        ; NOT adjusted for the gate-time or prescaler division ratio yet.

PrepDisp: ; Prepare the frequency (32-bit 'unadjusted' integer) for display:
        ; Multiply freq by 2^adjust_shifts to adjust for the prescaling
        ; and the timing period .  The result will be a frequency in HERTZ, 32-bit integer.
        ; Note: the adjustment factor may be ONE which means no shift at all.
        tstf    adjust_shifts
        BZ      NoAdjust
Adjust: clrc
        rlf     freq_lo , f
        rlf     freq_ml , f
        rlf     freq_mh , f
        rlf     freq_hi , f
        decfsz  adjust_shifts, f
        goto    Adjust

NoAdjust: ; Check the result against under- and overflow.
        ; (There should be none if the frequency didn't change too rapidly
        ;  between the range-detection and the actual measurement )
        movfw   freq_hi         ; underflow (freq = 0) ?
        iorwf   freq_mh,w
        iorwf   freq_ml,w
        iorwf   freq_lo,w
        BZ      freq_underflow  ; show "    0"

        btfsc   freq_hi,7       ; if overflow (freq > 7FFfffffh) ?
        goto    freq_overflow   ; show "E    "


        ; TheHWcave:  This section screens out any frequencies that
        ;             would cause problems or inaccurate calculations
        ;             We only accept frequencies < 256 Hz in frequency mode
        ;
        movf    freq_hi,w
        iorwf   freq_mh,w
        iorwf   freq_ml,w
        BNZ     F_Hirange       ; frequencies > 255 Hz go to extended RPM range check

        ;  frequency  is <= 255 Hz.

P_Zero: movf    pcnt,f          ; zero periods? This could happen at very low
        btfss   STATUS, Z       ; .. freqencies < 2Hz.
        goto    P_Conv          ; .. we must avoid dividing by zero
        movlw   .1              ; .. by preset to 1 x 1s period = 1Hz
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
P_Conv:                         ; come here if f < 256 Hz
        movlw   .62             ; limit to < 62 Hz
        subwf   freq_lo,w       ; .. to examine utility frequencies 50 & 60 Hz
        BC      P_Conv_2        ; skip if f >= 62 Hz
        movlw   VERY_LOW_MASK
        btfss   IOP_SWITCH      ; check the switch
        xorwf   pmodeflag,f     ; .. if pressed toggle VERY_LOW mode
        btfss   VERY_LOW        ; check if VERY_LOW mode
        goto    P_Conv_2        ; .. not set: goto normal 2 decimal mode

P_Conv_3
        ; calculate frequency with 3 decimal digits
        movlw   (PER2FREQ3 >> .24) & 0xff       ; conversion for resolution 1mHz
        movwf   freq2_hi
        movlw   (PER2FREQ3 >> .16) & 0xff
        movwf   freq2_mh
        movlw   (PER2FREQ3 >> .8) & 0xff
        movwf   freq2_ml
        movlw   PER2FREQ3 & 0xff
        movwf   freq2_lo
        goto    P_Conv_end

P_Conv_2
        ; calculate frequency with 2 decimals digits
        bcf     VERY_LOW                        ; exit very low mode
        movlw   (PER2FREQ2 >> .24) & 0xff       ; conversion for resolution 10 mHz
        movwf   freq2_hi
        movlw   (PER2FREQ2 >> .16) & 0xff
        movwf   freq2_mh
        movlw   (PER2FREQ2 >> .8) & 0xff
        movwf   freq2_ml
        movlw   PER2FREQ2 & 0xff
        movwf   freq2_lo
P_Conv_end:
        clrf    freq_hi         ; we re-use the frequency buffer for the 32-bit result
        clrf    freq_mh         ; .. but need to clear it first
        clrf    freq_ml
        clrf    freq_lo

P_Mul:  addx32  freq2, freq ; freq = freq + freq2
        decf    pcnt,f
        btfss   STATUS, Z
        goto    P_Mul

        ; TheHWcave:  This section divides the adjusted conversion factor by the
        ;             sum of the periods. This is a 32-bit divided by 16-bit
        ;             operation using repeated 32-bit subtraction. The result is
        ;             a 24-bit number which is the desired frequency (in millihertz).
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
        movfw   period_hi
        movwf   freq2_ml
        movfw   period_lo
        movwf   freq2_lo
        clrf    pdiv_mh
        clrf    pdiv_ml
        clrf    pdiv_lo
P_Div:  call    RefreshDisplay
        subx32  freq2, freq  ; freq = freq - freq2
        btfss   STATUS, C
        goto    P_DivEnd
        incf    pdiv_lo,f
        btfsc   STATUS, Z
        incf    pdiv_ml,f
        btfsc   STATUS, Z
        incf  pdiv_mh,f
        goto P_Div
P_DivEnd: clrf  freq_hi  ; copy the result back into the freq_xx variable
        movfw   pdiv_mh  ;
        movwf   freq_mh  ;
        movfw   pdiv_ml
        movwf   freq_ml
        movfw   pdiv_lo
        movwf   freq_lo
        bsf     PMODE_ON ; switch to display format XXX.XX Hz

F_Hirange:
        call    CvtAndDisplayFreq       ; Convert <freq> into BCD and show it on the display
        goto    MainLoop                ; end of main loop


RefreshDisplay:
        ; TheHWCave: This is a straight copy from the timing loop
        ; except the watchdog timer is included as well
        ;
        movfw   disp_index              ; [1] get the current digit number (disp_index = 0..4)
        call    Digit2MuxValue          ; [2,3,4,5,6,7] display (6 commands including call+retlw)
        movwf   bTemp                   ; [8] save the bit pattern for the multiplexer port
        movlw   display0                ; [9]  get the LED display data for the current digit...
        addwf   disp_index,w            ; [10] add current digit number to address of LED data
        movwf   FSR                     ; [11] move address into the PIC's poor 'data pointer'
        movfw   INDF                    ; [12] w := *(FSR) use indirection register to read from table
        movwf   LEDS_PORT               ; [13] set the LED segments
        movfw   bTemp                   ; [14] get the mupliplexer pattern (hurry, hurry !)
        movwf   ENABLE_PORT             ; [15] set the LED multiplexer

        incf    disp_timer,f            ; [16] increment display-multiplex timer
        btfsc   disp_timer,6            ; [17] (6-bit prescaler)
        incf    disp_index,f            ; [18] next display if rolled over
        bcf     disp_timer,6            ; [19] limit disp_timer to 6 bits (!)
        movfw   disp_index              ; [20] limit display index to  0...4
        sublw   .4                      ; [21] subtract #4 - W register -> C=0(!) if result negative (W>4)
        btfss   STATUS,C                ; [22] skip next instruction if C=1 (#4-W >= 0)
        clrf    disp_index              ; [23] if C=0 (disp_index>4) then disp_index=0
        clrwdt                          ; feed the dog
        retlw 0


;--------------------------------------------------------------------------
; frequency underflow (frequency < 1Hz)
;--------------------------------------------------------------------------

freq_underflow:
        movlw   BLANK                   ; display underflow as "    0"
        call    conv_char0
        movlw   BLANK
        call    conv_char1
        movlw   BLANK
        call    conv_char2
        movlw   BLANK
        call    conv_char3
        movlw   0
        call    conv_char4              ; use the 5th digit because its there !

        goto    MainLoop


;--------------------------------------------------------------------------
; frequency overflow (frequency > 50MHz)
;--------------------------------------------------------------------------

freq_overflow:
        movlw   CHAR_E                  ; display overflow as "E    "
        call    conv_char0
        movlw   BLANK
        call    conv_char1
        movlw   BLANK
        call    conv_char2
        movlw   BLANK                   ;
        call    conv_char3
        movlw   BLANK
        call    conv_char4              ; use the 5th digit because its there !

        goto    MainLoop                ; end of main loop


        END   ; directive 'end of program'



