Basically the program runs in an endless loop, with the exception of the initial lamp test, programming mode, and power-saving mode which are not explained here. In the main loop the following steps are performed:

1. Prepare a coarse frequency measurement for the automatic range switching: Program the asynchronous prescaler to divide by 64, so the highest external frequencies can be detected (theoretically 64 MHz, but this exceeds the PIC's specification).

2. Count the input pulses for 1/16 second, using the PIC's TIMER0 module in counter mode. During this time, the display multiplexer keeps running. In fact, the counting loop takes exactly 50 microseconds, including the multiplexer routine. 1250 counting loops result in a gate time of 1/16 seconds.In the sourcecode, this is done in the subroutine 'count_pulses'.

3. Decide which prescaler and which measuring interval should be used, depending on the coarse frequency measurement from step 2.

4. Reprogram the counter's prescaler so the divided input frequency is below 1 MHz (which is the maximum input frequency for the hardware counter, if the PIC is clocked with 4 MHz).
If the coarse measured frequency is way below 1 MHz, the prescaler is turned off to get the best possible frequency resolution.

5. Count the pulses during the measuring interval (alias gate time), which is 0.25, 0.5, or 1 second. During this time, the display multiplexer keeps running. Overflows of the 8-bit timer register ("hardware") are counted by software in two other 8-bit registers, so the effective pulse counter has 24 bits (8 hardware bits plus 16 software bits while counting).

6. Gate time finished -> stop counting pulses.

7. If the hardware prescaler was active while counting (see step 4), multiply the pulse count with the prescaler ratio so we don't have to care for the prescaler setting in the following steps.
If you know a bit about assembler programming: The multiplicator is always a power of two, so instead of a multiplication, the pulse count value (now expanded to 32 bit) is shifted left which is much easier on a PIC.

8. If the gate time was 0.5 seconds, multiply the pulse count by 2; if the gate time was 0.25 seconds, multiply the pulse count by 4. The result is the input frequency in Hertz, no matter which prescaler ratio or gate time was used. Like in the previous step, this "multiplication" is in fact a simple bit-shifting operation.

9. (Optional) Add the programmed frequency offset. If the result is negative, make it positive.

10. Split the frequency into eight (!) decimal digits. This is tricky with a PIC, see sourcecode. It is realized by repeatedly subtracting powers of ten from the 32-bit frequency value, beginning with ten millions (because the highest, theoretically possible frequency is 64 MHz).

11. Skip leading zeroes, and insert a decimal point after the kHz- or MHz digit (the kHz-point is ANDed with a blink flag)

12. Beginning with the first non-zero digit, convert five digits from binary code into seven-segment-patterns, and copy the result into the "display registers". The display multiplex routine which is executed while counting will write these registers to the LED display in steps 2 and 5 of the next main loop.

13. Poll the 'programming function' input ("RA5"). If this digital input is low, enter programming mode (not explained here). If not, go to step 1 to begin the next measurement.

Sounds tricky? Well, you don't have to understand the internal function as long as you don't want to modify the firmware!
