all: counter.hex

counter.hex: counter.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $<

compare: counter.hex
	diff -u counter.hex DL4YHF/counter2.hex

