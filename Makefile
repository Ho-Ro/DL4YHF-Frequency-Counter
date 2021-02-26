all: counter.hex counter_DL4YHF.hex

counter.hex: counter.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $<

counter_DL4YHF.hex: counter_DL4YHF.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $<

compare: counter_DL4YHF.hex
	diff -u $< DL4YHF/counter2.hex

clean:
	-rm -f *.cod *.lst *~
