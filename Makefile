all: counter_hires_event.hex counter.hex counter2_DL4YHF.hex


target=counter_hires_event


counter_hires_event.hex: counter_hires_event.asm Makefile
	gpasm $<

counter.hex: counter.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $<

counter2_DL4YHF.hex: counter_DL4YHF.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $< -o $@

compare: counter2_DL4YHF.hex
	diff -u  counter2_DL4YHF.hex DL4YHF/counter2.hex

clean:
	-rm -f *.cod *.lst *~

flash: $(target).hex
	diff -q $< $<.old || (ardpicprog --erase --burn -i $< && cp $< $<.old)

reflash: $(target).hex
	ardpicprog --erase --burn -i $<
