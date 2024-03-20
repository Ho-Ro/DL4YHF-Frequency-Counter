# SPDX-License-Identifier: GPL-3.0-or-later

TARGET = counter_hires_event

all: $(TARGET).hex counter.hex counter2_DL4YHF.hex

$(TARGET).hex: $(TARGET).asm macros.inc Makefile
	gpasm $<

counter.hex: counter.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $<

counter2_DL4YHF.hex: counter_DL4YHF.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $< -o $@

compare: counter2_DL4YHF.hex
	diff -u  counter2_DL4YHF.hex DL4YHF/counter2.hex

clean:
	-rm -f *.cod *.lst *~

flash: $(TARGET).hex
	diff -q $< $<.old || (ardpicprog --erase --burn -i $< && cp $< $<.old)

reflash: $(TARGET).hex
	ardpicprog --erase --burn -i $<
