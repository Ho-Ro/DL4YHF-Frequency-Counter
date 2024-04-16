# SPDX-License-Identifier: GPL-3.0-or-later

TARGET = counter_hires_event

PART = PIC16F628A

# use PICkit2 programmer: (implicite erase), program (-m), and (implicite verify)
PROGRAMMER = pk2cmd -p$(PART) -m -j -f

# use ArdPicProg programmer, program and verify
# PROGRAMMER = ardpicprog --erase --burn -i

all: $(TARGET).hex counter.hex counter2_DL4YHF.hex

$(TARGET).hex: $(TARGET).asm macros.inc Makefile
	gpasm $<

counter.hex: counter.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $<

counter2_DL4YHF.hex: counter_DL4YHF.asm Makefile
	gpasm -D DISPLAY_VARIANT_2 $< -o $@

compare: counter2_DL4YHF.hex
	diff -u  counter2_DL4YHF.hex DL4YHF/counter2.hex

.PHONY: clean
clean:
	-rm -f *.cod *.lst *~

# flash only if hex file has changed after last flash
.PHONY: flash
flash: $(TARGET).hex
	@diff -q $< $<.old 2>/dev/null || ( \
	  echo $(PROGRAMMER) $< \
	  && $(PROGRAMMER) $< \
	  && cp $< $<.old \
	)

# flash always
.PHONY: reflash
reflash: $(TARGET).hex
	$(PROGRAMMER) $<
