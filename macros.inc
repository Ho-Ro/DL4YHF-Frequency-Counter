; SPDX-License-Identifier: GPL-3.0-or-later

;**************************************************************************
;                                                                         *
; 16 and 32 bit arithmetic macros                                         *
;                                                                         *
;**************************************************************************

CQUAD   MACRO   value
        retlw   value>>24       ; high byte
        retlw   (value>>16)&0xFF; middle-high byte
        retlw   (value>>8)&0xFF ; middle-low  byte
        retlw   value&0xFF      ; low byte
        ENDM

;--------------------------------------------------------------------------
; MACRO to perform 32-bit subtraction - subtracts the four bytes at src
; from the four bytes at dst (most significant bytes first), returns the
; result in dst with src unchanged and the no carry flag set if underflow
;--------------------------------------------------------------------------

SUBx32  MACRO   src, dst        ;dst := dst - src

        bsf     STATUS, C
        movf    src+3, w
        btfss   STATUS, C
        incfsz  src+3, w
        subwf   dst+3, f
        movf    src+2, w
        btfss   STATUS, C
        incfsz  src+2, w
        subwf   dst+2, f
        movf    src+1, w
        btfss   STATUS, C
        incfsz  src+1, w
        subwf   dst+1, f
        movf    src+0, w
        btfss   STATUS, C
        incfsz  src+0, w
        subwf   dst+0, f

        ENDM


;--------------------------------------------------------------------------
; MACRO to perform 16-bit subtraction - subtracts the two bytes at src
; from the two bytes at dst (most significant bytes first), returns the
; result in dst with src unchanged and the no carry flag set if underflow
;--------------------------------------------------------------------------

SUBx16  MACRO   src, dst        ;dst := dst - src

        bsf     STATUS, C
        movf    src+1, w
        btfss   STATUS, C
        incfsz  src+1, w
        subwf   dst+1, f
        movf    src+0, w
        btfss   STATUS, C
        incfsz  src+0, w
        subwf   dst+0, f

        ENDM


;--------------------------------------------------------------------------
; MACRO to perform 32-bit addition - adds the four bytes at src
; to the four bytes at dst (most significant bytes first), returns the
; result in dst with src unchanged and the no carry flag set if underflow
;--------------------------------------------------------------------------

ADDx32  MACRO   src, dst          ;dst := dst + src

        bcf     STATUS, C
        movf    src+3, w
        btfsc   STATUS, C
        incfsz  src+3, w
        addwf   dst+3, f
        movf    src+2, w
        btfsc   STATUS, C
        incfsz  src+2, w
        addwf   dst+2, f
        movf    src+1, w
        btfsc   STATUS, C
        incfsz  src+1, w
        addwf   dst+1, f
        movf    src+0, w
        btfsc   STATUS, C
        incfsz  src+0, w
        addwf   dst+0, f

        ENDM


;--------------------------------------------------------------------------
; MACROs to move a 16bit and 32bit literal into the two/four bytes at dst
; (most significant bytes first), returns the result in dst
;--------------------------------------------------------------------------

MOVLx32 MACRO   lit, dst        ;dst := lit

        movlw   (lit >> 24) & 0xff
        movwf   dst
        movlw   (lit >> 16) & 0xff
        movwf   dst+1
        movlw   (lit >> 8) & 0xff
        movwf   dst+2
        movlw   lit & 0xff
        movwf   dst+3

        ENDM


MOVLx16 MACRO   lit, dst        ;dst := lit

        movlw   (lit >> 8) & 0xff
        movwf   dst
        movlw   lit & 0ffh
        movwf   dst+1

        ENDM


;**************************************************************************
;                                                                         *
; Macros end                                                              *
;                                                                         *
;**************************************************************************


