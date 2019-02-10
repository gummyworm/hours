;sets up a raster interrupt at the line given in .A to the vector given in
;(<.X, >.Y)
.import __irq_raster

.scope irq
        raster  = __irq_raster 
.endscope

.macro sync
	lda $9114     ; get the NMI timer A value
	; (42 to 49 cycles delay at this stage)
	ldy $9115     ; the reference timer on the screen
	sty $1e01
	cmp #8        ; are we more than 7 cycles ahead of time?
	bcc @0
	pha           ; yes, spend 8 extra cycles
	pla
	and #7        ; and reset the high bit
@0:	cmp #4
	bcc @1
	bit $24       ; waste 4 cycles
	and #3
@1:	cmp #2        ; spend the rest of the cycles
	bcs *+2
	bcs *+2
	lsr
	bcs *+2       ; now it has taken 82 cycles from the beginning of the IRQ
.endmacro
