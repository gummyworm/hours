.export __rnd_num
.export __rnd_seed
.CODE

__rnd_seed: .word 2019

; generate 1 random byte between 0 and 2^(.X)
.proc __rnd_num
@result=$f0
	lda #$00
	sta @result
@l0:	lsr __rnd_seed
	ror __rnd_seed+1
	bcc :+
	lda __rnd_seed
	eor #$aa  ; most significant bit *must* be set
	sta __rnd_seed
	lda __rnd_seed+1
	eor #$2b  ; least significant bit should be set
	sta __rnd_seed+1
:	rol @result
	dex
	bpl @l0
	lda @result
	rts
.endproc
