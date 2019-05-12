.export __sound_sfx
.export __sound_song
.export __sound_update

.export __sfx_hit
.export __sfx_hitenemy
.export __sfx_kill
.export __sfx_fire

.BSS
;--------------------------------------
cursfx: .word 0
sfxidx: .byte 0
sfxlen: .byte 0
sfxcnt: .byte 0	; frames to hold current sound for
effect: .byte 0

cursong: .word 0
songidx: .byte 0
tempo: .byte 0

DESCEND = $81
ASCEND = $82

;--------------------------------------
.CODE
sfxtab:
	.word sfx_hit
	.word sfx_hitenemy
	.word sfx_kill
	.word sfx_fire

fxtab:  .word $0000
	.word descend
	.word ascend

;--------------------------------------
sfx_hit:
.byte @hitlen	; length
.byte ASCEND,10,128,195,131,100	; frames to hold sound and values for each voice
@hitlen=*-sfx_hit+1

sfx_hitenemy:
.byte @hitenemylen
.byte 10, 0,0,131,140	; frames to hold sound and values for each voice
@hitenemylen=*-sfx_hitenemy+1

sfx_kill:
.byte @killlen
.byte DESCEND,20,0,0,181,140
@killlen=*-sfx_kill

sfx_fire:
.byte @firelen
.byte DESCEND,20,0,0,181,140
@firelen=*-sfx_fire

;--------------------------------------
__sfx_hit:
	lda #$00
	.byte $2c
__sfx_hitenemy:
	lda #$01
	.byte $2c
__sfx_kill:
	lda #$02
	.byte $2c
__sfx_fire:
	lda #$03
	jmp __sound_sfx

;--------------------------------------
.proc ascend
	inc $900a
	inc $900b
	inc $900c
	rts
.endproc

;--------------------------------------
.proc descend
	dec $900a
	dec $900b
	dec $900c
	rts
.endproc

;--------------------------------------
.proc __sound_update
@src=$f0
@fx=$f0
	lda #$9
	lda cursfx+1
	beq @playsong
@playsfx:
	dec sfxcnt
	beq @continue
	lda effect
	bne @dofx
	rts

@dofx:	asl
	tax
	lda fxtab,x
	sta @fx
	lda fxtab+1,x
	sta @fx+1
	jmp (@fx)

@continue:
	ldy sfxidx
	cpy sfxlen
	bcc @nextstep
@sfxdone:
	lda #$00
	sta sfxidx
	sta cursfx+1
	jsr off
	rts

@nextstep:
	lda #$00
	sta effect

	lda cursfx
	sta @src
	lda cursfx+1
	sta @src+1

	lda (@src),y
	bpl :+
	sec
	sbc #$80
	sta effect
	iny
	lda (@src),y
:	sta sfxcnt
	iny
	lda (@src),y
	sta $900a
	iny
	lda (@src),y
	sta $900b
	iny
	lda (@src),y
	sta $900c
	iny
	lda (@src),y
	sta $900d
	iny
	sty sfxidx
	rts

@playsong:
	rts
.endproc

;--------------------------------------
.proc __sound_sfx
@sfx=$f0
	asl
	tax
	lda sfxtab,x
	sta @sfx
	sta cursfx
	lda sfxtab+1,x
	sta @sfx+1
	sta cursfx+1

	ldy #$00
	lda (@sfx),y
	sta sfxlen

	; set cnt to 1 so that the sound will be played on next update
	lda #$01
	sta sfxcnt
	sta sfxidx
	rts
.endproc

;--------------------------------------
.proc __sound_song
	rts
.endproc

;--------------------------------------
.proc off
	ldy #$03
@l0:	sta $900a,y
	dey
	bpl @l0
	rts
.endproc
