.include "bullet.inc"
.include "chars.inc"
.include "constants.inc"
.include "enemy.inc"
.include "irq.inc"
.include "joy.inc"
.include "gen.inc"
.include "rand.inc"
.include "player.inc"
.include "screen.inc"
.include "sound.inc"
.include "sprite.inc"
.include "items.inc"

.import __BSS_SIZE__
.import __BSS_LOAD__

;--------------------------------------
.BSS
nextframe: .byte 0
testx: .byte 0
testy: .byte 0

.CODE
;--------------------------------------
	.word enter
	.word enter
	.byte "a0",$C3,$C2,$CD	; A0CBM

;--------------------------------------
enter:
	jsr clrbss

	; seed RNG
	lda $9004
	sta rnd::seed
	sta rnd::seed+1

	; init
	jsr videoinit
	jsr chars::init
	jsr sprite::init
	jsr player::init
	jsr joy::init
	jsr blt::init

	; clear the color and screen mem
	ldx #$00
:	lda #CHAR_COLOR | $08
	sta COLORMEM,x
	sta COLORMEM+$100,x
	lda #BLANK
	sta SCREEN,x
	sta SCREEN+$100,x
	dex
	bne :-

	; bottom row(s) are not multicolor
	lda #$02
	ldx #SCREEN_W*2
:	sta COLORMEM+(SCREEN_W*(SCREEN_H)),x
	dex
	bpl :-

	lda #$fe	; chars @ $1800, screen @ $1e00
	sta $9005
	lda #(BORDER_COLOR | (BG_COLOR << 4))|$08
	sta $900f
	lda #(AUX_COLOR << 4) | 8
	sta $900e

	;jsr spritetest

	; load the first room
	lda #$00
	jsr screen::buffer
	jsr screen::flip

	; spawn an enemy
	lda #AI_FIRE_AT_PLAYER
	sta $f0
	ldx #SCREEN_W*8-24
	ldy #90
	lda #EYE
	jsr enemy::spawn

	lda #AI_FIRE_AT_PLAYER
	sta $f0
	ldx #SCREEN_W*8-24
	ldy #20
	lda #EYE
	;jsr enemy::spawn

	; enable player
	jsr player::on

	lda #ARROW
	sta SCREEN+SCREEN_W*3+6

	; install raster interrupt
	ldx #<splitirq
	ldy #>splitirq
	lda #IRQ_RASTER_START
	jsr irq::raster

initui:
	lda #$01
	jsr player::harm
	lda #SWORD_U
	jsr item::add

main:	lda nextframe
	cmp #$01
	;jsr joy::fire
	bcc main
	jsr sprite::update
	jsr player::update
	jsr enemy::update
	jsr blt::update
	lda #$00
	sta nextframe
	jmp main

;--------------------------------------
splitirq:
	sync
	; wait for 8 raster lines
	lda #$f0
	sta $9005
	lda #$08
	sta $900f
	lda #$00

	jsr snd::update
	; TODO: plenty of raster time here
	lda $9004
	bne *-3

	lda #$fe
	sta $9005
	lda #(BORDER_COLOR | (BG_COLOR << 4))|$08
	sta $900f
	inc nextframe

	bit $9124
	jmp $ff56

;--------------------------------------
.proc clrbss
@addr=$f0
	lda #$00
	sta @addr
	lda #>__BSS_LOAD__
	sta @addr+1

	ldy #<__BSS_LOAD__
@l0:	lda #$00
	sta (@addr),y
	iny
	bne :+
	inc @addr+1
:	cpy #<(__BSS_SIZE__+__BSS_LOAD__)
	bne @l0
	lda @addr+1
	cmp #>(__BSS_SIZE__+__BSS_LOAD__)
	bne @l0
	rts
.endproc

;--------------------------------------
.proc videoinit
	ldx #3
@l0:	lda @init,x
	sta $9000,x
	dex
	bpl @l0
	rts
@init:	.byte $05,$19,$96,$2e
.endproc

.proc spritetest
.BSS
testx2: .byte 0
testy2: .byte 0

.CODE
	lda #EYE
	ldx testx
	ldy testy
	jsr sprite::on

	ldx #20
	stx testx2
	ldy testy2
	jsr sprite::on

	jsr sprite::update
@l0:
	ldx #10
	lda #$00
:	cmp $9004
	bne *-3
	ldy #100
	dey
	bne *-1
	dex
	bne :-

	ldx testx
	ldy testy
	jsr sprite::off

	ldx testx2
	ldy testy2
	jsr sprite::off

	inc testx
	inc testy
	inc testx2
	inc testy2

	lda #EYE
	ldx testx
	ldy testy
	jsr sprite::on
	lda #PLAYER
	ldx testx2
	ldy testy2
	jsr sprite::on
	jsr sprite::update
	jmp @l0
.endproc
