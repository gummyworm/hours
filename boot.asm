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

	lda #$ff	; chars @ $1c00, screen @ $1e00
	sta $9005
	lda #(BORDER_COLOR | (BG_COLOR << 4))|$08
	sta $900f
	lda #(AUX_COLOR << 4) | 8
	sta $900e

	; load the first room
	lda #$00
	jsr screen::buffer
	jsr screen::flip

	; spawn an enemy
	ldx #SCREEN_W*8-24
	ldy #90
	lda #EYE
	jsr enemy::spawn
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
	bne main
	jsr player::update
	jsr enemy::update
	jsr blt::update
	inc nextframe
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

	lda #$ff
	sta $9005
	lda #(BORDER_COLOR | (BG_COLOR << 4))|$08
	sta $900f
	lda #$00
	sta nextframe

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
