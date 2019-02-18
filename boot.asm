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

.segment "HEADER"
	.word @head
@head:	.word @next
	.word 2019
	.byte $9e
	.asciiz "4109"
@next:	.word 0

.segment "BOOT"
start:
	ldx #$00
	; clear the color and screen mem
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

	jsr joy::init
	lda #$ff	; chars @ $1c00, screen @ $1e00
	sta $9005
	lda #(BORDER_COLOR | (BG_COLOR << 4))|$08
	sta $900f
	lda #(AUX_COLOR << 4) | 8
	sta $900e
	jmp enter
	sei

.CODE
enter:
	jsr gen::screen
	ldx #SCREEN_W*8-15
	ldy #90
	lda #EYE
	jsr enemy::spawn

initui:
	ldx #<splitirq
	ldy #>splitirq
	lda #IRQ_RASTER_START
	jsr irq::raster
	lda #$01
	jsr player::harm
	jmp main

main:	lda nextframe
	bne main
	jsr player::update
	jsr enemy::update
	inc nextframe
	jmp main

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
	jmp $eabf

nextframe: .byte 0
