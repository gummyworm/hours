.include "constants.inc"
.include "joy.inc"
.include "gen.inc"
.include "rand.inc"
.include "player.inc"
.include "screen.inc"
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
:	lda #CHAR_COLOR | $08
	sta COLORMEM,x
	sta COLORMEM+$100,x
	lda #BLANK
	sta SCREEN,x
	sta SCREEN+$100,x
	dex
	bne :-
	jsr joy::init
	lda #$ff	; chars @ $1c00, screen @ $1e00
	sta $9005
	lda #(BORDER_COLOR | (BG_COLOR << 4))
	sta $900f
	lda #(AUX_COLOR << 4)
	sta $900e
	sei
	jmp enter

.CODE
enter:
	jsr gen::screen
	jsr player::off
	jsr player::on
main:
	lda #$6f
	cmp $9004
	bne *-3
	jsr player::update
	jmp main
