.CODE

.include "constants.inc"
.include "rand.inc"
.include "screen.inc"
.include "sprite.inc"

.export __enemy_clear
.export __enemy_spawn
.export __enemy_update

;--------------------------------------
num:     .byte $ff		; number of enemies onscreen
enemies: .res MAX_ENEMIES	; sprite (character) ID's
hp:  	 .res MAX_ENEMIES	; health points of each enemy
pos:     .res MAX_ENEMIES*2	; positions of each enemy

;--------------------------------------
; clear removes all enemies. Call this, e.g., during screen transition.
; clear
.proc __enemy_clear
	lda #$ff
	sta num
	rts
.endproc

;--------------------------------------
; spawns an enemy of the sprite in .A at (.X,.Y)
.proc __enemy_spawn
@xpos=$10
@ypos=$11
	pha

	; display the enemy
	stx @xpos
	sty @ypos
	jsr sprite::on

	; set variables for this enemy
	inc num
	ldy num
	pla
	sta enemies,y
	tya
	asl
	tay
	lda @xpos
	sta pos,y
	lda @ypos
	sta pos+1,y
	rts
.endproc

;--------------------------------------
; update updates all active enemies
.proc __enemy_update
@cnt=$f8
@prevx=$f9
@prevy=$fa
@newx=$fb
@newy=$fc
	lda num
	bmi @done
	sta @cnt

@l0:	ldx @cnt
	lda pos,x
	ldy pos+1,x
	sta @prevx
	sty @prevy

	ldx #$01
	jsr rnd::num
	adc @prevx
	sta @newx
	ldx #$01
	jsr rnd::num
	adc @prevy
	sta @newy

	ldx @prevx
	ldy @prevy
	jsr sprite::off

	ldx @newx
	ldy @newy
	jsr screen::canmove
	beq @next
	ldx @prevx
	ldy @prevy
	stx @newx
	sty @newy

@next:	ldx @cnt
	lda enemies,x
	ldx @newx
	ldy @newy
	jsr sprite::on
	dec @cnt
	bpl @l0

@done:	rts
.endproc
