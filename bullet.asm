.CODE

.include "constants.inc"
.include "enemy.inc"
.include "player.inc"
.include "screen.inc"
.include "sprite.inc"

.export __blt_add
.export __blt_init
.export __blt_update

;--------------------------------------
.BSS
chars: .res MAX_BULLETS
dirs: .res MAX_BULLETS
xpos: .res MAX_BULLETS
ypos: .res MAX_BULLETS

.CODE
;--------------------------------------
.proc __blt_init
	lda #$ff
	ldx #MAX_BULLETS-1
@l0:	sta chars,x
	dex
	bpl @l0
	rts
.endproc

;--------------------------------------
; __blt_update moves all active bullets and destroys thost that have collided
; with another character.
.proc __blt_update
@cnt=$40
@x=$41
@y=$42
@dir=$43
	lda #MAX_BULLETS-1
	sta @cnt

@l0:	ldx @cnt
	lda chars,x
	bmi @next

	ldy ypos,x
	lda xpos,x
	tax
	;jsr sprite::off
	jsr sprite::pointoff

	ldx @cnt
	lda dirs,x
	sta @dir
	ldy ypos,x
	lda xpos,x
	tax
	lda #BULLET_SPEED
	sta $f0
	lda @dir
	jsr screen::movem
	stx @x
	sty @y
	bcc @remove
@collision:
	ldx @x
	ldy @y
	lda @dir
	jsr enemy::collide1x1
	cmp #$00
	beq @redraw
	jsr player::collide
	bne @redraw
@remove:
	ldx @cnt
	lda #$ff
	sta chars,x
	bmi @next
@redraw:
	ldx @cnt
	lda @x
	sta xpos,x
	lda @y
	sta ypos,x
	tay
	lda chars,x
	ldx @x
	jsr sprite::point
	;jsr sprite::on

@next:	dec @cnt
	bpl @l0
	rts
.endproc

;--------------------------------------
; add puts a new bullet of the character in $f0 to the location (.X,.Y)
; moving in the direction .A.
.proc __blt_add
@x=$20
	pha
	stx @x

	ldx #MAX_BULLETS-1
@l0:	lda chars,x
	cmp #$ff
	beq @found
	dex
	bpl @l0
	pla
	rts	; no free bullets available
@found:
	pla
	sta dirs,x
	tya
	sta ypos,x
	lda @x
	sta xpos,x
	lda $f0
	sta chars,x
	ldx @x
	jsr sprite::point
	;jsr sprite::on
	rts
.endproc
