.CODE
;--------------------------------------
.export __joy_init
.proc __joy_init
        lda #$00
        sta $9113       ;set DDR for VIA #1 to input for joystick
        lda #$7f
        sta $9122       ;set DDR for VIA #2 to input for joy switch 3
        rts
.endproc

.export __joy_fire
.export __joy_up
.export __joy_down
.export __joy_left
.export __joy_right
; .Z is set if Down is pressed
__joy_up:
        lda #$04
	.byte $2c
__joy_down:
	lda #$08
	.byte $2c
__joy_left:
	lda #$10
	.byte $2c
__joy_fire:
	lda #$20
	and $9111
	rts

; .Z is set if Down is pressed
.export __joy_right
__joy_right:
	lda $9120
	and #$80
	rts
