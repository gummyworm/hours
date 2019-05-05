.export __util_call

.CODE
;--------------------------------------
.proc __util_call
@addr=$100
	stx @addr
	sty @addr+1
	jmp (@addr)
.endproc
