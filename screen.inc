.import __screen_canmove
.import __screen_getchar
.import __screen_move
.import __screen_flip
.import __screen_rvs
.import __screen_buffer
.import  __screen_iter_begin_topleft
.import  __screen_iter_begin_topright
.import __screen_iter_rowmajor_pos
.import __screen_iter_rowmajor_neg
.import __screen_iter_colmajor_neg
.import __screen_iter_colmajor_pos

.scope screen
	canmove = __screen_canmove
	getchar = __screen_getchar
	move = __screen_move
	flip = __screen_flip
	rvs = __screen_rvs
	buffer = __screen_buffer
	iter_rowmajor_pos = __screen_iter_rowmajor_pos
	iter_rowmajor_neg = __screen_iter_rowmajor_neg
	iter_colmajor_neg = __screen_iter_colmajor_neg
	iter_begin_topleft = __screen_iter_begin_topleft
	iter_begin_topright = __screen_iter_begin_topright
	iter_colmajor_pos = __screen_iter_colmajor_pos
.endscope
