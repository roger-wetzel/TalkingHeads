; SPREADPOINT
; TALKING HEADS (AMIGA, PAL, >= OCS, >= 68000, >= 512 KB)
; (C) 2022 DEPECHE

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o talking_heads -nosym talking_heads.s

AbsExecBase		equ	4
OpenLibrary		equ	-552
CloseLibrary	equ -414
Write			equ -48
Output			equ -60
AvailMem		equ	-216
AllocMem		equ -198
FreeMem			equ -210
TypeOfMem		equ -534
WaitTOF			equ -270
Forbid			equ	-132
Permit			equ	-138
LoadView		equ	-222

custom			equ	$dff000

pwidth			equ 40
pheight			equ 256
psize			equ pwidth*pheight
center			equ psize/2+(pwidth/2)

numdotseyes		equ	118
numdotsufa		equ	534
numdotslfa		equ	568

testing			equ	0
syncstamps		equ 0
framenumber		equ 0

; DMACON
; see http://coppershade.org/articles/Code/Reference/DMACON/
SET				equ	1<<15			; 0=clear, 1=set bits that are set to 1 below
BLTPRI			equ	1<<10			; Blitter DMA priority (over CPU micro) "blitter nasty"
DMAEN			equ	1<<9			; Enable all DMA below
BPLEN			equ	1<<8			; Bit plane DMA
COPEN			equ	1<<7			; Copper DMA
BLTEN			equ	1<<6			; Blitter DMA

*------	ALLOCATE MEMORY AND SAVE STATE -----------------------------------*

base
	movem.l	a0-a6/d0-d7,-(a7)		;
	bsr		alloc					;
	bne		exit					; out of memory error?

	if testing
	move.l	AbsExecBase.w,a6		;
	move.l	#MEMF_CHIP,d1			;
	jsr		AvailMem(a6)			;
	move.l	d0,$210.w				; Free (available) memory
	endif

	move.l	AbsExecBase.w,a6		;	
	moveq	#0,d0					;
	lea		gfx(pc),a1				;
	jsr		OpenLibrary(a6)			; open gfx library
	tst.l	d0						;
	beq		exit					; couldn't open gfx library!
	move.l	d0,a6					;
	move.l 	34(a6),-(a7)			; view
	move.l	d0,-(a7)				; gfx base
	move.l 	38(a6),-(a7)			; copper list
	sub.l	a1,a1					;
	jsr		LoadView(a6)			;
	jsr		WaitTOF(a6)				;
	jsr		WaitTOF(a6)				;
	move.l	AbsExecBase.w,a6		;	
	jsr		Forbid(a6)				;
	
	lea		custom,a6				;
	bsr		waitblitter				;

	move.w	$02(a6),-(a7)			; store DMA control
	move.w	$1c(a6),-(a7)			; store interrupt enable bits
	move.l	$6c.w,-(a7)				; store irq3
	move.w	#$7fff,d0				;
	move.w	d0,$9a(a6)				;
	move.w	d0,$9c(a6)				;
	move.w	d0,$96(a6)				; disable all DMAs

	clr.w	-(a7)					; store LED state
	btst	#1,$bfe001				;
	beq		ledst					;
	not.w	(a7)					;
ledst
	bset	#1,$bfe001				; LED dark

*------	INIT -------------------------------------------------------------*

	lea		dbplanes(pc),a1			;
	move.l	plane1base(pc),d0		; init plane double buffering
	move.l	d0,(a1)+				;
	move.l	plane2base(pc),d0		;
	move.l	d0,(a1)					;

	bsr		waitraster				; no flickering (?)
	move.l	clistbase(pc),$80(a6)	;
	move.w	#SET+DMAEN+BLTPRI+BLTEN,$96(a6) ;
	
	if syncstamps					;
	move.w	#$102,$100.w			; collect sync stamps at this address
	endif							;
	
	bsr		initplayer				;

	lea		glitch1end(pc),a0		; init with "empty" data -> no glitch
	move.l	a0,a1					;
	bsr		initglitch				;

	lea		precalcdata(pc),a0		; init precalculation
	lea		precalcdatapointer(pc),a1
	move.l	a0,(a1)					;

	bsr		lspinit					;
	bsr		waitblitter				;
	bsr		waitraster				; no flickering (?)

	lea		irq3(pc),a0				;
	move.l	a0,$6c.w				; vertical blanking interrupt

	bsr		waitraster				; no flickering (?)

	move.w	#SET+DMAEN+BLTPRI+BLTEN+BPLEN+COPEN,$96(a6) ;

	move.w	#$0030,$9c(a6)			; kill vertb and coper interrupt request
	move.w	#$c030,$9a(a6)			; enable vertb and coper interrupt

*------	MAIN LOOP --------------------------------------------------------*

mainloop
	bsr		precalc					; pre calculate
	move.w	doquit(pc),d0			;
	beq		mainloop				;
	
*------	RESTORE STATE AND EXIT -------------------------------------------*

	bsr		waitblitter				;
	
	tst.w	(a7)+					; restore state
	bne		leddark					;
	bclr	#1,$bfe001				; LED bright
leddark
	move.w	#$7fff,d0				;
	move.w	d0,$9a(a6)				;
	move.w	d0,$9c(a6)				;
	move.w	d0,$96(a6)				;
	
	moveq	#0,d0					; volume to zero
	move.w	d0,$a8(a6)				;
	move.w	d0,$b8(a6)				;
	move.w	d0,$c8(a6)				;
	move.w	d0,$d8(a6)				;
	
	move.l	(a7)+,$6c.w				; 
	move.w	(a7)+,d0				;
	or.w	#$c000,d0				;
	move.w	d0,$9a(a6)				;
	move.w	(a7)+,d0				;
	or.w	#$8000,d0				;
	move.w	d0,$96(a6)				;

	move.l	(a7)+,$80(a6)			; copper list
	move.l	(a7)+,a6				; gfx base
	move.l	(a7)+,a1				; view
	jsr		LoadView(a6)			;
	jsr		WaitTOF(a6)				;
	jsr		WaitTOF(a6)				;
	move.l	a6,a1					; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6		;
	jsr		CloseLibrary(a6)		; close gfx library
	jsr		Permit(a6)				;

	bsr		dealloc					;
exit
	movem.l	(a7)+,a0-a6/d0-d7		;
	moveq	#0,d0					;
	rts								;

gfx	dc.b	"graphics.library",0
	even

dbplanes
	dc.l	0,0						; double buffer planes
doquit
	dc.w	0						; signal to quit
frame
	dc.w	0						; frame counter
	dc.w	0						; frameglitch index

frameglitch
	dc.w	$00b0,1     			;
	dc.w	$03f0,3					;
	dc.w	$0690,11				;
	dc.w	$0810,9					;
	dc.w	$0990,10				;
	dc.w	$0b10,2					;
	dc.w	$0cb0,7					;
	dc.w	$0e20,8					;
	dc.w	$0f20,1					;
	dc.w	$1180,5					;
	dc.w	$13c0,11				;
	dc.w	$1595,8					;
	dc.w	$16a0,4					;
	dc.w	$1840,6					;
	dc.w	$1a00,5					;
	dc.w	$1b4c,4					;
	dc.w	$1d00,5					;
	dc.w	$1e20,1					;
	dc.w	$1f00,2					;
	dc.w	$ffff					; never reached

	rem
	dc.w	$0300,1     ; $1e  30  few
	dc.w	$0400,7     ; $1e  30  few
	dc.w	$0500,10    ; $3e  62  middle
	dc.w	$0600,11    ; $43  67  middle
	dc.w	$0700,4     ; $45  69  middle
	dc.w	$0800,6     ; $51  81  not so many
	dc.w	$0900,9     ; $51  81
	dc.w	$0a00,3     ; $58  88  many in lower area
	dc.w	$0b00,8     ; $58  88
	dc.w	$0c00,5     ; $6c 108  many
	dc.w	$0d00,0     ; $6f 111  many
	dc.w	$0e00,2     ; $b1 177  many many
	dc.w	$ffff					; never reached
	erem

*------	PRINT FRAME NUMBER -----------------------------------------------*

	if framenumber
printframenumber
	move.w 	frame(pc),d0			;
	move.l 	dbplanes(pc),a0			;
	addq.w 	#3,a0					;

	moveq 	#4-1,d7					; 1 word = 4 digits
.digit
	move.w 	d0,d1					;
	and.w 	#$000f,d1				;
	asl.w 	#3,d1					;
	lea		digits(pc,d1.w),a1		;
	move.l	a0,a2 					;
	moveq	#7-1,d1					; height
.print
	move.b	(a1)+,(a2)				;
	add.w	#pwidth,a2				;
	dbf		d1,.print				;
	asr.w	#4,d0					; next digit
	subq.w	#1,a0					; new x position
	dbf		d7,.digit				;
	rts								;

digits
	dc.b	$3e,$7f,$63,$63,$63,$7f,$3e,0		; 0
	dc.b	$0c,$1c,$3c,$0c,$0c,$0c,$0c,0		; 1
	dc.b	$7e,$7f,$03,$0c,$30,$7f,$7f,0		; 2
	dc.b	$7e,$7f,$03,$7f,$03,$7f,$7e,0		; 3
	dc.b	$63,$63,$63,$7f,$03,$03,$03,0		; 4
	dc.b	$7f,$7f,$60,$7e,$03,$7f,$7e,0		; 5
	dc.b	$3f,$7f,$60,$7e,$63,$7f,$3e,0		; 6
	dc.b	$7f,$7f,$06,$0c,$18,$18,$18,0		; 7
	dc.b	$3e,$7f,$63,$3e,$63,$7f,$3e,0		; 8
	dc.b	$3e,$7f,$63,$3f,$03,$7f,$7e,0		; 9
	dc.b	$3e,$7f,$63,$7f,$63,$63,$63,0		; A
	dc.b	$7e,$7f,$63,$7e,$63,$7f,$7e,0		; B
	dc.b	$3f,$7f,$60,$60,$60,$7f,$3f,0		; C
	dc.b	$7e,$7f,$63,$63,$63,$7f,$7e,0		; D
	dc.b	$3f,$7f,$60,$7f,$60,$7f,$3f,0		; E
	dc.b	$3f,$7f,$60,$7f,$60,$60,$60,0		; F

	even
	endif
	
*------	WAIT -------------------------------------------------------------*

; http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0123.html
waitblitter
	btst.b  #14-8,$02(a6)			; DMAB_BLTDONE = 14
waitblitter2
    btst.b  #14-8,$02(a6)			;
    bne		waitblitter2			;
	rts								;

waitraster
	cmp.b	#1,$06(a6)				;
	bne		waitraster				;
	btst	#0,$05(a6)				;
	bne		waitraster				;
	rts								;

*------	PRECALCULATION ---------------------------------------------------*

precalc
	move.l	precalcdatapointer(pc),a0

	if testing
	move.l	clistbase(pc),a2		; control color
	add.w	#precalccolor-clist,a2	;
	move.w	a0,(a2)					; control color
	endif

	lea		playmaindata(pc),a2		;
	tst.b	p_signaltoprecalc(a2)	;
	beq		precalc3				;
	clr.b	p_signaltoprecalc(a2)	;
	moveq	#2*2,d0					; advance pre calc data pointer (consume -1 and frame)
	clr.b	p_signalfromprecalc(a2)	; important
	bra		precend					;

precalc3
	move.w	d_ctrl(a0),d0			; ctrl/cmd
	cmp.w	#-1,d0					; signal player (start to play) and wait?
	bne		precalc2				;
	move.w	frame(pc),d0			; current frame
	cmp.w	d_frame(a0),d0			;
	blt		precalc4				; it's not time (right frame) yet
	tst.b	p_signalfromprecalc(a2)	; avoid race condition (see playcmd9)
	bne		precalc4				;
	st		p_signalfromprecalc(a2)	; -> ready to play
precalc4
	rts								;

precalc2
	cmp.w	#-2,d0					; angles/rotate?
	bne		precalc5				;
	lea		d_angles(a0),a2			; a, b, c
	bsr		mtx						;
	moveq	#8,d0					; consume 4 words: ctrl and 3 values
	bra		precend					;
	
precalc5
	cmp.w	#-3,d0					; pre translation values?
	bne		precalc6				;
	addq.l	#2,a0					; consume ctrl (-3)
	lea		tpointer(pc),a2			;
	move.l	a0,(a2)					;
	moveq	#8,d0					; consume 4 words: ctrl and 3 values
	bra		precend					;

precalc6
	cmp.w	#-4,d0					; post translation (x, y) values?
	bne		precalc7				;
	lea		posttx(pc),a2			;
	move.l	2(a0),(a2)				; long word: post tx and post ty
	bra		precendskip6			; consume 3 words: ctrl and 2 values

precalc7
	move.l	tpointer(pc),a3			; pointer to translation tx, ty, tz (applymtx)
	movem.w	d_buffer(a0),d1/d2		;
	asl.w	#2,d1					; destination (2d): *4 (long word index)
	asl.w	#2,d2					; source (3d): *4 (long word index)
	moveq	#0,d7					; important: upper word = previous offset

	tst.w	d0						; eyes frame (0)?
	bne		prectrl1				;
	lea		eyesframes(pc),a5		;
	move.l	(a5,d2.w),a5			; 3d source
	lea		mesheyesbuffers(pc),a2	;
	move.l	(a2,d1.w),a2			; 2d destination
	lea		2*numdotseyes(a2),a4	; offset to even byte destination (OR value)
	move.w	#numdotseyes-1,d7		;
	bsr		applymtx				;
	bra		precendskip6			;

prectrl1
	cmp.w	#1,d0					; upper face area (1)?
	bne		prectrl2				;
	lea		ufaceframes(pc),a5		;
	move.l	(a5,d2.w),a5			; 3d source
	lea		meshufabuffers(pc),a2	;
	move.l	(a2,d1.w),a2			; 2d destination
	lea		2*numdotsufa(a2),a4		; offset to even byte destination (OR value)
	move.w	#numdotsufa-1,d7		;
	bsr		applymtx				;
	bra		precendskip6			;

prectrl2
	lea		lfaceframes(pc),a5		; lower face area (2)
	move.l	(a5,d2.w),a5			; 3d source
	lea		meshlfabuffers(pc),a2	;
	move.l	(a2,d1.w),a2			; 2d destination
	lea		2*numdotslfa(a2),a4		; offset to even byte destination (OR value)
	move.w	#numdotslfa-1,d7		;
	bsr		applymtx				;

precendskip6
	moveq	#6,d0					; consume 3 words: ctrl and 2 values
precend
	lea		precalcdatapointer(pc),a1
	add.l	d0,(a1)					; advance pre calc data pointer
	lea		precalcdataend(pc),a0	;
	cmp.l	(a1),a0					; done?
	bne		precend2				;
	lea		precalcdata(pc),a0		;
	move.l	a0,(a1)					; restart
	lea		frame(pc),a0			;
	clr.l	(a0)					; reset frame number AND frameglitch index
	bsr		lspinit					; start music from beginning
precend2
	rts								;

precalcdatapointer	dc.l	0
tpointer			dc.l	0	; pointer to tx, ty, tz
posttx				dc.w	0	; post translation x (tx)
postty				dc.w	0	; post translation y (ty)

*------	COPER ------------------------------------------------------------*

coper
	lea		LSPVars(pc),a3			; LSPlayer writes to (double buffered) clist
	move.l	clistbase(pc),a2		;
	add.w	#lspdmacon+3-clist,a2	;
	move.l	a2,m_dmaconPatch(a3)	;
	
	move.w	frame(pc),d0			;
	cmp.w	#50,d0					; Stupid last minute hack (delay music for 1 sec)
	blt		mute					;
	bsr		lspplay					; LSP player tick (call once per frame)
mute
	moveq	#$0010,d0				;
	move.w	d0,$9c(a6)				; delete coper request bit
	move.w	d0,$9c(a6)				; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)				;

irq3exit
	movem.l	(a7)+,a0-a5/d0-d7		;
	rte								;

*------	IRQ3 -------------------------------------------------------------*

irq3
;	move	#$2200,sr				;
	movem.l	a0-a5/d0-d7,-(a7)		;

	move.w	$1e(a6),d0				; read interrupt request bits
	btst	#5,d0					;
	bne		vertb					;

	btst	#4,d0					;
	bne		coper					;
	
	; should never be reached
	bra		irq3exit				;

vertb
	lea		dbplanes(pc),a0			; bitplane double buffering
	move.l	(a0),a1					;
	move.b	boost(pc),d0			;
	bne		booston					;
	move.l	4(a0),(a0)+				;
	move.l	a1,(a0)					;
booston
	move.l	a1,$e0(a6)				;

	tst.b	d0						;
	bne		booston2				;
	bsr		cls						; trashed d0, a0
booston2
	bsr		glitch					;

	btst	#6,$bfe001				; left mouse button pressed?
	bne		action2					;
	lea		doquit(pc),a0			;
	st		(a0)					; -> signal main loop to quit
action2

; cycle sprite color
	lea		bartdata(pc),a5			;
	lea		bartc1(pc),a0			;
	add.w	b_cycling1(a5),a0		;
	move.w	(a0),d1					;
	move.l	clistbase(pc),a1		;
	add.w	#spritecolors+6-clist,a1	;
	move.w	d1,0*16(a1)				;
	move.w	d1,1*16(a1)				;
	move.w	d1,2*16(a1)				;
	move.w	d1,3*16(a1)				;

	bsr		bart					; scrolltext, trashes a2 at least
	bsr		waitblitter				; important
	
	move.l	scene(pc),a0			;
	jsr		(a0)					; call player
	if testing
	move.w	#$0020,$180(a6)			; dark green color indicates free capacity
	endif

	if framenumber
	bsr		printframenumber		;
	endif

	lea		frame(pc),a0			;
	movem.w	(a0),d0/d1				; current frame, frameglitch index
	lea		frameglitch(pc),a1		;
	movem.w	(a1,d1.w),d1/d2			; d2 = glitch number
	cmp.w	d1,d0					; play glitch at this frame?
	bne.s	noglitch				;
	
	addq.w	#4,2(a0)				; advance frameglitch index
	asl.w	#3,d2					; (*8) offset to glitch
	lea		glitches(pc),a2			;
	lea		base(pc),a0				;
	move.l	a0,a1					;
	add.l	(a2,d2.w),a0			;
	add.l	4(a2,d2.w),a1			;
	bsr		setglitch				;
noglitch
	lea		frame(pc),a0			;
	addq.w	#1,(a0)					; advance frame number

	moveq	#$0020,d0				;
	move.w	d0,$9c(a6)				; delete vertb request bit
	move.w	d0,$9c(a6)				; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)				;

	movem.l	(a7)+,a0-a5/d0-d7		;
	rte								;

*------	PLAYER -----------------------------------------------------------*

initplayer
	lea		scenes(pc),a5			;
	
	lea		initplaymain(pc),a0		;
	move.l	a0,0*s_size+s_init(a5)	;
	lea		playmain(pc),a0			;
	move.l	a0,0*s_size+s_player(a5)		;

	lea		scene(pc),a1			;
	move.l	a0,(a1)					; start with main player

	lea		initplayconversation(pc),a0		;
	move.l	a0,1*s_size+s_init(a5)	;
	lea		playconversation(pc),a0	;
	move.l	a0,1*s_size+s_player(a5)		;
	
	lea		playmaindata(pc),a5		;
	lea		playcmds(pc),a0			;
	move.l	a0,p_cmdsstart(a5)		;
	move.l	a0,p_cmdspointer(a5)	;
	lea		playcmdsend(pc),a0		;
	move.l	a0,p_cmdsend(a5)		;

	lea		playthread0data(pc),a5	;
	lea		playcmdst0(pc),a0		;
	move.l	a0,p_cmdsstart(a5)		;
	move.l	a0,p_cmdspointer(a5)	;
	lea		playcmdst0end(pc),a0	;
	move.l	a0,p_cmdsend(a5)		;

	lea		playthread1data(pc),a5	;
	lea		playcmdst1(pc),a0		;
	move.l	a0,p_cmdsstart(a5)		;
	move.l	a0,p_cmdspointer(a5)	;
	lea		playcmdst1end(pc),a0	;
	move.l	a0,p_cmdsend(a5)		;

	lea		playthread2data(pc),a5	;
	lea		playcmdst2(pc),a0		;
	move.l	a0,p_cmdsstart(a5)		;
	move.l	a0,p_cmdspointer(a5)	;
	lea		playcmdst2end(pc),a0	;
	move.l	a0,p_cmdsend(a5)		;

	lea		playthread3data(pc),a5	;
	lea		playcmdst3(pc),a0		;
	move.l	a0,p_cmdsstart(a5)		;
	move.l	a0,p_cmdspointer(a5)	;
	lea		playcmdst3end(pc),a0	;
	move.l	a0,p_cmdsend(a5)		;
	rts								;

playmain
	lea		playmaindata(pc),a5		;
	bsr		play					;
initplaymain
	rts								;

initplayconversation
	lea		playthread0data(pc),a5	;
	clr.b	p_repeatcounter(a5)		;
	clr.b	p_repeatuntilsignal(a5)	;
	move.l	p_cmdsstart(a5),p_cmdspointer(a5)

	lea		playthread1data(pc),a5	;
	clr.b	p_repeatcounter(a5)		;
	clr.b	p_repeatuntilsignal(a5)	;
	move.l	p_cmdsstart(a5),p_cmdspointer(a5)

	lea		playthread2data(pc),a5	;
	clr.b	p_repeatcounter(a5)		;
	clr.b	p_repeatuntilsignal(a5)	;
	move.l	p_cmdsstart(a5),p_cmdspointer(a5)

	lea		playthread3data(pc),a5	;
	clr.b	p_repeatcounter(a5)		;
	clr.b	p_repeatuntilsignal(a5)	;
	move.l	p_cmdsstart(a5),p_cmdspointer(a5)
	rts								;

playconversation
	lea		playthread0data(pc),a5	;
	bsr		play					;
	lea		playthread1data(pc),a5	;
	bsr		play					;
	lea		playthread2data(pc),a5	;
	bsr		play					;
	lea		playthread3data(pc),a5	;
;	bsr		play					; hell yeah
;	rts								; hell yeah

play
	move.l	p_cmdspointer(a5),a0	;
playloop
	move.b	(a0)+,d0				; cmd
	beq		playdone				; end of frame - eof (0)?
	
	subq.b	#1,d0					; set color? (1)
	bne		playcmd2				;
	moveq	#0,d1					; color
	move.b	(a0)+,d1				;
	asl.w	#8,d1					;
	move.b	(a0)+,d1				;
	move.l	clistbase(pc),a1		;
	add.w	#dotcolor-clist,a1		;
	move.w	d1,(a1)					; write color to clist
	bra		playcmddone				;

playcmd2
	subq.b	#1,d0					; repeat? (2)
	bne		playcmd3				;
	move.b	(a0)+,p_repeatcounter(a5)
	move.l	a0,p_cmdspointer(a5)	; start repetitions from here
	bra		playcmddone				;
	
playcmd3
	subq.b	#1,d0					; signal to pre calc? (3)
	bne		playcmd4				;
	st		p_signaltoprecalc(a5)	;
	bra		playcmddone				;

playcmd4
	subq.b	#1,d0					; draw eyes buffer? (4)
	bne		playcmd5				;
	tst.b	p_boost(a5)				;
	bne		playcmddone				;	
	moveq	#0,d1					;
	move.b	(a0)+,d1				; buffer
	asl.w	#2,d1					; create long word offset
	lea		mesheyesbuffers(pc),a2	;
	move.l	(a2,d1.w),a2			;
	lea		2*numdotseyes(a2),a3	;
	move.l	dbplanes(pc),a1			;
	add.l	#center,a1				;
	moveq	#100/20-1,d7			; total: numdotseyes = 118
	bsr		drawdots				; d0 and a0 are not trashed
	moveq	#0,d7					; important
	bsr		draw18dotsmore			;
	bra		playcmddone				;

playcmd5
	subq.b	#1,d0					; draw upper face area buffer? (5)
	bne		playcmd6				;
	tst.b	p_boost(a5)				;
	bne		playcmddone				;	
	moveq	#0,d1					;
	move.b	(a0)+,d1				; buffer
	asl.w	#2,d1					; create long word offset
	lea		meshufabuffers(pc),a2	;
	move.l	(a2,d1.w),a2			;
	lea		2*numdotsufa(a2),a3		;
	move.l	dbplanes(pc),a1			;
	add.l	#center,a1				;
	move.w	#520/20-1,d7			; total: numdotsufa = 534
	bsr		drawdots				; d0 and a0 are not trashed
	moveq	#0,d7					; important
	bsr		draw14dotsmore			;
	bra		playcmddone				;

playcmd6
	subq.b	#1,d0					; draw lower face area buffer? (6)
	bne		playcmd7				;
	tst.b	p_boost(a5)				;
	bne		playcmddone				;	
	moveq	#0,d1					;
	move.b	(a0)+,d1				; buffer
	asl.w	#2,d1					; create long word offset
	lea		meshlfabuffers(pc),a2	;
	move.l	(a2,d1.w),a2			;
	lea		2*numdotslfa(a2),a3		;
	move.l	dbplanes(pc),a1			;
	add.l	#center,a1				;
	move.w	#560/20-1,d7			; total: numdotslfa = 568
	bsr		drawdots				; d0 and a0 are not trashed
	moveq	#0,d7					; important
	bsr		draw8dotsmore			;
	bra		playcmddone				;

playcmd7
	subq.b	#1,d0					; play cmd block? (7)
	bne		playcmd8				;
	moveq	#0,d1					;
	move.b	(a0)+,d1				; block number
	asl.w	#2,d1					; create long word offset
	move.l	a0,p_crp(a5)			; store return pointer
	lea		cmdblocks(pc),a1		;
	lea		base(pc),a0				;
	add.l	(a1,d1.w),a0			; now point to block
	bra		playcmddone				;

playcmd8
	subq.b	#1,d0					; end of block - eob? (8)
	bne		playcmd9				;
	move.l	p_crp(a5),a0			; return to main cmds
	bra		playcmddone				;

playcmd9
	subq.b	#1,d0					; repeat until signal? (9)
	bne		playcmd10				;
	st		p_repeatuntilsignal(a5)	;
	clr.b	p_signalfromprecalc(a5)	;	
	move.l	a0,p_cmdspointer2(a5)	; start repetitions from here
	bra		playcmddone				;
	
playcmd10
	subq.b	#1,d0					; boost on? (10)
	bne		playcmd11				;
	st		p_boostrequest(a5)		; don't switch boost on immediately (needs to draw)
	bra		playcmddone				;

playcmd11
	subq.b	#1,d0					; boost off? (11)
	bne		playcmd12				;
	clr.b	p_boost(a5)				;
	bra		playcmddone				;

playcmd12
	subq.b	#1,d0					; switch scene? (12)
	bne		playcmd13				;
	moveq	#0,d1					;
	move.b	(a0)+,d1				; scene number
	asl.w	#3,d1					; *8 (s_size)
	lea		scenes(pc),a2			;
	move.l	s_init(a2,d1.w),a3		; points to init of scene
	movem.l	a0/a5,-(a7)				;
	jsr		(a3)					; call init
	movem.l	(a7)+,a0/a5				;
	move.l	s_player(a2,d1.w),a3	;
	lea		scene(pc),a4			;
	move.l	a3,(a4)					;
	bra		playcmddone				;
	
playcmd13
	subq.b	#1,d0					; play glitch? (13)
	bne		playcmddone				;
	moveq	#0,d1					;
	move.b	(a0)+,d1				; glitch number
	asl.w	#3,d1					; (*8) offset to glitch
	lea		glitches(pc),a2			;
	movem.l	a0/a5,-(a7)				;
	lea		base(pc),a0				;
	move.l	a0,a1					;
	add.l	(a2,d1.w),a0			;
	add.l	4(a2,d1.w),a1			;
	bsr		setglitch				;
	movem.l	(a7)+,a0/a5				;

playcmddone
	bra		playloop				;
	
playdone
	tst.b	p_boostrequest(a5)		;
	beq		playnobr				;
	st		p_boost(a5)				;
	clr.b	p_boostrequest(a5)		;
playnobr
	tst.b	p_repeatcounter(a5)		;
	beq		playnorepeat			;
	subq.b	#1,p_repeatcounter(a5)	;
	rts								;

playnorepeat
	tst.b	p_repeatuntilsignal(a5)	;
	beq		playnorepeatuntilsignal	;
	tst.b	p_signalfromprecalc(a5)	;
	beq		playsig					; -> no signal from pre calc received: repeat until signal
	clr.b	p_repeatuntilsignal(a5)	;
	clr.b	p_signalfromprecalc(a5)	;
	
	if syncstamps					;
	move.l	a2,-(a7)				;
	sub.l	a2,a2					;
	move.w	$100.w,a2				;
	move.w	frame(pc),(a2)+			; -> start masterseka and read values at $102
	move.w	a2,$100.w				;
	move.l	(a7)+,a2				;
	endif							;
	
	bra		playsig2				;
playsig
	move.l	p_cmdspointer2(a5),a0	;
playsig2

playnorepeatuntilsignal
	cmp.l	p_cmdsend(a5),a0		; end of play data?
	bne		playd2					;
	move.l	p_cmdsstart(a5),a0		; restart
playd2
	move.l	a0,p_cmdspointer(a5)	;
	rts								;

cmd_eof						equ	0
cmd_setcolor				equ	1
cmd_repeat					equ	2
cmd_signaltoprecalc			equ	3
cmd_eyes					equ	4 ; play eyes buffer
cmd_ufa						equ	5 ; play upper face area buffer
cmd_lfa						equ	6 ; play lower face area buffer
cmd_playblock				equ	7
cmd_eob						equ	8
cmd_repeatuntilsignal		equ 9
cmd_booston					equ	10
cmd_boostoff				equ	11
cmd_scene					equ	12
cmd_glitch					equ 13

playmaindata		dc.l	0	; play cmds pointer
					dc.l	0	; cmds start
					dc.l	0	; cmds end
					dc.l	0	; cmds return pointer (crp)
					dc.l	0	; cmds pointer 2
					dc.b	0	; cmds repeat pointer
					dc.b	0	; repeat until signal
					dc.b	0	; signal from precalc: ready to play
					dc.b	0	; signal to precalc: continue pre calc
					dc.b	0	; boost request
boost				dc.b	0	; boost on/off (on: no cls, no drawing)
	rsreset
p_cmdspointer		rs.l	1
p_cmdsstart			rs.l	1
p_cmdsend			rs.l	1
p_crp				rs.l	1
p_cmdspointer2		rs.l	1	; used by cmd_repeatuntilsignal
p_repeatcounter		rs.b	1
p_repeatuntilsignal	rs.b	1
p_signalfromprecalc	rs.b	1
p_signaltoprecalc	rs.b	1
p_boostrequest		rs.b	1
p_boost				rs.b	1
p_size				rs.l	0	; size of this "struct"

	even
playthread0data		ds.b	p_size,0
	even
playthread1data		ds.b	p_size,0
	even
playthread2data		ds.b	p_size,0
	even
playthread3data		ds.b	p_size,0

	even

scene
	dc.l	0
scenes
	dc.l	0,0	; main scene
	dc.l	0,0	; scene conversation
	rsreset
s_init		rs.l	1
s_player	rs.l	1
s_size		rs.l	0

playcmds
	dc.b	cmd_repeatuntilsignal,	cmd_eof	; nop until we receive signal from precalc

	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$01,$11,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,		cmd_setcolor,$0d,$df,	cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,		cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,		cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,		cmd_eof
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,		cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof

	dc.b	cmd_signaltoprecalc	; calc blink
	; play these at least for the next 50 frames
	dc.b	cmd_repeat,50,		cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof

	dc.b	cmd_repeatuntilsignal
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof

	dc.b	cmd_signaltoprecalc	; calc go

; blink
;	dc.b	cmd_glitch,1

	dc.b	cmd_playblock,0,	cmd_eof
	dc.b	cmd_playblock,0,	cmd_eof
	dc.b	cmd_repeatuntilsignal
	dc.b	cmd_playblock,0,	cmd_eof

; go (blink)
	dc.b	cmd_playblock,6,	cmd_eof
	dc.b	cmd_repeat,200
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
; blink
	dc.b	cmd_playblock,0,	cmd_eof

; go (no blink)
	dc.b	cmd_playblock,1,	cmd_eof
	
	dc.b	cmd_signaltoprecalc	; calc shake

; blink	
	dc.b	cmd_playblock,0,	cmd_eof

	dc.b	cmd_booston
	dc.b	cmd_repeat,200
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_boostoff
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof

; blink
	dc.b	cmd_playblock,0,	cmd_eof

	dc.b	cmd_booston
	dc.b	cmd_repeat,150
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_boostoff
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
; blink (short wait at end)
	dc.b	cmd_playblock,8,	cmd_eof

	dc.b	cmd_booston
	dc.b	cmd_repeatuntilsignal
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_boostoff

	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof

; shake
	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,7,	cmd_eof	; shake (right to middle, blink, middle to left)
	dc.b	cmd_playblock,5,	cmd_eof	; shake left to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	
	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,3,	cmd_eof ; shake right to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_playblock,4,	cmd_eof	; shake middle to left
	dc.b	cmd_playblock,5,	cmd_eof	; shake left to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof

	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,7,	cmd_eof	; shake (right to middle, blink, middle to left)
	dc.b	cmd_playblock,5,	cmd_eof	; shake left to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof

	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,3,	cmd_eof ; shake right to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_playblock,4,	cmd_eof	; shake middle to left
	dc.b	cmd_playblock,5,	cmd_eof	; shake left to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof

;	dc.b	cmd_glitch,1

	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,3,	cmd_eof ; shake right to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_playblock,4,	cmd_eof	; shake middle to left
	dc.b	cmd_playblock,5,	cmd_eof	; shake left to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof

;	dc.b	cmd_glitch,1

	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,7,	cmd_eof	; shake (right to middle, blink, middle to left)
	dc.b	cmd_playblock,5,	cmd_eof	; shake left to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof

	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,3,	cmd_eof ; shake right to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_playblock,4,	cmd_eof	; shake middle to left
	dc.b	cmd_playblock,5,	cmd_eof	; shake left to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof

	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,3,	cmd_eof ; shake right to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_playblock,4,	cmd_eof	; shake middle to left
	dc.b	cmd_playblock,5,	cmd_eof	; shake left to middle
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof

	dc.b	cmd_playblock,2,	cmd_eof ; shake middle to right
	dc.b	cmd_playblock,9,	cmd_eof	; fading shake (right to middle, blink, middle to left)

	dc.b	cmd_signaltoprecalc	; conversation
	dc.b	cmd_eof

	dc.b	cmd_repeatuntilsignal,	cmd_eof	; nop until we receive signal from precalc

	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$01,$11, cmd_eof

	dc.b	cmd_signaltoprecalc	; calc (rest of) conversation animation

	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$02,$22, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$03,$33, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$04,$44, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$05,$55, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$06,$66, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$07,$77, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$08,$88, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$09,$99, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$0a,$aa, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$0b,$bb, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$0c,$cc, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$0d,$dd, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$0d,$de, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$0d,$dd, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$0d,$de, cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,	cmd_setcolor,$0d,$df, cmd_eof

	dc.b	cmd_booston
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_repeat,120,	cmd_eof
	dc.b	cmd_boostoff
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof

	dc.b	cmd_eyes,1, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,2, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,3, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,4, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,5, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,6, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,7, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,8, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,9, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,11, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,12, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,13, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,14, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,15, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,16, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,17, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,18, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,19, cmd_ufa,1, cmd_lfa,26,		cmd_eof

	dc.b	cmd_booston
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
	dc.b	cmd_repeat,140,	cmd_eof
	dc.b	cmd_boostoff
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof


	dc.b	cmd_repeatuntilsignal
;	dc.b		cmd_setcolor,$02,$f2  ; testing
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof

;	dc.b		cmd_setcolor,$0f,$00  ; testing

	dc.b	cmd_scene,1
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof
; dummy
	dc.b	cmd_eyes,0, cmd_ufa,0, cmd_lfa,0,		cmd_eyes,10, cmd_ufa,1, cmd_lfa,26,		cmd_eof

	dc.b	cmd_signaltoprecalc	; splash

	dc.b	cmd_repeatuntilsignal,	cmd_eof	; nop until we receive signal from precalc

	dc.b	cmd_playblock,13,	cmd_eof
	dc.b	cmd_playblock,14,	cmd_eof
	dc.b	cmd_playblock,13,	cmd_eof
	dc.b	cmd_playblock,14,	cmd_eof
	dc.b	cmd_playblock,13,	cmd_eof
	dc.b	cmd_playblock,14,	cmd_eof
	dc.b	cmd_playblock,14,	cmd_eof

; sine of the times
	dc.b	cmd_signaltoprecalc	; sine of the times

	dc.b	cmd_repeatuntilsignal,	cmd_eof	; nop until we receive signal from precalc

	dc.b	cmd_signaltoprecalc	; calc rest of sine of the times
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$01,$11,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$0d,$df,	cmd_eof

	; blink	
	dc.b	cmd_playblock,17
	dc.b	cmd_booston
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,	cmd_eof
	dc.b	cmd_repeat,150,		cmd_eof
	dc.b	cmd_boostoff

	; blink	
	dc.b	cmd_playblock,17
	dc.b	cmd_booston
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,	cmd_eof
	dc.b	cmd_repeat,150,		cmd_eof
	dc.b	cmd_boostoff

	; blink	
	dc.b	cmd_playblock,17
	dc.b	cmd_booston
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,	cmd_eof
	dc.b	cmd_repeat,150,		cmd_eof
	dc.b	cmd_boostoff

	; blink	
	dc.b	cmd_playblock,17
	dc.b	cmd_booston
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,	cmd_eof
	dc.b	cmd_repeat,100,		cmd_eof
	dc.b	cmd_boostoff

	dc.b	cmd_repeatuntilsignal
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,	cmd_eof

	dc.b	cmd_playblock,16,	cmd_eof
	dc.b	cmd_playblock,15,	cmd_eof
	dc.b	cmd_playblock,18,	cmd_eof
	dc.b	cmd_playblock,19,	cmd_eof

	dc.b	cmd_playblock,15,	cmd_eof
	dc.b	cmd_playblock,18,	cmd_eof
	dc.b	cmd_playblock,19,	cmd_eof

;	dc.b	cmd_playblock,15,	cmd_eof
;	dc.b	cmd_playblock,18,	cmd_eof
;	dc.b	cmd_playblock,19,	cmd_eof

;	dc.b	cmd_playblock,15,	cmd_eof
;	dc.b	cmd_playblock,18,	cmd_eof
;	dc.b	cmd_playblock,19,	cmd_eof

	dc.b	cmd_playblock,15,	cmd_eof
	dc.b	cmd_playblock,18,	cmd_eof
	dc.b	cmd_playblock,19,	cmd_eof

	dc.b	cmd_playblock,15,	cmd_eof
	dc.b	cmd_playblock,18,	cmd_eof
	
	dc.b	cmd_playblock,20,	cmd_eof	; fade out

; sine of the times: double vision

;	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$01,$11,	cmd_eof
;	dc.b	cmd_setcolor,$0d,$df,	cmd_eof

	dc.b	cmd_repeat,25,	cmd_eof	; wait half a second

	dc.b	cmd_eyes,25,		cmd_ufa,25,		cmd_lfa,25
	dc.b	cmd_eyes,01,		cmd_ufa,01,		cmd_lfa,01,		cmd_setcolor,$01,$11,	cmd_eof

	dc.b	cmd_eyes,26,		cmd_ufa,26,		cmd_lfa,26
	dc.b	cmd_eyes,02,		cmd_ufa,02,		cmd_lfa,02,		cmd_setcolor,$02,$22,	cmd_eof

	dc.b	cmd_eyes,27,		cmd_ufa,27,		cmd_lfa,27
	dc.b	cmd_eyes,03,		cmd_ufa,03,		cmd_lfa,03,		cmd_setcolor,$03,$33,	cmd_eof

	dc.b	cmd_eyes,28,		cmd_ufa,28,		cmd_lfa,28
	dc.b	cmd_eyes,04,		cmd_ufa,04,		cmd_lfa,04,		cmd_setcolor,$04,$44,	cmd_eof

	dc.b	cmd_eyes,29,		cmd_ufa,29,		cmd_lfa,29
	dc.b	cmd_eyes,05,		cmd_ufa,05,		cmd_lfa,05,		cmd_setcolor,$05,$55,	cmd_eof

	dc.b	cmd_eyes,30,		cmd_ufa,30,		cmd_lfa,30
	dc.b	cmd_eyes,06,		cmd_ufa,06,		cmd_lfa,06,		cmd_setcolor,$06,$6,	cmd_eof

	dc.b	cmd_eyes,31,		cmd_ufa,31,		cmd_lfa,31
	dc.b	cmd_eyes,07,		cmd_ufa,07,		cmd_lfa,07,		cmd_setcolor,$07,$77,	cmd_eof

	dc.b	cmd_eyes,32,		cmd_ufa,32,		cmd_lfa,32
	dc.b	cmd_eyes,08,		cmd_ufa,08,		cmd_lfa,08,		cmd_setcolor,$08,$88,	cmd_eof

	dc.b	cmd_eyes,33,		cmd_ufa,33,		cmd_lfa,33
	dc.b	cmd_eyes,09,		cmd_ufa,09,		cmd_lfa,09,		cmd_setcolor,$09,$99,	cmd_eof

	dc.b	cmd_eyes,34,		cmd_ufa,34,		cmd_lfa,34
	dc.b	cmd_eyes,10,		cmd_ufa,10,		cmd_lfa,10,		cmd_setcolor,$0a,$aa,	cmd_eof

	dc.b	cmd_eyes,35,		cmd_ufa,35,		cmd_lfa,35
	dc.b	cmd_eyes,11,		cmd_ufa,11,		cmd_lfa,11,		cmd_setcolor,$0b,$bb,	cmd_eof

	dc.b	cmd_eyes,36,		cmd_ufa,36,		cmd_lfa,36
	dc.b	cmd_eyes,12,		cmd_ufa,12,		cmd_lfa,12,		cmd_setcolor,$0c,$cc,	cmd_eof

	dc.b	cmd_eyes,37,		cmd_ufa,37,		cmd_lfa,37
	dc.b	cmd_eyes,13,		cmd_ufa,13,		cmd_lfa,13,		cmd_setcolor,$0d,$dd,	cmd_eof

	dc.b	cmd_eyes,38,		cmd_ufa,38,		cmd_lfa,38
	dc.b	cmd_eyes,14,		cmd_ufa,14,		cmd_lfa,14,		cmd_setcolor,$0d,$de,	cmd_eof

	dc.b	cmd_eyes,39,		cmd_ufa,39,		cmd_lfa,39
	dc.b	cmd_eyes,15,		cmd_ufa,15,		cmd_lfa,15,		cmd_setcolor,$0d,$df,	cmd_eof

	dc.b	cmd_playblock,24,	cmd_eof
	dc.b	cmd_playblock,22,	cmd_eof
	dc.b	cmd_playblock,23,	cmd_eof

	dc.b	cmd_playblock,21,	cmd_eof
	dc.b	cmd_playblock,22,	cmd_eof
	dc.b	cmd_playblock,23,	cmd_eof

	dc.b	cmd_playblock,21,	cmd_eof
	dc.b	cmd_playblock,22,	cmd_eof
	dc.b	cmd_playblock,23,	cmd_eof

	dc.b	cmd_playblock,21,	cmd_eof
	dc.b	cmd_playblock,22,	cmd_eof
;	dc.b	cmd_playblock,23,	cmd_eof

	dc.b	cmd_eyes,35,		cmd_ufa,35,		cmd_lfa,35
	dc.b	cmd_eyes,11,		cmd_ufa,11,		cmd_lfa,11,		cmd_setcolor,$0d,$de,	cmd_eof

	dc.b	cmd_eyes,36,		cmd_ufa,36,		cmd_lfa,36
	dc.b	cmd_eyes,12,		cmd_ufa,12,		cmd_lfa,12,		cmd_setcolor,$0d,$dd,	cmd_eof

	dc.b	cmd_eyes,37,		cmd_ufa,37,		cmd_lfa,37
	dc.b	cmd_eyes,13,		cmd_ufa,13,		cmd_lfa,13,		cmd_setcolor,$0c,$cc,	cmd_eof

	dc.b	cmd_eyes,38,		cmd_ufa,38,		cmd_lfa,38
	dc.b	cmd_eyes,14,		cmd_ufa,14,		cmd_lfa,14,		cmd_setcolor,$0b,$bb,	cmd_eof

	dc.b	cmd_eyes,39,		cmd_ufa,39,		cmd_lfa,39
	dc.b	cmd_eyes,15,		cmd_ufa,15,		cmd_lfa,15,		cmd_setcolor,$0a,$aa,	cmd_eof

	dc.b	cmd_eyes,40,		cmd_ufa,40,		cmd_lfa,40
	dc.b	cmd_eyes,16,		cmd_ufa,16,		cmd_lfa,16,		cmd_setcolor,$09,$99,	cmd_eof

	dc.b	cmd_eyes,41,		cmd_ufa,41,		cmd_lfa,41
	dc.b	cmd_eyes,17,		cmd_ufa,17,		cmd_lfa,17,		cmd_setcolor,$08,$88,	cmd_eof

	dc.b	cmd_eyes,42,		cmd_ufa,42,		cmd_lfa,42
	dc.b	cmd_eyes,18,		cmd_ufa,18,		cmd_lfa,18,		cmd_setcolor,$07,$77,	cmd_eof

	dc.b	cmd_eyes,43,		cmd_ufa,43,		cmd_lfa,43
	dc.b	cmd_eyes,19,		cmd_ufa,19,		cmd_lfa,19,		cmd_setcolor,$06,$66,	cmd_eof

	dc.b	cmd_eyes,44,		cmd_ufa,44,		cmd_lfa,44
	dc.b	cmd_eyes,20,		cmd_ufa,20,		cmd_lfa,20,		cmd_setcolor,$05,$55,	cmd_eof

	dc.b	cmd_eyes,45,		cmd_ufa,45,		cmd_lfa,45
	dc.b	cmd_eyes,21,		cmd_ufa,21,		cmd_lfa,21,		cmd_setcolor,$04,$44,	cmd_eof

	dc.b	cmd_eyes,46,		cmd_ufa,46,		cmd_lfa,46
	dc.b	cmd_eyes,22,		cmd_ufa,22,		cmd_lfa,22,		cmd_setcolor,$03,$33,	cmd_eof

	dc.b	cmd_eyes,47,		cmd_ufa,47,		cmd_lfa,47
	dc.b	cmd_eyes,23,		cmd_ufa,23,		cmd_lfa,23,		cmd_setcolor,$02,$22,	cmd_eof

	dc.b	cmd_eyes,48,		cmd_ufa,48,		cmd_lfa,48
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$01,$11,	cmd_eof

	dc.b	cmd_setcolor,$00,$00,	cmd_eof

; falling
	rem
	dc.b	cmd_signaltoprecalc ; falling (part 1)
	dc.b	cmd_repeatuntilsignal,	cmd_eof	; nop until we receive signal from precalc

	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$01,$11,	cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,51,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,52,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,53,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,54,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,55,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,56,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,57,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,58,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,59,	cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$0d,$df,	cmd_eof

	dc.b	cmd_signaltoprecalc	; falling (part 2)

	dc.b	cmd_booston
	dc.b	cmd_repeat,80
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_boostoff
	dc.b	cmd_playblock,26,	cmd_eof

	dc.b	cmd_booston
	dc.b	cmd_repeat,60
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_boostoff
	dc.b	cmd_playblock,26,	cmd_eof

	dc.b	cmd_booston
	dc.b	cmd_repeatuntilsignal
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_boostoff
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_eof

	dc.b	cmd_eyes,05,		cmd_ufa,05,		cmd_lfa,05,		cmd_eof
	dc.b	cmd_eyes,06,		cmd_ufa,06,		cmd_lfa,06,		cmd_eof
	dc.b	cmd_eyes,07,		cmd_ufa,07,		cmd_lfa,07,		cmd_eof
	dc.b	cmd_eyes,08,		cmd_ufa,08,		cmd_lfa,08,		cmd_eof
	dc.b	cmd_eyes,09,		cmd_ufa,09,		cmd_lfa,09,		cmd_eof
	dc.b	cmd_eyes,10,		cmd_ufa,10,		cmd_lfa,10,		cmd_eof
	dc.b	cmd_eyes,11,		cmd_ufa,11,		cmd_lfa,11,		cmd_eof
	dc.b	cmd_eyes,12,		cmd_ufa,12,		cmd_lfa,12,		cmd_eof
	dc.b	cmd_eyes,13,		cmd_ufa,13,		cmd_lfa,13,		cmd_eof
	dc.b	cmd_eyes,14,		cmd_ufa,14,		cmd_lfa,14,		cmd_eof
	dc.b	cmd_eyes,15,		cmd_ufa,15,		cmd_lfa,15,		cmd_eof
	dc.b	cmd_eyes,16,		cmd_ufa,16,		cmd_lfa,16,		cmd_eof
	dc.b	cmd_eyes,17,		cmd_ufa,17,		cmd_lfa,17,		cmd_eof
	dc.b	cmd_eyes,18,		cmd_ufa,18,		cmd_lfa,18,		cmd_eof
	dc.b	cmd_eyes,19,		cmd_ufa,19,		cmd_lfa,19,		cmd_eof
	dc.b	cmd_eyes,20,		cmd_ufa,20,		cmd_lfa,20,		cmd_eof
	dc.b	cmd_eyes,21,		cmd_ufa,21,		cmd_lfa,21,		cmd_eof
	dc.b	cmd_eyes,22,		cmd_ufa,22,		cmd_lfa,22,		cmd_eof
	dc.b	cmd_eyes,23,		cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_eyes,24,		cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,25,		cmd_ufa,25,		cmd_lfa,25,		cmd_eof
	dc.b	cmd_eyes,26,		cmd_ufa,26,		cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,27,		cmd_ufa,27,		cmd_lfa,27,		cmd_eof
	dc.b	cmd_eyes,28,		cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,29,		cmd_ufa,29,		cmd_lfa,29,		cmd_eof
	dc.b	cmd_eyes,30,		cmd_ufa,30,		cmd_lfa,30,		cmd_eof
	dc.b	cmd_eyes,31,		cmd_ufa,31,		cmd_lfa,31,		cmd_eof
	dc.b	cmd_eyes,32,		cmd_ufa,32,		cmd_lfa,32,		cmd_eof

	dc.b	cmd_repeat,60
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_playblock,25,	cmd_eof

	dc.b	cmd_repeat,40
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_playblock,25,	cmd_eof

;	dc.b	cmd_repeat,40
;	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,	cmd_eof
;	dc.b	cmd_playblock,25,	cmd_eof

;	dc.b	cmd_repeat,20
;	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,	cmd_eof
;	dc.b	cmd_playblock,25,	cmd_eof

	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_setcolor,$01,$11,	cmd_eof
	erem

	dc.b	cmd_signaltoprecalc	; wake up (restart)
	dc.b	cmd_eof
playcmdsend

; blocks
; important: a block must end with cmd_eob (not cmd_eof)

cmdblock0 ; blink
	dc.b	cmd_eyes,50,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,51,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,52,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,53,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,54,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,55,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,56,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,57,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,58,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,59,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof

	dc.b	cmd_repeat,200
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20
	dc.b	cmd_eob

cmdblock1 ; go (not blinking)
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,1,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,2,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,3,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,4,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,5,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,6,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,7,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,8,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,9,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,10,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,11,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,12,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,13,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,14,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,15,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,16,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,17,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,18,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,19,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,21,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,22,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,23,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,24,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,25
	dc.b	cmd_eob

cmdblock2	; shake (middle to right)
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,	cmd_eof
	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25,	cmd_eof
	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26,	cmd_eof
	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,	cmd_eof
	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29,	cmd_eof
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30,	cmd_eof
	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33,	cmd_eof
	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34,	cmd_eof
	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35,	cmd_eof
	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36,	cmd_eof
	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37,	cmd_eof
	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38,	cmd_eof
	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39,	cmd_eof
	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40,	cmd_eof
	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41,	cmd_eof
	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42,	cmd_eof
	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43,	cmd_eof
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,	cmd_eof
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,	cmd_eof	; quadratic
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,	cmd_eof
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45			; quadratic
	dc.b	cmd_eob

cmdblock3	; shake (right to middle)
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,	cmd_eof
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,	cmd_eof ; quadratic
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,	cmd_eof
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,	cmd_eof	; quadratic
	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43,	cmd_eof
	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42,	cmd_eof
	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41,	cmd_eof
	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40,	cmd_eof
	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39,	cmd_eof
	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38,	cmd_eof
	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37,	cmd_eof
	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36,	cmd_eof
	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35,	cmd_eof
	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34,	cmd_eof
	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31,	cmd_eof
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30,	cmd_eof
	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,	cmd_eof
	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27,	cmd_eof
	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26,	cmd_eof
	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25,	cmd_eof
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24
	dc.b	cmd_eob

cmdblock4	; shake (middle to left)
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22,	cmd_eof
	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,	cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,	cmd_eof
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,	cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,	cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,	cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,	cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,	cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,	cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,	cmd_eof
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,	cmd_eof
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,	cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9,	cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8,	cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7,	cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6,	cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5,	cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4,	cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3,	cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,	cmd_eof ; quadratic
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,	cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0			; quadratic
	dc.b	cmd_eob

cmdblock5 ; shake (left to middle)
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,	cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,	cmd_eof	; quadratic
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,	cmd_eof ; quadratic
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2,	cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3,	cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4,	cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5,	cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6,	cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7,	cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8,	cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9,	cmd_eof
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,	cmd_eof
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,	cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,	cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,	cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,	cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,	cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,	cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,	cmd_eof
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,	cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,	cmd_eof
; 20 is in use (go stop frame)
	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,	cmd_eof
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22
	dc.b	cmd_eob

cmdblock6 ; go (blinking eyes)
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,1,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,2,	cmd_eof
	dc.b	cmd_eyes,50,	cmd_ufa,20,		cmd_lfa,3,	cmd_eof
	dc.b	cmd_eyes,51,	cmd_ufa,20,		cmd_lfa,4,	cmd_eof
	dc.b	cmd_eyes,52,	cmd_ufa,20,		cmd_lfa,5,	cmd_eof
	dc.b	cmd_eyes,53,	cmd_ufa,20,		cmd_lfa,6,	cmd_eof
	dc.b	cmd_eyes,54,	cmd_ufa,20,		cmd_lfa,7,	cmd_eof
	dc.b	cmd_eyes,55,	cmd_ufa,20,		cmd_lfa,8,	cmd_eof
	dc.b	cmd_eyes,56,	cmd_ufa,20,		cmd_lfa,9,	cmd_eof
	dc.b	cmd_eyes,57,	cmd_ufa,20,		cmd_lfa,10,	cmd_eof
	dc.b	cmd_eyes,58,	cmd_ufa,20,		cmd_lfa,11,	cmd_eof
	dc.b	cmd_eyes,59,	cmd_ufa,20,		cmd_lfa,12,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,13,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,14,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,15,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,16,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,17,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,18,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,19,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,21,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,22,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,23,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,24,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,25
	dc.b	cmd_eob

cmdblock7 ; shake (right to middle, blink, middle to left)
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,	cmd_eof
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,	cmd_eof ; quadratic
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,	cmd_eof
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,	cmd_eof	; quadratic
	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43,	cmd_eof
	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42,	cmd_eof
	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41,	cmd_eof
	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40,	cmd_eof
	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39,	cmd_eof
	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38,	cmd_eof
	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37,	cmd_eof
	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36,	cmd_eof
	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35,	cmd_eof
	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34,	cmd_eof
	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31,	cmd_eof
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30,	cmd_eof
	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29,	cmd_eof
	dc.b	cmd_eyes,69,	cmd_ufa,28,		cmd_lfa,28,	cmd_eof
	dc.b	cmd_eyes,68,	cmd_ufa,27,		cmd_lfa,27,	cmd_eof
	dc.b	cmd_eyes,67,	cmd_ufa,26,		cmd_lfa,26,	cmd_eof
	dc.b	cmd_eyes,66,	cmd_ufa,25,		cmd_lfa,25,	cmd_eof
	dc.b	cmd_eyes,65,	cmd_ufa,24,		cmd_lfa,24, cmd_eof

	dc.b	cmd_eyes,64,	cmd_ufa,23,		cmd_lfa,23, cmd_eof
	
	dc.b	cmd_eyes,63,	cmd_ufa,22,		cmd_lfa,22,	cmd_eof
	dc.b	cmd_eyes,62,	cmd_ufa,21,		cmd_lfa,21,	cmd_eof
	dc.b	cmd_eyes,61,	cmd_ufa,19,		cmd_lfa,19,	cmd_eof
	dc.b	cmd_eyes,60,	cmd_ufa,18,		cmd_lfa,18,	cmd_eof

	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,	cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,	cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,	cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,	cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,	cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,	cmd_eof
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,	cmd_eof
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,	cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9,	cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8,	cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7,	cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6,	cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5,	cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4,	cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3,	cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,	cmd_eof ; quadratic
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,	cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0			; quadratic
	dc.b	cmd_eob

cmdblock8 ; blink (short wait)
	dc.b	cmd_eyes,50,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,51,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,52,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,53,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,54,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,55,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,56,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,57,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,58,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof
	dc.b	cmd_eyes,59,	cmd_ufa,20,		cmd_lfa,20,	cmd_eof

	dc.b	cmd_repeat,100
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20
	dc.b	cmd_eob

cmdblock9 ; fading shake (right to middle, blink, middle to left)
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,	cmd_eof
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,	cmd_eof ; quadratic
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,	cmd_eof
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,	cmd_eof	; quadratic
	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43,	cmd_eof
	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42,	cmd_eof
	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41,	cmd_eof
	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40,	cmd_eof
	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39,	cmd_eof
	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38,	cmd_eof
	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37,	cmd_eof
	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36,	cmd_eof
	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35,	cmd_eof
	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34,	cmd_eof
	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33,	cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,	cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31,	cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30,	cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29,	cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,69,	cmd_ufa,28,		cmd_lfa,28,	cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,68,	cmd_ufa,27,		cmd_lfa,27,	cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,67,	cmd_ufa,26,		cmd_lfa,26,	cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,66,	cmd_ufa,25,		cmd_lfa,25,	cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,65,	cmd_ufa,24,		cmd_lfa,24, cmd_setcolor,$06,$66,	cmd_eof

	dc.b	cmd_eyes,64,	cmd_ufa,23,		cmd_lfa,23, cmd_setcolor,$05,$55,	cmd_eof
	
	dc.b	cmd_eyes,63,	cmd_ufa,22,		cmd_lfa,22,	cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,62,	cmd_ufa,21,		cmd_lfa,21,	cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,61,	cmd_ufa,19,		cmd_lfa,19,	cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,60,	cmd_ufa,18,		cmd_lfa,18,	cmd_setcolor,$01,$11
	dc.b	cmd_eob

cmdblock10	; conversation blink go left
	dc.b	cmd_eyes,0,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,0
	dc.b	cmd_eob

cmdblock11	; conversation blink tschak right
	dc.b	cmd_eyes,10,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,11,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,18,	cmd_ufa,1,	cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,1
	dc.b	cmd_eob

cmdblock12	; conversation lower face go head (left)
	dc.b	cmd_lfa,0,		cmd_eof
	dc.b	cmd_lfa,1,		cmd_eof
	dc.b	cmd_lfa,2,		cmd_eof
	dc.b	cmd_lfa,3,		cmd_eof
	dc.b	cmd_lfa,4,		cmd_eof
	dc.b	cmd_lfa,5,		cmd_eof
	dc.b	cmd_lfa,6,		cmd_eof
	dc.b	cmd_lfa,7,		cmd_eof
	dc.b	cmd_lfa,8,		cmd_eof
	dc.b	cmd_lfa,9,		cmd_eof
	dc.b	cmd_lfa,10,		cmd_eof
	dc.b	cmd_lfa,11,		cmd_eof
	dc.b	cmd_lfa,12,		cmd_eof
	dc.b	cmd_lfa,13,		cmd_eof
	dc.b	cmd_lfa,14,		cmd_eof
	dc.b	cmd_lfa,15,		cmd_eof
	dc.b	cmd_lfa,16,		cmd_eof
	dc.b	cmd_lfa,17,		cmd_eof
	dc.b	cmd_lfa,18,		cmd_eof
	dc.b	cmd_lfa,19,		cmd_eof
	dc.b	cmd_lfa,20,		cmd_eof
	dc.b	cmd_lfa,21,		cmd_eof
	dc.b	cmd_lfa,22,		cmd_eof
	dc.b	cmd_lfa,23,		cmd_eof
	dc.b	cmd_lfa,24,		cmd_eof
	dc.b	cmd_lfa,25,		cmd_eof
	dc.b	cmd_repeat,80,	cmd_lfa,0,		cmd_eof

	dc.b	cmd_lfa,0,		cmd_eof
	dc.b	cmd_lfa,1,		cmd_eof
	dc.b	cmd_lfa,2,		cmd_eof
	dc.b	cmd_lfa,3,		cmd_eof
	dc.b	cmd_lfa,4,		cmd_eof
	dc.b	cmd_lfa,5,		cmd_eof
	dc.b	cmd_lfa,6,		cmd_eof
	dc.b	cmd_lfa,7,		cmd_eof
	dc.b	cmd_lfa,8,		cmd_eof
	dc.b	cmd_lfa,9,		cmd_eof
	dc.b	cmd_lfa,10,		cmd_eof
	dc.b	cmd_lfa,11,		cmd_eof
	dc.b	cmd_lfa,12,		cmd_eof
	dc.b	cmd_lfa,13,		cmd_eof
	dc.b	cmd_lfa,14,		cmd_eof
	dc.b	cmd_lfa,15,		cmd_eof
	dc.b	cmd_lfa,16,		cmd_eof
	dc.b	cmd_lfa,17,		cmd_eof
	dc.b	cmd_lfa,18,		cmd_eof
	dc.b	cmd_lfa,19,		cmd_eof
	dc.b	cmd_lfa,20,		cmd_eof
	dc.b	cmd_lfa,21,		cmd_eof
	dc.b	cmd_lfa,22,		cmd_eof
	dc.b	cmd_lfa,23,		cmd_eof
	dc.b	cmd_lfa,24,		cmd_eof
	dc.b	cmd_lfa,25,		cmd_eof
	dc.b	cmd_repeat,130,	cmd_lfa,0,		cmd_eof

	dc.b	cmd_lfa,0
	dc.b	cmd_eob

cmdblock13	; splash (w/o blink)
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$01,$11,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,		cmd_setcolor,$0d,$df,	cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,		cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,		cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,		cmd_eof
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,		cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof

	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,		cmd_eof
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22,		cmd_eof
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25,		cmd_eof
	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27,		cmd_eof

	dc.b	cmd_repeat,70
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof

	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$01,$11,	cmd_eof

	dc.b	cmd_repeat,25,	cmd_eof	; wait a short time

	dc.b	cmd_eob

cmdblock14	; splash (with blink)
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_setcolor,$01,$11,	cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,		cmd_setcolor,$0d,$df,	cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,		cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,		cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,		cmd_eof

	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,		cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof

	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,		cmd_eof
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22,		cmd_eof
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25,		cmd_eof
	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27,		cmd_eof

	dc.b	cmd_repeat,20
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof

	dc.b	cmd_eyes,31,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,33,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,34,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,35,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,36,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,37,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,38,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,39,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof

	dc.b	cmd_repeat,40
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof

	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_setcolor,$01,$11,	cmd_eof

	dc.b	cmd_repeat,25,	cmd_eof	; wait a short time

	dc.b	cmd_eob

cmdblock15 ; sine of the times
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,		cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2,		cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3,		cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4,		cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5,		cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6,		cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7,		cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8,		cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9,		cmd_eof
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,		cmd_eof
cmdblock19
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,		cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,		cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,		cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,		cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,		cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,		cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,		cmd_eof
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,		cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof
	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,		cmd_eof
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22,		cmd_eof
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
cmdblock16
	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25,		cmd_eof
	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27,		cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29,		cmd_eof
	
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30,		cmd_eof
	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31,		cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_eof
	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33,		cmd_eof
	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34,		cmd_eof
	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35,		cmd_eof
	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36,		cmd_eof
	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37,		cmd_eof
	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38,		cmd_eof
	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39,		cmd_eof
	
	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40,		cmd_eof
	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41,		cmd_eof
	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42,		cmd_eof
	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43,		cmd_eof
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,		cmd_eof
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,		cmd_eof
	dc.b	cmd_eyes,46,	cmd_ufa,46,		cmd_lfa,46,		cmd_eof
	dc.b	cmd_eyes,47,	cmd_ufa,47,		cmd_lfa,47,		cmd_eof
	dc.b	cmd_eyes,48,	cmd_ufa,48,		cmd_lfa,48,		cmd_eof
	dc.b	cmd_eyes,47,	cmd_ufa,47,		cmd_lfa,47,		cmd_eof
	dc.b	cmd_eyes,46,	cmd_ufa,46,		cmd_lfa,46,		cmd_eof
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,		cmd_eof
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,		cmd_eof
	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43,		cmd_eof
	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42,		cmd_eof
	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41,		cmd_eof

	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40,		cmd_eof
	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39,		cmd_eof
	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38,		cmd_eof
	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37,		cmd_eof
	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36,		cmd_eof
	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35,		cmd_eof
	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34,		cmd_eof
	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33,		cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_eof
	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31,		cmd_eof
	
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30,		cmd_eof
	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29,		cmd_eof
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof	
	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27,		cmd_eof
	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25,		cmd_eof
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof	
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22,		cmd_eof	
	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,		cmd_eof
	
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_eof	
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,		cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,		cmd_eof	
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,		cmd_eof	
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,		cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,		cmd_eof	
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,		cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,		cmd_eof
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,		cmd_eof	

	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,		cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9,		cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8,		cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7,		cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6,		cmd_eof	
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5,		cmd_eof	
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4,		cmd_eof	
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3,		cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2,		cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1,		cmd_eof
	
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0
	dc.b	cmd_eob

cmdblock17
	dc.b	cmd_eyes,50,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,51,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,52,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,53,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,54,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,55,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,56,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,57,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,58,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24
	dc.b	cmd_eob

cmdblock18
	dc.b	cmd_eyes,61,		cmd_ufa,1,		cmd_lfa,1,		cmd_eof
	dc.b	cmd_eyes,62,		cmd_ufa,2,		cmd_lfa,2,		cmd_eof
	dc.b	cmd_eyes,63,		cmd_ufa,3,		cmd_lfa,3,		cmd_eof
	dc.b	cmd_eyes,64,		cmd_ufa,4,		cmd_lfa,4,		cmd_eof
	dc.b	cmd_eyes,65,		cmd_ufa,5,		cmd_lfa,5,		cmd_eof
	dc.b	cmd_eyes,66,		cmd_ufa,6,		cmd_lfa,6,		cmd_eof
	dc.b	cmd_eyes,67,		cmd_ufa,7,		cmd_lfa,7,		cmd_eof
	dc.b	cmd_eyes,68,		cmd_ufa,8,		cmd_lfa,8,		cmd_eof
	dc.b	cmd_eyes,69,		cmd_ufa,9,		cmd_lfa,9,		cmd_eof
	dc.b	cmd_eyes,10,		cmd_ufa,10,		cmd_lfa,10
	dc.b	cmd_eob

cmdblock20
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,		cmd_setcolor,$0d,$de,	cmd_eof
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,		cmd_setcolor,$0d,$dd,	cmd_eof
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,		cmd_setcolor,$0c,$cc,	cmd_eof
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,		cmd_setcolor,$0b,$bb,	cmd_eof
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,		cmd_setcolor,$0a,$aa,	cmd_eof
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,		cmd_setcolor,$09,$99,	cmd_eof
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,		cmd_setcolor,$08,$88,	cmd_eof
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,		cmd_setcolor,$07,$77,	cmd_eof
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_setcolor,$06,$66,	cmd_eof
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_setcolor,$05,$55,	cmd_eof
	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,		cmd_setcolor,$04,$44,	cmd_eof
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22,		cmd_setcolor,$03,$33,	cmd_eof
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_setcolor,$02,$22,	cmd_eof
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,		cmd_setcolor,$01,$11,	cmd_eof
	dc.b	cmd_eob

cmdblock21 ; sine of the times (two heads)
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof
	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1
	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25,		cmd_eof
	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2
	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26,		cmd_eof
	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3
	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27,		cmd_eof
	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof
	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5
	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29,		cmd_eof
	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30,		cmd_eof
	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7
	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31,		cmd_eof
	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_eof
	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9
	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33,		cmd_eof
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10
	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34,		cmd_eof

cmdblock23
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11
	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35,		cmd_eof

	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12
	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36,		cmd_eof

	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13
	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37,		cmd_eof

	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14
	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38,		cmd_eof

	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15
	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39,		cmd_eof
cmdblock24
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16
	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40,		cmd_eof

	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17
	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41,		cmd_eof

	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18
	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42,		cmd_eof

	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19
	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43,		cmd_eof

	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,		cmd_eof

	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,		cmd_eof

	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22
	dc.b	cmd_eyes,46,	cmd_ufa,46,		cmd_lfa,46,		cmd_eof

	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23
	dc.b	cmd_eyes,47,	cmd_ufa,47,		cmd_lfa,47,		cmd_eof

	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24
	dc.b	cmd_eyes,48,	cmd_ufa,48,		cmd_lfa,48,		cmd_eof

	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25
	dc.b	cmd_eyes,47,	cmd_ufa,47,		cmd_lfa,47,		cmd_eof

	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26
	dc.b	cmd_eyes,46,	cmd_ufa,46,		cmd_lfa,46,		cmd_eof

	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27
	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45,		cmd_eof

	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28
	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44,		cmd_eof

	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29
	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43,		cmd_eof
	
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30
	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42,		cmd_eof

	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31
	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41,		cmd_eof

	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32
	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40,		cmd_eof

	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33
	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39,		cmd_eof

	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34
	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38,		cmd_eof

	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35
	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37,		cmd_eof

	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36
	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36,		cmd_eof

	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37
	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35,		cmd_eof

	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38
	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34,		cmd_eof

	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39
	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33,		cmd_eof
	
	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32,		cmd_eof

	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41
	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31,		cmd_eof

	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30,		cmd_eof

	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43
	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29,		cmd_eof

	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44
	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28,		cmd_eof

	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45
	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27,		cmd_eof

	dc.b	cmd_eyes,46,	cmd_ufa,46,		cmd_lfa,46
	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26,		cmd_eof

	dc.b	cmd_eyes,47,	cmd_ufa,47,		cmd_lfa,47
	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25,		cmd_eof

	dc.b	cmd_eyes,48,	cmd_ufa,48,		cmd_lfa,48
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24,		cmd_eof

	dc.b	cmd_eyes,47,	cmd_ufa,47,		cmd_lfa,47
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof

	dc.b	cmd_eyes,46,	cmd_ufa,46,		cmd_lfa,46
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22,		cmd_eof

	dc.b	cmd_eyes,45,	cmd_ufa,45,		cmd_lfa,45
	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,		cmd_eof

	dc.b	cmd_eyes,44,	cmd_ufa,44,		cmd_lfa,44
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof

	dc.b	cmd_eyes,43,	cmd_ufa,43,		cmd_lfa,43
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_eof

	dc.b	cmd_eyes,42,	cmd_ufa,42,		cmd_lfa,42
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,		cmd_eof

	dc.b	cmd_eyes,41,	cmd_ufa,41,		cmd_lfa,41
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,		cmd_eof

	dc.b	cmd_eyes,40,	cmd_ufa,40,		cmd_lfa,40
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,		cmd_eof

	dc.b	cmd_eyes,39,	cmd_ufa,39,		cmd_lfa,39
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,		cmd_eof

	dc.b	cmd_eyes,38,	cmd_ufa,38,		cmd_lfa,38
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,		cmd_eof

	dc.b	cmd_eyes,37,	cmd_ufa,37,		cmd_lfa,37
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,		cmd_eof

	dc.b	cmd_eyes,36,	cmd_ufa,36,		cmd_lfa,36
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,		cmd_eof

	dc.b	cmd_eyes,35,	cmd_ufa,35,		cmd_lfa,35
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,		cmd_eof

	dc.b	cmd_eyes,34,	cmd_ufa,34,		cmd_lfa,34
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,		cmd_eof

	dc.b	cmd_eyes,33,	cmd_ufa,33,		cmd_lfa,33
	dc.b	cmd_eyes,09,	cmd_ufa,09,		cmd_lfa,09,		cmd_eof

	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32
	dc.b	cmd_eyes,08,	cmd_ufa,08,		cmd_lfa,08,		cmd_eof

	dc.b	cmd_eyes,31,	cmd_ufa,31,		cmd_lfa,31
	dc.b	cmd_eyes,07,	cmd_ufa,07,		cmd_lfa,07,		cmd_eof
	
	dc.b	cmd_eyes,30,	cmd_ufa,30,		cmd_lfa,30
	dc.b	cmd_eyes,06,	cmd_ufa,06,		cmd_lfa,06,		cmd_eof

	dc.b	cmd_eyes,29,	cmd_ufa,29,		cmd_lfa,29
	dc.b	cmd_eyes,05,	cmd_ufa,05,		cmd_lfa,05,		cmd_eof

	dc.b	cmd_eyes,28,	cmd_ufa,28,		cmd_lfa,28
	dc.b	cmd_eyes,04,	cmd_ufa,04,		cmd_lfa,04,		cmd_eof	

	dc.b	cmd_eyes,27,	cmd_ufa,27,		cmd_lfa,27
	dc.b	cmd_eyes,03,	cmd_ufa,03,		cmd_lfa,03,		cmd_eof

	dc.b	cmd_eyes,26,	cmd_ufa,26,		cmd_lfa,26
	dc.b	cmd_eyes,02,	cmd_ufa,02,		cmd_lfa,02,		cmd_eof

	dc.b	cmd_eyes,25,	cmd_ufa,25,		cmd_lfa,25
	dc.b	cmd_eyes,01,	cmd_ufa,01,		cmd_lfa,01,		cmd_eof

	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24
	dc.b	cmd_eyes,00,	cmd_ufa,00,		cmd_lfa,00,		cmd_eof

	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23
	dc.b	cmd_eyes,01,	cmd_ufa,01,		cmd_lfa,01,		cmd_eof	

	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22
	dc.b	cmd_eyes,02,	cmd_ufa,02,		cmd_lfa,02,		cmd_eof	

	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21
	dc.b	cmd_eyes,03,	cmd_ufa,03,		cmd_lfa,03,		cmd_eof
	
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20
	dc.b	cmd_eyes,04,	cmd_ufa,04,		cmd_lfa,04,		cmd_eof

	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19
	dc.b	cmd_eyes,05,	cmd_ufa,05,		cmd_lfa,05,		cmd_eof	

	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18
	dc.b	cmd_eyes,06,	cmd_ufa,06,		cmd_lfa,06,		cmd_eof

	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17
	dc.b	cmd_eyes,07,	cmd_ufa,07,		cmd_lfa,07,		cmd_eof	

	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16
	dc.b	cmd_eyes,08,	cmd_ufa,08,		cmd_lfa,08,		cmd_eof	

	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15
	dc.b	cmd_eyes,09,	cmd_ufa,09,		cmd_lfa,09,		cmd_eof

	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14
	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10,		cmd_eof	

	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13
	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11,		cmd_eof

	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12
	dc.b	cmd_eyes,12,	cmd_ufa,12,		cmd_lfa,12,		cmd_eof

	dc.b	cmd_eyes,11,	cmd_ufa,11,		cmd_lfa,11	
	dc.b	cmd_eyes,13,	cmd_ufa,13,		cmd_lfa,13,		cmd_eof	

	dc.b	cmd_eyes,10,	cmd_ufa,10,		cmd_lfa,10
	dc.b	cmd_eyes,14,	cmd_ufa,14,		cmd_lfa,14,		cmd_eof

	dc.b	cmd_eyes,9,		cmd_ufa,9,		cmd_lfa,9
	dc.b	cmd_eyes,15,	cmd_ufa,15,		cmd_lfa,15,		cmd_eof

	dc.b	cmd_eyes,8,		cmd_ufa,8,		cmd_lfa,8
	dc.b	cmd_eyes,16,	cmd_ufa,16,		cmd_lfa,16,		cmd_eof

	dc.b	cmd_eyes,7,		cmd_ufa,7,		cmd_lfa,7
	dc.b	cmd_eyes,17,	cmd_ufa,17,		cmd_lfa,17,		cmd_eof

	dc.b	cmd_eyes,6,		cmd_ufa,6,		cmd_lfa,6
	dc.b	cmd_eyes,18,	cmd_ufa,18,		cmd_lfa,18,		cmd_eof	

	dc.b	cmd_eyes,5,		cmd_ufa,5,		cmd_lfa,5
	dc.b	cmd_eyes,19,	cmd_ufa,19,		cmd_lfa,19,		cmd_eof	

	dc.b	cmd_eyes,4,		cmd_ufa,4,		cmd_lfa,4
	dc.b	cmd_eyes,20,	cmd_ufa,20,		cmd_lfa,20,		cmd_eof	

	dc.b	cmd_eyes,3,		cmd_ufa,3,		cmd_lfa,3
	dc.b	cmd_eyes,21,	cmd_ufa,21,		cmd_lfa,21,		cmd_eof

	dc.b	cmd_eyes,2,		cmd_ufa,2,		cmd_lfa,2
	dc.b	cmd_eyes,22,	cmd_ufa,22,		cmd_lfa,22,		cmd_eof

	dc.b	cmd_eyes,1,		cmd_ufa,1,		cmd_lfa,1
	dc.b	cmd_eyes,23,	cmd_ufa,23,		cmd_lfa,23,		cmd_eof
	
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0
	dc.b	cmd_eyes,24,	cmd_ufa,24,		cmd_lfa,24
	dc.b	cmd_eob

cmdblock22
	dc.b	cmd_eyes,61,		cmd_ufa,1,		cmd_lfa,1
	dc.b	cmd_eyes,25,		cmd_ufa,25,		cmd_lfa,25,		cmd_eof

	dc.b	cmd_eyes,62,		cmd_ufa,2,		cmd_lfa,2
	dc.b	cmd_eyes,26,		cmd_ufa,26,		cmd_lfa,26,		cmd_eof

	dc.b	cmd_eyes,63,		cmd_ufa,3,		cmd_lfa,3
	dc.b	cmd_eyes,27,		cmd_ufa,27,		cmd_lfa,27,		cmd_eof

	dc.b	cmd_eyes,64,		cmd_ufa,4,		cmd_lfa,4
	dc.b	cmd_eyes,28,		cmd_ufa,28,		cmd_lfa,28,		cmd_eof

	dc.b	cmd_eyes,65,		cmd_ufa,5,		cmd_lfa,5
	dc.b	cmd_eyes,29,		cmd_ufa,29,		cmd_lfa,29,		cmd_eof

	dc.b	cmd_eyes,66,		cmd_ufa,6,		cmd_lfa,6
	dc.b	cmd_eyes,30,		cmd_ufa,30,		cmd_lfa,30,		cmd_eof

	dc.b	cmd_eyes,67,		cmd_ufa,7,		cmd_lfa,7
	dc.b	cmd_eyes,31,		cmd_ufa,31,		cmd_lfa,31,		cmd_eof

	dc.b	cmd_eyes,68,		cmd_ufa,8,		cmd_lfa,8
	dc.b	cmd_eyes,32,		cmd_ufa,32,		cmd_lfa,32,		cmd_eof

	dc.b	cmd_eyes,69,		cmd_ufa,9,		cmd_lfa,9
	dc.b	cmd_eyes,33,		cmd_ufa,33,		cmd_lfa,33,		cmd_eof

	dc.b	cmd_eyes,10,		cmd_ufa,10,		cmd_lfa,10
	dc.b	cmd_eyes,34,		cmd_ufa,34,		cmd_lfa,34
	dc.b	cmd_eob

cmdblock25 ; falling blinking eyes
	dc.b	cmd_eyes,41,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,42,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,43,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,44,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,45,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,46,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,47,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,48,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof
	dc.b	cmd_eyes,49,		cmd_ufa,32,		cmd_lfa,32,	cmd_eof

	dc.b	cmd_repeat,110
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32, cmd_eof
	dc.b	cmd_eyes,32,	cmd_ufa,32,		cmd_lfa,32
	dc.b	cmd_eob

cmdblock26 ; falling start (blinking)
	dc.b	cmd_repeat,30
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0,		cmd_eof

	dc.b	cmd_eyes,51,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,52,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,53,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,54,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,55,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,56,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,57,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,58,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,59,	cmd_ufa,0,		cmd_lfa,0,		cmd_eof
	dc.b	cmd_eyes,0,		cmd_ufa,0,		cmd_lfa,0
	dc.b	cmd_eob

; threads

playcmdst0 ; eyes and upper face go head (left)
	dc.b	cmd_repeat,50
	dc.b	cmd_eyes,0,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_playblock,10,	cmd_eof

	dc.b	cmd_repeat,100
	dc.b	cmd_eyes,0,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_playblock,10,	cmd_eof

	dc.b	cmd_repeat,110
	dc.b	cmd_eyes,0,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_playblock,10,	cmd_eof

	dc.b	cmd_repeat,100
	dc.b	cmd_eyes,0,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_playblock,10,	cmd_eof

	dc.b	cmd_repeat,90
	dc.b	cmd_eyes,0,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_playblock,10,	cmd_eof

	dc.b	cmd_repeat,130
	dc.b	cmd_eyes,0,		cmd_ufa,0,	cmd_eof
	dc.b	cmd_playblock,10,	cmd_eof
playcmdst0end

playcmdst1 ; eyes and upper face go (former tschak) head (right)
	dc.b	cmd_repeat,100
	dc.b	cmd_eyes,10,		cmd_ufa,1,	cmd_eof
	dc.b	cmd_playblock,11,	cmd_eof

	dc.b	cmd_repeat,80
	dc.b	cmd_eyes,10,		cmd_ufa,1,	cmd_eof
	dc.b	cmd_playblock,11,	cmd_eof

	dc.b	cmd_repeat,130
	dc.b	cmd_eyes,10,		cmd_ufa,1,	cmd_eof
	dc.b	cmd_playblock,11,	cmd_eof

	dc.b	cmd_repeat,40
	dc.b	cmd_eyes,10,		cmd_ufa,1,	cmd_eof
	dc.b	cmd_playblock,11,	cmd_eof

	dc.b	cmd_repeat,120
	dc.b	cmd_eyes,10,		cmd_ufa,1,	cmd_eof
	dc.b	cmd_playblock,11,	cmd_eof
playcmdst1end

playcmdst2 ; lower face go head (left)  MASTER thread

	dc.b	cmd_playblock,12,	cmd_eof

	dc.b	cmd_playblock,12,	cmd_eof
	dc.b	cmd_repeat,20,		cmd_lfa,0,	cmd_eof

	dc.b	cmd_playblock,12,	cmd_eof

	dc.b	cmd_playblock,12,	cmd_eof

	dc.b	cmd_playblock,12,	cmd_eof

	dc.b	cmd_lfa,0,		cmd_setcolor,$0c,$ce, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$0c,$cd, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$0c,$cc, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$0b,$bb, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$0a,$aa, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$09,$99, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$08,$88, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$07,$77, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$06,$66, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$05,$55, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$04,$44, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$03,$33, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$02,$22, cmd_eof
	dc.b	cmd_lfa,0,		cmd_setcolor,$00,$00, cmd_eof

	dc.b	cmd_scene,0,	cmd_eof
playcmdst2end

playcmdst3 ; lower face go (former tschak) head (left)
	dc.b	cmd_repeat,50,	cmd_lfa,26,		cmd_eof

	dc.b	cmd_lfa,27,		cmd_eof
	dc.b	cmd_lfa,28,		cmd_eof
	dc.b	cmd_lfa,29,		cmd_eof
	dc.b	cmd_lfa,30,		cmd_eof
	dc.b	cmd_lfa,31,		cmd_eof
	dc.b	cmd_lfa,32,		cmd_eof
	dc.b	cmd_lfa,33,		cmd_eof
	dc.b	cmd_lfa,34,		cmd_eof
	dc.b	cmd_lfa,35,		cmd_eof

	dc.b	cmd_lfa,35,		cmd_eof

	dc.b	cmd_lfa,36,		cmd_eof

	dc.b	cmd_lfa,36,		cmd_eof
	dc.b	cmd_lfa,36,		cmd_eof
	dc.b	cmd_lfa,36,		cmd_eof

	dc.b	cmd_lfa,37,		cmd_eof
	dc.b	cmd_lfa,38,		cmd_eof
	dc.b	cmd_lfa,40,		cmd_eof
	dc.b	cmd_lfa,41,		cmd_eof
	dc.b	cmd_lfa,42,		cmd_eof
	dc.b	cmd_lfa,43,		cmd_eof
	dc.b	cmd_lfa,44,		cmd_eof
	dc.b	cmd_lfa,45,		cmd_eof
	dc.b	cmd_lfa,46,		cmd_eof
	dc.b	cmd_lfa,47,		cmd_eof
	dc.b	cmd_lfa,48,		cmd_eof
	dc.b	cmd_lfa,49,		cmd_eof
	dc.b	cmd_lfa,50,		cmd_eof
	dc.b	cmd_lfa,51,		cmd_eof
;	dc.b	cmd_lfa,52,		cmd_eof
;	dc.b	cmd_lfa,53,		cmd_eof
;	dc.b	cmd_lfa,54,		cmd_eof
;	dc.b	cmd_lfa,55,		cmd_eof
	dc.b	cmd_repeat,90,	cmd_lfa,26,		cmd_eof

	dc.b	cmd_lfa,27,		cmd_eof
	dc.b	cmd_lfa,28,		cmd_eof
	dc.b	cmd_lfa,29,		cmd_eof
	dc.b	cmd_lfa,30,		cmd_eof
	dc.b	cmd_lfa,31,		cmd_eof
	dc.b	cmd_lfa,32,		cmd_eof
	dc.b	cmd_lfa,33,		cmd_eof
	dc.b	cmd_lfa,34,		cmd_eof
	dc.b	cmd_lfa,35,		cmd_eof

	dc.b	cmd_lfa,35,		cmd_eof

	dc.b	cmd_lfa,36,		cmd_eof

	dc.b	cmd_lfa,36,		cmd_eof
	dc.b	cmd_lfa,36,		cmd_eof
	dc.b	cmd_lfa,36,		cmd_eof

	dc.b	cmd_lfa,37,		cmd_eof
	dc.b	cmd_lfa,38,		cmd_eof
	dc.b	cmd_lfa,40,		cmd_eof
	dc.b	cmd_lfa,41,		cmd_eof
	dc.b	cmd_lfa,42,		cmd_eof
	dc.b	cmd_lfa,43,		cmd_eof
	dc.b	cmd_lfa,44,		cmd_eof
	dc.b	cmd_lfa,45,		cmd_eof
	dc.b	cmd_lfa,46,		cmd_eof
	dc.b	cmd_lfa,47,		cmd_eof
	dc.b	cmd_lfa,48,		cmd_eof
	dc.b	cmd_lfa,49,		cmd_eof
	dc.b	cmd_lfa,50,		cmd_eof
	dc.b	cmd_lfa,51,		cmd_eof
;	dc.b	cmd_lfa,52,		cmd_eof
;	dc.b	cmd_lfa,53,		cmd_eof
;	dc.b	cmd_lfa,54,		cmd_eof
;	dc.b	cmd_lfa,55,		cmd_eof
	dc.b	cmd_repeat,70,	cmd_lfa,26,		cmd_eof
playcmdst3end

	even
cmdblocks
	dc.l	cmdblock0-base
	dc.l	cmdblock1-base
	dc.l	cmdblock2-base
	dc.l	cmdblock3-base
	dc.l	cmdblock4-base
	dc.l	cmdblock5-base
	dc.l	cmdblock6-base
	dc.l	cmdblock7-base
	dc.l	cmdblock8-base
	dc.l	cmdblock9-base
	dc.l	cmdblock10-base
	dc.l	cmdblock11-base
	dc.l	cmdblock12-base
	dc.l	cmdblock13-base
	dc.l	cmdblock14-base
	dc.l	cmdblock15-base
	dc.l	cmdblock16-base
	dc.l	cmdblock17-base
	dc.l	cmdblock18-base
	dc.l	cmdblock19-base
	dc.l	cmdblock20-base
	dc.l	cmdblock21-base
	dc.l	cmdblock22-base
	dc.l	cmdblock23-base
	dc.l	cmdblock24-base
	dc.l	cmdblock25-base
	dc.l	cmdblock26-base

*------	CLS --------------------------------------------------------------*

cls	move.l	dbplanes(pc),a0			;
	moveq	#0,d0					;
	bsr		waitblitter				;
	move.l	a0,$54(a6) 				; destination D
	move.w	#$0100,$40(a6)			; bltcon0
	move.w	d0,$42(a6)				; bltcon1
	move.l	d0,$64(a6)				; modulos a,d
	move.w	#(pheight<<6)+(pwidth/2),$58(a6); bltsize and start
	rts								;

*------	GLITCH -----------------------------------------------------------*

; a0 = glitch cmds
; a1 = glitch cmds end

setglitch
	lea		glitchdata(pc),a5		;
	move.l	g_cmdspointer(a5),a2	;
	cmp.l	g_cmdsend(a5),a2		; end of glitch data?
	beq		initglitch				;
	rts								; nop because another glitch is running

initglitch
	lea		glitchdata(pc),a5		;
	move.l	a0,g_cmdspointer(a5)	;
	move.l	a1,g_cmdsend(a5)		;
	clr.b	g_wait(a5)				;
	rts								;

glitch
	lea		glitchdata(pc),a5		;
	move.l	g_cmdspointer(a5),a0	;
	cmp.l	g_cmdsend(a5),a0		; end of glitch data?
	bne		glitchdo				;
	rts								; end of glitch / nothing to do

glitchdo
	tst.b	g_wait(a5)				; wait?
	beq		glitchloop				;
	subq.b	#1,g_wait(a5)			; wait/skip this frame
	rts								;

glitchloop
	move.b	(a0)+,d0				; cmd
	beq		glitchdone				; end of frame - eof (0)?
	
	subq.b	#1,d0					; set position? (1)
	bne		glitchcmd2				;
	moveq	#0,d1					; glitch number
	move.b	(a0)+,d1				;
	asl.w	#2,d1					; create long word offset
	lea		gpos(pc),a1				;
	move.l	(a1,d1.w),a1			;
	add.l	clistbase(pc),a1		;
		
	moveq	#0,d2					; horizontal pos value
	move.b	(a0)+,d2				;
	lea		ghpos(pc),a2			;
	move.b	(a2,d2.w),d2			; pos value
	
	move.b	d2,3(a1)				; 1st sprite
	addq.b	#8,d2					;
	move.b	d2,3+8(a1)				; 2nd sprite
	addq.b	#4,d2					;
	move.b	d2,3+8+8(a1)			; 3rd sprite
	bra		glitchcmddone			;

glitchcmd2
	subq.b	#1,d0					; wait? (2)
	bne		glitchcmd3				;
	move.b	(a0)+,g_wait(a5)		;
	bra		glitchcmddone			;
	
glitchcmd3
	subq.b	#1,d0					; color? (3)
	bne		glitchcmddone			;

	moveq	#0,d1					; glitch number
	move.b	(a0)+,d1				;
	asl.w	#2,d1					; create long word offset
	lea		gdata(pc),a1			;
	move.l	(a1,d1.w),a1			;
	add.l	clistbase(pc),a1		;

	moveq	#0,d3					;
	move.b	(a0)+,d3				; color
	
	moveq	#0,d2					;
	btst	#0,d3					;
	beq		gcmd3b0					;
	moveq	#-1,d2					; sprite data $ffff
gcmd3b0
	move.w	d2,6(a1)				; dc.w	$0144,keep,$0146,"d2" ; 0
	move.w	d2,6+8(a1)				; dc.w	$014c,keep,$014e,"d2" ; 1
	move.w	d2,6+8+8(a1)			; dc.w	$0154,keep,$0156,"d2" ; 2

	moveq	#0,d2					; sprite data $0000
	btst	#1,d3					;
	beq		gcmd3b1					;
	moveq	#-1,d2					;
gcmd3b1
	move.w	d2,2(a1)				; dc.w	$0144,"d2",$0146,keep ; 0
	move.w	d2,2+8(a1)				; dc.w	$014c,"d2",$014e,keep ; 1
	move.w	d2,2+8+8(a1)			; dc.w	$0154,"d2",$0156,keep ; 2

glitchcmddone
	bra		glitchloop				;

glitchdone
	move.l	a0,g_cmdspointer(a5)	;
	rts								;

gcmd_eof		equ	0
gcmd_pos		equ	1
gcmd_wait		equ	2
gcmd_color		equ	3

glitchdata		dc.l	0	; glitch cmds pointer
				dc.l	0	; cmds end
				dc.b	0	; wait delay
	rsreset
g_cmdspointer	rs.l	1
g_cmdsend		rs.l	1
g_wait			rs.b	1

	even

; all glitch files are even by design

glitch0
	incbin	"glitch0"
glitch0end

glitch1
	incbin	"glitch1"
glitch1end

glitch2
	incbin	"glitch2"
glitch2end

glitch3
	incbin	"glitch3"
glitch3end

glitch4
	incbin	"glitch4"
glitch4end

glitch5
	dc.b gcmd_pos,8,7,	gcmd_color,8,1,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,12,4,	gcmd_color,12,3,	gcmd_eof
	dc.b gcmd_wait,4,	gcmd_eof
	dc.b gcmd_color,8,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,12,0,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,8,1,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_pos,2,5,	gcmd_color,2,2,	gcmd_eof
	dc.b gcmd_color,12,3,	gcmd_eof
	dc.b gcmd_pos,3,0,	gcmd_color,3,3,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,8,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_color,3,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_pos,10,2,	gcmd_color,10,3,	gcmd_color,12,0,	gcmd_eof
	dc.b gcmd_color,8,1,	gcmd_eof
	dc.b gcmd_color,3,3,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,2,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,6,2,	gcmd_color,6,3,	gcmd_pos,9,4,	gcmd_color,9,1,	gcmd_eof
	dc.b gcmd_color,3,0,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,9,0,	gcmd_eof
	dc.b gcmd_color,6,0,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_pos,16,0,	gcmd_color,16,3,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,2,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,9,1,	gcmd_color,10,3,	gcmd_color,16,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,6,3,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,16,3,	gcmd_eof
	dc.b gcmd_color,8,0,	gcmd_color,10,0,	gcmd_eof
	dc.b gcmd_color,9,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,16,0,	gcmd_eof
	dc.b gcmd_color,6,0,	gcmd_color,10,3,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,9,1,	gcmd_eof
	dc.b gcmd_wait,8,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_eof
	dc.b gcmd_wait,6,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_eof
	dc.b gcmd_wait,8,	gcmd_eof
	dc.b gcmd_color,9,0,	gcmd_eof
glitch5end
	even

glitch6
	dc.b gcmd_pos,22,2,	gcmd_color,22,1,	gcmd_eof
	dc.b gcmd_wait,4,	gcmd_eof
	dc.b gcmd_pos,0,6,	gcmd_color,0,2,	gcmd_eof
	dc.b gcmd_pos,12,2,	gcmd_color,12,3,	gcmd_eof
	dc.b gcmd_pos,6,0,	gcmd_color,6,2,	gcmd_eof
	dc.b gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_pos,8,2,	gcmd_color,8,3,	gcmd_eof
	dc.b gcmd_pos,2,0,	gcmd_color,2,1,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,0,0,	gcmd_pos,13,6,	gcmd_color,13,1,	gcmd_eof
	dc.b gcmd_color,12,0,	gcmd_pos,18,1,	gcmd_color,18,3,	gcmd_eof
	dc.b gcmd_color,6,0,	gcmd_eof
	dc.b gcmd_color,22,1,	gcmd_eof
	dc.b gcmd_color,8,0,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_pos,7,3,	gcmd_color,7,1,	gcmd_eof
	dc.b gcmd_pos,23,4,	gcmd_color,23,1,	gcmd_eof
	dc.b gcmd_pos,10,6,	gcmd_color,10,2,	gcmd_eof
	dc.b gcmd_color,0,2,	gcmd_color,13,0,	gcmd_color,18,0,	gcmd_eof
	dc.b gcmd_color,12,3,	gcmd_eof
	dc.b gcmd_pos,3,7,	gcmd_color,3,3,	gcmd_color,6,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,7,0,	gcmd_color,8,3,	gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_color,2,1,	gcmd_color,23,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,3,0,	gcmd_color,10,0,	gcmd_color,18,3,	gcmd_eof
	dc.b gcmd_color,13,1,	gcmd_eof
	dc.b gcmd_color,0,0,	gcmd_eof
	dc.b gcmd_color,12,0,	gcmd_eof
	dc.b gcmd_color,6,0,	gcmd_color,7,1,	gcmd_eof
	dc.b gcmd_color,3,3,	gcmd_color,23,1,	gcmd_eof
	dc.b gcmd_color,8,0,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_color,18,0,	gcmd_eof
	dc.b gcmd_color,10,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,3,0,	gcmd_color,13,0,	gcmd_eof
	dc.b gcmd_color,7,0,	gcmd_eof
	dc.b gcmd_color,23,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,18,3,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_eof
	dc.b gcmd_color,7,1,	gcmd_eof
	dc.b gcmd_color,23,1,	gcmd_eof
	dc.b gcmd_wait,19,	gcmd_eof
	dc.b gcmd_color,18,0,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,7,0,	gcmd_eof
	dc.b gcmd_color,23,0,	gcmd_eof
glitch6end
	even

glitch7
	dc.b gcmd_pos,16,3,	gcmd_color,16,1,	gcmd_eof
	dc.b gcmd_wait,4,	gcmd_eof
	dc.b gcmd_color,16,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_pos,4,7,	gcmd_color,4,3,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,16,1,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,16,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,4,3,	gcmd_eof
	dc.b gcmd_wait,4,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_eof
glitch7end
	even

glitch8
	dc.b gcmd_pos,4,7,	gcmd_color,4,2,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_pos,10,3,	gcmd_color,10,3,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,6,7,	gcmd_color,6,3,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_eof
	dc.b gcmd_pos,2,2,	gcmd_color,2,3,	gcmd_eof
	dc.b gcmd_pos,14,4,	gcmd_color,14,1,	gcmd_eof
	dc.b gcmd_pos,11,4,	gcmd_color,11,1,	gcmd_eof
	dc.b gcmd_color,6,0,	gcmd_color,10,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_eof
	dc.b gcmd_color,4,2,	gcmd_color,14,0,	gcmd_eof
	dc.b gcmd_color,11,0,	gcmd_eof
	dc.b gcmd_color,6,3,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,2,3,	gcmd_pos,3,0,	gcmd_color,3,3,	gcmd_color,10,3,	gcmd_eof
	dc.b gcmd_color,14,1,	gcmd_eof
	dc.b gcmd_color,11,1,	gcmd_pos,15,1,	gcmd_color,15,2,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_color,6,0,	gcmd_pos,22,3,	gcmd_color,22,2,	gcmd_eof
	dc.b gcmd_pos,8,0,	gcmd_color,8,3,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_color,3,0,	gcmd_eof
	dc.b gcmd_color,14,0,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_color,11,0,	gcmd_color,15,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,3,3,	gcmd_color,4,2,	gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_color,8,0,	gcmd_eof
	dc.b gcmd_color,15,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,10,3,	gcmd_eof
	dc.b gcmd_color,3,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,15,0,	gcmd_color,22,2,	gcmd_eof
	dc.b gcmd_color,8,3,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,0,1,	gcmd_color,0,1,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_color,0,0,	gcmd_color,8,0,	gcmd_eof
	dc.b gcmd_wait,4,	gcmd_eof
	dc.b gcmd_color,0,1,	gcmd_eof
	dc.b gcmd_color,22,2,	gcmd_eof
	dc.b gcmd_color,8,3,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,0,0,	gcmd_color,4,0,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_eof
	dc.b gcmd_wait,16,	gcmd_eof
	dc.b gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_color,8,0,	gcmd_eof
glitch8end
	even

glitch9
	dc.b gcmd_pos,22,6,	gcmd_color,22,3,	gcmd_eof
	dc.b gcmd_wait,4,	gcmd_eof
	dc.b gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,14,7,	gcmd_color,14,3,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,22,3,	gcmd_eof
	dc.b gcmd_pos,4,6,	gcmd_color,4,3,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,14,0,	gcmd_eof
	dc.b gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_pos,5,0,	gcmd_color,5,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,16,7,	gcmd_color,16,1,	gcmd_eof
	dc.b gcmd_color,5,0,	gcmd_color,14,3,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,4,3,	gcmd_color,16,0,	gcmd_eof
	dc.b gcmd_color,5,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,10,2,	gcmd_color,10,2,	gcmd_color,14,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,12,7,	gcmd_color,12,3,	gcmd_color,16,1,	gcmd_eof
	dc.b gcmd_color,5,0,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,12,0,	gcmd_color,14,3,	gcmd_color,16,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,10,2,	gcmd_eof
	dc.b gcmd_color,4,3,	gcmd_eof
	dc.b gcmd_pos,11,5,	gcmd_color,11,1,	gcmd_color,12,3,	gcmd_eof
	dc.b gcmd_pos,18,6,	gcmd_color,18,1,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,11,0,	gcmd_color,12,0,	gcmd_eof
	dc.b gcmd_color,18,0,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,11,1,	gcmd_eof
	dc.b gcmd_color,18,1,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,11,0,	gcmd_eof
	dc.b gcmd_color,18,0,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,14,0,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_eof
glitch9end
	even

glitch10
	dc.b gcmd_pos,12,2,	gcmd_color,12,2,	gcmd_eof
	dc.b gcmd_pos,2,4,	gcmd_color,2,3,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_pos,22,3,	gcmd_color,22,1,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,4,1,	gcmd_color,4,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,12,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,2,3,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_eof
	dc.b gcmd_color,12,2,	gcmd_eof
	dc.b gcmd_color,22,1,	gcmd_eof
	dc.b gcmd_wait,4,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_color,12,0,	gcmd_eof
	dc.b gcmd_color,4,2,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_color,12,2,	gcmd_eof
	dc.b gcmd_wait,4,	gcmd_eof
	dc.b gcmd_color,4,0,	gcmd_eof
	dc.b gcmd_wait,13,	gcmd_eof
	dc.b gcmd_color,12,0,	gcmd_eof
glitch10end
	even

glitch11
	dc.b gcmd_pos,10,1,	gcmd_color,10,2,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_pos,22,7,	gcmd_color,22,1,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_pos,18,1,	gcmd_color,18,3,	gcmd_eof
	dc.b gcmd_pos,16,5,	gcmd_color,16,1,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,10,2,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_pos,12,1,	gcmd_color,12,1,	gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,10,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,12,0,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,18,0,	gcmd_color,22,1,	gcmd_eof
	dc.b gcmd_pos,2,2,	gcmd_color,2,2,	gcmd_color,16,0,	gcmd_eof
	dc.b gcmd_color,12,1,	gcmd_eof
	dc.b gcmd_wait,2,	gcmd_eof
	dc.b gcmd_color,18,3,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_color,16,1,	gcmd_eof
	dc.b gcmd_color,12,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,22,0,	gcmd_eof
	dc.b gcmd_wait,1,	gcmd_eof
	dc.b gcmd_color,2,2,	gcmd_color,18,0,	gcmd_eof
	dc.b gcmd_color,16,0,	gcmd_eof
	dc.b gcmd_wait,3,	gcmd_eof
	dc.b gcmd_color,2,0,	gcmd_color,18,3,	gcmd_eof
	dc.b gcmd_color,16,1,	gcmd_eof
	dc.b gcmd_wait,17,	gcmd_eof
	dc.b gcmd_color,18,0,	gcmd_eof
	dc.b gcmd_color,16,0,	gcmd_eof
glitch11end
	even

glitches
	dc.l	glitch0-base, glitch0end-base
	dc.l	glitch1-base, glitch1end-base
	dc.l	glitch2-base, glitch2end-base
	dc.l	glitch3-base, glitch3end-base
	dc.l	glitch4-base, glitch4end-base
	dc.l	glitch5-base, glitch5end-base
	dc.l	glitch6-base, glitch6end-base
	dc.l	glitch7-base, glitch7end-base
	dc.l	glitch8-base, glitch8end-base
	dc.l	glitch9-base, glitch9end-base
	dc.l	glitch10-base, glitch10end-base
	dc.l	glitch11-base, glitch11end-base

*------	COPPER INSTRUCTION LIST ------------------------------------------*

clist
	dc.w	$008e,$2c81 ; DIWSTRT
	dc.w	$0090,$2cc1 ; DIWSTOP
	dc.w	$0092,$0038 ;
	dc.w	$0094,$00d0 ;
	
	dc.w	$0100,$1200	; 1 bit plane
	dc.w	$0102,$0010	; font shadow: shift uneven bit planes 1 pixel

; http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0159.html
	dc.w	$0104,%1100000 ; sprites have prio

	dc.w	$0108,$0000
	dc.w	$010a,$0000

; sprites off
	dc.w	$0144,$0000,$0146,$0000 ; data 0
	dc.w	$014c,$0000,$014e,$0000 ; data 1
	dc.w	$0154,$0000,$0156,$0000 ; data 2
	dc.w	$0164,$0000,$0166,$0000 ; data 4
	dc.w	$016c,$0000,$016e,$0000 ; data 5
	dc.w	$0174,$0000,$0176,$0000	; data 6

; unused sprites (3 and 7)
	dc.w	$0158,$2040,$015a,$2100 ; ctrl 3
	dc.w	$0178,$2040,$017a,$2100 ; ctrl 7
	dc.w	$015c,$0000,$015e,$0000 ; data 3
	dc.w	$017c,$0000,$017e,$0000 ; data 7

	if testing
	dc.w	$1007,$fffe
	dc.w	$0180
precalccolor
	dc.w	0
	dc.w	$2c07,$fffe
	endif

	dc.w	$0180,$0000
	dc.w	$0182
dotcolor
	dc.w	0

	if framenumber
	dc.w	$0182,$0fbb
	endif

spritecolors
	dc.w	$01a0,$0000,$01a2,0,$01a4,$0222,$01a6,$0444 ; sprite 0 and 1 color
	dc.w	$01a8,$0000,$01aa,0,$01ac,$0222,$01ae,$0444 ; sprite 2 and 3 color
	dc.w	$01b0,$0000,$01b2,0,$01b4,$0222,$01b6,$0444 ; sprite 4 and 5 color
	dc.w	$01b8,$0000,$01ba,0,$01bc,$0222,$01be,$0444 ; sprite 6 and 7 color

ppbart
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0

srv	equ	$2c+5 ; 5 = offset in order to align with scroller
srh	equ	21

;	dc.w	$2cdf,$fffe,$0180,$00f5,$0180,$0000   testing

;	dc.w	(0*srh+srv)<<8+$07,$fffe ; -> $07 is too late to execute all the instructions in time
	dc.w	(0*srh+srv-1)<<8+$df,$fffe
g0pos
	dc.w	$0140,(0*srh+srv)<<8, $0142,(1*srh+srv)<<8
	dc.w	$0148,(0*srh+srv)<<8, $014a,(1*srh+srv)<<8
	dc.w	$0150,(0*srh+srv)<<8, $0152,(1*srh+srv)<<8
g1pos
	dc.w	$0160,(0*srh+srv)<<8, $0162,(1*srh+srv)<<8
	dc.w	$0168,(0*srh+srv)<<8, $016a,(1*srh+srv)<<8
	dc.w	$0170,(0*srh+srv)<<8, $0172,(1*srh+srv)<<8
g0data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g1data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

;	dc.w	(1*srh+srv)<<8+$07,$fffe
	dc.w	(1*srh+srv-1)<<8+$df,$fffe
g2pos
	dc.w	$0140,(1*srh+srv)<<8, $0142,(2*srh+srv)<<8
	dc.w	$0148,(1*srh+srv)<<8, $014a,(2*srh+srv)<<8
	dc.w	$0150,(1*srh+srv)<<8, $0152,(2*srh+srv)<<8
g3pos
	dc.w	$0160,(1*srh+srv)<<8, $0162,(2*srh+srv)<<8
	dc.w	$0168,(1*srh+srv)<<8, $016a,(2*srh+srv)<<8
	dc.w	$0170,(1*srh+srv)<<8, $0172,(2*srh+srv)<<8
g2data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g3data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

;	dc.w	(2*srh+srv)<<8+$07,$fffe
	dc.w	(2*srh+srv-1)<<8+$df,$fffe
g4pos
	dc.w	$0140,(2*srh+srv)<<8, $0142,(3*srh+srv)<<8
	dc.w	$0148,(2*srh+srv)<<8, $014a,(3*srh+srv)<<8
	dc.w	$0150,(2*srh+srv)<<8, $0152,(3*srh+srv)<<8
g5pos
	dc.w	$0160,(2*srh+srv)<<8, $0162,(3*srh+srv)<<8
	dc.w	$0168,(2*srh+srv)<<8, $016a,(3*srh+srv)<<8
	dc.w	$0170,(2*srh+srv)<<8, $0172,(3*srh+srv)<<8
g4data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g5data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

;	dc.w	(3*srh+srv)<<8+$07,$fffe
	dc.w	(3*srh+srv-1)<<8+$df,$fffe
g6pos
	dc.w	$0140,(3*srh+srv)<<8, $0142,(4*srh+srv)<<8
	dc.w	$0148,(3*srh+srv)<<8, $014a,(4*srh+srv)<<8
	dc.w	$0150,(3*srh+srv)<<8, $0152,(4*srh+srv)<<8
g7pos
	dc.w	$0160,(3*srh+srv)<<8, $0162,(4*srh+srv)<<8
	dc.w	$0168,(3*srh+srv)<<8, $016a,(4*srh+srv)<<8
	dc.w	$0170,(3*srh+srv)<<8, $0172,(4*srh+srv)<<8

g6data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
;dc.w	$0180,$0fc0 testing

g7data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6
;dc.w	$0180,$00fc testing

;	dc.w	(4*srh+srv)<<8+$07,$fffe
	dc.w	(4*srh+srv-1)<<8+$df,$fffe
g8pos
	dc.w	$0140,(4*srh+srv)<<8, $0142,(5*srh+srv)<<8
	dc.w	$0148,(4*srh+srv)<<8, $014a,(5*srh+srv)<<8
	dc.w	$0150,(4*srh+srv)<<8, $0152,(5*srh+srv)<<8
g9pos
	dc.w	$0160,(4*srh+srv)<<8, $0162,(5*srh+srv)<<8
	dc.w	$0168,(4*srh+srv)<<8, $016a,(5*srh+srv)<<8
	dc.w	$0170,(4*srh+srv)<<8, $0172,(5*srh+srv)<<8
g8data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g9data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

;	dc.w	(5*srh+srv)<<8+$07,$fffe
	dc.w	(5*srh+srv-1)<<8+$df,$fffe
g10pos
	dc.w	$0140,(5*srh+srv)<<8, $0142,(6*srh+srv)<<8
	dc.w	$0148,(5*srh+srv)<<8, $014a,(6*srh+srv)<<8
	dc.w	$0150,(5*srh+srv)<<8, $0152,(6*srh+srv)<<8
g11pos
	dc.w	$0160,(5*srh+srv)<<8, $0162,(6*srh+srv)<<8
	dc.w	$0168,(5*srh+srv)<<8, $016a,(6*srh+srv)<<8
	dc.w	$0170,(5*srh+srv)<<8, $0172,(6*srh+srv)<<8
g10data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g11data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

;	dc.w	(6*srh+srv)<<8+$07,$fffe
	dc.w	(6*srh+srv-1)<<8+$df,$fffe
g12pos
	dc.w	$0140,(6*srh+srv)<<8, $0142,(7*srh+srv)<<8
	dc.w	$0148,(6*srh+srv)<<8, $014a,(7*srh+srv)<<8
	dc.w	$0150,(6*srh+srv)<<8, $0152,(7*srh+srv)<<8
g13pos
	dc.w	$0160,(6*srh+srv)<<8, $0162,(7*srh+srv)<<8
	dc.w	$0168,(6*srh+srv)<<8, $016a,(7*srh+srv)<<8
	dc.w	$0170,(6*srh+srv)<<8, $0172,(7*srh+srv)<<8
g12data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g13data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

;	dc.w	(7*srh+srv)<<8+$07,$fffe
	dc.w	(7*srh+srv-1)<<8+$df,$fffe
g14pos
	dc.w	$0140,(7*srh+srv)<<8, $0142,(8*srh+srv)<<8
	dc.w	$0148,(7*srh+srv)<<8, $014a,(8*srh+srv)<<8
	dc.w	$0150,(7*srh+srv)<<8, $0152,(8*srh+srv)<<8
g15pos
	dc.w	$0160,(7*srh+srv)<<8, $0162,(8*srh+srv)<<8
	dc.w	$0168,(7*srh+srv)<<8, $016a,(8*srh+srv)<<8
	dc.w	$0170,(7*srh+srv)<<8, $0172,(8*srh+srv)<<8
g14data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g15data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

;	dc.w	(8*srh+srv)<<8+$07,$fffe
	dc.w	(8*srh+srv-1)<<8+$df,$fffe
g16pos
	dc.w	$0140,(8*srh+srv)<<8, $0142,(9*srh+srv)<<8
	dc.w	$0148,(8*srh+srv)<<8, $014a,(9*srh+srv)<<8
	dc.w	$0150,(8*srh+srv)<<8, $0152,(9*srh+srv)<<8
g17pos
	dc.w	$0160,(8*srh+srv)<<8, $0162,(9*srh+srv)<<8
	dc.w	$0168,(8*srh+srv)<<8, $016a,(9*srh+srv)<<8
	dc.w	$0170,(8*srh+srv)<<8, $0172,(9*srh+srv)<<8
g16data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g17data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

;	dc.w	(9*srh+srv)<<8+$07,$fffe
	dc.w	(9*srh+srv-1)<<8+$df,$fffe
g18pos
	dc.w	$0140,(9*srh+srv)<<8, $0142,((10*srh+srv)<<8)&$ffff
	dc.w	$0148,(9*srh+srv)<<8, $014a,((10*srh+srv)<<8)&$ffff
	dc.w	$0150,(9*srh+srv)<<8, $0152,((10*srh+srv)<<8)&$ffff
g19pos
	dc.w	$0160,(9*srh+srv)<<8, $0162,((10*srh+srv)<<8)&$ffff
	dc.w	$0168,(9*srh+srv)<<8, $016a,((10*srh+srv)<<8)&$ffff
	dc.w	$0170,(9*srh+srv)<<8, $0172,((10*srh+srv)<<8)&$ffff
g18data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g19data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

	dc.w	$ffe1,$fffe

;	dc.w	(10*srh+srv + 1)<<8+$07,$fffe
	dc.w	((10*srh+srv+1-1)<<8+$df)&$ffff,$fffe

	; sprites off
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6

	dc.w	$0607,$fffe,$0100,$3200	; 3 bit planes
bartcol	
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0707,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0807,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0907,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0a07,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0b07,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0c07,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0d07,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0e07,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$0f07,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$1007,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$1107,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$1207,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$1307,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
	dc.w	$1407,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
bartline
	dc.w	$1507,$fffe
	dc.w	$0188,0,$0184,0,$018c,0,$018a,0,$0186,0,$018e,0
bartlineend
	dc.w	$1607,$fffe
	dc.w	$0100,$1200
	
;	dc.w	(11*srh+srv)<<8+$07,$fffe
	dc.w	((11*srh+srv-1)<<8+$df)&$ffff,$fffe
g22pos
	dc.w	$0140,((11*srh+srv)<<8)&$ffff, $0142,((12*srh+srv)<<8+$06)&$ffff
	dc.w	$0148,((11*srh+srv)<<8)&$ffff, $014a,((12*srh+srv)<<8+$06)&$ffff
	dc.w	$0150,((11*srh+srv)<<8)&$ffff, $0152,((12*srh+srv)<<8+$06)&$ffff
g23pos
	dc.w	$0160,((11*srh+srv)<<8)&$ffff, $0162,((12*srh+srv)<<8+$06)&$ffff
	dc.w	$0168,((11*srh+srv)<<8)&$ffff, $016a,((12*srh+srv)<<8+$06)&$ffff
	dc.w	$0170,((11*srh+srv)<<8)&$ffff, $0172,((12*srh+srv)<<8+$06)&$ffff
g22data
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
g23data
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6
	
	dc.w	$1907,$fffe
;	dc.w	$0180,$0f00
	dc.w	$009c,$8010	; set coper interrupt bit
	dc.w	($19+11)<<8+$07,$fffe
lspdmacon
	dc.w	$0096,$8000

;	dc.w	$0180,$0000
	
;	dc.w	(12*srh+srv + 1)<<8+$07,$fffe
	dc.w	((12*srh+srv+1-1)<<8+$df)&$ffff,$fffe
	; end of sprites
	dc.w	$0144,0,$0146,0 ; 0
	dc.w	$014c,0,$014e,0 ; 1
	dc.w	$0154,0,$0156,0 ; 2
	dc.w	$0164,0,$0166,0 ; 4
	dc.w	$016c,0,$016e,0 ; 5
	dc.w	$0174,0,$0176,0	; 6
	
	dc.w	$ffff,$fffe
clistend

bartlinesize	equ	bartlineend-bartline

gpos
	dc.l	g0pos-clist,  g1pos-clist,  g2pos-clist,  g3pos-clist,  g4pos-clist,  g5pos-clist
	dc.l	g6pos-clist,  g7pos-clist,  g8pos-clist,  g9pos-clist,  g10pos-clist, g11pos-clist
	dc.l	g12pos-clist, g13pos-clist, g14pos-clist, g15pos-clist, g16pos-clist, g17pos-clist
	dc.l	g18pos-clist, g19pos-clist, 0,		   0,				g22pos-clist, g23pos-clist
	
gdata
	dc.l	g0data-clist,  g1data-clist,  g2data-clist,  g3data-clist,  g4data-clist,  g5data-clist
	dc.l	g6data-clist,  g7data-clist,  g8data-clist,  g9data-clist,  g10data-clist, g11data-clist
	dc.l	g12data-clist, g13data-clist, g14data-clist, g15data-clist, g16data-clist, g17data-clist
	dc.l	g18data-clist, g19data-clist, 0,		     0,				g22data-clist, g23data-clist

ghpos
	dc.b	$40+0, $40+20, $40+40, $40+60, $40+80, $40+100, $40+120, $40+140	; 8 columns
	even

*------	PRECALCULATION DATA ----------------------------------------------*

; 512 = 90 deg

precalcdata
; wake up

;	dc.w	-3, 0,0,-300
;	dc.w	-4,-96,60,	-2,120,120,40,	0,0,0,	1,0,0,	2,0,0

;	dc.w	-3,	0,0,-400	; move away (z translation)

;	dc.w	-4,	-100,0
;	dc.w	-2,	0,0,1024
;	dc.w	0,0,0,	1,0,0,	2,0,0
	
	
;	dc.w	-3,	0,0,-400	; move away (z translation)
;	dc.w	-4,	100,0
;	dc.w	-2,	0,0,0
;	dc.w	0,1,0,	1,1,0,	2,1,0
	
	dc.w	-4,	0,0
; 												eyes		upper		lower
	dc.w	-2,	-240,0,0,	-3,	0,0,-240*2,		0,0,0,		1,0,0,		2,0,0
	dc.w	-2,	-212,0,0,   -3, 0,0,-212*2,		0,1,0,		1,1,0,		2,1,0
	dc.w	-2,	-186,0,0,	-3, 0,0,-186*2,		0,2,0,		1,2,0,		2,2,0
	dc.w	-2,	-162,0,0,	-3, 0,0,-162*2,		0,3,0,		1,3,0,		2,3,0
	dc.w	-2,	-140,0,0,	-3, 0,0,-140*2,		0,4,0,		1,4,0,		2,4,0
	dc.w	-2,	-120,0,0,	-3, 0,0,-120*2,		0,5,0,		1,5,0,		2,5,0
	dc.w	-2, -102,0,0,	-3, 0,0,-102*2,		0,6,0,		1,6,0,		2,6,0
	dc.w	-2, -86,0,0,	-3, 0,0,-86*2,		0,7,0,		1,7,0,		2,7,0
	dc.w	-2, -72,0,0,    -3, 0,0,-72*2,		0,8,0,		1,8,0,		2,8,0
	dc.w	-2, -60,0,0,	-3, 0,0,-60*2,		0,9,0,		1,9,0,		2,9,0
	dc.w	-2, -48,0,0,	-3, 0,0,-48*2,		0,10,0,		1,10,0,		2,10,0
	dc.w	-2, -38,0,0,	-3, 0,0,-38*2,		0,11,0,		1,11,0,		2,11,0
	dc.w	-2, -30,0,0,	-3, 0,0,-30*2,		0,12,0,		1,12,0,		2,12,0
	dc.w	-2, -24,0,0,  	-3, 0,0,-24*2,		0,13,0,		1,13,0,		2,13,0
	dc.w	-2, -18,0,0,	-3, 0,0,-18*2,		0,14,0,		1,14,0,		2,14,0
	dc.w	-2, -14,0,0,	-3, 0,0,-14*2,		0,15,0,		1,15,0,		2,15,0
	dc.w	-2, -10,0,0,	-3, 0,0,-10*2,		0,16,0,		1,16,0,		2,16,0
	dc.w	-2, -6,0,0,		-3, 0,0,-6*2,		0,17,0,		1,17,0,		2,17,0
	dc.w	-2, -4,0,0, 	-3, 0,0,-4*2,		0,18,0,		1,18,0,		2,18,0
	dc.w	-2, -2,0,0,		-3, 0,0,-2*2,		0,19,0,		1,19,0,		2,19,0
	; "quadratic gain"
	dc.w	-2, 0,0,0,		-3, 0,0,0,			0,20,0,		1,20,0,		2,20,0
	; "quadratic gain"
	; "quadratic gain"
	; "quadratic gain"
	dc.w	-1,$f6 ; signal (show) and wait

; blinking eyes
	dc.w	0,50,0
	dc.w	0,51,1
	dc.w	0,52,2
	dc.w	0,53,3
	dc.w	0,54,4
	dc.w	0,55,5
	dc.w	0,56,6
	dc.w	0,57,7
	dc.w	0,58,8
	dc.w	0,59,9
	dc.w	-1,$144 ; signal (show) and wait

; go (buffer with index 20 is in use)
;	dc.w	2,0,0
	dc.w	2,1,1
	dc.w	2,2,2
	dc.w	2,3,3
	dc.w	2,4,4
	dc.w	2,5,5
	dc.w	2,6,6
	dc.w	2,7,7
	dc.w	2,8,8
	dc.w	2,9,9
	dc.w	2,10,10
	dc.w	2,11,11
	dc.w	2,12,12
	dc.w	2,13,13
	dc.w	2,14,14
	dc.w	2,15,15
	dc.w	2,16,16
	dc.w	2,17,17
	dc.w	2,18,18
	dc.w	2,19,19
 ; buffer 20 is in use
	dc.w	2,21,20
	dc.w	2,22,21
	dc.w	2,23,22
	dc.w	2,24,23
	dc.w	2,25,24
	dc.w	2,26,25

	dc.w	-1,$2ec ; signal (show) and wait
	
	dc.w	-3, 0,0,0
	
; shake							eyes		upper		lower
	dc.w	-2,	0,-160,0,		0,0,0,		1,0,0,		2,0,0
	; quadratic gain -160
	dc.w	-2,	0,-158,0,		0,1,0,		1,1,0,		2,1,0
	; quadratic gain -158
	dc.w	-2,	0,-156,0,		0,2,0,		1,2,0,		2,2,0
	dc.w	-2,	0,-154,0,		0,3,0,		1,3,0,		2,3,0
	dc.w	-2, 0,-150,0,		0,4,0,		1,4,0,		2,4,0
	dc.w	-2, 0,-146,0,		0,5,0,		1,5,0,		2,5,0
	dc.w	-2, 0,-142,0,    	0,6,0,		1,6,0,		2,6,0
	dc.w	-2, 0,-138,0,		0,7,0,		1,7,0,		2,7,0
	dc.w	-2, 0,-132,0,		0,8,0,		1,8,0,		2,8,0
	dc.w	-2, 0,-126,0,		0,9,0,		1,9,0,		2,9,0
	dc.w	-2, 0,-120,0,		0,10,0,		1,10,0,		2,10,0
	dc.w	-2, 0,-114,0,  		0,11,0,		1,11,0,		2,11,0
	dc.w	-2, 0,-106,0,		0,12,0,		1,12,0,		2,12,0
	dc.w	-2, 0,-98,0,		0,13,0,		1,13,0,		2,13,0
	dc.w	-2, 0,-88,0,		0,14,0,		1,14,0,		2,14,0
	dc.w	-2, 0,-80,0,		0,15,0,		1,15,0,		2,15,0
	dc.w	-2, 0,-70,0,		0,16,0,		1,16,0,		2,16,0
	dc.w	-2, 0,-60,0,		0,17,0,		1,17,0,		2,17,0
	dc.w	-2, 0,-48,0, 		0,18,0,		1,18,0,		2,18,0,		0,60,9
	dc.w	-2, 0,-38,0,		0,19,0,		1,19,0,		2,19,0,		0,61,8
	; 20 is still used
	dc.w	-2, 0,-26,0,		0,21,0,		1,21,0,		2,21,0,		0,62,7
	dc.w	-2, 0,-14,0,		0,22,0,		1,22,0,		2,22,0,		0,63,6
	dc.w	-2, 0,0,0,  		0,23,0,		1,23,0,		2,23,0,		0,64,5
	dc.w	-2, 0,14,0,			0,24,0,		1,24,0,		2,24,0,		0,65,4
	dc.w	-2, 0,26,0,			0,25,0,		1,25,0,		2,25,0,		0,66,3
	dc.w	-2, 0,38,0,			0,26,0,		1,26,0,		2,26,0,		0,67,2
	dc.w	-2, 0,48,0,			0,27,0,		1,27,0,		2,27,0,		0,68,1
	dc.w	-2, 0,60,0, 		0,28,0,		1,28,0,		2,28,0,		0,69,0
	dc.w	-2, 0,70,0,			0,29,0,		1,29,0,		2,29,0
	dc.w	-2, 0,80,0,			0,30,0,		1,30,0,		2,30,0
	dc.w	-2, 0,88,0,			0,31,0,		1,31,0,		2,31,0
	dc.w	-2, 0,98,0,			0,32,0,		1,32,0,		2,32,0
	dc.w	-2, 0,106,0,  		0,33,0,		1,33,0,		2,33,0
	dc.w	-2, 0,114,0,		0,34,0,		1,34,0,		2,34,0
	dc.w	-2, 0,120,0,		0,35,0,		1,35,0,		2,35,0
	dc.w	-2, 0,126,0,		0,36,0,		1,36,0,		2,36,0
	dc.w	-2, 0,132,0,		0,37,0,		1,37,0,		2,37,0
	dc.w	-2, 0,138,0, 		0,38,0,		1,38,0,		2,38,0
	dc.w	-2, 0,142,0,		0,39,0,		1,39,0,		2,39,0
	dc.w	-2, 0,146,0,		0,40,0,		1,40,0,		2,40,0
	dc.w	-2, 0,150,0,		0,41,0,		1,41,0,		2,41,0
	dc.w	-2, 0,154,0,		0,42,0,		1,42,0,		2,42,0
	dc.w	-2, 0,156,0,  		0,43,0,		1,43,0,		2,43,0
	dc.w	-2, 0,158,0,		0,44,0,		1,44,0,		2,44,0
	; quadratic gain 158
	dc.w	-2, 0,160,0,		0,45,0,		1,45,0,		2,45,0
	; quadratic gain 160
	dc.w	-1,$905 ; signal (show shake) and wait

; conversation
	dc.w	-3,	0,0,500	; move away (z translation)
	
	; go (left side)
;	dc.w	-4,	-70,0
;	dc.w	-2,	0,160,0
	dc.w	-4,	-70,-20
	dc.w	-2,	-80,160,0

	dc.w	0,0,0		; eyes blink
	dc.w	0,1,1
	dc.w	0,2,2
	dc.w	0,3,3
	dc.w	0,4,4
	dc.w	0,5,5
	dc.w	0,6,6
	dc.w	0,7,7
	dc.w	0,8,8
	dc.w	0,9,9
	
	dc.w	1,0,0		; go upper
	dc.w	2,0,0		; go lower (0 = neutral)

	; go (former tschak) (right side)
;	dc.w	-4,	70,0
;	dc.w	-2,	0,-160,0
	dc.w	-3,	0,0,600	; move away (z translation)
	dc.w	-4,	60,20
	dc.w	-2,	80,-180,0

	dc.w	0,10,0		; eyes blink
	dc.w	0,11,1
	dc.w	0,12,2
	dc.w	0,13,3
	dc.w	0,14,4
	dc.w	0,15,5
	dc.w	0,16,6
	dc.w	0,17,7
	dc.w	0,18,8
	dc.w	0,19,9

	dc.w	1,1,0		; tschak upper
	dc.w	2,26,0		; tschak lower (0 = neutral)

	dc.w	-1,$c7a	; signal

;	dc.w	2,27,26		; tschak lower
;	dc.w	2,28,27
;	dc.w	2,29,28
;	dc.w	2,30,29
;	dc.w	2,31,30
;	dc.w	2,32,31
;	dc.w	2,33,32
;	dc.w	2,34,33
;	dc.w	2,35,34
;	dc.w	2,36,35
;	dc.w	2,37,36
;	dc.w	2,38,37
;	dc.w	2,39,38
;	dc.w	2,40,39
;	dc.w	2,41,40
;	dc.w	2,42,41
;	dc.w	2,43,42
;	dc.w	2,44,43
;	dc.w	2,45,44
;	dc.w	2,46,45
;	dc.w	2,47,46
;	dc.w	2,48,47
;	dc.w	2,49,48
;	dc.w	2,50,49
;	dc.w	2,51,50
;	dc.w	2,52,51
;	dc.w	2,53,52
;	dc.w	2,54,53
;	dc.w	2,55,54

	dc.w	2,27,1		; go (no longer tschak) lower
	dc.w	2,28,2
	dc.w	2,29,3
	dc.w	2,30,4
	dc.w	2,31,5
	dc.w	2,32,6
	dc.w	2,33,7
	dc.w	2,34,8
	dc.w	2,35,9
	dc.w	2,36,10
	dc.w	2,37,11
	dc.w	2,38,12
	dc.w	2,39,13
	dc.w	2,40,14
	dc.w	2,41,15
	dc.w	2,42,16
	dc.w	2,43,17
	dc.w	2,44,18
	dc.w	2,45,19
	dc.w	2,46,20
	dc.w	2,47,21
	dc.w	2,48,22
	dc.w	2,49,23
	dc.w	2,50,24
	dc.w	2,51,25
;	dc.w	2,52,51
;	dc.w	2,53,52
;	dc.w	2,54,53
;	dc.w	2,55,54

	; go (left side)
;	dc.w	-4,	-70,0
;	dc.w	-2,	0,160,0
	dc.w	-3,	0,0,500	; move away (z translation)
	dc.w	-4,	-70,-20
	dc.w	-2,	-80,160,0

	dc.w	2,1,1		; go lower
	dc.w	2,2,2
	dc.w	2,3,3
	dc.w	2,4,4
	dc.w	2,5,5
	dc.w	2,6,6
	dc.w	2,7,7
	dc.w	2,8,8
	dc.w	2,9,9
	dc.w	2,10,10
	dc.w	2,11,11
	dc.w	2,12,12
	dc.w	2,13,13
	dc.w	2,14,14
	dc.w	2,15,15
	dc.w	2,16,16
	dc.w	2,17,17
	dc.w	2,18,18
	dc.w	2,19,19
	dc.w	2,20,20
	dc.w	2,21,21
	dc.w	2,22,22
	dc.w	2,23,23
	dc.w	2,24,24
	dc.w	2,25,25

	dc.w	-1,$da8	; signal (show conversation) and wait

; splash (go - former tschak)
	dc.w	-4,	0,0
	dc.w	-2, 0,0,0

; 								eyes		upper		lower
	dc.w	-3,	0,0,1200,		0,0,0,		1,0,0,		2,0,0	;26
	dc.w	-3, 0,0,1082,		0,1,0,		1,1,0,		2,1,0	;27
	dc.w	-3, 0,0,769,		0,2,0,		1,2,0,		2,2,0	;28
	dc.w	-3, 0,0,492,		0,3,0,		1,3,0,		2,3,0	;29
	dc.w	-3, 0,0,250,		0,4,0,		1,4,0,		2,4,1	;30
	dc.w	-3, 0,0,46,			0,5,0,		1,5,0,		2,5,2	;31
	dc.w	-3, 0,0,-124,		0,6,0,		1,6,0,		2,6,3	;32
	dc.w	-3, 0,0,-259,		0,7,0,		1,7,0,		2,7,4	;33
	dc.w	-3, 0,0,-363,		0,8,0,		1,8,0,		2,8,5	;34
	dc.w	-3, 0,0,-439,		0,9,0,		1,9,0,		2,9,6	;35
	
	dc.w	-3, 0,0,-488,		0,10,0,		1,10,0,		2,10,7	;36
	dc.w	-3, 0,0,-516,		0,11,0,		1,11,0,		2,11,8	;37
	dc.w	-3, 0,0,-524,		0,12,0,		1,12,0,		2,12,9	;38
	dc.w	-3, 0,0,-517,		0,13,0,		1,13,0,		2,13,10	;39
	dc.w	-3, 0,0,-497,		0,14,0,		1,14,0,		2,14,11	;40
	dc.w	-3, 0,0,-467,		0,15,0,		1,15,0,		2,15,12	;41
	dc.w	-3, 0,0,-429,		0,16,0,		1,16,0,		2,16,13	;42
	dc.w	-3, 0,0,-387,		0,17,0,		1,17,0,		2,17,14	;43
	dc.w	-3, 0,0,-341,		0,18,0,		1,18,0,		2,18,15	;44
	dc.w	-3, 0,0,-295,		0,19,0,		1,19,0,		2,19,16	;45
	dc.w	-3, 0,0,-248,		0,20,0,		1,20,0,		2,20,17	;46

	dc.w	-3, 0,0,-204,		0,21,0,		1,21,0,		2,21,18	;47
	dc.w	-3, 0,0,-161,		0,22,0,		1,22,0,		2,22,19	;48
	dc.w	-3, 0,0,-122,		0,23,0,		1,23,0,		2,23,20	;49
	dc.w	-3, 0,0,-87,		0,24,0,		1,24,0,		2,24,21	;50
	dc.w	-3, 0,0,-55,		0,25,0,		1,25,0,		2,25,22	;51
	dc.w	-3, 0,0,-28,		0,26,0,		1,26,0,		2,26,23	;52
	dc.w	-3, 0,0,-5,			0,27,0,		1,27,0,		2,27,24	;53
	dc.w	-3, 0,0,0,			0,28,0,		1,28,0,		2,28,25	;54

	dc.w	0,31,1 ; blink
	dc.w	0,32,2
	dc.w	0,33,3
	dc.w	0,34,4
	dc.w	0,35,5
	dc.w	0,36,6
	dc.w	0,37,7
	dc.w	0,38,8
	dc.w	0,39,9

	dc.w	-1,$1455	; signal (show splash) and wait

; sine of the times
	dc.w	-4,0,0,	-2,0,0,0,	0,24,0,	1,24,0,	2,24,0
	dc.w	0,50,1
	dc.w	0,51,2
	dc.w	0,52,3
	dc.w	0,53,4
	dc.w	0,54,5
	dc.w	0,55,6
	dc.w	0,56,7
	dc.w	0,57,8
	dc.w	0,58,9

	dc.w	-1,$1840	; signal

	dc.w	-4,-80,60,	-2,120,140,0,	0,0,0,	1,0,0,	2,0,0
	dc.w	-4,-79,59,	-2,118,138,0,	0,1,0,	1,1,0,	2,1,0,	0,61,1
	dc.w	-4,-79,59,	-2,118,138,0,	0,2,0,	1,2,0,	2,2,0,	0,62,2
	dc.w	-4,-78,58,	-2,116,136,0,	0,3,0,	1,3,0,	2,3,0,	0,63,3
	dc.w	-4,-77,57,	-2,114,134,0,	0,4,0,	1,4,0,	2,4,0,	0,64,4
	dc.w	-4,-75,56,	-2,112,132,0,	0,5,0,	1,5,0,	2,5,0,	0,65,5
	dc.w	-4,-73,55,	-2,110,128,0,	0,6,0,	1,6,0,	2,6,0,	0,66,6
	dc.w	-4,-71,53,	-2,106,124,0,	0,7,0,	1,7,0,	2,7,0,	0,67,7
	dc.w	-4,-69,51,	-2,102,120,0,	0,8,0,	1,8,0,	2,8,0,	0,68,8
	dc.w	-4,-66,49,	-2,98,116,0,	0,9,0,	1,9,0,	2,9,0,	0,69,9
	dc.w	-4,-63,47,	-2,94,110,0,	0,10,0,	1,10,0,	2,10,0
	dc.w	-4,-60,45,	-2,90,104,0,	0,11,0,	1,11,0,	2,11,0
	dc.w	-4,-56,42,	-2,84,98,0,	0,12,0,	1,12,0,	2,12,0
	dc.w	-4,-52,39,	-2,78,92,0,	0,13,0,	1,13,0,	2,13,0
	dc.w	-4,-48,36,	-2,72,84,0,	0,14,0,	1,14,0,	2,14,0
	dc.w	-4,-44,33,	-2,66,76,0,	0,15,0,	1,15,0,	2,15,0
	dc.w	-4,-40,30,	-2,60,70,0,	0,16,0,	1,16,0,	2,16,0
	dc.w	-4,-35,26,	-2,52,60,0,	0,17,0,	1,17,0,	2,17,0
	dc.w	-4,-30,22,	-2,44,52,0,	0,18,0,	1,18,0,	2,18,0
	dc.w	-4,-25,19,	-2,38,44,0,	0,19,0,	1,19,0,	2,19,0
	dc.w	-4,-20,15,	-2,30,36,0,	0,20,0,	1,20,0,	2,20,0
	dc.w	-4,-15,11,	-2,22,26,0,	0,21,0,	1,21,0,	2,21,0
	dc.w	-4,-10,7,	-2,14,18,0,	0,22,0,	1,22,0,	2,22,0
	dc.w	-4,-5,3,	-2,6,8,0,	0,23,0,	1,23,0,	2,23,0
;	dc.w	-4,0,0,	-2,0,0,0,	0,24,0,	1,24,0,	2,24,0
	dc.w	-4,5,-3,	-2,-6,-8,0,	0,25,0,	1,25,0,	2,25,0
	dc.w	-4,10,-7,	-2,-14,-18,0,	0,26,0,	1,26,0,	2,26,0
	dc.w	-4,15,-11,	-2,-22,-26,0,	0,27,0,	1,27,0,	2,27,0
	dc.w	-4,20,-15,	-2,-30,-36,0,	0,28,0,	1,28,0,	2,28,0
	dc.w	-4,25,-19,	-2,-38,-44,0,	0,29,0,	1,29,0,	2,29,0
	dc.w	-4,30,-22,	-2,-44,-52,0,	0,30,0,	1,30,0,	2,30,0
	dc.w	-4,35,-26,	-2,-52,-60,0,	0,31,0,	1,31,0,	2,31,0
	dc.w	-4,40,-30,	-2,-60,-70,0,	0,32,0,	1,32,0,	2,32,0
	dc.w	-4,44,-33,	-2,-66,-76,0,	0,33,0,	1,33,0,	2,33,0
	dc.w	-4,48,-36,	-2,-72,-84,0,	0,34,0,	1,34,0,	2,34,0
	dc.w	-4,52,-39,	-2,-78,-92,0,	0,35,0,	1,35,0,	2,35,0
	dc.w	-4,56,-42,	-2,-84,-98,0,	0,36,0,	1,36,0,	2,36,0
	dc.w	-4,60,-45,	-2,-90,-104,0,	0,37,0,	1,37,0,	2,37,0
	dc.w	-4,63,-47,	-2,-94,-110,0,	0,38,0,	1,38,0,	2,38,0
	dc.w	-4,66,-49,	-2,-98,-116,0,	0,39,0,	1,39,0,	2,39,0
	dc.w	-4,69,-51,	-2,-102,-120,0,	0,40,0,	1,40,0,	2,40,0
	dc.w	-4,71,-53,	-2,-106,-124,0,	0,41,0,	1,41,0,	2,41,0
	dc.w	-4,73,-55,	-2,-110,-128,0,	0,42,0,	1,42,0,	2,42,0
	dc.w	-4,75,-56,	-2,-112,-132,0,	0,43,0,	1,43,0,	2,43,0
	dc.w	-4,77,-57,	-2,-114,-134,0,	0,44,0,	1,44,0,	2,44,0
	dc.w	-4,78,-58,	-2,-116,-136,0,	0,45,0,	1,45,0,	2,45,0
	dc.w	-4,79,-59,	-2,-118,-138,0,	0,46,0,	1,46,0,	2,46,0
	dc.w	-4,79,-59,	-2,-118,-138,0,	0,47,0,	1,47,0,	2,47,0
	dc.w	-4,80,-60,	-2,-120,-140,0,	0,48,0,	1,48,0,	2,48,0

	dc.w	-1,$1aa3 ; signal (show sine of the times) and wait

; falling
	rem
	dc.w	-4,0,0,		-2,0,0,0,	-3,0,0,0
	dc.w	0,0,0,		1,0,0,		2,0,0

	dc.w	0,51,1
	dc.w	0,52,2
	dc.w	0,53,3
	dc.w	0,54,4
	dc.w	0,55,5
	dc.w	0,56,6
	dc.w	0,57,7
	dc.w	0,58,8
	dc.w	0,59,9

	dc.w	-1,$2095 ; signal (show) and wait
	
	dc.w	-2, 0,0,0,		-3,2,0,0,			0,5,0,		1,5,0,		2,5,0
	dc.w	-2, 0,0,-2,		-3,4,0,0,			0,6,0,		1,6,0,		2,6,0
	dc.w	-2, 0,0,-2,		-3,6,0,0,			0,7,0,		1,7,0,		2,7,0
	dc.w	-2, 0,0,-4,		-3,8,0,0,			0,8,0,		1,8,0,		2,8,0
	dc.w	-2, 0,0,-6,		-3,12,0,0,			0,9,0,		1,9,0,		2,9,0
	dc.w	-2, 0,0,-10,	-3,18,0,0,			0,10,0,		1,10,0,		2,10,0
	dc.w	-2, 0,0,-12,	-3,22,0,0,			0,11,0,		1,11,0,		2,11,0
	dc.w	-2, 0,0,-16,	-3,30,0,0,			0,12,0,		1,12,0,		2,12,0
	dc.w	-2, 0,0,-20,	-3,38,0,0,			0,13,0,		1,13,0,		2,13,0
	dc.w	-2, 0,0,-26,	-3,48,0,0,			0,14,1,		1,14,0,		2,14,0
	dc.w	-2, 0,0,-32,	-3,58,0,0,			0,15,2,		1,15,0,		2,15,0
	dc.w	-2, 0,0,-40,	-3,72,0,0,			0,16,3,		1,16,0,		2,16,1
	dc.w	-2, 0,0,-48,	-3,86,0,0,			0,17,4,		1,17,0,		2,17,2
	dc.w	-2, 0,0,-56,	-3,102,0,0,			0,18,5,		1,18,0,		2,18,3
	dc.w	-2, 0,0,-66,	-3,120,0,0,			0,19,6,		1,19,0,		2,19,4
	dc.w	-2, 0,0,-78,	-3,140,0,0,			0,20,7,		1,20,0,		2,20,5
	dc.w	-2, 0,0,-90,	-3,162,0,0,			0,21,8,		1,21,0,		2,21,6
	dc.w	-2, 0,0,-104,	-3,186,0,0,			0,22,9,		1,22,0,		2,22,7
	dc.w	-2, 0,0,-118,	-3,214,0,0,			0,23,0,		1,23,0,		2,23,8
	dc.w	-2, 0,0,-134,	-3,242,0,0,			0,24,0,		1,24,0,		2,24,9
	dc.w	-2, 0,0,-152,	-3,274,0,0,			0,25,0,		1,25,0,		2,25,10
	dc.w	-2, 0,0,-172,	-3,308,0,0,			0,26,0,		1,26,0,		2,26,11
	dc.w	-2, 0,0,-192,	-3,346,0,0,			0,27,0,		1,27,0,		2,27,12
	dc.w	-2, 0,0,-214,	-3,386,0,0,			0,28,0,		1,28,0,		2,28,13
	dc.w	-2, 0,0,-238,	-3,429,0,0,			0,29,0,		1,29,0,		2,29,14
	dc.w	-2, 0,0,-264,	-3,474,0,0,			0,30,0,		1,30,0,		2,30,15
	dc.w	-2, 0,0,-290,	-3,524,0,0,			0,31,0,		1,31,0,		2,31,16
	dc.w	-2, 0,0,-320,	-3,576,0,0,			0,32,0,		1,32,0,		2,32,17

	; blinking eyes falling
	dc.w	0,41,1
	dc.w	0,42,2
	dc.w	0,43,3
	dc.w	0,44,4
	dc.w	0,45,5
	dc.w	0,46,6
	dc.w	0,47,7
	dc.w	0,48,8
	dc.w	0,49,9

	dc.w	-1,$21f6 ; signal (show) and wait
	erem
precalcdataend

	rsreset
d_ctrl		rs.w	1
d_frame		rs.w	0	; = same offset value (2) as d_buffer and d_angles
d_angles	rs.w	0	; = same offset value (2) as d_buffer
d_buffer	rs.w	1	; = same offset value (2) as d_angles

*------	DRAWDOT MACRO ----------------------------------------------------*

	macro DRAWDOT
	rept \1
	add.w	(a2)+,a1
	move.b	(a3)+,d0
	or.b	d0,(a1)
	endr
	endm

*------	DRAW DOTS --------------------------------------------------------*

; a1 = buffer to draw to
; a2 = dots data
; a3 = dots "byte to OR"
; d7.w = numdots / 20 - 1
drawdots
	DRAWDOT 2
draw18dotsmore	; d7 must be 0
	DRAWDOT 4
draw14dotsmore	; d7 must be 0
	DRAWDOT 6
draw8dotsmore	; d7 must be 0
	DRAWDOT 8
	dbf		d7,drawdots
	rts

*------	ROTATE AND PROJECT ----------------------------------------*

; a2 = angles
mtx	lea		sin(pc),a0			;
	lea		cos(pc),a1			;

	move.w	#$07ff,d5			;
	move.w	(a2)+,d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d0		; sin a
	move.w	(a1,d6.w),d3		; cos a

	move.w	(a2)+,d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d1		; sin b
	move.w	(a1,d6.w),d4		; cos b

	move.w	(a2)+,d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d2		; sin c
	move.w	(a1,d6.w),d5		; cos c

	lea		matrix(pc),a0		;
	move.w	d0,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d6,a1				;
	move.w	d3,d7				;
	muls	d2,d7				;
	asr.l	#8,d7				;
	move.w	d7,a2				;
	muls	d5,d6				;
	asr.l	#8,d6				;
	sub.w	d7,d6				;
	move.w	d6,6(a0)			;
	move.w	d3,d7				;
	muls	d5,d7				;
	asr.l	#8,d7				;
	move.w	d7,a3				;
	move.w	a1,d6				;
	muls	d2,d6				;
	asr.l	#8,d6				;
	add.w	d7,d6				;
	move.w	d6,8(a0)			;
	move.w	a3,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d0,d7				;
	muls	d2,d7				;
	asr.l	#8,d7				;
	add.w	d7,d6				;
	move.w	d6,12(a0)			;
	move.w	a2,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d0,d7				;
	muls	d5,d7				;
	asr.l	#8,d7				;
	sub.w	d7,d6				;
	move.w	d6,14(a0)			;
	muls	d4,d5				;
	asr.l	#8,d5				;
	move.w	d5,(a0)				;
	muls	d4,d2				;
	asr.l	#8,d2				;
	move.w	d2,2(a0)			;
	muls	d4,d0				;
	asr.l	#8,d0				;
	move.w	d0,10(a0)			;
	muls	d4,d3				;
	asr.l	#8,d3				;
	move.w	d3,16(a0)			;
	neg.w	d1					;
	move.w	d1,4(a0)			;
	rts

matrix	ds.w	9,0				; 3*3 rotation matrix

; a2 = destination
; a3 = pointer to translation values (z,x,y)
; a5 = 3d data
; d7 (upper word) = offset
; d7 (lower word) = num points
applymtx
	move.w	postty(pc),d3		;
	swap	d3					;
	move.w	posttx(pc),d3		;

	lea		matrix(pc),a0		; 3*3 rotation matrix
applymtxloop
	move.l	a0,a1				;

	move.b	(a5)+,d0			; z
	ext.w	d0					;
	asl.w	#3,d0				;

	move.b	(a5)+,d1			; y
	ext.w	d1					;
	asl.w	#3,d1				;

	move.b	(a5)+,d2			; x
	ext.w	d2					;
	asl.w	#3,d2				;

	add.w	(a3),d1				; translate x
	add.w	2(a3),d2			; translate y
	add.w	4(a3),d0			; translate z
	
	move.w	d1,d4				;
	muls	(a1)+,d4			;
	move.w	d2,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;
	move.w	d0,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;
	move.w	d1,d5				;
	muls	(a1)+,d5			;
	move.w	d2,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	move.w	d0,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	muls	(a1)+,d1			;
	muls	(a1)+,d2			;
	muls	(a1)+,d0			;
	add.l	d1,d0				;
	add.l	d2,d0				;
	asr.l	#8,d0				;
;	move.w	d0,d6				; (no need for z value)
	add.w	#$0980,d0			; distance
	divs	d0,d4				;
	divs	d0,d5				;

	add.w	d3,d4				; post translation x (tx)
	swap	d3					;
	add.w	d3,d5				; post translation y (ty)
	swap	d3					;

	cmp.w	#-159,d4			; clip x
	blt		clip				;
	cmp.w	#159,d4				;
	bgt		clip				;
	
	cmp.w	#-127,d5			; clip y
	blt		clip				;
	cmp.w	#127,d5				;
	bgt		clip				;

	move.w	d5,d2				; = muls #pwidth,d5
	asl.w	#5,d5				;
	asl.w	#3,d2				;
	add.w	d2,d5				;

	move.w	d4,d2
	asr.w	#3,d4				; / 8
	add.w	d4,d5				; byte position/offset in plane
	swap	d7					; previous
	move.w	d7,d0				;
	move.w	d5,d7				; new previous
	sub.w	d0,d5				; we store the difference
	move.w	d5,(a2)+			; (difference) position
	swap	d7					;

	and.w	#%0111,d2			;
	move.b	pattern(pc,d2.w),(a4)+

	dbf		d7,applymtxloop		;
	rts							;

clip
	moveq	#0,d4				; x (center)
	moveq	#0,d5				; y (center)

	swap	d7					; previous
	move.w	d7,d0				;
	move.w	d5,d7				; new previous
	sub.w	d0,d5				; we store the difference
	move.w	d5,(a2)+			; (difference) position
	swap	d7					;

	clr.b	(a4)+				; zero to OR (no effect) - test with sf (a4)+

	dbf		d7,applymtxloop		;
	rts							;

pattern
	dc.b	%10000000,%01000000,%00100000,%00010000
	dc.b	%00001000,%00000100,%00000010,%00000001

*------	BART SIMPSON TEXTSCROLLER ---------------------------------*

fheight	equ	15

bart
	lea		bartdata(pc),a5			;
	lea		bartc1(pc),a0			;
	add.w	b_cycling1(a5),a0		;
	move.l	clistbase(pc),a1		;
	add.w	#bartcol+(fheight*bartlinesize)-clist+2,a1;
	moveq	#fheight-1,d7			;
bartcy
	move.w	(a0),16(a1)				;
	move.w	(a0)+,4(a1)				;
	sub.w	#bartlinesize,a1		;
	dbf		d7,bartcy				;	
	addq.w	#2,b_cycling1(a5)		;
	cmp.w	#bartc1end-bartc1,b_cycling1(a5);
	bne		bartcy2					;
	clr.w	b_cycling1(a5)			;
bartcy2
	lea		bartc2(pc),a0			;
	add.w	b_cycling2(a5),a0		;
	move.l	clistbase(pc),a1		;
	add.w	#bartcol-clist+2,a1		;
	moveq	#fheight-1,d7			;
bartcy3
	move.w	(a0),(a1)				;
	move.w	(a0),20(a1)				;
	move.w	(a0),12(a1)				;
	move.w	(a0)+,8(a1)				;
	add.w	#bartlinesize,a1		;
	dbf		d7,bartcy3				;
	addq.w	#2,b_cycling2(a5)		;
	cmp.w	#bartc2end-bartc2,b_cycling2(a5);
	bne		bartst					;
	clr.w	b_cycling2(a5)			;

bartst
	tst.b	b_delay(a5)				; stop/delay scroller?
	beq		bart2					;
	subq.b	#1,b_delay(a5)			;
	rts								;
bart2
	moveq	#4-1,d7					; scroll 4 columns in 1 frame
	tst.w	b_charbit(a5)			;
	bge		bart9					;
bart5
	lea		base(pc),a0				;
	add.l	#btext-base,a0			; lea btext(pc),a0
	add.w	b_textpointer(a5),a0	;
	moveq	#0,d0					;
	move.b	(a0)+,d0				;
	addq.w	#1,b_textpointer(a5)	;

	cmp.b	#-1,d0					; delay?
	bne		bart44					;
	move.b	(a0),b_delay(a5)		;
	addq.w	#1,b_textpointer(a5)	; consume delay value
	rts								;

; _ = unused/void/empty
bartp
	dc.b	18,6,11,0,21,9,6,0 				; SPACE!"_A´'_
	dc.b	9,0,0,0,9,15,6,21 				; (___)-./
	dc.b	16,9,16,16,16,16,16,16,16,16 	; 0123456789
	dc.b	6,21,20,14,9,15,4				; :ARROW^TRADEMARKo?SMALLSPACE
	dc.b	21,17,16,16,16,16,18,17,6,17,17,17,19  ; ABC...M
	dc.b	18,17,17,17,17,16,18,17,17,19,17,16,16 ; N...Z
	even

bart44
	cmp.b	#-2,d0					; glitch?
	bne		bart45					;
	addq.w	#1,b_textpointer(a5)	; consume glitch number
	moveq	#0,d0					;
	move.b	(a0),d0					; glitch number
	asl.w	#3,d0					; (*8) offset to glitch
	lea		glitches(pc),a2			;
	lea		base(pc),a0				;
	move.l	a0,a1					;
	add.l	(a2,d0.w),a0			;
	add.l	4(a2,d0.w),a1			;
	move.l	a5,-(a7)				;
	bsr		setglitch				;
	move.l	(a7)+,a5				;
	bra		bart5					;

bart45
	tst.b	(a0)					; next frame end of text?
	bne		bart4					;
	clr.w	b_textpointer(a5)		; restart text
bart4
	sub.b	#" ",d0					;
	sf		b_charbit(a5)			;
	move.b	bartp(pc,d0.w),b_charbit+1(a5)	; get width of char = bit to test
	muls	#fheight*4,d0			; offset to char data
	lea		bfont(pc),a0			;
	add.w	d0,a0					;
	move.l	a0,b_chardata(a5)		;

bart9
	move.l	b_chardata(a5),a2		;
	move.w	b_charbit(a5),d5		; bit to test in font
	move.w	d5,d0					;
	asr.w	#3,d0					;
	addq.w	#3,a2					;
	sub.w	d0,a2					;
	moveq	#0,d6					;
	bset	d7,d6					;
	move.l	scrollplanes(pc),a0		;
	add.w	#2*pwidth-1,a0			;
	
	moveq	#fheight-1,d3			; print 1 column of letter
bartpxloop
	btst	d5,(a2)					;
	beq		bartpx					;
	or.b	d6,(a0)					;
bartpx
	addq.w	#4,a2					;
	add.w	#pwidth,a0				;
	dbf		d3,bartpxloop			;
	
	subq.w	#1,b_charbit(a5)		;
	blt		bart5					;
	dbf		d7,bart9				;

	move.l	scrollplanes(pc),a0		;
	add.w	#(fheight+1)*pwidth-2,a0;
	moveq	#0,d0					;
	bsr		waitblitter				;
	move.l	a0,$50(a6)				;
	move.l	a0,$54(a6)				;
	move.l	d0,$64(a6)				; modulos A/D
	move.l	#$ffff0fff,$44(a6)		;
	move.l	#$49f00002,$40(a6)		;
	move.w	#(fheight*64)+(pwidth/2),$58(a6);
	rts								;

bartdata
	dc.w	0						; textpointer
	dc.w	0						; char width, bit to test in char data
	dc.l	0						; address of char data
	dc.w	0						; char color cycling 1
	dc.w	0						; char color cycling 2
	dc.b	0						; delay, frames to wait
	even

	rsreset
b_textpointer	rs.w	1
b_charbit		rs.w	1
b_chardata		rs.l	1
b_cycling1		rs.w	1
b_cycling2		rs.w	1
b_delay			rs.b	1

bartc1
	dc.w	$0444,$0555,$0666,$0777,$0888,$0999,$0aaa,$0999
	dc.w	$0888,$0777,$0666,$0555
bartc1end
	dc.w	$0444,$0555,$0666,$0777,$0888,$0999,$0aaa,$0999
	dc.w	$0888,$0777,$0666,$0555,$0444,$0555

bartc2
	dc.w	$0000,$0100,$0200,$0300,$0400,$0500,$0600,$0700
	dc.w	$0800,$0900,$0a00,$0b00,$0c00,$0d00,$0e00,$0f00
	dc.w	$0f10,$0f20,$0f30,$0f40,$0f50,$0f60,$0f70,$0f80
	dc.w	$0f90,$0fa0,$0fb0,$0fc0,$0fd0,$0fe0,$0ff0,$0ff1
	dc.w	$0ff2,$0ff3,$0ff4,$0ff5,$0ff6,$0ff7,$0ff8,$0ff9
	dc.w	$0ffa,$0ffb,$0ffc,$0ffd,$0ffe,$0fff,$0eff,$0dff
	dc.w	$0cff,$0bff,$0aff,$09ff,$08ff,$07ff,$06ff,$05ff
	dc.w	$04ff,$03ff,$02ff,$01ff,$00ff,$00ef,$00df,$00cf
	dc.w	$00bf,$00af,$009f,$008f,$007f,$006f,$005f,$004f
	dc.w	$003f,$002f,$001f,$000f,$000e,$000d,$000c,$000b
	dc.w	$000a,$0009,$0008,$0007,$0006,$0005,$0004,$0003
	dc.w	$0002,$0001
bartc2end
	dc.w	$0000,$0100,$0200,$0300,$0400,$0500,$0600,$0700
	dc.w	$0800,$0900,$0a00,$0b00,$0c00,$0d00,$0e00,$0f00

*------	MEMORY MANAGEMENT ------------------------------------------------*

BESTMEMORY			equ	0
MEMF_CHIP			equ 1<<1
MEMF_CLEAR   		equ 1<<16

clistsize			equ	clistend-clist
lspbanksize			equ lspbankend-lspbank

mesheyesbufsize		equ 3*numdotseyes	; word offset + byte to OR = 3
meshufabufsize		equ	3*numdotsufa 	; word offset + byte to OR = 3
meshlfabufsize		equ 3*numdotslfa 	; word offset + byte to OR = 3

memtable
clistbase			dc.l	0,MEMF_CHIP,clistsize
lspbankbase			dc.l	0,MEMF_CHIP,lspbanksize

memtable2
plane1base			dc.l	0,MEMF_CHIP+MEMF_CLEAR,psize
plane2base			dc.l	0,MEMF_CHIP+MEMF_CLEAR,psize
scrollplanes		dc.l	0,MEMF_CHIP+MEMF_CLEAR,pwidth*(fheight+2)

; note: let's assume that smaller chunks of memory are better available

; eyes mesh buffers
mesheyesbuf1base	dc.l	0,BESTMEMORY,mesheyesbufsize*35
mesheyesbuf2base	dc.l	0,BESTMEMORY,mesheyesbufsize*35

; upper face area buffers
meshufabuf1base		dc.l	0,BESTMEMORY,meshufabufsize*20
meshufabuf2base		dc.l	0,BESTMEMORY,meshufabufsize*10
meshufabuf3base		dc.l	0,BESTMEMORY,meshufabufsize*19

; lower face area buffers (two talking heads need 1+25 + 1+25 = 52)
meshlfabuf1base		dc.l	0,BESTMEMORY,meshlfabufsize*20
meshlfabuf2base		dc.l	0,BESTMEMORY,meshlfabufsize*20
meshlfabuf3base		dc.l	0,BESTMEMORY,meshlfabufsize*12

memtableend

entrysize 	equ	12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

mesheyesbuffers		ds.l	35+35,0
meshufabuffers		ds.l	20+10+19,0
meshlfabuffers		ds.l	20+20+12,0

alloc
	lea		clist(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr		TypeOfMem(a6)			;
	btst	#1,d0					; chipmem?
	beq		notchipmem				;

	lea		clist(pc),a0			; mark data that is in chipmen already
	lea		clistbase(pc),a1		;
	move.l	a0,(a1)					;
		
	lea		base(pc),a0				;
	add.l	#lspbank-base,a0		;
	lea		lspbankbase(pc),a1		;
	move.l	a0,(a1)					;	

notchipmem
	lea		memtable(pc),a5			;
	moveq	#entries-1,d7			;
alloc2	
	tst.l	(a5)					; not to be allocated?
	bne		alloc3					;
	move.l	8(a5),d0				; bytesize
	move.l	4(a5),d1				; requirements
	move.l	AbsExecBase.w,a6		;
	jsr		AllocMem(a6)			; allocmem
	tst.l	d0						; out of memory?
	beq		printerrorandfreemem	;
	move.l	d0,(a5)					;
alloc3	
	add.w	#entrysize,a5			; next entry
	dbf		d7,alloc2				;
	bsr		initmemory				;
	moveq	#0,d0					; ok, all entries allocated
	rts								;

printerrorandfreemem
	bsr		printoutofmemory		;
dealloc
	move.l	AbsExecBase.w,a6		;
	jsr		TypeOfMem(a6)			;
	lea		memtable(pc),a5			;
	moveq	#entries-1,d7			;
	btst	#1,d0					; chipmem?
	beq		free2					; we are not in chipmem so free all entries
	lea		memtable2(pc),a5		;
	moveq	#entries-entrieschip-1,d7;
free2
	tst.l	(a5)					; end of memtable?
	beq		free3					;
	move.l	(a5),a1					; address of memory block
	move.l	8(a5),d0				; bytesize
	move.l	AbsExecBase.w,a6		;
	jsr		FreeMem(a6)				;
	add.l	#entrysize,a5			;
	dbf		d7,free2				;
free3
	moveq	#-1,d0					; alloc error
	rts								;

initmemory
; copy copper list to chip memory
	lea		clist(pc),a0			;
	move.l	clistbase(pc),a1		;
	move.w	#clistsize-1,d0			;
copyclist
	move.b	(a0)+,(a1)+				;
	dbf		d0,copyclist			;

; init scrolltext bitplane pointer
	move.l	clistbase(pc),a0		;
	add.w	#ppbart-clist+2,a0		;
	move.l	scrollplanes(pc),d0		;
	move.w	d0,4(a0)				;
	swap	d0						;
	move.w	d0,(a0)					;
	swap	d0						;
	add.l	#pwidth,d0				;
	move.w	d0,12(a0)				;
	swap	d0						;
	move.w	d0,8(a0)				;

; init mesh eyes buffers
	lea		mesheyesbuffers(pc),a1	;
	move.w	#mesheyesbufsize,d0		;
	moveq	#35-1,d7				;
	move.l	mesheyesbuf1base(pc),a0	;
meshb1
	move.l	a0,(a1)+				;
	add.w	d0,a0					;
	dbf		d7,meshb1				;

	moveq	#35-1,d7				;
	move.l	mesheyesbuf2base(pc),a0	;
meshb2
	move.l	a0,(a1)+				;
	add.w	d0,a0					;
	dbf		d7,meshb2				;

; init mesh upper face area buffers
	lea		meshufabuffers(pc),a1	;
	move.w	#meshufabufsize,d0		;
	moveq	#20-1,d7				;
	move.l	meshufabuf1base(pc),a0	;
meshu1
	move.l	a0,(a1)+				;
	add.w	d0,a0					;
	dbf		d7,meshu1				;

	moveq	#10-1,d7				;
	move.l	meshufabuf2base(pc),a0	;
meshu2
	move.l	a0,(a1)+				;
	add.w	d0,a0					;
	dbf		d7,meshu2				;

	moveq	#19-1,d7				;
	move.l	meshufabuf3base(pc),a0	;
meshu3
	move.l	a0,(a1)+				;
	add.w	d0,a0					;
	dbf		d7,meshu3				;

; init mesh lower face area buffers
	lea		meshlfabuffers(pc),a1	;
	move.w	#meshlfabufsize,d0		;
	moveq	#20-1,d7				;
	move.l	meshlfabuf1base(pc),a0	;
meshl1
	move.l	a0,(a1)+				;
	add.w	d0,a0					;
	dbf		d7,meshl1				;

	moveq	#20-1,d7				;
	move.l	meshlfabuf2base(pc),a0	;
meshl2
	move.l	a0,(a1)+				;
	add.w	d0,a0					;
	dbf		d7,meshl2				;

	moveq	#12-1,d7				;
	move.l	meshlfabuf3base(pc),a0	;
meshl3
	move.l	a0,(a1)+				;
	add.w	d0,a0					;
	dbf		d7,meshl3				;
	
; addresses to 3d frames for faster access
; eyes (total 10 frames)
	lea		blink(pc),a0			;
	lea		eyesframes(pc),a1		;
	moveq	#10-1,d7				; 10 frames
initfrm1
	move.l	a0,(a1)+				;
	add.w	#numdotseyes*3,a0		;
	dbf		d7,initfrm1				;

; upper face area (total 1 frame)
	lea		uface(pc),a0			;
	lea		ufaceframes(pc),a1		;
	move.l	a0,(a1)					; 1 frame neutral upper face area

; lower face area (total 55 frames)
	lea		lface(pc),a0			;
	lea		lfaceframes(pc),a1		;
	move.l	a0,(a1)+				; 1 frame neutral lower face area
	lea		go(pc),a0				;
	moveq	#25-1,d7				; 25 "go" frames
initfrm2
	move.l	a0,(a1)+				;
	add.w	#numdotslfa*3,a0		;
	dbf		d7,initfrm2				;
;	lea		base(pc),a0				;
;	add.l	#tschak-base,a0			; lea tschak(pc),a0 is out of reach	
;	moveq	#29-1,d7				; 29 "tschak" frames
;initfrm3
;	move.l	a0,(a1)+				;
;	add.w	#numdotslfa*3,a0		;
;	dbf		d7,initfrm3				;

	lea		base(pc),a0				; copy lspbank
	add.l	#lspbank-base,a0		;
	move.l	lspbankbase(pc),a1		;
	move.l	#lspbanksize,d0			;
copylspbank
	move.b	(a0)+,(a1)+				;
	subq.l	#1,d0					;
	bne		copylspbank				;
	rts								;

printoutofmemory
	moveq	#0,d0					;
	lea		dos(pc),a1				;
	move.l	AbsExecBase.w,a6		;
	jsr		OpenLibrary(a6)			;
	move.l	d0,a6					;
	beq		error					;
	jsr		Output(a6)				;
	move.l	d0,d1					;
	beq		error					;
	move.l	#textend-text,d3		; length
	lea		text(pc),a1				;
	move.l	a1,d2					;
	jsr		Write(a6)				;
	tst.l	d0						;
	beq		error					;
	move.l	a6,a1					;
	move.l	AbsExecBase.w,a6		;
	jsr		CloseLibrary(a6)		;
error
	moveq	#0,d0					;
	rts								;

dos	dc.b	"dos.library",0

text
	dc.b	"Error: Could not allocate enough memory",10
textend
	even

;*****************************************************************
;
;	Light Speed Player v1.05 (modified)
;	Fastest Amiga MOD player ever :)
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	twitter: @leonard_coder
;
;	--------How to use--------- 
;
;	bsr LSP_MusicDriver+0 : Init LSP player code
;		In:	a0: LSP music data(any memory)
;			a1: LSP sound bank(chip memory)
;			a2: DMACON 8bits byte address (should be odd address!)
;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count
;
;	bsr LSP_MusicDriver+4 : LSP player tick (call once per frame)
;		In:	a6: must be $dff000
;			Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;		Out:None
;
;*****************************************************************

lspplay		lea		LSPVars(pc),a1
			move.l	(a1),a0					; byte stream
process		moveq	#0,d0
cloop		move.b	(a0)+,d0
			bne		swCode
			addi.w	#$0100,d0
			bra		cloop
swCode		add.w	d0,d0
			move.l	m_codeTableAddr(a1),a2	; code table
			move.w	(a2,d0.w),d0			; code
			beq		noInst
			cmp.w	m_escCodeRewind(a1),d0
			beq		r_rewind
			cmp.w	m_escCodeSetBpm(a1),d0
			beq		r_chgbpm

			add.b	d0,d0
			bcc		noVd
			move.b	(a0)+,$d9(a6)
noVd		add.b	d0,d0
			bcc		noVc
			move.b	(a0)+,$c9(a6)
noVc		add.b	d0,d0
			bcc		noVb
			move.b	(a0)+,$b9(a6)
noVb		add.b	d0,d0
			bcc		noVa
			move.b	(a0)+,$a9(a6)
noVa		
			move.l	a0,(a1)+	; store byte stream ptr
			move.l	(a1),a0		; word stream

			tst.b	d0
			beq		noPa

			add.b	d0,d0
			bcc		noPd
			move.w	(a0)+,$d6(a6)
noPd		add.b	d0,d0
			bcc		noPc
			move.w	(a0)+,$c6(a6)
noPc		add.b	d0,d0
			bcc		noPb
			move.w	(a0)+,$b6(a6)
noPb		add.b	d0,d0
			bcc		noPa
			move.w	(a0)+,$a6(a6)
noPa		
			tst.w	d0
			beq		noInst

			moveq	#0,d1
			move.l	m_lspInstruments-4(a1),a2	; instrument table
			lea		resetv+12(pc),a4

			lea		$d0(a6),a5
			moveq	#4-1,d2
vloop		add.w	d0,d0
			bcs		setIns
			add.w	d0,d0
			bcc		skip
			move.l	(a4),a3
			move.l	(a3)+,(a5)
			move.w	(a3)+,4(a5)
			bra		skip
setIns		add.w	(a0)+,a2
			add.w	d0,d0
			bcc		noReset
			bset	d2,d1
			move.w	d1,$96(a6)
noReset		move.l	(a2)+,(a5)
			move.w	(a2)+,4(a5)
			move.l	a2,(a4)
skip		subq.w	#4,a4
			sub.w	#$10,a5
			dbf		d2,vloop

			move.l	m_dmaconPatch-4(a1),a3		; dmacon patch
			move.b	d1,(a3)						; dmacon			

noInst		move.l	a0,(a1)			; store word stream (or byte stream if coming from early out)
			rts

r_rewind	move.l	m_byteStreamLoop(a1),a0
			move.l	m_wordStreamLoop(a1),m_wordStream(a1)
			bra		process

r_chgbpm	move.b	(a0)+,m_currentBpm+1(a1)	; BPM
			bra		process

lspinit		lea		base(pc),a0				; a0: music data (any mem) + 10
			add.l	#lspmusic-base,a0
			move.l	lspbankbase(pc),a1		; a1: sound bank data (chip mem)
			move.l	clistbase(pc),a2		; a2: 16bit DMACON word address
			lea		lspdmacon+3-clist(a2),a2

			lea		LSPVars(pc),a3
			move.w	(a0)+,m_currentBpm(a3)	; default BPM
			move.w	(a0)+,m_escCodeRewind(a3)
			move.w	(a0)+,m_escCodeSetBpm(a3)
			move.l	(a0)+,-(a7)				; who cares? replace with addq.w #4,a0?
			move.l	a2,m_dmaconPatch(a3)
;PATCHED			move.w	#$8000,-1(a2)			; Be sure DMACon word is $8000 (note: a2 should be ODD address)
			move.w	(a0)+,d0				; instrument count
			lea		-12(a0),a2				; LSP data has -12 offset on instrument tab ( to win 2 cycles in fast player :) )
			move.l	a2,m_lspInstruments(a3)	; instrument tab addr ( minus 4 )
			subq.w	#1,d0
			move.l	a1,d1
relocLoop	bset.b	#0,3(a0)				; bit0 is relocation done flag
			bne		relocated
			add.l	d1,(a0)
			add.l	d1,6(a0)
relocated	lea		12(a0),a0
			dbf		d0,relocLoop
			move.w	(a0)+,d0				; codes count (+2)
			move.l	a0,m_codeTableAddr(a3)	; code table
			add.w	d0,d0
			add.w	d0,a0
			move.l	(a0)+,d0				; word stream size
			move.l	(a0)+,d1				; byte stream loop point
			move.l	(a0)+,d2				; word stream loop point
			move.l	a0,m_wordStream(a3)
			lea		(a0,d0.l),a1			; byte stream
			move.l	a1,m_byteStream(a3)
			add.l	d2,a0
			add.l	d1,a1
			move.l	a0,m_wordStreamLoop(a3)
			move.l	a1,m_byteStreamLoop(a3)
			lea		m_currentBpm(a3),a0
			move.l	(a7)+,d0				; music len in frame ticks? who cares? REMOVE?
			rts

	rsreset

m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 m_lfmDmaConPatch
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone			rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
sizeof_LSPVars		rs.w	0

LSPVars		ds.b	sizeof_LSPVars
			
resetv		dc.l	0,0,0,0

*------	SINE TABLE USING 1024 ANGLE STEPS AND FACTOR 256 ----------*

sin	dc.w	$0000,$0002,$0003,$0005,$0006,$0008,$0009,$000b
	dc.w	$000d,$000e,$0010,$0011,$0013,$0014,$0016,$0018
	dc.w	$0019,$001b,$001c,$001e,$001f,$0021,$0022,$0024
	dc.w	$0026,$0027,$0029,$002a,$002c,$002d,$002f,$0030
	dc.w	$0032,$0033,$0035,$0037,$0038,$003a,$003b,$003d
	dc.w	$003e,$0040,$0041,$0043,$0044,$0046,$0047,$0049
	dc.w	$004a,$004c,$004d,$004f,$0050,$0052,$0053,$0055
	dc.w	$0056,$0058,$0059,$005b,$005c,$005e,$005f,$0061
	dc.w	$0062,$0063,$0065,$0066,$0068,$0069,$006b,$006c
	dc.w	$006d,$006f,$0070,$0072,$0073,$0075,$0076,$0077
	dc.w	$0079,$007a,$007b,$007d,$007e,$0080,$0081,$0082
	dc.w	$0084,$0085,$0086,$0088,$0089,$008a,$008c,$008d
	dc.w	$008e,$0090,$0091,$0092,$0093,$0095,$0096,$0097
	dc.w	$0099,$009a,$009b,$009c,$009e,$009f,$00a0,$00a1
	dc.w	$00a2,$00a4,$00a5,$00a6,$00a7,$00a8,$00aa,$00ab
	dc.w	$00ac,$00ad,$00ae,$00af,$00b1,$00b2,$00b3,$00b4
	dc.w	$00b5,$00b6,$00b7,$00b8,$00b9,$00ba,$00bc,$00bd
	dc.w	$00be,$00bf,$00c0,$00c1,$00c2,$00c3,$00c4,$00c5
	dc.w	$00c6,$00c7,$00c8,$00c9,$00ca,$00cb,$00cc,$00cd
	dc.w	$00ce,$00cf,$00cf,$00d0,$00d1,$00d2,$00d3,$00d4
	dc.w	$00d5,$00d6,$00d7,$00d7,$00d8,$00d9,$00da,$00db
	dc.w	$00dc,$00dc,$00dd,$00de,$00df,$00e0,$00e0,$00e1
	dc.w	$00e2,$00e3,$00e3,$00e4,$00e5,$00e5,$00e6,$00e7
	dc.w	$00e7,$00e8,$00e9,$00e9,$00ea,$00eb,$00eb,$00ec
	dc.w	$00ed,$00ed,$00ee,$00ee,$00ef,$00ef,$00f0,$00f1
	dc.w	$00f1,$00f2,$00f2,$00f3,$00f3,$00f4,$00f4,$00f5
	dc.w	$00f5,$00f5,$00f6,$00f6,$00f7,$00f7,$00f8,$00f8
	dc.w	$00f8,$00f9,$00f9,$00f9,$00fa,$00fa,$00fa,$00fb
	dc.w	$00fb,$00fb,$00fc,$00fc,$00fc,$00fd,$00fd,$00fd
	dc.w	$00fd,$00fd,$00fe,$00fe,$00fe,$00fe,$00fe,$00ff
	dc.w	$00ff,$00ff,$00ff,$00ff,$00ff,$00ff,$0100,$0100
	dc.w	$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100
cos	dc.w	$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100
	dc.w	$0100,$0100,$0100,$00ff,$00ff,$00ff,$00ff,$00ff
	dc.w	$00ff,$00ff,$00fe,$00fe,$00fe,$00fe,$00fe,$00fd
	dc.w	$00fd,$00fd,$00fd,$00fd,$00fc,$00fc,$00fc,$00fb
	dc.w	$00fb,$00fb,$00fa,$00fa,$00fa,$00f9,$00f9,$00f9
	dc.w	$00f8,$00f8,$00f8,$00f7,$00f7,$00f6,$00f6,$00f5
	dc.w	$00f5,$00f5,$00f4,$00f4,$00f3,$00f3,$00f2,$00f2
	dc.w	$00f1,$00f1,$00f0,$00ef,$00ef,$00ee,$00ee,$00ed
	dc.w	$00ed,$00ec,$00eb,$00eb,$00ea,$00e9,$00e9,$00e8
	dc.w	$00e7,$00e7,$00e6,$00e5,$00e5,$00e4,$00e3,$00e3
	dc.w	$00e2,$00e1,$00e0,$00e0,$00df,$00de,$00dd,$00dc
	dc.w	$00dc,$00db,$00da,$00d9,$00d8,$00d7,$00d7,$00d6
	dc.w	$00d5,$00d4,$00d3,$00d2,$00d1,$00d0,$00cf,$00cf
	dc.w	$00ce,$00cd,$00cc,$00cb,$00ca,$00c9,$00c8,$00c7
	dc.w	$00c6,$00c5,$00c4,$00c3,$00c2,$00c1,$00c0,$00bf
	dc.w	$00be,$00bd,$00bc,$00ba,$00b9,$00b8,$00b7,$00b6
	dc.w	$00b5,$00b4,$00b3,$00b2,$00b1,$00af,$00ae,$00ad
	dc.w	$00ac,$00ab,$00aa,$00a8,$00a7,$00a6,$00a5,$00a4
	dc.w	$00a2,$00a1,$00a0,$009f,$009e,$009c,$009b,$009a
	dc.w	$0099,$0097,$0096,$0095,$0093,$0092,$0091,$0090
	dc.w	$008e,$008d,$008c,$008a,$0089,$0088,$0086,$0085
	dc.w	$0084,$0082,$0081,$0080,$007e,$007d,$007b,$007a
	dc.w	$0079,$0077,$0076,$0075,$0073,$0072,$0070,$006f
	dc.w	$006d,$006c,$006b,$0069,$0068,$0066,$0065,$0063
	dc.w	$0062,$0061,$005f,$005e,$005c,$005b,$0059,$0058
	dc.w	$0056,$0055,$0053,$0052,$0050,$004f,$004d,$004c
	dc.w	$004a,$0049,$0047,$0046,$0044,$0043,$0041,$0040
	dc.w	$003e,$003d,$003b,$003a,$0038,$0037,$0035,$0033
	dc.w	$0032,$0030,$002f,$002d,$002c,$002a,$0029,$0027
	dc.w	$0026,$0024,$0022,$0021,$001f,$001e,$001c,$001b
	dc.w	$0019,$0018,$0016,$0014,$0013,$0011,$0010,$000e
	dc.w	$000d,$000b,$0009,$0008,$0006,$0005,$0003,$0002
	dc.w	$0000,$fffe,$fffd,$fffb,$fffa,$fff8,$fff7,$fff5
	dc.w	$fff3,$fff2,$fff0,$ffef,$ffed,$ffec,$ffea,$ffe8
	dc.w	$ffe7,$ffe5,$ffe4,$ffe2,$ffe1,$ffdf,$ffde,$ffdc
	dc.w	$ffda,$ffd9,$ffd7,$ffd6,$ffd4,$ffd3,$ffd1,$ffd0
	dc.w	$ffce,$ffcd,$ffcb,$ffc9,$ffc8,$ffc6,$ffc5,$ffc3
	dc.w	$ffc2,$ffc0,$ffbf,$ffbd,$ffbc,$ffba,$ffb9,$ffb7
	dc.w	$ffb6,$ffb4,$ffb3,$ffb1,$ffb0,$ffae,$ffad,$ffab
	dc.w	$ffaa,$ffa8,$ffa7,$ffa5,$ffa4,$ffa2,$ffa1,$ff9f
	dc.w	$ff9e,$ff9d,$ff9b,$ff9a,$ff98,$ff97,$ff95,$ff94
	dc.w	$ff93,$ff91,$ff90,$ff8e,$ff8d,$ff8c,$ff8a,$ff89
	dc.w	$ff87,$ff86,$ff85,$ff83,$ff82,$ff80,$ff7f,$ff7e
	dc.w	$ff7c,$ff7b,$ff7a,$ff78,$ff77,$ff76,$ff74,$ff73
	dc.w	$ff72,$ff70,$ff6f,$ff6e,$ff6d,$ff6b,$ff6a,$ff69
	dc.w	$ff68,$ff66,$ff65,$ff64,$ff63,$ff61,$ff60,$ff5f
	dc.w	$ff5e,$ff5c,$ff5b,$ff5a,$ff59,$ff58,$ff56,$ff55
	dc.w	$ff54,$ff53,$ff52,$ff51,$ff4f,$ff4e,$ff4d,$ff4c
	dc.w	$ff4b,$ff4a,$ff49,$ff48,$ff47,$ff46,$ff44,$ff43
	dc.w	$ff42,$ff41,$ff40,$ff3f,$ff3e,$ff3d,$ff3c,$ff3b
	dc.w	$ff3a,$ff39,$ff38,$ff37,$ff36,$ff35,$ff34,$ff33
	dc.w	$ff32,$ff31,$ff31,$ff30,$ff2f,$ff2e,$ff2d,$ff2c
	dc.w	$ff2b,$ff2a,$ff29,$ff29,$ff28,$ff27,$ff26,$ff25
	dc.w	$ff24,$ff24,$ff23,$ff22,$ff21,$ff20,$ff20,$ff1f
	dc.w	$ff1e,$ff1d,$ff1d,$ff1c,$ff1b,$ff1b,$ff1a,$ff19
	dc.w	$ff19,$ff18,$ff17,$ff17,$ff16,$ff15,$ff15,$ff14
	dc.w	$ff13,$ff13,$ff12,$ff12,$ff11,$ff11,$ff10,$ff10
	dc.w	$ff0f,$ff0e,$ff0e,$ff0d,$ff0d,$ff0c,$ff0c,$ff0b
	dc.w	$ff0b,$ff0b,$ff0a,$ff0a,$ff09,$ff09,$ff08,$ff08
	dc.w	$ff08,$ff07,$ff07,$ff07,$ff06,$ff06,$ff06,$ff05
	dc.w	$ff05,$ff05,$ff04,$ff04,$ff04,$ff04,$ff03,$ff03
	dc.w	$ff03,$ff03,$ff02,$ff02,$ff02,$ff02,$ff02,$ff01
	dc.w	$ff01,$ff01,$ff01,$ff01,$ff01,$ff01,$ff00,$ff00
	dc.w	$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$ff00
	dc.w	$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$ff00
	dc.w	$ff00,$ff00,$ff00,$ff01,$ff01,$ff01,$ff01,$ff01
	dc.w	$ff01,$ff01,$ff02,$ff02,$ff02,$ff02,$ff02,$ff03
	dc.w	$ff03,$ff03,$ff03,$ff04,$ff04,$ff04,$ff04,$ff05
	dc.w	$ff05,$ff05,$ff06,$ff06,$ff06,$ff07,$ff07,$ff07
	dc.w	$ff08,$ff08,$ff08,$ff09,$ff09,$ff0a,$ff0a,$ff0b
	dc.w	$ff0b,$ff0b,$ff0c,$ff0c,$ff0d,$ff0d,$ff0e,$ff0e
	dc.w	$ff0f,$ff10,$ff10,$ff11,$ff11,$ff12,$ff12,$ff13
	dc.w	$ff13,$ff14,$ff15,$ff15,$ff16,$ff17,$ff17,$ff18
	dc.w	$ff19,$ff19,$ff1a,$ff1b,$ff1b,$ff1c,$ff1d,$ff1d
	dc.w	$ff1e,$ff1f,$ff20,$ff20,$ff21,$ff22,$ff23,$ff24
	dc.w	$ff24,$ff25,$ff26,$ff27,$ff28,$ff29,$ff29,$ff2a
	dc.w	$ff2b,$ff2c,$ff2d,$ff2e,$ff2f,$ff30,$ff31,$ff31
	dc.w	$ff32,$ff33,$ff34,$ff35,$ff36,$ff37,$ff38,$ff39
	dc.w	$ff3a,$ff3b,$ff3c,$ff3d,$ff3e,$ff3f,$ff40,$ff41
	dc.w	$ff42,$ff43,$ff44,$ff46,$ff47,$ff48,$ff49,$ff4a
	dc.w	$ff4b,$ff4c,$ff4d,$ff4e,$ff4f,$ff51,$ff52,$ff53
	dc.w	$ff54,$ff55,$ff56,$ff58,$ff59,$ff5a,$ff5b,$ff5c
	dc.w	$ff5e,$ff5f,$ff60,$ff61,$ff63,$ff64,$ff65,$ff66
	dc.w	$ff68,$ff69,$ff6a,$ff6b,$ff6d,$ff6e,$ff6f,$ff70
	dc.w	$ff72,$ff73,$ff74,$ff76,$ff77,$ff78,$ff7a,$ff7b
	dc.w	$ff7c,$ff7e,$ff7f,$ff80,$ff82,$ff83,$ff85,$ff86
	dc.w	$ff87,$ff89,$ff8a,$ff8c,$ff8d,$ff8e,$ff90,$ff91
	dc.w	$ff93,$ff94,$ff95,$ff97,$ff98,$ff9a,$ff9b,$ff9d
	dc.w	$ff9e,$ff9f,$ffa1,$ffa2,$ffa4,$ffa5,$ffa7,$ffa8
	dc.w	$ffaa,$ffab,$ffad,$ffae,$ffb0,$ffb1,$ffb3,$ffb4
	dc.w	$ffb6,$ffb7,$ffb9,$ffba,$ffbc,$ffbd,$ffbf,$ffc0
	dc.w	$ffc2,$ffc3,$ffc5,$ffc6,$ffc8,$ffc9,$ffcb,$ffcd
	dc.w	$ffce,$ffd0,$ffd1,$ffd3,$ffd4,$ffd6,$ffd7,$ffd9
	dc.w	$ffda,$ffdc,$ffde,$ffdf,$ffe1,$ffe2,$ffe4,$ffe5
	dc.w	$ffe7,$ffe8,$ffea,$ffec,$ffed,$ffef,$fff0,$fff2
	dc.w	$fff3,$fff5,$fff7,$fff8,$fffa,$fffb,$fffd,$fffe

	dc.w	$0000,$0002,$0003,$0005,$0006,$0008,$0009,$000b
	dc.w	$000d,$000e,$0010,$0011,$0013,$0014,$0016,$0018
	dc.w	$0019,$001b,$001c,$001e,$001f,$0021,$0022,$0024
	dc.w	$0026,$0027,$0029,$002a,$002c,$002d,$002f,$0030
	dc.w	$0032,$0033,$0035,$0037,$0038,$003a,$003b,$003d
	dc.w	$003e,$0040,$0041,$0043,$0044,$0046,$0047,$0049
	dc.w	$004a,$004c,$004d,$004f,$0050,$0052,$0053,$0055
	dc.w	$0056,$0058,$0059,$005b,$005c,$005e,$005f,$0061
	dc.w	$0062,$0063,$0065,$0066,$0068,$0069,$006b,$006c
	dc.w	$006d,$006f,$0070,$0072,$0073,$0075,$0076,$0077
	dc.w	$0079,$007a,$007b,$007d,$007e,$0080,$0081,$0082
	dc.w	$0084,$0085,$0086,$0088,$0089,$008a,$008c,$008d
	dc.w	$008e,$0090,$0091,$0092,$0093,$0095,$0096,$0097
	dc.w	$0099,$009a,$009b,$009c,$009e,$009f,$00a0,$00a1
	dc.w	$00a2,$00a4,$00a5,$00a6,$00a7,$00a8,$00aa,$00ab
	dc.w	$00ac,$00ad,$00ae,$00af,$00b1,$00b2,$00b3,$00b4
	dc.w	$00b5,$00b6,$00b7,$00b8,$00b9,$00ba,$00bc,$00bd
	dc.w	$00be,$00bf,$00c0,$00c1,$00c2,$00c3,$00c4,$00c5
	dc.w	$00c6,$00c7,$00c8,$00c9,$00ca,$00cb,$00cc,$00cd
	dc.w	$00ce,$00cf,$00cf,$00d0,$00d1,$00d2,$00d3,$00d4
	dc.w	$00d5,$00d6,$00d7,$00d7,$00d8,$00d9,$00da,$00db
	dc.w	$00dc,$00dc,$00dd,$00de,$00df,$00e0,$00e0,$00e1
	dc.w	$00e2,$00e3,$00e3,$00e4,$00e5,$00e5,$00e6,$00e7
	dc.w	$00e7,$00e8,$00e9,$00e9,$00ea,$00eb,$00eb,$00ec
	dc.w	$00ed,$00ed,$00ee,$00ee,$00ef,$00ef,$00f0,$00f1
	dc.w	$00f1,$00f2,$00f2,$00f3,$00f3,$00f4,$00f4,$00f5
	dc.w	$00f5,$00f5,$00f6,$00f6,$00f7,$00f7,$00f8,$00f8
	dc.w	$00f8,$00f9,$00f9,$00f9,$00fa,$00fa,$00fa,$00fb
	dc.w	$00fb,$00fb,$00fc,$00fc,$00fc,$00fd,$00fd,$00fd
	dc.w	$00fd,$00fd,$00fe,$00fe,$00fe,$00fe,$00fe,$00ff
	dc.w	$00ff,$00ff,$00ff,$00ff,$00ff,$00ff,$0100,$0100
	dc.w	$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100

bfont
	incbin	"bartfontfinal"

eyesframes		ds.l	10,0
ufaceframes		ds.l	1,0
;lfaceframes		ds.l	1+25+29,0	; there are 55 different frames
lfaceframes		ds.l	1+25,0	; there are 26 different frames

; 1 frame with 534 vertices * 3 = 1602
uface
	incbin	"3duface"

; 1 frame with 568 vertices * 3 = 1704
lface
	incbin	"3dlface"

; 10 frames with 118 vertices * 3 = 3894
blink
	incbin	"3dblink"

; 25 frames with 568 vertices * 3 = 42600
go
	incbin	"3dgo"

; 29 frames with 568 vertices * 3 = 49416
;tschak
;	incbin	"3dtschak"

	even
lspbank
	incbin	"talking_heads.lsbank"
lspbankend

	even
lspmusic
	incbin	"talking_heads.lsmusic",10	; skip header (10 bytes)

btext
	dc.b	-1,25

	dc.b	"THIS IS REAL AND IT WAS ABOUT TIME AG$IN."
	dc.b	"      SPREADPOINT PRESENTS    ;;;    TALKING HEADS ",-2,4,"  ",-1,150

	dc.b	"      RELEASED AT    @@EVOKE 2022@    ",-1,150, "ON 6 AUGUST 2022.      "
	dc.b	"      CODE AND GRAPHICS (LITERALLY,  ;  DEPECHE      "
	dc.b	"      MUSIC  ;  LORD      "
	dc.b	"      FLASHING GLITCHES (FLIT",-2,0,"CHES=,  ;  "
	dc.b	"INSPIRED BY LOGICOM$%S ASTROPH$GE INTRO      "

	dc.b	"      > > >      "

	dc.b	"      THIS 64 KB INTRO WAS ORIGINALLY INTENDED AS PART OF ANOTHER PRODUCTION. "
	dc.b	"HOWEVER THIS HAS BEEN DELAYED AND WILL PROBABLY FOLLOW LATER THIS YEAR."
	dc.b	"      "

	dc.b	"      > > >      "

	dc.b	"      BEFORE WE DIVE INTO THE ENDLESS GREETINGS LIST "
	dc.b	"DEPECHE SENDS HIS REGARDS TO      ;;;      EVERYONE AT"
	dc.b	"     SPREADPOINT",-2,6,"    ",-1,150
	dc.b	"  "
	dc.b    "      4MAT      "
	dc.b	"      BIFAT      "
	dc.b	"      BLUEBERRY (SHRINKLER IS GREAT,      "
	dc.b	"      DIPSWITCH      "
	dc.b	"      GASM$N      "
	dc.b	"      HOFFMAN (THE ALW$YS ACTIVE MULTI TALENT,      "
	dc.b	"      LASERBE$M      "
	dc.b	"      LEMMY (THANKS FOR YOUR SUPPORT WITH THE FLOPPY DISK DRIVE,      "
	dc.b	"      LEONARD (LIGHT SPEED PLAYER IS GREAT TOO,      "
	dc.b	"      MCCOY (STILL SPREADING THE NEWS AROUND THE WORLD,      "
	dc.b	"      PSENOUGH (FIRST CLASS REPORTS,      "
	dc.b    "      SKOPE (SPREADPOINTY IS A NICE ADJECTIVE,      "
	dc.b	"      STINGRAY      "
	dc.b    "      THE MEG$-MIGHTY SWISS CRACKING ASSOCI$TION "
	dc.b    "(THANKS FOR ADDING 2<5 YEARS TO SC$.CH,      "
	dc.b	"      THE PEOPLE AT DEMOZOO > KESTR$ BITWORLD > POUET      "
	dc.b	"      UNLOCK      "
	dc.b	"      VIRGILL (NATION$L WAFFLE DAY IS SUCH A BANGER,      "
	dc.b	"      ZODIAC (LEADER IN PRODUCING SHIT=,      "

	dc.b	"      > > >      "

	dc.b	"      LORD ON "
	dc.b	"THE KEYS NOW. THIS TIME I HAD FUN MAKING AN ELECTRONIC BODY MUSIC TRACK. "
	dc.b	"THANKS TO STEFF FOR THE GREAT WEB BASED BASSOONTRACKER WHICH "
	dc.b	"I USED TO GET THIS TIMING CRAZY SOUNDTRACKER MODULE OUT TO YOU. "
	dc.b	"WHILE CREATING THIS MEMORIES FROM MY EARLY AMIG$ DAYS FLOODED "
	dc.b	"ME AND I WISH TO THANK SOME FRIENDS FOR THE LONG TIME FRIENDSHIP "
	dc.b	"WHICH BROUGHT ME THRU MY VERY TROUBLED YOUTH: ALL MEMBERS OF SPREADPOINT "
	dc.b	"> UWE > PAULY > MY COSYSOP JEANIE > TORMENTOR > DELT$ > MWS "
	dc.b	"> SMURF > SUPERFORMANCE > WHITEHEAT2XLC AND OF COURSE GREETINGS "
	dc.b	"TO MY RECENT SCENE FRIENDS FROM UNDERGROUND CONFERENCE "
	dc.b	"(SORRY COULD NOT MAKE IT THIS YEAR, > CCC > EVOKE AND YOU!      "

;	dc.b	"      LET%S TALK ABOUT THE ANIM$TIONS. THEY ALL HAVE A NAME. "
;	dc.b    "CAN YOU FIND OUT WHICH NAME GOES WITH WHICH ANIM$TION?"
;	dc.b	"      ;      CONVERSATION      "
;	dc.b	"      DOUBLE VISION      "
;	dc.b	"      FALLING      "
;	dc.b	"      SHAKE      "
;	dc.b	"      SINE OF THE TIMES      "
;	dc.b	"      SPLASH      "
;	dc.b	"      WAKE UP      "
	
	dc.b	"      > > >      "

	dc.b	"      NOW THE OFFICIAL SPREADPOINT GREETINGS "
	dc.b	"TO THE PRESENT AND THE PAST. WE SHALL SEE ABOUT THE FUTURE. ARE YOU READY?      ;;;"
	dc.b	"      ABYSS      "
	dc.b	"      ADEPT      "	
	dc.b	"      ALCATR$Z      "
	dc.b	"      ALT$IR      "
	dc.b	"      APEX      "
	dc.b	"      ATTENTIONWHORE      "
	dc.b	"      BATM$N GROUP      "
	dc.b	"      BRAINSTORM      "
	dc.b	"      DEFJAM      "
	dc.b	"      DRIFTERS      "
	dc.b	"      FAIRLIGHT      "
	dc.b    "      HIGH QUALITY CRACKINGS    "
	dc.b	"      LOGICOM$      "
	dc.b	"      LOONIES      "
	dc.b	"      MELON DEZIGN      "
	dc.b	"      RADW$R ENTERPRISES      "
	dc.b	"      RAZOR 1911      "
	dc.b	"      REBELS      "
	dc.b    "      SANITY      "
	dc.b	"      SCOOPEX      "
	dc.b	"      SECTION 8      "
	dc.b	"      SPACEB$LLS      "
	dc.b	"      SPACEPIGS      "
	dc.b	"      SWISS CRACKING ASSOCI$TION      "
	dc.b	"      THE BLACK LOTUS      "
	dc.b	"      THE ELECTRONIC KNIGHTS      "
	dc.b	"      THE LIGHT CIRCLE      "
	dc.b	"      THE SILENTS      "
	dc.b	"      THE STAR FRONTIERS      "
	dc.b	"      TRSI      "
	dc.b	"      UNIT $      "
	dc.b	"      UP ROUGH      "
	dc.b	"      VISION FACTORY      "
	dc.b	"      VOID      "

	dc.b	"      > > >      "
	
	dc.b	"      THAT%S IT.      "
	dc.b	"JOIN THE ",-2,1,"POWER!      "
	dc.b	"SPREADPOINT INTERGAL$CTIC SIGNING OFF.      "
	dc.b	"      > > >      "
	dc.b	"            "
	dc.b	-1,100
	dc.b	" "
	dc.b	0

;	HIT:	 !"#$%&'()*+,-./:;<=>?@   NOTE: @ = 4 px SPACE
;	FOR:	S!"_A´'_(___)-./:A^To?S         _ = unused/void/empty
;           P                R M  P
;           A                R    A
;           C                O    C
;           E                W    E
