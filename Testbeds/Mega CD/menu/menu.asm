; Copyright (c) 2012, Team Evanesco
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
; 
; Redistributions of source code must retain the above copyright notice, this
; list of conditions and the following disclaimer.
; Redistributions in binary form must reproduce the above copyright notice,
; this list of conditions and the following disclaimer in the documentation
; and/or other materials provided with the distribution.
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS 
; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH 
; DAMAGE.

		org		$200000								; Offset for start of Word RAM

Header:
		dc.b	"MAIN"								; Header ID
		dc.w	(EntryPoint-Header), 0				; Entry point, reserved
		even

EntryPoint:
		lea		$FF0000, a0							; Start of scratch RAM
		moveq	#0, d0								; Clear d0
		moveq	#0, d1								; Clear d1
		move.b	#$F0, d1							; Loop over $100 bytes
		
@clearDMAQueue:
		move.l	d0, (a0)+
		dbf		d1, @clearDMAQueue
		
		lea		VBlank(pc), a1						; Load Address for VBL routine
		jsr     $368								; Make appropriate change in Interrupt Jump table
		
		move	#$2200, sr							; Re-enable ints.
		
		move.w	#$8134, $C00004						; Display on, DMA on, VBI on, 28 row, Genesis mode.
		move.w	#$8230, $C00004						; Plabe A address
		move.w	#$8328, $C00004						; Window plane address
		
		move.w	#$8407, $C00004						; Plane B address
		move.w	#$857C, $C00004						; Sprite table address
		move.w	#$8700, $C00004						; Backdrop is first colour in palette 0
	
		move.w	#$8B02, $C00004						; Per scanline scrolling
			
		move.w	#$8C81, $C00004						; No interlace, 40 tiles wide.
		move.w	#$8D3F, $C00004						; Location of HScroll
		move.w	#$8F02, $C00004						; Auto inc is 2
		
		move.w	#$9001, $C00004						; Plane size is 64x32
		
		move	#$2700, sr							; Disable interrupts.
		
		bsr.w	ClearFG								; Clear the FG plane
		bsr.w	ClearBG								; Clear the BG plane
		
		lea		Font, a0							; Load from RAM
		move.w	#$E00, d0							; Start of VRAM
		move.w	#$80, d1							; Load 64 tiles
		bsr.w	Load_Tiles							; Write to VRAM
		
		lea		MainMenu_art, a0					; Load from decompressed art
		moveq	#0, d0								; Start of VRAM
		move.w	#$69, d1							; Load 105 tiles
		bsr.w	Load_Tiles							; Write to VRAM
		
;		lea		MainMenu_BG_Map, a1					; Load the BG maps to a1
;		move.l	#$60000003, d0						; The BG maps
;		moveq	#$27, d1							; $27 columns
;		moveq	#$1B, d2							; $1B rows
;		bsr.w	ShowVDPGraphics						; Write to the BG.
		
		lea		MainMenu_FG_Map, a1					; Load the BG maps to a1
		move.l	#$40000003, d0						; The BG maps
		moveq	#$27, d1							; $27 columns
		moveq	#$1B, d2							; $1B rows
		bsr.w	ShowVDPGraphics						; Write to the FG.
		
		lea		ASCII_MenuChoices, a1				; Load loading text.
		move.l	#$42000003, d5						; Write to the the FG maps.
		bsr.w	WriteASCIIToScreen					; Write some ASCII to screen.

		move.w	#$8174, $C00004						; Enable the display.
		move.l	#0, $FF0200							; Clear status long		
		move	#$2200, sr							; Enable interrupters.
		
		lea		MainMenu_Pal, a0					; Fade in palette a0
		moveq	#0, d0								; Offset is 0
		bsr.w	Load_Palette						; Display palette.
		
		move.b	#0, (SelectedFMV).w					; Clear out selected FMV.
		
MainLoop:
		stop	#$2500								; Wait for VBlank
		
		move.b	(ControlStateArea+1).w, d0			; Get button state to d0 (if we AND the address, it will get destroyed)
		and.b	#$C, d0 							; Is Left/Right pressed?
		bne.s	@leftRightPressed					; If so, branch.
		
		move.b	(ControlStateArea+1).w, d0			; Get button state to d0 (if we AND the address, it will get destroyed)
		and.b	#$80, d0 							; Is start pressed?
		bne.s	@startPressed						; If so, branch.
		
		bra.w	MainLoop
		
@leftRightPressed:
		move.b	(ControlStateArea+1).w, d0			; Get button state to d0 (if we AND the address, it will get destroyed)
		and.b	#$8, d0 							; Is Right pressed?
		bne.s	@rightPressed						; If so, branch.
		
		cmp.b	#0, (SelectedFMV).w					; Are we at the left limit?
		beq.w	MainLoop							; If so, branch back into the main loop.
		
		subq.b	#1, (SelectedFMV).w					; Move the cursor to the left one.
		
		bra.s	@continue

@rightPressed:
		cmp.b	#7, (SelectedFMV).w					; Are we at the right limit?
		bhs.w	MainLoop							; If so, branch back into the main loop.
		
		addq.b	#1, (SelectedFMV).w					; Move the cursor to the right one.

@continue:
		move.l	#$42200003, d1						; VRAM address
		moveq	#0, d0								; Clear d0
		move.b	(SelectedFMV).w, d0					; FMV to play
		bsr.w	DisplayHEXOnScreen
		
		bra.w	MainLoop							; Jump back into the main loop
		
@startPressed:
		
		bra.w	MainLoop							; Jump back into the main loop
		
;===================================================================================================
; Interrupts
;===================================================================================================

VBlank:
		movem.l	d0-a6, -(sp)						; Back up registers.
		bsr.w	JoypadRead							; Read joypads.
		move.b	#1, $A12000							; Trigger Level 2 int on Sub CPU
		movem.l	(sp)+, d0-a6						; Restore registers.
		rte
		
;===================================================================================================
; VDP routines
;===================================================================================================

Load_Palette:
;Loads a palette of 16 entries
; a0 = data source
; d0 = palette number
		move.l 	d1, -(a7)
		moveq 	#0, d1
		move.w 	#$C000, d1
		lsl.w 	#5, d0
		add.w	 d0, d1
		swap 	d1
		move.l 	d1, $C00004
		lea 	$C00000, a1
		rept 	16
		move.w (a0)+, (a1)
		endr
		move.l (a7)+, d1
		rts
	
Load_Tiles:
;Loads tiles, a0 = in
;d0 = destination in vram1
;d1 = amount of tiles
;Breaks d0, d1, d2, d3, a0, a1
		bsr.s 	CalcOffset
		lea 	$C00000, a1
		
@LoadLoop:
		moveq	#7, d3

@loop2:
		move.l (a0)+, (a1)
		dbf		d3, @loop2
	
		dbf 	d1, @LoadLoop
		rts
	
CalcOffset:
;Calculate VDP command shits from an adress in d0.... breaks.... nothing!
;Optimization time!!!
		movem.l d0-d3, -(a7)
		moveq 	#0, d2
		moveq 	#0, d3
		move.w 	#$4000, d2
		move.w 	d0, d3
		and.w 	#$3FFF, d3
		add.w 	d3, d2
		and.w 	#$C000, d0
		lsr.w 	#8, d0
		lsr.w 	#6, d0 
		swap 	d2
		or.l 	d0, d2
		move.l 	d2, $C00004
		movem.l (a7)+, d0-d3
		rts
	
; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to map tile to VDP screen
;
; Address to the map goes in a5
; Number of columns to d0
; Number of rows to d1
; VDP address to d2.
; ---------------------------------------------------------------------------

Screen_Map:		
		lea		($C00000).l,a6					; Load VDP data port address to a6
		lea		$04(a6),a4						; Load VDP address port address to a4
		move.l	#$00800000,d4					; Prepare line add value

SM_Row:
		move.l	d2,(a4)							; Set VDP to VRam write mode
		move.w	d0,d3							; Reload number of columns

SM_Column:
		move.w	(a5)+,(a6)						; Dump map to VDP map slot
		dbf		d3,SM_Column					; Repeat til columns have dumped
		add.l	d4,d2							; Increase to next row on VRam
		dbf		d1,SM_Row						; Repeat til all rows have dumped
		rts										; Return To Sub
		
ShowVDPGraphics:			; XREF: SegaScreen; TitleScreen; SS_BGLoad
		lea	($C00000).l,a6
		move.l	#$800000,d4

loc_142C:
		move.l	d0,4(a6)
		move.w	d1,d3

loc_1432:
		move.w	(a1)+,(a6)
		dbf	d3,loc_1432
		add.l	d4,d0
		dbf	d2,loc_142C
		rts	
		
; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to map tile to VDP screen
;
; Address to the map goes in a5
; Number of columns to d0
; Number of rows to d1
; VDP address to d2.
; Value to add to everything to d6.
; ---------------------------------------------------------------------------

Screen_Map_WithAdd:		
		lea		($C00000).l, a6					; Load VDP data port address to a6
		lea		4(a6), a4						; Load VDP address port address to a4
		move.l	#$00800000, d4					; Prepare line add value

@SM_Row:
		move.l	d2, (a4)						; Set VDP to VRam write mode
		move.w	d0, d3							; Reload number of columns

@SM_Column:
		move.w	(a5)+, d5						; Dump map to d5
		add.w	d6, d5							; Add addition offset to the tile.
		move.w	d5, (a6)						; Write to VDP
		dbf		d3, @SM_Column					; Repeat til columns have dumped
		add.l	d4, d2							; Increase to next row on VRam
		dbf		d1, @SM_Row						; Repeat til all rows have dumped
		rts										; Return To Sub


;===================================================================================================
; Controller input routines
;===================================================================================================
		
		SelectedFMV: EQU $FFFFFF80
		ControlStateArea: EQU $FFFFFF82									; Area in RAM that holds the button states

; Routine to read the currently pressed buttons from all three IO ports (control 1&2, EXT)
; Outputted format is in S ACB RL DU

JoypadRead:
;		move.w	#$100, $A11100										; Stop Z80 and wait
;		
;@waitZ80:
;		btst	#0, $A11101											; Has the Z80 stopped?
;		bne.s	@waitZ80											; If not, wait.
		
		lea	ControlStateArea, a0									; Area where joypad states are written
		lea	$A10003, a1												; First joypad port
		
		moveq	#2, d7												; Read all 3 control ports
		
@readJoypads:
		move.b	#0, (a1)											; Assert /TH
		rept 4
		nop															; Wait until data is ready.
		endr

		move.b	(a1), d0											; Read back controller states. (00SA00DU)
		lsl.b	#2, d0												; Shift start and A into the high 2 bits
		and.b	#$C0, d0											; Get only S+A buttons
			
		move.b	#$40, (a1)											; De-assert /TH
		rept 4
		nop															; Wait until data is ready.
		endr
		
		move.b	(a1), d1											; Read back the controller states. (11CBRLDU)
		and.b	#$3F, d1											; Get only CBRLDU alone
		or.b	d1, d0												; OR together the control states
		not.b	d0													; Invert the bits
		
		move.b	(a0), d1											; Get the current button press bits from RAM
		eor.b	d0, d1												; OR the pressed buttons with the last frame's pressed buttons
		
		move.b	d0, (a0)+											; Write the pressed bits
		and.b	d0, d1												; AND the bits together.
		move.b	d1, (a0)+											; Write the held bits
		
		addq.w	#2, a1												; Use next control port
		dbf	d7, @readJoypads										; Loop until all joypads are read
		
;		move.w	#$0, $A11100										; Re-start the Z80
		rts

		
;===================================================================================================
; Video clrearing routines
;===================================================================================================

ClearFG:
		move.l	#$40000003, d5
        bra.s	DoClear

ClearBG:
		move.l	#$60000003, d5
		
DoClear:
		move.l	d5, $C00004
		moveq	#0, d0
		moveq	#0, d1
		move.w	#$375, d0

@loop:
		move.l	d1, $C00000
		dbf		d0, @loop
		rts

        include		"common/loader.asm"
        even
        
PalSys_FadeTick:
		rts
		
;===================================================================================================
; Displaying routines
;===================================================================================================

DisplayHEXOnScreen:
		lea		$C00000, a0
		move.l	d1, 4(a0)
		
		move.b	d0, -(sp)
		and.b	#$F0, d0
		lsr.b	#4, d0
		
		cmp.b	#$A, d0
		blo.s	@writeHighNybble
		
		add.w	#$07, d0
		
@writeHighNybble:
		add.w	#VRAM_ASCII+$10, d0
		move.w	d0, (a0)
		
		moveq	#0, d0
		move.b	(sp)+, d0
		and.b	#$0F, d0
		
		cmp.b	#$A, d0
		blo.s	@writeLowNybble
		
		add.w	#$07, d0
		
@writeLowNybble:
		add.w	#VRAM_ASCII+$10, d0
		move.w	d0, (a0)
		
@done:
		rts
		
WriteASCIIToScreen:	
_Tab		equ	$09									; Byte to indicate a tab.
_NewLine	equ	$0A									; Byte to indicate a newline. 
_Space		equ	$0D									; Byte to indicate a space.
_End		equ	$7D									; Byte to indicate end of text.
VRAM_ASCII	equ	$70									; The offset to add to the VDP tile
		
LoadASCII:
		move.l	d5,($C00004).l

LoadText_Loop:
LoadText_Loop_NoDelay:
		moveq	#0,d1
		move.b	(a1)+,d1
		move.b	d1,d2

		cmp.b	#_Tab,d2
		beq	LoadASCII_AddTab
		cmp.b	#_Space,d2
		beq	LoadASCII_AddSpace
		cmp.b	#_End,d2
		bne	LoadASCII_Print

		rts

LoadASCII_Print:
		cmp.b	#_NewLine,d2
		beq		LoadASCII_NewLine

		move.b	d2,d1
		add.w	#VRAM_ASCII,d1
		subi.w	#$0020, d1
		move.w	d1,($C00000)
		
LoadASCII_Fix:
		bra		LoadText_Loop

LoadASCII_NewLine:
		add.l	#$800000,d5
		bra		LoadASCII
		
LoadASCII_AddSpace:
		cmp.b	#_Tab,d2
		beq		LoadASCII_Fix

		add.l	#$800000,d5
		bra		LoadASCII

LoadASCII_AddTab:
		add.l	#$100000,d5
		bra		LoadASCII

LoadASCII_Test2:
		cmp.b	#_NewLine,d2
		beq		LoadASCII_Fix
		rts
		
;===================================================================================================
; Data section
;===================================================================================================

MainMenu_Art:
		incbin		"menu/art.bin"
		even
		
MainMenu_Pal:
		incbin		"menu/pal.bin"
		even
		
MainMenu_BG_Map:
		incbin		"menu/bg_map.bin"
		even
		
MainMenu_FG_Map:
		incbin		"menu/fg_map.bin"
		even
		
ASCII_MenuChoices:
		dc.b	"  FMV to play: $00", _NewLine
		dc.b	_NewLine, _NewLine, _NewLine, _NewLine, _NewLine, _NewLine
		dc.b	_NewLine, _NewLine, _NewLine, _NewLine, _NewLine, _NewLine
		dc.b	_NewLine, _NewLine, _NewLine, _NewLine, _NewLine, _NewLine
		dc.b	_NewLine, _NewLine, _NewLine
		dc.b	"        Press Start to Play FMV"
		dc.b	_End
		even
		
Font:	incbin	"menu/Font.bin"
		even
		
FMV_MainCode:
		include	"../../Decoder/Mega CD/Main CPU/main.asm"