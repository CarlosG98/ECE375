;***********************************************************
;*
;*	Carlos_Gonzalez_Lab4_sourcecode.asm
;*
;*	This program utilizes the LCD Display on the TekBot
;*	The TekBot will display a message based on the button pressed
;*	
;*
;***********************************************************
;*
;*	 Author: Carlos Gonzalez
;*	   Date: 1/31/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register is
								; required for LCD Driver

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		LDI mpr, LOW(RAMEND)	;Low byte of End SRAM Address
		OUT SPL, mpr			;Write byte to SPL		 
		LDI mpr, HIGH(RAMEND)	;High byte of End SRAM Address
		OUT SPH, mpr			;Write byte to SPH
		; Initialize LCD Display
		RCALL LCDInit			;Call LCD init function from driver
		; Move string 1 from Program Memory to Data Memory
		LD Z, (STRING1_BEG<<1) ;Low byte of string 1
		LDI ZH, HIGH(STRING1_BEG<<1);High byte of string 1
		LDI YL, $00; point Y to destination in data memory (first line)
		LDI YH, $01; point Y to destination in data memory (first line)
		DO:
			LPM mpr, Z+ ; load data to register
			ST Y+, mpr; store data of register to data memory
			cpi ZL, LOW(STRING1_END<<1); compare lower byte of Z to $0f
			brne DO ; branch to do if not equal
		NEXT:
		;Move string 2 from Program Memory to Data Memory
		LDI ZL, LOW(STRING2_BEG<<1) ;low byte of string 2
		LDI ZH, HIGH(STRING2_BEG<<1);high byte of string 2
		LDI YL, $10; point Y to destination in data memory(second line)
		LDI YH, $01; point Y to destination in data memory(second line)
		DO_2:
			LPM mpr, Z+; load program memory to register
			ST Y+, mpr ; store data of register to data memory
			CPI ZL, LOW(STRING2_END<<1); compare lower byte of Z to $1f
			BRNE DO_2 ;loop until last char is stored to data memory
		NEXT_2:
			;Initialize Port D for input
			LDI	mpr, $00	;set Port D data direction register
			OUT DDRD, mpr	;for input
			LDI mpr, $ff	;set Port D data register
			OUT PORTD, mpr	;all Port D inputs are tri-state
		; NOTE that there is no RET or RJMP from INIT, this
		; is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program
		; Display the strings on the LCD Display
		in mpr, PIND	;get input value from PORT D
		andi mpr, (1<<0| 1<<1 | 1<<7);
		cpi mpr, (1<<1 | 1<<7); check for PD0
		brne	NEXT1 ;go to next check if not equal
		rcall	LCDWrite ;write message 1 to LCD
		rjmp	MAIN			; jump back to main and create an infinite
							; while loop.  Generally, every main program is an
							; infinite while loop, never let the main program
								; just run off
								;
NEXT1:
		cpi	mpr, (1<<0 | 1<<7) ;check if P1 was pressed
		brne	NEXT2; go to next check if not equal
		rcall LCDWrRev; write message 2 to display
		rjmp	MAIN; jump back to main
NEXT2:
		cpi mpr, (1<<0 | 1<<1); check if button 7 was pressed
		brne	MAIN; go back to main if not equal
		rcall	LCDClr; clear LCD
		rjmp	MAIN; loop back to main
;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: LCDWrRev
; Desc: Writes line 1 onto line2, and line 2 onto line 1 
;		copied from driver and altered for correct functionality
;-----------------------------------------------------------
LCDWrRev:							;LCD Write Reverse
		rcall LCDWrLn1Rev; 
		rcall LCDWrLn2Rev
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: LCDWrLn1Rev
; Desc: Write line1 onto line2
;		
;-----------------------------------------------------------
LCDWrLn1Rev:
			push 	mpr				; Save mpr
		push	ZL				; Save Z pointer
		push	ZH				;
		push	count			; Save the count register
		push	line			; Save the line register
								
		ldi		ZL, low(LCDLn1Addr)
		ldi		ZH, high(LCDLn1Addr)		 
		ldi		line, LCDLine2	; Set LCD line to Line 2
		rcall	LCDSetLine		; Restart at the beginning of line 1
		rcall	LCDWriteLine	; Write the line of text

		pop		line
		pop		count			; Restore the counter
		pop		ZH				; Restore Z pointer
		pop		ZL				;
		pop 	mpr				; Restore mpr
		ret		

;-----------------------------------------------------------
; Func: LCDWrLn2Rev
; Desc: Write line2 onto line1
;		
;-----------------------------------------------------------
LCDWrLn2Rev:
		push 	mpr				; Save mpr
		push	ZL				; Save Z pointer
		push	ZH				;
		push	count			; Save the count register
		push	line			; Save the line register
								
		ldi		ZL, low(LCDLn2Addr)
		ldi		ZH, high(LCDLn2Addr)		 
		ldi		line, LCDLine1	; Set LCD line to Line 2
		rcall	LCDSetLine		; Restart at the beginning of line 2
		rcall	LCDWriteLine	; Write the line of text

		pop		line
		pop		count			; Restore the counter
		pop		ZH				; Restore Z pointer
		pop		ZL				;
		pop 	mpr				; Restore mpr
		ret



;***********************************************************
;*	Stored Program Data
;***********************************************************
;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING1_BEG:
.DB				"Carlos Gonzalez "		; Declaring data in ProgMem
STRING1_END:

STRING2_BEG:
.DB				"Hello, World"		; Declaring data in ProgMem
STRING2_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver


