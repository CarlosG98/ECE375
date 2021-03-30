;***********************************************************
;*
;*	Carlos_Gonzalez_Lab8_sourcecode.asm
;*
;*	Morse Code Transmitter
;*
;*
;***********************************************************
;*
;*	 Author: Carlos Gonzalez
;*	   Date: 2/27/21
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	charCounter = r3		; character counter for LCD line 2
.def	Achar = r5				; will hold ascii value for 'A'
.def	Zchar = r6				; will hold ascii value for 'Z'
.def	currChar = r7			; holds current char as we loop through user message
.def	numChars = r8			; tracks number of chars entered by user
;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

.org	$0046					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer
		ldi		mpr, LOW(RAMEND)		; get low byte of ramend
		out		SPL, mpr				; write to low byte of stack pointer
		ldi		mpr, HIGH(RAMEND)		; get high byte
		out		SPH, mpr				; write to sph
		; Configure I/O ports
		ldi		mpr, $FF				; set pins as output
		out		DDRB, mpr				;
		ldi		mpr, $00				; Initialize Port B data register
		out		PORTB, mpr				; such that outputs are low
			
		ldi		mpr, $00				; Set Port D data direction register
		out		DDRD, mpr				; as input
		ldi		mpr, $FF				; Inititalize Port D data register
		out		PORTD, mpr				; so all are tri-state

		; Configure External Interrupts, if needed
		ldi		mpr, 0b00000010		; detect on falling edge
		sts		EICRA, mpr			; set INT0
		ldi		mpr, 0b10100010		; detect on falling edge
		sts		EICRB, mpr			; set INT7, 6, 4
		ldi		mpr, 0b11010001		; allow INT7,6,4,0 to be detected
		out		EIMSK, mpr			; set mask

		; Configure Timer/Counter1
		ldi		mpr, 0b00000000		;set to normal mode
		out		TCCR1A, mpr			;
		ldi		mpr, 0b00000101		; with prescaler 1024
		out		TCCR1B, mpr			;

		ldi		mpr, $00			; start charCounter at 0
		mov		charCounter, mpr	;

		ldi		mpr, $41			; set A value
		mov		Achar, mpr			;
		ldi		mpr, $5A			; set Z value
		mov		Zchar, mpr			;

		rcall	LCDInit				; initialize LCD
		rcall	WelcomeMessage		; print initial message

			

MAIN:
		rcall checkBtnPress			; poll for first PD0 press
		rcall	selectMessage		; begin message seletion loop
		rjmp	MAIN

;-----------------------------------------------------------
; WelcomeMessage:	
; Desc:	prints the welcome message when booting TekBot 
;		
;-----------------------------------------------------------
WelcomeMessage:	

		; If needed, save variables by pushing to the stack
		push	mpr

		; Execute the function here
		ldi		ZL, LOW(STRING1_BEG<<1)		; get low byte of top message
		ldi		ZH, HIGH(STRING1_BEG<<1)	; high byte
		ldi		YL, $00						; point to top line of LCD
		ldi		YH, $01						;
		DO:
			;Move string 1 from Program Mem. to Data Mem.
			LPM mpr, Z+ ; load data to register
			ST Y+, mpr; store data of register to data memory
			cpi ZL, LOW(STRING1_END<<1); compare lower byte of Z to $0f
			brne DO ; branch to do if not equal
		
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

		rcall	LCDWrite	; write message to LCD
		; Restore any saved variables by popping from stack
		pop		mpr

		ret						; End a function with RET


;-----------------------------------------------------------
; Func:	checkBtnPress
; Desc:	polls for first PD0 press. Once pressed, user can 
;		begin message selecition/transmission
;-----------------------------------------------------------
checkBtnPress:	

		; If needed, save variables by pushing to the stack
		push	mpr

		; Execute the function here
		PRESS_LOOP:
			in		mpr, PIND			; read PIND 
			ori		mpr, 0b11111110		; we only care about PD0
			cpi		mpr, 0b11111110		; compare
			brne	PRESS_LOOP			; if not pressed, loop

		rcall	LCDClr					;clear LCD	
		rcall	LCDInit					; reinit lcd

		pop		mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: selectMessage
; Desc:	message selection loop where user will input messages 
;		to transmit
;-----------------------------------------------------------
selectMessage:	

		; If needed, save variables by pushing to the stack

		; Execute the function here
		ldi		mpr, 0b00000000		; turn off LEDs
		out		PORTB, mpr			;
		ldi		mpr, 1				; char selected will start at 1
		mov		numChars, mpr		; number of chars entered by user 
		rcall	LCDClr				; clr LCD
		rcall	LCDInit				; reinit
		rcall	PrintMsg3			; print "Enter word: "
		ldi		YL, $10				; point Y to bottom line of LCD
		ldi		YH, $01				;

		ldi		mpr, 0				; set character counter
		mov		charCounter, mpr	; to zero
		
		rjmp	PRINT_CHARS
PRINT_CHARS:
		mov		mpr, charCounter	; add char offset
		add		mpr, Achar			; to Achar
		st		Y, mpr				; store char to current Y position on LCD
		rcall	LCDWrLn2			; print it
		rjmp	NEXT			; begin input-check loop
NEXT:
		in		mpr, PIND			; read in PIND
		rcall	Wait_130			; debouncing delay
		ori		mpr, $2e			; only care about PIND7,6,4,0. set others to 1 
		cpi		mpr, 0b10111111		; check for PD6 press
		brne	NEXT_2				; if not pressed, go to next check
		rjmp	FWD_CHAR			;
FWD_CHAR:
		mov		mpr, charCounter	; 
		inc		mpr					; increment ascii value for next character in alphabet
		cpi		mpr, 26				; check if past 'Z'
		breq	RESET_CHAR1			; if so, reset to 'A'
		mov		charCounter, mpr	; place back in original reg
		rjmp	PRINT_CHARS
RESET_CHAR1:
		ldi		mpr, 0				;
		mov		charCounter, mpr	; reset to 'A'
		rjmp	PRINT_CHARS
NEXT_2:	
		cpi		mpr, 0b01111111		; check for PD7 press
		brne	NEXT_3				; if not pressed, go to next check
		rjmp	BCK_CHAR
BCK_CHAR:
		mov		mpr, charCounter	; check if at 'A'
		cpi		mpr, 0				;
		breq	RESET_CHAR2			; if so, wrap to 'Z'
		dec		mpr					; else, decrement char
		mov		charCounter, mpr	;
		rjmp	PRINT_CHARS
RESET_CHAR2:
		ldi		mpr, 26				; if 'A'
		dec		mpr					; decrement to Z
		mov		charCounter, mpr
		rjmp	PRINT_CHARS
NEXT_3:
		cpi		mpr, 0b11111110		; check for PD0 press
		brne	NEXT_4				; if not , go to last check
		rjmp	CONFIRM_CHAR
CONFIRM_CHAR:
		inc		numChars			; increment number of chars entered
		ldi		mpr, 17				; check if max chars
		cp		numChars, mpr		;
		breq	Transmission		; if so, transmitt message
		mov		mpr, charCounter	;
		add		mpr, Achar			; else place into data memory
		st		Y+,	mpr				; replace curr char with incremented char
		ldi		mpr, 0				;
		mov		charCounter, mpr	; reset back to 'A' for next char selection 
		rjmp	PRINT_CHARS
NEXT_4:
		cpi		mpr, 0b11101111		; if PD4 pressed
		brne	PRINT_CHARS			; if nothing, loop again
		ldi		mpr, 0b00010000		; light transmission indicator LED
		out		PORTB, mpr
Transmission:
		rcall	matchLetter			; begin identifying letters in message
		rjmp	selectMessage
		; Restore any saved variables by popping from stack
	

;-----------------------------------------------------------
; matchLetter:	
; Desc:	loops through message entered by user. loops through
;		charTable until match is found. If matched, will use 
;		associated 1's and 3's to transmitt
;-----------------------------------------------------------
matchLetter:	

		; If needed, save variables by pushing to the stack
		push mpr
		; Execute the function here
		ldi		YL, $10		; point Y to beginning of user message
		ldi		YH, $01		;

OuterLetterLoop:
		ld		mpr, Y+					; get the first letter from the LCD
		mov		currChar, mpr			; 
		dec		numChars				; 1 less char to transmitt
		ldi		ZL, LOW(charTable<<1)	; point Z to beginning of look up table
		ldi		ZH, HIGH(charTable<<1)	;
InnerletterLoop:
		lpm		mpr, Z+			; start at 'A' in lookup table
		cp		currChar, mpr	; compare values
		brne	InnerletterLoop	; if not equal, look for next letter
		rcall	transmittLetter	; if equal, transmitt letter
		rcall	Wait_3s			; wait 3 seconds between letters
		ldi		mpr, 0			;
		cp		numChars, mpr	; check if no more letters to transmitt
		brne	OuterLetterLoop ; if more, transmitt next char
		pop		mpr
		ret		; 
		; Restore any saved variables by popping from stack


;-----------------------------------------------------------
; PrintMsg3:	
; Desc:	Prints "Enter word: "
;	
;-----------------------------------------------------------
PrintMsg3:	

		; If needed, save variables by pushing to the stack
		push	mpr

		ldi		ZL, LOW(STRING3_BEG<<1)		; low byte of message 3
		ldi		ZH, HIGH(STRING3_BEG<<1)	; high byte
		ldi		YL, $00						; point Y to top line of LCD
		ldi		YH, $01						;
		DO_3:
			;Move string 1 from Program Mem. to Data Mem.
			LPM mpr, Z+ ; load data to register
			ST Y+, mpr; store data of register to data memory
			cpi ZL, LOW(STRING3_END<<1); compare lower byte of Z to $0f
			brne DO_3 ; branch to do if not equal
		rcall	LCDWrLn1
		
		; Restore any saved variables by popping from stack
		pop		mpr

		ret						; End a function with RET

;----------------------------------------------------------------
; Sub:	Wait_130s
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;		used register variable names that were defined in LCDDriver.asm
;----------------------------------------------------------------
Wait_130:
		push	wait			; Save wait register
		push	count			; Save ilcnt register
		push	line			; Save olcnt register

		ldi		wait, 13		; wait 10*13 ms
Loop:	ldi		line, 224		; load olcnt register
OLoop:	ldi		count, 237		; load ilcnt register
ILoop:	dec		count			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		line		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		wait		; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		line		; Restore olcnt register
		pop		count		; Restore ilcnt register
		pop		wait		; Restore wait register
		ret				; Return from subroutine

;-----------------------------------------------------------
; transmittLetter:	
; Desc:	loops through letters dots and dashes in order and
;		transmitts them
;-----------------------------------------------------------
transmittLetter:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr
		push	currChar
		inc		currChar	; next letter in alphabet
		TLoop:
		; Execute the function here
			lpm		mpr, Z+		;get dot/dash value from table
			cpi		mpr, 0		;0 is just placeholder
			breq	TLoop		;do nothing if 0, loop
			rcall	Wait_1s		;wait 1s between parts of same letter
			cpi		mpr, 1			;if 1, its a dot
			breq	transmittDot
			cpi		mpr, 3			;if 3, its a dash
			breq	transmittDash	
			cp		mpr, currChar	; if not at next letter
			brne	TLoop			; keep looping
		; Restore any saved variables by popping from stack
		pop		currChar
		pop		mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; transmittDot:	
; Desc:	lights LEDs to display dot
;		
;-----------------------------------------------------------
transmittDot:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr
		; Execute the function here
		ldi		mpr, 0b11110000		; light up LEDs 7-4
		out		PORTB, mpr			;
		rcall	Wait_1s				; wait a second
		ldi		mpr, 0b00010000		; turn of lights
		out		PORTB, mpr			;
		; Restore any saved variables by popping from stack
		pop		mpr
		rjmp	TLOOP						; End a function with RET

;-----------------------------------------------------------
; transmittDash:	Template function header
; Desc:	lights up LEDs to show dash
;		
;-----------------------------------------------------------
transmittDash:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr
		; Execute the function here
		ldi		mpr, 0b11110000		; light up LEDs
		out		PORTB, mpr			;
		rcall	Wait_3s				; light for 3s
		ldi		mpr, 0b00010000		; turn off
		out		PORTB, mpr			;
		; Restore any saved variables by popping from stack
		pop		mpr
		rjmp	TLOOP				

;-----------------------------------------------------------
; Wait_1s:	
; Desc:	Uses Timer/Counter1 to wait 1s
;	
;-----------------------------------------------------------
Wait_1s:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr
		; Execute the function here
		ldi		mpr, $c2		; value of $c2f7 is needed for 1s delay in normal mode
		out		TCNT1H, mpr		; with prescaler of 1024
		ldi		mpr, $f7		; write to TCNT1
		out		TCNT1L, mpr		;

		ONEs_Loop:
				in		mpr, TIFR			;read in TOV1
				andi	mpr, 0b00000100		;check if set
				breq	ONEs_Loop			; loop if not set
				ldi		mpr, 0b00000100		;reset TOV1
				out		TIFR, mpr
		; Restore any saved variables by popping from stack
		pop		mpr
		ret						; End a function with RET


;-----------------------------------------------------------
; Wait_3s:	
; Desc:	uses Timer/Counter1 to wait 3s
;	
;-----------------------------------------------------------
Wait_3s:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr
		; Execute the function here
		ldi		mpr, $48		; value of $48e5 is needed
		out		TCNT1H, mpr		; for 3s delay in normal mode
		ldi		mpr, $e5		;
		out		TCNT1L, mpr		; write to TCNT1

		ThreeS_Loop:
				in		mpr, TIFR			; read in TOV1
				andi	mpr, 0b00000100		; check if set
				breq	ThreeS_Loop			; if not, loop
				ldi		mpr, 0b00000100		; reset TOV1
				out		TIFR, mpr			;
		; Restore any saved variables by popping from stack
		pop		mpr
		ret						; End a function with RET


;-----------------------------------------------------------
; Func:	Template function header
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
FUNC:	; Begin a function with a label

		; If needed, save variables by pushing to the stack

		; Execute the function here
		
		; Restore any saved variables by popping from stack

		ret						; End a function with RET
;***********************************************************
;*	Stored Program Data
;***********************************************************
;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING1_BEG:
.DB				"Welcome!"		; Declaring data in ProgMem
STRING1_END:

STRING2_BEG:
.DB				"Please press PD0"		; Declaring data in ProgMem
STRING2_END:

STRING3_BEG:	
.DB				"Enter word: "
STRING3_END:

charTable:
.DB		65, 1, 3, 0				; 'A', 1 = dot, 3 = dash
.DB		66, 3, 1, 1, 1, 0		; 'B'
.DB		67, 3, 1, 3, 1, 0		; 'C'
.DB		68, 3, 1, 1				; 'D'
.DB		69, 1					; 'E'
.DB		70, 1, 1, 3, 1, 0		; 'F'
.DB		71,	3, 3, 1				; 'G'
.DB		72, 1, 1, 1, 1, 0		; 'H'
.DB		73, 1, 1, 0				; 'I'
.DB		74, 1, 3, 3, 3, 0		; 'J'
.DB		75, 3, 1, 3				; 'K'
.DB		76, 1, 3, 1, 1,	0		; 'L'
.DB		77, 3, 3, 0				; 'M'
.DB		78, 3, 1, 0				; 'N'
.DB		79, 3, 3, 3				; 'O'
.DB		80, 1, 3, 3, 1, 0		; 'P'
.DB		81, 3, 3, 1, 3, 0		; 'Q'
.DB		82, 1, 3, 1				; 'R'
.DB		83, 1, 1, 1				; 'S'
.DB		84, 3					; 'T'
.DB		85, 1, 1, 3				; 'U'
.DB		86, 1, 1, 1, 3, 0		; 'V'
.DB		87, 1, 3, 3				; 'W'
.DB		88, 3, 1, 1, 3, 0		; 'X'
.DB		89, 3, 1, 3, 3, 0		; 'Y'
.DB		90, 3, 3, 1, 1, 0		; 'Z'
.DB		91, 0					; signal end of alphabet
;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

