;***********************************************************
;*
;*	Carlos_Gonzalez_Lab6_sourcecode.asm
;*
;*	BumpBot Interrupt Version
;*
;*	This is the skeleton file for Lab 6 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Carlos Gonzalez
;*	   Date: 2/14/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;************************************************************
;* Variable and Constant Declarations
;************************************************************
.def	mpr = r16				; Multi-Purpose Register
.def	waitcnt = r23				; Wait Loop Counter
.def	ilcnt = r24				; Inner Loop Counter
.def	olcnt = r25				; Outer Loop Counter
.def	A = r3			; Top line counter
.def	B = r4			; Bottome Line Counter

.equ	WTime = 100				; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit
.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00				; Move Backward Command
.equ	TurnR = (1<<EngDirL)			; Turn Right Command
.equ	TurnL = (1<<EngDirR)			; Turn Left Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used
.org	$0002					; External Interrupt Request 0
		rcall HitRight			; call hit right subroutine
		reti

.org	$0004					; External Interrupt Request 1
		rcall HitLeft			; call hit left subroutine
		reti

.org	$0006					; External Interrupt Request 2
		rcall ClrTopLn			; reset top counter
		reti

.org	$0008					; External Interrupt Request 3
		rcall ClrBtmLn			; reset bottom counter
		reti
		; This is just an example:
;.org	$002E					; Analog Comparator IV
;		rcall	HandleAC		; Call function to handle interrupt
;		reti					; Return from interrupt

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		ldi		mpr, LOW(RAMEND)
		out		SPL, mpr
		ldi		mpr, HIGH(RAMEND)
		out		SPH, mpr
		; Initialize Port B for output
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low		
		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State
		
		;Initialize LCD Display
		rcall	LCDInit			; Initialize LCD Display
		ldi		mpr, 0			;
		mov		A, mpr			; variable A counts hit right whisker
		ldi		XL, $00			; point X to first char on top line of LCD 
		ldi		XH, $01			;
		rcall	Bin2ASCII		; convert binary to ascii char
		rcall	LCDWrLn1		; write 0 to line 1

		ldi		mpr, 0			; 
		mov		B, mpr			; variable B counts hit left whisker	
		ldi		XL, $10			; point X to first char on bottome line of LCD
		ldi		XH, $01			;
		rcall	Bin2ASCII		; convert binary to ascii char
		rcall	LCDWrLn2		; write 0 to line 2
		
		

		; Initialize external interrupts
		; Set the Interrupt Sense Control to falling edge 
		ldi		mpr, 0b10101010	; INT3:0 to trigger on falling edge
		sts		EICRA, mpr		; store in External Interrupt Control Register A
		; Configure the External Interrupt Mask
		ldi		mpr, 0b00001111	; allow detection of INT3:0
		out		EIMSK, mpr		;
		; Turn on interrupts
		sei
			; NOTE: This must be the last thing to do in the INIT function

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		; TODO: ???
		ldi		mpr, MovFwd	; Load FWD command
		out		PORTB, mpr		; sent to motors
		rjmp	MAIN			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
;	You will probably want several functions, one to handle the 
;	left whisker interrupt, one to handle the right whisker 
;	interrupt, and maybe a wait function
;------------------------------------------------------------

;----------------------------------------------------------------
; Sub:	HitRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
HitRight:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		rcall	IncTop		; Increment top counter
		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	WaitL			; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	WaitL			; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		ldi		mpr, 0b00001111	; avoid queued interrupts
		out		EIFR, mpr		;

		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	HitLeft
; Desc:	Handles functionality of the TekBot when the left whisker
;		is triggered.
;----------------------------------------------------------------
HitLeft:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		rcall	IncBtm		; Increment Bottom Counter
		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	WaitL			; Call wait function

		; Turn right for a second
		ldi		mpr, TurnR	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	WaitL			; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		ldi		mpr, 0b00001111	; avoid queued interrupts
		out		EIFR, mpr		;

		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	WaitL
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
WaitL:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt		; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret				; Return from subroutine

;-----------------------------------------------------------
; Func: IncTop
; Desc: Increments right whisker counter and displays it on 
;		top line of LCD
;-----------------------------------------------------------
IncTop:							; Begin a function with a label

		push	mpr; Save variable by pushing them to the stack
		
		ldi		XL, $00		; point X to first char in top line
		ldi		XH, $01		;
		inc		A			; increment counter
		mov		mpr, A		; move value to mpr
		rcall	Bin2ASCII	; convert value to ascii
		rcall	LCDWrLn1	; write value to line 1 on LCD

		; Restore variable by popping them from the stack in reverse order
		pop		mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: IncBtm
; Desc: Increments left whisker counter and displays value on
;		bottom line of LCD
;-----------------------------------------------------------
IncBtm:							; Begin a function with a label

		push	mpr; Save variable by pushing them to the stack
		
		ldi		XL, $10		; point X to first char of bottom line of LCD
		ldi		XH, $01		;
		inc		B			; increment left whisker counter
		mov		mpr, B		; move value to mpr
		rcall	Bin2ASCII	; convert value to ascii
		rcall	LCDWrLn2	; write value to bottom line of LCD

		; Restore variable by popping them from the stack in reverse order
		pop		mpr
		ret						; End a function with RET


;-----------------------------------------------------------
; Func: ClrTopLn
; Desc: Resets right whisker counter to zero and displays 
;		value to top line of LCD
;-----------------------------------------------------------
ClrTopLn:							; Begin a function with a label

		push	mpr			; Save variable by pushing them to the stack

		rcall	LCDClrLn1
		; Execute the function here
		ldi		XL, $00		; point X to top line
		ldi		XH, $01		;
		clr		A			; reset counter to 0
		mov		mpr, A		; move to mpr
		rcall	Bin2ASCII	; convert to ascii
		rcall	LCDWrLn1	; write to top line of LCD

		ldi		mpr, 0b00001111	;avoid queued interrupt
		out		EIFR, mpr		;
		; Restore variable by popping them from the stack in reverse order
		pop		mpr
		ret						; End a function with RET
;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
ClrBtmLn:							; Begin a function with a label

		push	mpr			; Save variable by pushing them to the stack

		rcall	LCDClrLn2
		; Execute the function here
		ldi		XL, $10		; point X to bottom line of LCD
		ldi		XH, $01		;
		clr		B			; clear left whisker counter to 0
		mov		mpr, B		; move to mpr
		rcall	Bin2ASCII	; convert value to ascii
		rcall	LCDWrLn2	; write to bottom line of LCD

		ldi		mpr, 0b00001111	; avoid queued interrupt
		out		EIFR, mpr		;
		; Restore variable by popping them from the stack in reverse order
		pop		mpr
		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************



;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"
