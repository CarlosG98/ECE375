;***********************************************************
;*
;*	Carlos_Gonzalez_Lab7_sourcecode.asm
;*
;*	TekBot with variable forward speed which can be set
;*	using buttons. 
;*
;*
;***********************************************************
;*
;*	 Author: Carlos Gonzalez
;*	   Date: 2/22/21
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	SpdLvl = r18			; Counter register for speed level
.equ	EngEnR = 4				; right Engine Enable Bit
.equ	EngEnL = 7				; left Engine Enable Bit
.equ	EngDirR = 5				; right Engine Direction Bit
.equ	EngDirL = 6				; left Engine Direction Bit

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

.org	$0002
		rcall	CheckSpdDec		; external interrupt request 0
		reti
		; place instructions in interrupt vectors here, if needed
.org	$0004
		rcall	CheckSpdInc		; external interrupt request 1
		reti

.org	$0006
		rcall	MinSpeed		; external interrupt request 2
		reti
	
.org	$0008
		rcall MaxSpeed			; external interrupt request 3
		reti

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
			;
		ldi		mpr, 0b10101010		; detect on falling edge
		sts		EICRA, mpr			;
		ldi		mpr, 0b00001111		; allow INT3:0 to be detected
		out		EIMSK, mpr
		; Configure 8-bit Timer/Counters
		ldi		mpr, 0b01111001	; no prescaling, fast PWM, inverting
		out		TCCR0, mpr		; Timer/Counter control register 0
		out		TCCR2, mpr		; Timer/Counter control register 2
		
		ldi		mpr, 255		; set compare value to 255
		out		OCR0, mpr		; write to OCR0
		out		OCR2, mpr		; and OCR2

		ldi		SpdLvl, 15		; set speed counter to 15, or full speed.
		; Set TekBot to Move Forward (1<<EngDirR|1<<EngDirL)
		ldi		mpr, (1<<EngDirR|1<<EngDirL)	
		out		PORTB, mpr
		; Set initial speed, display on Port B pins 3:0
		ori		mpr, $0F		;initital speed is level 15 (1111)
		out		PORTB, mpr		;

		ldi		mpr, -58
		ldi		r18, 15
		sub		mpr, r18
		nop

		; Enable global interrupts (if any are used)
		sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func:	CheckSpdDec
; Desc:	Checks if Speed Level is already at the minimum.
;		If able to decrement, calls DecSpeed, else returns 
;-----------------------------------------------------------
CheckSpdDec:
		; Execute the function here
		cpi		SpdLvl, 0
		brne	DecSpeed
		
		ret					; End a function with RET
;-----------------------------------------------------------
; Func:	DecSpeed
; Desc:	Decreases the speed of the TekBot by one level.
;-----------------------------------------------------------
DecSpeed:

		; save variables by pushing to the stack
		push	mpr


		dec		SpdLvl			; decrement speed level
		ldi		mpr, 17			; multiply speed level by 17
		mul		mpr, SpdLvl		; to get PWM value
		mov		mpr, r0			; product is placed in r0, move to mpr
		out		OCR0, mpr		; update OCR0
		out		OCR2, mpr		; and OCR2

		ldi		mpr, (1<<EngDirR|1<<EngDirL)	;set upper nibble of PORTB
		or		mpr, SpdLvl						;set lower nibble of PORTB
		out		PORTB, mpr						; 

		ldi		mpr, 0b00001111	; avoid queued interrupts
		out		EIFR, mpr		;

		; Restore any saved variables by popping from stack
		pop		mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	CheckSpdInc
; Desc:	Checks if speed level is already at max speed.
;		Calls IncSpeed if not at max level.
;-----------------------------------------------------------
CheckSpdInc:
		; Execute the function here
		cpi		SpdLvl, 15
		brne	IncSpeed
		ret						; End a function with RET
;-----------------------------------------------------------
; Func:	IncSpeed
; Desc:	Increases the speed of the TekBot by one level.
;-----------------------------------------------------------
IncSpeed:
		; save variables by pushing to the stack
		push	mpr

		inc		SpdLvl			; increment speed level
		ldi		mpr, 17			; multiply speed level by 17
		mul		mpr, SpdLvl		; to get PWM value
		mov		mpr, r0			; product is placed in r0, move to mpr
		out		OCR0, mpr		; update OCR0
		out		OCR2, mpr		; and OCR2

		ldi		mpr, (1<<EngDirR|1<<EngDirL)	; set uppder nibble of PORTB
		or		mpr, SpdLvl						; set lower nibble
		out		PORTB, mpr						;

		ldi		mpr, 0b00001111	; avoid queued interrupts
		out		EIFR, mpr		;

				
		; Restore any saved variables by popping from stack
		pop		mpr
		ret						; End a function with RET


;-----------------------------------------------------------
; Func:	MaxSpeed
; Desc:	Set the TekBot to its maximum speed, regardless of
;		current speed.
;-----------------------------------------------------------
MaxSpeed:

		; save variables by pushing to the stack
		push	mpr
		
		ldi		SpdLvl, 15		; set speed level to 15, its max.
		ldi		mpr, 255		; max PWM is 255
		out		OCR0, mpr		; update OCR0
		out		OCR2, mpr		; update OCR2

		ldi		mpr, (1<<EngDirR|1<<EngDirL)	; set upper nibble of PORTB
		or		mpr, SpdLvl						; set lower nibble
		out		PORTB, mpr						;

		ldi		mpr, 0b00001111	; avoid queued interrupts
		out		EIFR, mpr		;

		; Restore any saved variables by popping from stack
		pop		mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	MinSpeed
; Desc:	Sets the TekBot's speed to its minimum regardless of
;		its current speed level.
;-----------------------------------------------------------
MinSpeed:	
		; save variables by pushing to the stack
		push	mpr
		
		ldi		SpdLvl, 0		; speed level to 0, its minimum
		ldi		mpr, 0			; no need to multiply since 0*17 = 0 PWM
		out		OCR0, mpr		; update OCR0
		out		OCR2, mpr		; update OCR2

		ldi		mpr, (1<<EngDirR|1<<EngDirL)	; set upper nibble of PORTB
		or		mpr, SpdLvl						; set lower nibble
		out		PORTB, mpr						;

		ldi		mpr, 0b00001111	; avoid queued interrupts
		out		EIFR, mpr		;

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
		; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program