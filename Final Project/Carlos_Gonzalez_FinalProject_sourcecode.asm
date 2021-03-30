;***********************************************************
;*	This is the final project template for ECE375 Winter 2021
;***********************************************************
;*	 Author: Carlos Gonzalez
;*   Date: 3/12/21
;***********************************************************
.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;*	(feel free to edit these or add others)
;***********************************************************
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	A = r2					; A variable
.def	B = r3					; Another variable
.def	rem1 = r4				;
.def	rem2 = r5				;
.def	rem3 = r6				;
.def	rem4 = r7				;
.def	zero = r8				; Zero register, set to zero in INIT, useful for calculations
.def	C =	r10					; variable registers C-F
.def	D = r11					;
.def	E = r12					;
.def	F = r13					;
.def	GMByte1	= r16			; ls byte of GM
.def	GMByte2	= r17			; second byte
.def	GMByte3	= r18			; third byte
.def	GMByte4	= r19			; ms byte
.def	RadiusL = r20			; low byte of radius
.def	RadiusH = r21			; high byte of radius
.def	mpr = r23				; Multipurpose register 
.def	Counter = r22	

;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
.dseg
.org	$0100						; data memory allocation for operands
operand1:		.byte 10			; allocate 10 bytes for a variable named op1


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment
;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt
.org	$0046					; End of Interrupt Vectors
;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:	; The initialization routine
		;initialize stack pointer
		ldi		mpr, low(RAMEND)
		out		SPL, mpr
		ldi		mpr, high(RAMEND)
		out		SPH, mpr

		clr		zero						; will hold value of zero
		rcall	getGM						; get the selected planet's GM
		ldi		ZL, low(OrbitalRadius<<1)	; get orbital radius
		ldi		ZH, high(OrbitalRadius<<1)	;
		lpm		mpr, Z+						; 
		mov		RadiusL, mpr				; place in radius registers
		lpm		mpr, Z+						;
		mov		RadiusH, mpr				;
; To do
; your source code goes here
MAIN:	
		; first check if radius less than 1001
		ldi		r22, $e9
		ldi		r23, $03
		cp		RadiusL, r22
		cpc		RadiusH, r23
		brlo	radiusError			; if lower than 1001, set error value
		rjmp	gmCheck				; else go to GM check
radiusError:
		ldi		YL, low(Velocity)
		ldi		YH, high(Velocity)
		ldi		r22, $ff			; set error value
		ldi		r23, $ff
		st		Y+, r16
		st		Y+, r17
		rjmp	LAST				; skip calculations
gmCheck:
		;check if GM <= 1000
		cp		GMByte1, r22		;
		cpc		GMByte2, r23		;
		cpc		GMByte3, zero		;
		cpc		GMByte4, zero		;
		brlo	gmError				; if GM <= 1000, set error value
		rjmp	probA				; else begin problem A calculation
gmError:
		ldi		YL, low(Period)		;
		ldi		YH, high(Period)	;
		ldi		r21, $fe			;
		ldi		r22, $ff			;
		ldi		r23, $ff			;
		st		Y+, r21
		st		Y+, r22
		st		Y+, r23
		rjmp	LAST				; skip calculations if error
; PROBLEM A
probA:
		clr		mpr
		clr		Counter
		rcall	Div3216			; divide GM by Radius
		rcall	setQuotient		; place result in data memory
		; load input for square root into registers for calculation
		; using GM's registers for this part
		mov		GMByte1, rlo
		mov		GMByte2, rhi
		mov		GMByte3, A
		mov		GMByte4, B	
		rcall	sqrt32Bit		; get sqrt of quotient
		;check if Velocity result is 0
		clr		zero
		ldi		mpr, $00
		cp		rlo, mpr
		cpc		rhi, zero	
		breq	velocityError	; if velocity == 0, set error value
		rcall	setVelocity		; else, place result in data memory
		rjmp	probB			; move on to problem B calculations
velocityError:
		ldi		YL, low(Velocity)
		ldi		YH, high(Velocity)
		ldi		mpr, $fe			;low byte of -2
		ldi		Counter, $ff		;high byte of -2
		st		Y+, mpr				;
		st		Y, Counter
		rjmp	LAST				;skip calculations
probB:
		;beginning of PROBLEM B ( Period of Revolution )
		; retrieve radius
		ldi		ZL, low(OrbitalRadius<<1)
		ldi		ZH, high(OrbitalRadius<<1)
		; use GM registers for this part
		lpm		GMByte1, Z	; since finding r * r
		lpm		GMByte3, Z+ ; load same value into
		lpm		GMByte2, Z  ; two pairs of registers
		lpm		GMByte4, Z+ ;
		rcall	sqrRadius	; calculate r * r
		rcall	cubeRadius	; now calculate r^2 * r
		ldi		r25, 40		; load 4*pi^2 where pi^2 = 10
		rcall	calcProduct	; calculate r^3 * 4*10
		rcall	setProduct	; place result in memory

		rcall	getGMpart2	; get GM again
		rcall	Div5632		; divide product by GM
		movw	GMByte2:GMByte1, r1:r0	; move result into GM registers
		movw	GMByte4:GMByte3, r3:r2	; for calculating sqrt
		rcall	sqrt32bit		; get the period
checkPeriod:
		clr		zero
		ldi		mpr, 25			;check if period is < 25
		cp		r0, mpr
		cpc		r1, zero	
		cpc		r2, zero
		brlo	periodError		;if < 25, set error value
		rcall	setPeriod		; else, place value into memory
		rjmp	LAST			;
periodError:
		ldi		YL, low(Period)
		ldi		YH, high(Period)
		ldi		r22, $fe
		ldi		r23, $ff
		ldi		r24, $ff
		st		Y+, r22
		st		Y+, r23
		st		Y+, r24
LAST:
		jmp	Grading				; this should be the very last instruction of your code

;-----------------------------------------------------------
;	Procedures and Subroutines
;-----------------------------------------------------------
; your code can go here as well
Div3216:
		clr		zero
		clr		rlo
		clr		rhi
		movw	rhi:rlo, GMByte2:GMByte1 ; make a copy of the dividend
		movw	B:A, GMByte4:GMByte3	 ; place into registers holding result value
		ldi		Counter, 33				 ; bit counter
		sub		rem1, rem1				 ; clear remainer registers w/ carry
		clr		rem2
		clr		rem3
		clr		rem4
DivLoop:
		rol		rlo						 ; rotate left result value
		rol		rhi						
		rol		A
		rol		B
		dec		Counter					 ; dec bit counter
		breq	Done					 ; done if no more bits
		rol		rem1					 ; rotate left remainer
		rol		rem2					
		rol		rem3
		rol		rem4
		sub		rem1, RadiusL			 ;subtract divisor from remainer
		sbc		rem2, RadiusH
		sbc		rem3, zero
		sbc		rem4, zero
		brcc	Skip					 ; if negative, reverse subtraction
		add		rem1, RadiusL			
		adc		rem2, RadiusH
		adc		rem3, zero
		adc		rem4, zero
		clc								; clear carry flag
		rjmp	DivLoop
Skip:	
		sec								; set carry flag
		rjmp	DivLoop	
Done:
		ret

setQuotient:

		ldi		YL, low(Quotient)		;point Y to mem location of Quotient
		ldi		YH, high(Quotient)		;

		st		Y+, rlo					; store lowest byte
		st		Y+, rhi					; store byte 2
		st		Y, A					; store byte 3
		ret

sqrt32bit:
		push	GMByte1					;save square for restores
		push	GMByte2
		push	GMByte3
		push	GMByte4
		
		clr		rem1					;clear remainer
		clr		rem2
		clr		rem3
		clr		rem4
		clr		rlo						; clear registers holding result value
		clr		rhi	
		clr		A
		clr		B
		ldi		Counter, 16				; loop 16  times
SqrtLoop:
		lsl		rlo						; root *2
		rol		rhi
		rol		A
		rol		B
		lsl		GMByte1					;shift two high bits into remainer
		rol		GMByte2
		rol		GMByte3
		rol		GMByte4
		rol		rem1					; 
		rol		rem2
		rol		rem3
		rol		rem4
		lsl		GMByte1					; shift second high bit
		rol		GMByte2
		rol		GMByte3
		rol		GMByte4
		rol		rem1
		rol		rem2
		rol		rem3
		rol		rem4
		cp		rlo, rem1				; compare root and remainder
		cpc		rhi, rem2
		cpc		A, rem3
		cpc		B, rem4
		brcc	SqrtSkip				;if remainder < root
		inc		rlo						; root++
		sub		rem1, rlo				; root - remainder
		sbc		rem2, rhi
		sbc		rem3, A
		sbc		rem4, B
		inc		rlo						;root++
SqrtSkip:
		dec		Counter
		brne	SqrtLoop				;check if bits processed
		lsr		B						;root/2
		ror		A
		ror		rhi
		ror		rlo
		pop		GMByte4					;restore original square
		pop		GMByte3
		pop		GMByte2
		pop		GMByte1
		ret

setVelocity:
		
		ldi		YL, low(Velocity)
		ldi		YH, high(Velocity)

		st		Y+, rlo
		st		Y, rhi
		ret

sqrRadius:
		clr		A	; use A as zero register
		mul		GMByte2, GMByte4	; mult high bytes
		movw	mpr:Counter, r1:r0	; use r23:r22 to hold answer, move product to answer
		mul		GMByte1, GMByte3	; mult low bytes
		movw	RadiusH:RadiusL, r1:r0		; place result in r21:r20 to hold answer
		mul		GMByte2, GMByte3	; mul high byte x low byte
		add		RadiusH, r0			; add to answer registers (r23:r20)
		adc		Counter, r1			;
		adc		mpr, A				;
		mul		GMByte4, GMByte1	; mul high byte x low byte
		add		RadiusH, r0			; add result to answer registers
		adc		Counter, r1			;
		adc		mpr, A				;
		movw	D:C, r21:r20	; move answer to F:C registers for cubing function
		movw	F:E, r23:r22
		ret

cubeRadius:

		clr		A
		clr		r18
		clr		r19
		clr		r20
		clr		r21
		clr		r22
		sub		r23,r23
		mul		GMByte1, C		;mult low bytes
		movw	r19:r18, r1:r0	; move to answer regs
		mul		D, GMByte2
		movw	r21:r20, r1:r0	;byte2 x byte2
		mul		F, GMByte2		; mul highest bytes
		movw	r23:r22, r1:r0
		mul		D, GMByte1
		add		r19, r0
		adc		r20, r1
		adc		r21, r2
		mul		E, GMByte1
		add		r20, r0
		adc		r21, r1
		adc		r22, r2
		mul		F, GMbyte1
		add		r21, r0
		adc		r22, r1
		adc		r23, r2
		mul		E, GMByte2
		add		r21, r0
		adc		r22, r1
		adc		r23, r2
		mul		C, GMByte2
		add		r19, r0
		adc		r20, r1
		adc		r21, r2
		ret

calcProduct:
		
		clr		r2
		clr		r8
		clr		r9
		clr		r10
		clr		r11
		clr		r12
		clr		r13
		sub		r14, r14
		mul		r18, r25	; low byte x 40
		movw	r9:r8, r1:r0
		mul		r20, r25	; byte 3 x 40
		movw	r11:r10, r1:r0
		mul		r22, r25	; byte 5 x 40
		movw	r13:r12, r1:r0
		mul		r19, r25	; byte 2 x 40
		add		r9, r0		; add to result, with alignment
		adc		r10, r1
		adc		r11, r2
		mul		r21, r25	; byte 4 x 40
		add		r11, r0
		adc		r12, r1
		adc		r13, r2
		mul		r23, r25	; byte 6 x 40
		add		r13, r0
		adc		r14, r1
		adc		r15, r2
		ret
		
setProduct:
		
		ldi		YL, low(Product)
		ldi		YH, high(Product)

		st		Y+, r8
		st		Y+, r9
		st		Y+, r10
		st		Y+, r11
		st		Y+, r12
		st		Y+, r13
		st		Y+, r14

		ret


Div5632:

		;place product into dividend registers
		movw	r17:r16, r9:r8
		movw	r19:r18, r11:r10
		movw	r21:r20, r13:r12
		mov		r22, r14
		ldi		r27, 57			; bit counter
		clr		r14				; holds value zero
		;copy dividend into answer registers
		movw	r1:r0, r9:r8
		movw	r3:r2, r11:r10
		movw	r5:r4, r13:r12
		mov		r6, r14
		sub		r7, r7			; clear remainder registers w/ carry
		clr		r8
		clr		r9
		clr		r10
		clr		r11
		clr		r12
		clr		r13
DivLoop2:
		rol		r0				; rotate left answer registers
		rol		r1
		rol		r2
		rol		r3
		rol		r4
		rol		r5
		rol		r6
		dec		r27				; dec bit counter
		breq	Done2
		rol		r7				; rotate left remainder registers
		rol		r8
		rol		r9
		rol		r10
		rol		r11
		rol		r12
		rol		r13
		sub		r7, r23			;sub divisor from remainder
		sbc		r8, r24
		sbc		r9, r25
		sbc		r10, r26
		sbc		r11, r14		; sub zero w/carry 
		sbc		r12, r14		;
		sbc		r13, r14
		brcc	Skip2
		add		r7, r23
		adc		r8, r24
		adc		r9, r25
		adc		r10, r26
		adc		r11, r14
		adc		r12, r14
		adc		r13, r14
		clc						;clear carry flag
		rjmp	DivLoop2
Skip2:	
		sec						; set carry flag
		rjmp	DivLoop2
Done2:
		ret

setPeriod:
		
		ldi		YL, low(Period)
		ldi		YH, high(Period)

		st		Y+, r0
		st		Y+, r1
		st		Y+, r2
		ret

getGM:
		push	mpr
		ldi		ZL,	low(SelectedPlanet<<1)		; get desired planet's index
		ldi		ZH, high(SelectedPlanet<<1)
		lpm		mpr, Z							; load into mpr
		mov		A, mpr							; mov index to A
		ldi		mpr, 4							; to get planet's data, mul index by 4
		mul		A, mpr							; A * 4 gets us at least sig byte of planet's GM
		mov		mpr, rlo						; result is in R0, move it back to mpr
		ldi		ZL, low(PlanetInfo<<1)			; Point to planet array
		ldi		ZH, high(PlanetInfo<<1)
		add		ZL, mpr							; add offset
		lpm		mpr, Z+							; place byte into mpr
		mov		GMByte1, mpr					; start placing bytes into registers
		lpm		mpr, Z+
		mov		GMByte2, mpr
		lpm		mpr, Z+
		mov		GMByte3, mpr
		lpm		mpr, Z+
		mov		GMByte4, mpr
		pop		mpr
		ret

getGMpart2:

		ldi		ZL,	low(SelectedPlanet<<1)		; get desired planet's index
		ldi		ZH, high(SelectedPlanet<<1)
		lpm		mpr, Z							; load into mpr
		mov		A, mpr							; mov index to A
		ldi		mpr, 4							; to get planet's data, mul index by 4
		mul		A, mpr							; A * 4 gets us at least sig byte of planet's GM
		mov		mpr, rlo						; result is in R0, move it back to mpr
		ldi		ZL, low(PlanetInfo<<1)			; Point to planet array
		ldi		ZH, high(PlanetInfo<<1)
		add		ZL, mpr							; add offset

		lpm		r23, Z+					; place byte into registers r23:r20		
		lpm		r24, Z+
		lpm		r25, Z+
		lpm		r26, Z+

		ret
;***********************************************************
;*	Custom stored data
;*	(feel free to edit these or add others)
;***********************************************************
SomeConstant:	.DB	0x86, 0xA4



;***end of your code***end of your code***end of your code***end of your code***end of your code***
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************

Grading:
		nop					; Check the results in data memory begining at address $0E00 (The TA will set a breakpoint here)
rjmp Grading


;***********************************************************
;*	Stored program data that you cannot change
;***********************************************************

; Contents of program memory will be changed during testing
; The label names (OrbitalRadius, SelectedPlanet, PlanetInfo, MercuryGM, etc) are not changed
; NOTE: All values are provided using the little-endian convention.
OrbitalRadius:	.DB	0x64, 0x19				; the radius that should be used during computations (in kilometers)
											; in this example, the value is 6,500 kilometers
											; the radius will be provided as a 16 bit unsigned value (unless you are
											; completing the extra credit, in which case the radius is an unsigned 24 bit value)

SelectedPlanet:	.DB	0x01, 0x00				; This is how your program knows which GM value should be used.
											; SelectedPlanet is an unsigned 8 bit value that provides you with the
											; index of the planet (and hence, tells you which GM value to use).
											; Note: only the first byte is used. The second byte is just for padding.
											; In this example, the value is 2. If we check the planet at index 2, (from the data below)
											; that corresponds to Earth.
											; if the value was 7, that would correspond to the planet Neptune

PlanetInfo:									; Note that these values will be changed during testing!
MercuryGM:		.DB	0x0E, 0x56, 0x00, 0x00	; Gravitational parameters will be provided as unsigned 32 bit integers (little-endian)
VenusGM:		.DB	0x24, 0xF5, 0x04, 0x00	; the units are in: (km * km * km)/(sec * sec)
EarthGM:		.DB	0x08, 0x15, 0x06, 0x00	; <-- note that this is 398,600
MarsGM:			.DB	0x4E, 0xA7, 0x00, 0x00
JupiterGM:		.DB	0x30, 0x13, 0x8D, 0x07	; A word of advice... treat these like an array, where each element
SaturnGM:		.DB	0xF8, 0xC7, 0x42, 0x02	; occupies 4 bytes of memory.
UranusGM:		.DB	0xD0, 0x68, 0x58, 0x00	; Mercury is at index 0, Venus is at index 1, ...and the final planet is at index 8.
NeptuneGM:		.DB	0x38, 0x4B, 0x68, 0x00
FinalGM:		.DB	0xFF, 0xFF, 0xFF, 0xFF


;***********************************************************
;*	Data Memory Allocation for Results
;*	Your answers need to be stored into these locations (using little-endian representation)
;*	These exact variable names will be used when testing your code!
;***********************************************************
.dseg
.org	$0E00						; data memory allocation for results - Your grader only checks $0E00 - $0E14
Quotient:		.byte 3				; This is the intermediate value that is generated while you are computing the satellite's velocity.
									; It is a 24 bit unsigned value.
Velocity:		.byte 2				; This is where you will store the computed velocity. It is a 16 bit signed number.
									; The velocity value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).
Product:		.byte 7				; This is the intermediate product that is generated while you are computing the orbital period.
Period:			.byte 3				; This is where the orbital period of the satellite will be placed.
									; It is a 24 bit signed value.
									; The period value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program

