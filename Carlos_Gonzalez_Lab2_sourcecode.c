/*
 * BumpBot.c
 *
 * Created: 1/14/2021 2:38:33 PM
 * Author : Carlos Gonzalez
 */ 

/*
This code will cause a TekBot connected to the AVR board to
move forward and when it touches an obstacle, it will reverse
and turn away from the obstacle and resume forward motion.

PORT MAP
Port B, Pin 4 -> Output -> Right Motor Enable
Port B, Pin 5 -> Output -> Right Motor Direction
Port B, Pin 7 -> Output -> Left Motor Enable
Port B, Pin 6 -> Output -> Left Motor Direction
Port D, Pin 1 -> Input -> Left Whisker
Port D, Pin 0 -> Input -> Right Whisker
*/

#define F_CPU 16000000
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

int main(void)
{
	
	DDRB = 0b11110000; //set bit 7-4 as output
	PORTB = 0b11110000; // bit 7 & 4 disable motors, bit 5 & 6 turn leds on (moving forward)
	DDRD = 0b00000000; // set bits 7-0 as inputs
	PORTD = 0b11111111; // enable pull up resistors
	
	while (1) // loop forever
	{
		PORTB = 0b01100000; //move forward
		
		if((PIND == 0b11111110) || (PIND == 0b11111100)){ //if right whisker or both is hit
			PORTB = 0b00000000; //set bot to reverse
			_delay_ms(1000); //reverse for 1 second
			PORTB = 0b00100000; // move bot left
			_delay_ms(1000); //move left for 1 second
		} 
		else if(PIND == 0b11111101){ //if left whisker is hit
			PORTB = 0b00000000; //set bot to reverse
			_delay_ms(1000); //reverse for 1 second
			PORTB = 0b01000000; // move bot right
			_delay_ms(1000); //move right for 1 second
		}
	}
}

