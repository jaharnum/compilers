/*
File name: buffer.c
Compiler: MS Visual Studio 2017
Author: Jamie Harnum, #040898399
Course: CST8152 - Compilers, Lab Section: 13
Assignment: 1
Date: [date of the final version]
Professor: Sv. Ranev
Purpose: Create a character buffer that demonstrates proper use of memory allocation and defensive coding practices.
Function list:
				b_allocate()
				b_addc()
				b_capacity()
				b_clear()
				b_compact()
				b_eob()
				b_free()
				b_getc()
				b_getcoffset()
				b_isempty()
				b_isfull()
				b_limit()
				b_location()
				b_mark()
				b_mode()
				b_print()
				b_reset()
				b_retract()
				b_rewind()
				b_rflag()
*/
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

#include "buffer.h"

/*
Purpose: Create and allocate space to a new BufferDescriptor struct.
Author: Jamie Harnum
History / Versions: Jan 29: 0.1
Called functions: None
Parameters:
			short init_capacity: Starting capacity for the buffer. Must be between 0 and SHORT_MAX-1 inclusive. If set to 0, default to size 200
			char inc_factor: Buffer increment factor. Used to calculate a new buffer capacity in additive and multiplicative modes.
					In additive mode: Increment factor must be between 1 and 255 inclusive and represents the direct amount to be added.
					In multiplicative mode: Increment factor must be between 1 and 100 inclusive and represents a percentage to increase the capacity by.
			char o_mode: Operational mode indicator.
					There are only three accepted values: 'f' for fixed-size, 'a' for additive, and 'm' for multiplicative.
					These values set the buffer mode to 0, 1, and -1 respectively.
Return value: Returns a pointer to the Buffer that has been created.
Algorithm:
			- Check that o_mode is an accepted value
			- Check that init_capacity is an accepted value
			- Check that inc_factor is an accepted value based on the o_mode
			- Create a new Buffer struct using the provided values
			- Return a pointer to the new Buffer
*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);

/*
Purpose: Adds a character to the buffer array and expands the array if appropriate according to the current size and mode of the buffer.
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: 
					b_isfull()

Parameters: 
			pBuffer const pBD: Pointer to the BufferDescriptor of the Buffer to add a char to
			char symbol: Character to be added to the buffer
Return value: A pointer to the Buffer structure or NULL if any error occurs
Algorithm: 
			- resets the flag r_flag bit to 0
			- tries to add symbol to the character array of the given buffer
			- if the buffer is operational and not full, the symbol will be stored in the buffer and addc_offset by 1 and returns
			- if the buffer is already full, check the op mode
					- if 0, return NULL and do not change anything in the buffer
					- if 1, try to increase the current capacity of the butter by adding inc_factor to capacity.
							-check if this causes it to exceed MAX_VALUE-1
								-if it is a positive number but exceeds, assign MAX_VALUE-1 to capacity and continue
								-if it is negative, return NULL
					- if -1, check if its at max capacity (if so, return NULL)
						- if not, try to increase capacity in the following manner:
								available space = maximum buffer capacity - current capacity
								new increment = available space * inc_factor/100
								new capacity = current capacity + new increment
						- if this exceeds max value but current is less than max, assign max_value-1
			- if 1 or -1 and has not failed, expand using realloc()
				-if realloc() fails, return NULL
				- if the location in memory of the buffer has been changed due to reallocation, set r_flag to 1 using bitwise op
				- add symbol to buffer, change the value of addc_offset by 1, save new capacity in capacity variable
				- return pointer to the Buffer structure
*/
pBuffer b_addc(pBuffer const pBD, char symbol);

/*
Purpose: Retain the memory space that has been allocated to the buffer, but re-initialize data members so the buffer will appear empty and the next added character will be at the beginning of the list
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: [function(s) called by this function]
Parameters: Buffer * const pBD - the pointer to the Buffer structure to be cleared
Return value: If a runtime error is possible, the function returns -1 to notify of failure.
Algorithm: 
			- set getc_offset to 0
			- set addc_offset to 0
			- reset EOB flag if set
			- set *cb_head to null (or 0?)
*/
int b_clear(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
}

/*
Purpose: Free all memory occupied by the character buffer and the Buffer structure itself
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: None
Parameters: Buffer * const pBD - pointer to the Buffer to be freed
Return value: Void
Algorithm:  - free buffer array - do i need to step thru all the chars or can i just get it from the head of the array * the offset?
			- free buffer struct
			- notify user of success
*/
void b_free(Buffer * const pBD);

/*
Purpose: Check if the Buffer is full
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: None
Parameters: Buffer * const pBD - a pointer to the Buffer being checked
Return value: Returns 1 if the character buffer is full and 0 otherwise. 
			If there is a possible runtime error, it will return -1
Algorithm: Compare addc_object*sizeof(char) to the current buffer capacity
*/
int b_isfull(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
}

/*
Purpose: Returns the current limit of the character buffer measured in chars.
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: None
Parameters: Buffer * const pBD - a pointer to the Buffer to get the limit of
Return value: A short representing the limit of the Buffer. The current limit is the amount of space measured in chars that is being used by all added characters.
			If there is a possible runtime error, it will return -1.
Algorithm: N/A
*/
short b_limit(Buffer * const pBD) {
		if (pBD == 0) {
			return RT_FAIL_1;
		}
		else {
			return pBD->getc_offset;
		}
}

/*
Purpose: Return the capacity of a given Buffer
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: None
Parameters: Buffer * const pBD - the Buffer to get the capacity of
Return value: Returns the short value representing the buffer capacity
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
short b_capacity(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
	else {
		return pBD->capacity;
	}
}

/*
Purpose: Changes the marker (markc_offset) of a given Buffer to a given value
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: None
Parameters:		pBuffer const pBD - a pointer to the BufferDescription we wish to change
				short mark - the new value for markc_offset
Return value: The short representing the current value of markc_offset for this Buffer
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
short b_mark(pBuffer const pBD, short mark) {
	if (pBD == 0 || !(0 < mark < pBD->addc_offset)) {
		return RT_FAIL_1;
	}
	else {
		pBD->markc_offset = mark;
		return pBD->markc_offset;
	}
}

/*
Purpose: [brief explanation of purpose of the function]
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: [function(s) called by this function]
Parameters: [for each formal parameter: type, specific range or values if applicable]
Return value: [type, specific values if applicable]
			If there is a possible runtime error, it will return -1.
Algorithm: [outline main steps / sections only; do not include implementation details; for small clear functions leave this empty]
*/
int b_mode(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
}

/*
Purpose: Returns the inc_factor of a given Buffer
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: None
Parameters: Buffer * const pBD - a pointer to the Buffer to get the inc_factor of
Return value: Returns the non-negative value of inc_factor
			If there is a possible runtime error, it will return 0x100.
Algorithm:
*/
size_t b_incfactor(Buffer * const pBD);

/*
Purpose: Add chars from a file to the buffer
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions:	b_addc()
					fgetc()
					feof()
					unget()
Parameters: FILE * const fi - the file to read from
			Buffer * const pBD - the Buffer to add the file's contents to
Return value: The number of characters that were added to the buffer.
			If loading from file fails, it will return -2.
			If there is a possible runtime error, it will return -1.
Algorithm: 
			- Get chars from file one at a time and try to add each one to the buffer
				- if b_addc() returns NULL, unget() the last character and return LOAD_FAIL
			- if all characters are added successfully, unget() the last character to remove the EOF marker and return the number of chars added (current - previous)
*/
int b_load(FILE * const fi, Buffer * const pBD) {
	if (pBD == 0 || fi == 0) {
		return RT_FAIL_1;
	}
}

/*
Purpose: Check if the buffer is empty
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: 
Parameters: Buffer * const pBD - the pointer to the Buffer to check
Return value: Returns 1 if the Buffer is empty and returns 0 if the Buffer is not empty.
			If there is a possible runtime error, it will return -1.
Algorithm: [outline main steps / sections only; do not include implementation details; for small clear functions leave this empty]
*/
int b_isempty(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
	else if (pBD->addc_offset == 0) {
		return 1;
	} 

	return 0;
}

/*
Purpose: Gets the character at getc_offset
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: [function(s) called by this function]
Parameters: Buffer * const pBD - a pointer to the Buffer to get the char from
Return value: Returns the char at getc_offset
			If there is a possible runtime error, it will return -2.
Algorithm: 
			- Checks if getc_offset and addc_offset are equal.
				- If they are, set EOB flag to 1
				- If they are not, reset EOB (set to 0)
			- Get the char at getc_offset 
			- Increment getc_offset by 1, 
			- Return the char
*/
char b_getc(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_2;
	}
}

/*
Purpose: Checks if the End of Buffer bit has been set 
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: None
Parameters: Buffer * const pBD - A pointer to the Buffer to be checked
Return value: Returns 1 if the End of Buffer bit is 1
			Returns 0 if the EOB bit is 0
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
int b_eob(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
	/*use if(x & CHECK_EOB) to check - TRUE if set, FALSE if not set*/
}

/*
Purpose: Prints the content of the Buffer character by character
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: b_isempty()
				  b_getc()
				  b_eob()
				  printf()
Parameters: Buffer * const pBD - a pointer to the Buffer to print from
Return value: The number of characters printed.
			If there is a possible runtime error, it will return -1.
Algorithm: 
			- check if the buffer is empty
			- go through the buffer using b_getc() and print each char using printf()
			- use b_eob() to detect the end of buffer content
*/
int b_print(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
}

/*
Purpose: Reallocates the buffer's memory to a more appropriate value given the current contents
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: [function(s) called by this function]
Parameters: Buffer * const pBD - a pointer to the Buffer being reallocated
			char symbol - the symbol to be added to the end of the character buffer to show that the buffer has been appropriately reallocated.
Return value: A pointer to the reallocated Buffer
			If there is a possible runtime error, it will return null.
Algorithm: 
			- Set new capacity to addc_offset + 1 * sizeof(char) (i.e. addc_offset+1 converted to bytes)
			- use realloc to set the new capacity
			- update all Buffer descriptor values needed
			- use addc_offset to add symbol to the end of the character buffer and increment it
			- check if newbuffer's pointer is = the old buffers pointer. if not, set the R flag. 
*/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	if (pBD == 0) {
		return NULL;
	}
}

/*
Purpose: Check if the rflag bit has been set.
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: [function(s) called by this function]
Parameters: [for each formal parameter: type, specific range or values if applicable]
Return value: The value of the flags field
			If there is a possible runtime error, it will return -1.
Algorithm: - use bitwise logic to check if the r_flag bit has been set
			- print the results of the check
			- return the current value of the flags field
*/
char b_rflag(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
}

/*
Purpose: Decrements getc_offset, i.e. retracts the buffer by 1 char.
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: [function(s) called by this function]
Parameters: Buffer * const pBD - a pointer to the Buffer to be retracted
Return value: The new getc_offset.
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
short b_retract(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
}

/*
Purpose: Sets the value of getc_offset to the current markc_offset
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: [function(s) called by this function]
Parameters: Buffer * const pBD - a pointer to the Buffer to be altered
Return value: The new value of getc_offset.
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
short b_reset(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
}

/*
Purpose: Returns getc_offset to the calling function.
Author: Jamie Harnum
History / Versions: 01/02: V 0.5
					29/01: V 0.1
Called functions: 
Parameters: Buffer * const pBD - the Buffer to get the offset for
Return value: The current value of getc_offset
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
short b_getcoffset(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
	else {
		return pBD->getc_offset;
	}
}

/*
Purpose: Set getc_offset and markc_offset to 0 so the buffer can be read over again.
Author: Jamie Harnum
History / Versions: 01/02: V 0.5
					29/01: V 0.1
Called functions:
Parameters: Buffer * const pBD - The Buffer to change the values of.
Return value: If the function runs successfully, it will return 0.
			If there is a possible runtime error, it will return -1.
Algorithm:
*/
int b_rewind(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}
	else {
		pBD->getc_offset = 0;
		pBD->markc_offset = 0;

		return 0;
	}
}

/*
Purpose: Get a pointer to the location indicated by markc_offset
Author: Jamie Harnum
History / Versions: 29/01: V 0.1
Called functions: 
Parameters: Buffer * const pBD - a pointer to the Buffer required
Return value: A pointer to the char in the Buffer indicated by the value of markc_offset
			If there is a possible runtime error, it will return NULL
Algorithm:
*/
char * b_location(Buffer * const pBD) {
	if (pBD == 0) {
		return NULL;
	}
}