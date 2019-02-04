/*
File name: buffer.c
Compiler: MS Visual Studio 2017
Author: Jamie Harnum, #040898399
Course: CST8152 - Compilers, Lab Section: 13
Assignment: 1
Date: Feb 3 2019
Professor: Sv. Ranev
Purpose: Create a character buffer that demonstrates proper use of memory allocation and defensive coding practices.
Function list:
				b_allocate()
				b_addc()
				b_clear()
				b_free()
				b_isfull()
				b_limit()
				b_capacity()
				b_mark()
				b_mode()
				b_incfactor()
				b_load()
				b_isempty()
				b_getc()
				b_eob()
				b_print()
				b_compact()
				b_rflag()
				b_retract()
				b_reset()
				b_getcoffset()
				b_rewind()
				b_location()

*/
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

#include "buffer.h"

/*
Purpose: Create and allocate space to a new BufferDescriptor struct.
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: 0.1
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
			- Allocate memory for a new Buffer struct using the provided values
			- Return a pointer to the new Buffer
*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {

	if (!(o_mode != 'f' || o_mode != 'a' || o_mode != 'm')) {
		printf("o_mode is an invalid value\n");
		return NULL;
	}

	if (init_capacity < 0 || init_capacity > MAX_CAPACITY) {
		printf("init_capacity is an invalid value\n");
		return NULL;
	}
	
	/*Allocate memory for a BufferDescription pointer*/
	Buffer *pBuffer = (Buffer*) calloc(1, sizeof(Buffer));

	if (pBuffer == NULL) {
		printf("Unable to allocate memory for the Buffer\n");
		return NULL;
	}

	/*If the init_capacity is 0, the capacity and increment factor are set to their default values*/
	if (init_capacity == 0) {
		init_capacity = DEFAULT_INIT_CAPACITY;
		inc_factor = DEFAULT_INC_FACTOR;
	}

	/*Set o_mode values*/
	switch (o_mode) {

	case 'a':
		pBuffer->mode = 1;
		if (inc_factor < 1 || inc_factor > 255) {
			printf("Invalid inc_factor for this mode\n");
			return NULL;
		}
		break;
	case 'm':
		pBuffer->mode = -1;
		if (inc_factor < 1 || inc_factor>100) {
			printf("Invalid inc_factor for this mode\n");
			return NULL;
		}
		break;
	case 'f':
		inc_factor = 0;
		pBuffer->mode = 0;
	}

	/*Allocate memory for character array.
		Because capacity is already in bytes, there is no need to multiply it by sizeof(char)*/
	char *charBuffer = (char*) malloc(init_capacity);

	/*Check if allocation was successful*/
	if (charBuffer == NULL) {
		printf("Unable to allocate memory for the Buffer\n");
		return NULL;
	}
	else {
		pBuffer->cb_head = charBuffer;
	}

	/*Set the rest of the member variables*/
	pBuffer->flags = DEFAULT_FLAGS;
	pBuffer->capacity = init_capacity;
	pBuffer->inc_factor = inc_factor;
	pBuffer->addc_offset = 0;
	pBuffer->getc_offset = 0;
	pBuffer->markc_offset = 0;

	return pBuffer;

}

/*
Purpose: Adds a character to the buffer array and expands the array if appropriate according to the current size and mode of the buffer.
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
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
pBuffer b_addc(pBuffer const pBD, char symbol) {

	if (pBD == 0) {
		return NULL;
	}

	/*Reset r_flag bit to 0*/
	pBD->flags &= RESET_R_FLAG;

	/*If the buffer is not full, add the character and return the buffer pointer*/
	if (b_isfull(pBD) == 0) {

		pBD->cb_head[pBD->addc_offset] = symbol;
		pBD->addc_offset += 1;

		return pBD;

	}
	else {

		short new_capacity;
		short available_space = MAX_CAPACITY - pBD->capacity;
		double inc_factor;

		/*if MAX has been reached, return NULL*/
		if (pBD->capacity == MAX_CAPACITY) {
			return NULL;
		}
		else {

			/*If the buffer is full but not at MAX yet, action is determined by the mode*/
			switch (pBD->mode) {
			case 0:
				/*Fixed mode, no change*/
				return NULL;
			case 1:
				/*Additive mode: the capacity is increased by the inc_factor*/
				if (pBD->inc_factor > available_space) {
					new_capacity = MAX_CAPACITY;
				}
				else {
					new_capacity = pBD->capacity + pBD->inc_factor;

					/*This shouldn't be able to happen since we've already checked the available space left*/
					if (new_capacity < 0) {
						return NULL;
					}
				}
				break;
			case -1:
				/*Multiplicative mode: Increment by the percentage of the available space indicated by the inc_factor*/
				inc_factor = available_space * ((double)pBD->inc_factor / 100.0);

				 if ((short)inc_factor <= 0 && available_space!=0) {
					new_capacity = MAX_CAPACITY;
				 }
				else {
					new_capacity = pBD->capacity + (short)inc_factor;
				}

			}

			/* if 1 or -1 and has not failed, expand array size using realloc()*/
			char *cBuffer = (char*)realloc(pBD->cb_head, new_capacity);

			if (cBuffer == NULL) {
				return NULL;
			}
			else if (cBuffer != pBD->cb_head) {
				pBD->cb_head = cBuffer;
				pBD->flags |= SET_R_FLAG;
			}

			/*Add symbol to buffer and change capacity variable*/
			pBD->cb_head[pBD->addc_offset] = symbol;
			pBD->addc_offset += 1;
			pBD->capacity = new_capacity;

			return pBD;
		}
	}

}

/*
Purpose: Retain the memory space that has been allocated to the buffer, but re-initialize data members so the buffer will appear empty and the next added character will be at the beginning of the list
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					02/02: V 0.5
					29/01: V 0.1
Called functions:
Parameters: Buffer * const pBD - the pointer to the Buffer structure to be cleared
Return value:	If successful, returns 1.
				If a runtime error is possible, the function returns -1 to notify of failure. 
Algorithm: 
			- set getc_offset to 0
			- set addc_offset to 0
			- reset EOB flag
			- set *cb_head to 0 (the char itself, not the pointer)
*/
int b_clear(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}

	pBD->getc_offset = 0;
	pBD->addc_offset = 0;
	*(pBD->cb_head) = 0;

	pBD->flags &= RESET_EOB;

	return 1;
}

/*
Purpose: Free all memory occupied by the character buffer and the Buffer structure itself
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
Called functions: b_isempty()
Parameters: Buffer * const pBD - pointer to the Buffer to be freed
Return value: Void
Algorithm:  - free buffer array
			- free buffer description struct
*/
void b_free(Buffer * const pBD) {
	if (pBD == NULL) {
		return;
	}
	if (!b_isempty(pBD)) {
		free(pBD->cb_head);
	}

	free(pBD);
}

/*
Purpose: Check if the Buffer is full
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
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

	if (pBD->capacity == pBD->addc_offset) {
		return 1;
	}

	return 0;
}

/*
Purpose: Returns the current limit of the character buffer measured in chars.
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
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
			/*the number of chars currently in the buffer will be equal to the index to add the next char at*/
			return pBD->addc_offset;
		}
}

/*
Purpose: Return the capacity of a given Buffer
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
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
History / Versions: 03/02: V 1.0
					29/01: V 0.1
Called functions: None
Parameters:		pBuffer const pBD - a pointer to the BufferDescription we wish to change
				short mark - the new value for markc_offset
Return value: The short representing the current value of markc_offset for this Buffer
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
short b_mark(pBuffer const pBD, short mark) {
	if (pBD == 0 || mark < 0 || mark > pBD->addc_offset) {
		return RT_FAIL_1;
	}
	else {
		pBD->markc_offset = mark;
		return pBD->markc_offset;
	}
}

/*
Purpose: Get the mode of a particular buffer
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
Called functions: 
Parameters: Buffer * const pBD - a pointer to the Buffer in question
Return value: The int representing the buffer mode
			If there is a possible runtime error, it will return 10.
Algorithm:
*/
int b_mode(Buffer * const pBD) {
	if (pBD == 0) {
		printf("Null pointer error\n");
		return 10;
	}

	return pBD->mode;
}

/*
Purpose: Returns the inc_factor of a given Buffer
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
Called functions: None
Parameters: Buffer * const pBD - a pointer to the Buffer to get the inc_factor of
Return value: Returns the value of inc_factor
			If there is a possible runtime error, it will return 0x100.
Algorithm:
*/
size_t b_incfactor(Buffer * const pBD) {
	if (pBD == NULL) {
		return 0x100;
	}

	return pBD->inc_factor;
}

/*
Purpose: Add chars from a file to the buffer
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
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
			- if all characters are added successfully, unget() the last character to remove the EOF marker and return the number of chars added
*/
int b_load(FILE * const fi, Buffer * const pBD) {
	if (pBD == 0 || fi == 0) {
		return RT_FAIL_1;
	}
	char newChar;
	short prev_offset = pBD->addc_offset;
	short post_offset;
	pBuffer addResult;

	while (!feof(fi)) {

		newChar = fgetc(fi);

		if (newChar!=EOF) {
			addResult = b_addc(pBD, newChar);

			if (addResult == NULL) {
				printf("Last character read from input file is: %c %i\n", newChar, newChar);
				ungetc(newChar, fi);
				//pBD->addc_offset -= 1;
				return LOAD_FAIL;
			}
		}
	}
	
	post_offset = pBD->addc_offset;

	return post_offset - prev_offset; 
}

/*
Purpose: Check if the buffer is empty
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
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
History / Versions: 03/02: V 1.0
					29/01: V 0.1
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

	if (pBD->getc_offset == pBD->addc_offset - 1) {
		pBD->flags |= SET_EOB;
	}
	else {
		pBD->flags &= RESET_EOB;
	}

	char c = pBD->cb_head[pBD->getc_offset];
	pBD->getc_offset += 1;

	return c;
	
}

/*
Purpose: Checks if the End of Buffer bit has been set 
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
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

	/*If the result of flags & CHECK is 1 it means the EOB bit is set to 1*/
	if (pBD->flags & CHECK_EOB) {
		return 1;
	}
	else {
		return 0;
	}

}

/*
Purpose: Prints the content of the Buffer character by character
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					02/02: V 0.5
					29/01: V 0.1
Called functions: b_isempty()
				  b_getc()
				  b_eob()
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
	int numChars = pBD->addc_offset;

	if (b_isempty(pBD)) {
		return 0;
	}

	char c;
	int eob = 0;
	/*While the end of buffer bit is not set (i.e. the end of the buffer has not been reached)*/
	while (!eob) {

		if (b_eob(pBD)) {
			eob = 1;
			break;
		}

		c = b_getc(pBD);

		if (c == RT_FAIL_2) {
			return RT_FAIL_1;
		}
		else {
			printf("%c",c); 
		}
	}
	printf("\n");
	return numChars - pBD->getc_offset;


}

/*
Purpose: Reallocates the buffer's memory to a more appropriate value given the current contents
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
Called functions: 
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

	short new_capacity = (pBD->addc_offset + 1) * sizeof(char);

	char *pChar = (char*) realloc(pBD->cb_head, new_capacity);

	if (pChar == NULL) {
		return NULL;
	}
	
	/*Are we pointing to a new block? If so, set the r_flag*/
	if (pChar != pBD->cb_head) {
		pBD->cb_head = pChar;
		pBD->flags |= SET_R_FLAG;
	}

	pBD->capacity = new_capacity;
	pBD->cb_head[pBD->addc_offset] = symbol;
	pBD->addc_offset += 1;

	return pBD;
}

/*
Purpose: Check if the rflag bit has been set.
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
Called functions: 
Parameters: Buffer * const pBD - a pointer to the buffer to check
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

	/*If the result is 1, the flag is set to 1*/
	if ((pBD->flags & CHECK_R_FLAG)==1) { 
		printf("The r_flag bit is set to 1\n");
	}
	else {
		printf("The r_flag bit is set to 0\n");
	}

	return (char) pBD->flags;
}

/*
Purpose: Decrements getc_offset, i.e. retracts the buffer by 1 char.
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					02/02: V 0.5
					29/01: V 0.1
Called functions: 
Parameters: Buffer * const pBD - a pointer to the Buffer to be retracted
Return value: The new getc_offset.
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
short b_retract(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}

	if (pBD->getc_offset < 0) {
		pBD->getc_offset -= 1;
		return pBD->getc_offset;
	}
	else {
		return RT_FAIL_1;
	}
}

/*
Purpose: Sets the value of getc_offset to the current markc_offset
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					29/01: V 0.1
Called functions: 
Parameters: Buffer * const pBD - a pointer to the Buffer to be altered
Return value: The new value of getc_offset.
			If there is a possible runtime error, it will return -1.
Algorithm: 
*/
short b_reset(Buffer * const pBD) {
	if (pBD == 0) {
		return RT_FAIL_1;
	}

	pBD->getc_offset = pBD->markc_offset;

	return pBD->getc_offset;
}

/*
Purpose: Returns getc_offset to the calling function.
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					01/02: V 0.5
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
History / Versions: 03/02: V 1.0
					01/02: V 0.5
					29/01: V 0.1
Called functions:
Parameters: Buffer * const pBD - The Buffer to change the values of.
Return value: If the function runs successfully, it will return 1.
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
		pBD->flags &= RESET_EOB;

		return 1;
	}
}

/*
Purpose: Get a pointer to the location indicated by markc_offset
Author: Jamie Harnum
History / Versions: 03/02: V 1.0
					02/02: V 0.5
					29/01: V 0.1
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

	char *mChar = "";

	*mChar = pBD->cb_head[pBD->markc_offset];

	if (!mChar) {
		return NULL;
	}
	else {
		return mChar;
	}

}