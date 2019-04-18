/*  File name: test.c
 *  Purpose: A test main program for Assignment #1, CST8152
 *  Version: 1.0
 *  Author: Jamie Harnum using code from Sv. Ranev
 *  Date: 1 February 2019
 */

 /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define directive does not have any effect on other compiler projects (gcc, Borland).
  */
#define _CRT_SECURE_NO_WARNINGS

/* Check for memory leaks during testing */
#define _CRTDBG_MAP_ALLOC
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <crtdbg.h>

#include "buffer.h"

/*check for ANSI C compliancy */
#define ANSI_C 0
#if defined(__STDC__)
#undef ANSI_C
#define ANSI_C 1
#endif

/*Declaration of a buffer display function*/
void display(Buffer *ptr_Buffer);

int main(int argc, char **argv) {

	pBuffer ptr_Buffer;   /* pointer to Buffer structure */
	
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);

	/*ptr_Buffer = b_allocate(0, 1022, 'm'); */ /*valid - 1022 is set to default bc of 0*/
	/*ptr_Buffer = b_allocate(11241523, 10, 'f") */ /*invalid - init_capacity too large*/

	if (ptr_Buffer == NULL) {
		printf("Could not allocate Buffer");
	}
	else {
		display(ptr_Buffer);
	}

	return 0;

}

void display(Buffer *ptr_Buffer) {
	printf("\nPrinting buffer parameters:\n\n");
	printf("The capacity of the buffer is:  %d\n", b_capacity(ptr_Buffer));
	printf("The current size of the buffer is:  %d\n", b_limit(ptr_Buffer));
	printf("The operational mode of the buffer is:   %d\n", b_mode(ptr_Buffer));
	printf("The increment factor of the buffer is:  %lu\n", b_incfactor(ptr_Buffer));
	printf("The current mark of the buffer is:  %d\n", b_mark(ptr_Buffer, b_limit(ptr_Buffer)));
	printf("The value of the flags field is: %04hX\n", ptr_Buffer->flags);
	printf("\nPrinting buffer contents:\n\n");
	b_rewind(ptr_Buffer);
	if (b_isempty(ptr_Buffer)) printf("Empty buffer.\n");
	b_print(ptr_Buffer);
}
