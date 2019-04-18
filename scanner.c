/*
File name: scanner.c
Compiler: MS Visual Studio 2017
Author: Jamie Harnum, #040898399
Course: CST8152 - Compilers, Lab Section: 13
Assignment: 2
Date: March 26, 2019
Professor: Sv. Ranev
Purpose: Provides the logic for assigning tokens to lexemes from the PLATYPUS language using table.h and token.h
Function list: 
	int scanner_init(Buffer)
	Token malar_next_token(void)
	int get_next_state(int, char, int*)
	int char_class (char)
	Token aa_func02(char [])
	Token aa_func03(char [])
	Token aa_func05(char [])
	Token aa_func08(char [])
	Token aa_func10(char [])
	Token aa_func11(char [])
	int iskeyword(char [])
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef TABLE_H_
#include "table.h"
#endif

/*#define DEBUG  /* for conditional processing */
/*#undef  DEBUG*/
#ifndef SCANNER_C_
#define SCANNER_C_

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(Buffer * psc_buf) {
  	if(b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
Purpose: Primary method of identifying the appropriate tokens for lexemes
Author: Jamie Harnum
History / Versions: V 1: Mar25/19
Called functions: 
	b_getc()
	b_retract()
	b_location()
	b_getcoffset()
	get_next_state()
	aa_func02()
	aa_func03()
	aa_func05()
	aa_func08()
	aa_func10()
	aa_func11()
Parameters: N/A
Return value: Token representing the lexeme
Algorithm: - Uses a switch statement to check if the current character is a lexeme on its own
			- if a token is not returned by the switch statement, uses the Finite State Machine
			and the transition table (using get_next_state()) in order to determine the type of lexeme
			- once get_next_state returns an accepting state, the appropriate accepting state function is called
			- the accepting state function returns the appropriate lexeme to be returned by this function
*/
Token malar_next_token(void) {

	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */

	/*DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED */
	short length; /*for checking lexeme length*/

	while (1) { /* endless loop broken by token returns it will generate a warning */

		/*GET THE NEXT SYMBOL FROM THE INPUT BUFFER*/

		c = b_getc(sc_buf);

		/*Check if the character is a lexeme of its own or is otherwise part of a predefined lexeme (for example, .AND. or !! signifying a line of commented characters*/
		switch (c) {
		case ' ': continue;
		case '{': t.code = LBR_T; /*no attribute*/ return t;
		case '}': t.code = RBR_T; /*no attribute*/ return t;
		case '(': t.code = LPR_T; /*no attribute*/ return t;
		case ')': t.code = RPR_T; /*no attribute*/ return t;
		case ',': t.code = COM_T; /*na*/ return t;
		case ';': t.code = EOS_T; /*na*/ return t;
		case '=': c = b_getc(sc_buf); /*check for a following equal sign*/
			if (c == '=') { t.code = REL_OP_T; t.attribute.rel_op = EQ; return t; }
			t.code = ASS_OP_T; b_retract(sc_buf); return t;
		case '<': c = b_getc(sc_buf);
			switch (c) {
			case '>': t.code = REL_OP_T; t.attribute.rel_op = NE; return t;/*not equal <>*/
			case '<': t.code = SCC_OP_T; return t; /*concat <<, no attribute*/
			default: b_retract(sc_buf); /*return the extra char*/
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			}
		case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t;
		case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;
		case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;
		case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;
		case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;
		case '\n': ++line; continue; 
		case '\t': case '\v': case '\f': case '\r': continue;
		case '.': /*the . character may not be a lexeme on its own, but is the start of two possible lexemes: .AND. and .OR.*/
			lexstart = b_getcoffset(sc_buf); /*mark for retract*/
			c = b_getc(sc_buf);

			switch (c) {
				case 'A': c = b_getc(sc_buf);
					if (c == 'N') {
						c = b_getc(sc_buf);
						if (c == 'D') {
							c = b_getc(sc_buf);

							if (c == '.') {
								t.code = LOG_OP_T;
								t.attribute.log_op = AND;
								return t;
							}
						}
				}
				while (b_getcoffset(sc_buf) != lexstart) {
					b_retract(sc_buf);
				}
				t.code = ERR_T;
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';

				return t;

			case 'O': c = b_getc(sc_buf); 
				if (c == 'R') {
					c = b_getc(sc_buf); if (c == '.') {
						t.code = LOG_OP_T; t.attribute.log_op = OR; return t;
					}
				}
				
				while (b_getcoffset(sc_buf) != lexstart) {
					b_retract(sc_buf);
				}

				t.code = ERR_T;
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';

				return t;

			default: 
				while (b_getcoffset(sc_buf) != lexstart) {
				b_retract(sc_buf);
				}
					 t.code = ERR_T;
					 t.attribute.err_lex[0] = '.';
					 t.attribute.err_lex[1] = '\0';

					 return t;
			}
		case 255: t.code = SEOF_T; t.attribute.seof = SEOF_EOF; return t; /*End of File*/
		case '\0': t.code = SEOF_T; t.attribute.seof = SEOF_0; return t; /*End of C-type string*/
		case '!': c = b_getc(sc_buf); /*We assume that the programmer is trying to comment a line
									  if a lexeme starts with !, even if not followed by another !*/
			if (c != '!') {
				/*report error - ! and whatever char follows it*/
				t.code = ERR_T;

				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';

				while (c != '\n') {
					c = b_getc(sc_buf);
					if (c == 255 || c == '\0') {
						b_retract(sc_buf);
						return t;
					}
				}
				++line;
				return t;
			}
			while (c != '\n') {
				c = b_getc(sc_buf);
				if (c == 255 || c == '\0') {
					b_retract(sc_buf);
					return t;
				}
			}
			++line;
			continue;
		}/*end switch*/

		/* Part 2: Implementation of Finite State Machine (DFA)
				   or Transition Table driven Scanner
				   Note: Part 2 must follow Part 1 to catch the illegal symbols
		*/

		/*SET THE MARK AT THE BEGINING OF THE LEXEME AND SAVE IT IN lexstart*/
		b_retract(sc_buf);
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));

		/*CODE YOUR FINATE STATE MACHINE HERE (FSM or DFA)
		IT IMPLEMENTS THE FOLLOWING ALGORITHM:

		FSM0. Begin with state = 0 and the input character c*/

		/*FSM1. Get the next state from the transition table calling
			  state = get_next_state(state, c, &accept);
		FSM2. Get the next character
		FSM3. If the state is not accepting (accept == NOAS), go to step FSM1
			  If the step is accepting, token is found, leave the machine and
			  call an accepting function as described below.    */

	/*Continue to call get_next_state to traverse the transition table until it returns an accepting state*/
		while (accept == NOAS) {
			
				c = b_getc(sc_buf);
				
				state = get_next_state(state, c, &accept);
			
		}

		/*If final state has retract (either ASWR or ER), go back one char*/
		if (accept == ASWR) {
			b_retract(sc_buf);
		}

		lexend = b_getcoffset(sc_buf); /*Set lexend*/

		/*CREATE  A TEMPORRARY LEXEME BUFFER HERE;*/
		length = lexend - lexstart;

		lex_buf = b_allocate(length, 15, 'a');

		if (lex_buf == NULL) {
			fprintf(stderr, "Allocation error: Could not create lexeme buffer\n");
			exit(1);
		}

		/*Retract until the getc_offset is at the start of the lexeme*/
		while (b_getcoffset(sc_buf) != lexstart) {
			b_retract(sc_buf);
		}

		

		/*add lexeme chars to the lex_buf*/
		while (b_limit(lex_buf) != length) {

			char l;

			l =  b_getc(sc_buf); 

			if (!b_addc(lex_buf, l)) {
				fprintf(stderr, "Lexeme buffer overflow, could not add char\n");
				exit(1);
			}

		}

		if (!b_addc(lex_buf, (char)'\0')) {
			fprintf(stderr, "Lexeme buffer overflow, could not add end of string char\n");
			exit(1);
		}
		

			/*call the appropriate accepting function to return the token value*/
			t = aa_table[state](b_location(lex_buf));

			b_free(lex_buf);

			return t;
		} /*end while(1)*/
	} 




/*DO NOT MODIFY THE CODE OF THIS FUNCTION
YOU CAN REMOVE THE COMMENTS*/

int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
       assert(next != IS);

#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*
Purpose: Identifies which column of the transition table a particular character belongs to
Author: Jamie Harnum
History / Versions: V1: Mar25/19
Called functions: isalpha(), isdigit()
Parameters: c - the character to find the column of
Return value: int val - the index of the appropriate column
*/
int char_class (char c)
{
        int val;

		if (isalpha((unsigned char)c)) {
			val = 0;
		}
		else if (isdigit((unsigned char)c)) {
			if (c == '0') {
				val = 1;
			}
			else {
				val = 2;
			}
		}
		else if (c == '.') {
			val = 3;
		}
		else if (c == '@') {
			val = 4;
		}
		else if (c == '"') {
			val = 5;
		}
		else if (c=='\0'||c==EOF) {
			val = 7;
		}
		else {
			val = 6;
		}

        return val;
}

/*
Purpose: Accepting function for the arithmetic variable identifier and keywords (VID / AVID / KW)
Author: Jamie Harnum
History / Versions: V1 MAR25/19
Called functions: iskeyword(), strcpy()
Parameters: character array - the lexeme to identify a token for
Return value: The appropriate token for the lexeme
Algorithm: - check if the lexeme is a keyword
			- if not, it must be an AVID
			- assign token and return*/
Token aa_func02(char lexeme[]){
	Token t = { 0 };
	int kw = -1;
	int i = 0;

	/*printf("lexeme: | %s | \n", lexeme);*/

	kw = iskeyword(lexeme);

	if (kw >= 0) {
		t.code = KW_T;
		t.attribute.kwt_idx = kw;
	}
	else {
		t.code = AVID_T;

		/*check lexeme length*/
		while (lexeme[i] != '\0') {
			++i;
		}

		/*if the lexeme is too long, shorten it*/
		if (i > VID_LEN) {
			int j = 0;
			i = 0;
			while (i < VID_LEN) {
				t.attribute.vid_lex[i] = lexeme[i];
				++i;
			}

			t.attribute.vid_lex[i] = '\0';
		}
		
		else {
			strcpy(t.attribute.vid_lex, lexeme);
		}

	}

  return t;
}

/*
Purpose: Accepting function for string variable identifiers (SVID)
Author: Jamie Harnum
History / Versions: V1 MAR25/19
Called functions: strcpy()
Parameters: Character array - the lexeme being assigned a token
Return value: The token assigned
*/
Token aa_func03(char lexeme[]){
	Token t = { 0 };
	int i = 0;
	t.code = SVID_T;

	/*check lexeme length*/
	while (lexeme[i] != '\0') {
		++i;
	}

	/*if the lexeme is too long, shorten it*/
	if (i > VID_LEN) {
		int j = 0;
		i = 0;
		while (i < VID_LEN-1) {
			t.attribute.vid_lex[i] = lexeme[i];
			++i;
		}
		t.attribute.vid_lex[i] = '@';
		t.attribute.vid_lex[i+1] = '\0';
	}
	else {
		strcpy(t.attribute.vid_lex, lexeme);
	}

  return t;
}

/*
Purpose: Accepting function for integer literals / decimal integer literals (IL/DIL)
Author: Jamie Harnum
History / Versions: V1 MAR25/19
Called functions: atoi(), strcpy()
Parameters: Character array - the lexeme being assigned a token
Return value: The token assigned
Algorithm: Check that the integer value will fit into a short int of 2 bytes before assigning either an error token or a IL token
*/
Token aa_func05(char lexeme[]) {
	Token t = { 0 };

	long int value = atol(lexeme); /*should i test that this has worked? invalid values shouldn't be able to get this far*/

	/*value must be able to fit into a short int of two bytes*/
	if (value > INL_MAX || value < 0) {
		/*send to error accepting function*/
		t = aa_table[ES](lexeme);
	}
	else {
		short attribute = (short)value;
		t.code = INL_T;
		t.attribute.int_value = attribute;
	}

	return t;
}

/*
Purpose: Accepting function for floating point literals (FPL)
Author: Jamie Harnum
History / Versions: V1 MAR25/19
Called functions: sscanf(), strcpy()
Parameters: Character array - the lexeme being assigned a token
Return value: The token assigned
Algorithm: - Uses sscanf to try to assign the lexeme to a float
			- if the lexeme will not fit into a float of four bites, returns an error token
			- otherwise returns the FPL token
*/
Token aa_func08(char lexeme[]){
	Token t = { 0 };

	double value;
	value = atof(lexeme);

	if (value != 0.0 && (value < FLT_MIN || value > FLT_MAX)) {
		/*send to error accepting function*/
		t = aa_table[ES](lexeme);
	}
	else {
		t.code = FPL_T;
		t.attribute.flt_value = (float)value;
	}
  return t;
}

/*
Purpose: Accepting function for string literals (SL)
Author : Jamie Harnum
History / Versions : V1 MAR25 / 19
Called functions : b_isempty(), b_limit(), b_addc()
Parameters : Character array - the lexeme being assigned a token
Return value : The token assigned
Algorithm : -checks if str_LTBL (string literal buffer) has been allocated properly (was getting null pointer errors when this wasn't here)
			-adds all chars from the lexeme to the string literal buffer
*/
Token aa_func10(char lexeme[]){
	Token t = { 0 };
	int i;

	/*printf("lexeme = %s\n", lexeme);*/

	t.code = STR_T;

	/*attribute is the addc_offset where the first char of the lexeme will be stored*/
	t.attribute.str_offset = b_limit(str_LTBL);

	i = 0;
	while (lexeme[i] != '\0') {
		if (lexeme[i] == '\"') {
			/*ignore*/
		}
		else {

			/*This is probably a spec violation!*/
		if (str_LTBL->cb_head == NULL) {
			//printf("cb_head is null in aa10\n");
			str_LTBL = b_allocate(100, 15, 'm');
		}

			b_addc(str_LTBL, lexeme[i]);
		}

		if (lexeme[i] == '\n') {
			++line;
		}
		
		++i;
	}

	b_addc(str_LTBL,'\0');



  return t;
}

/*
Purpose: Accepting function for errors
Author: Jamie Harnum
History / Versions: V1 MAR25/19
Called functions: 
Parameters: Character array - the lexeme being assigned a token
Return value: The token assigned
Algorithm: - Checks the length of the lexeme to see if it needs to be shortened
			- Either shortens and adds that shortened lexeme to the token attributes or adds the entire lexeme
*/
Token aa_func11(char lexeme[]){
	Token t;
	int i = 0;
	
	t.code = ERR_T;

	/*check if lexeme contains line breaks before potentially shortening*/
	while(lexeme[i]!='\0'){
		
		if (lexeme[i] == '\n') {
			++line;
		}

		++i;
		
	}

	/*if the lexeme is too long, shorten it and append "...\0"*/
	if (i > ERR_LEN) {
		int j = 0;
		i = 0;
		while (i < ERR_LEN - 3) {
			t.attribute.err_lex[i] = lexeme[i];
			
			++i;
		}
		for (; j < 3; ++j) {
			t.attribute.err_lex[i] = '.';
			++i;
		}
		
		t.attribute.err_lex[i] = '\0';
		
	}
	else {
		
		strcpy(t.attribute.err_lex, lexeme);
	}
	
  return t;
}


/*
Purpose: Checks if a particular lexeme is a keyword
Author: Jamie Harnum
History / Versions: V1 MAR25/19
Called functions: strcmp()
Parameters: Character array - the lexeme being assigned a token
Return value: The index of the keyword in the keyword table, or -1 if not a keyword
Algorithm: Goes through the kw_table values and compares them to the lexeme
*/
int iskeyword(char * kw_lexeme){
	
	int i;

	for (i = 0; i < KWT_SIZE; ++i) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0) {
			return i;
		}
	}
	
	return -1; /*not a keyword*/
	
}

#endif