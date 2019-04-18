/*
File name: table.h
Compiler: MS Visual Studio 2017
Author: Jamie Harnum, #040898399
Course: CST8152 - Compilers, Lab Section: 13
Assignment: 2
Date: March 26, 2019
Professor: Sv. Ranev
Purpose: Defines the transition table, keywords, accepting states, and accepting state functions for the scanner
Function list: Functions are defined in scanner.c
		Token aa_func02(char *lexeme); 
		Token aa_func03(char *lexeme); 
		Token aa_func05(char *lexeme); 
		Token aa_func08(char *lexeme); 
		Token aa_func10(char *lexeme); 
		Token aa_func11(char *lexeme); 

*/
#ifndef TABLE_H_
#define TABLE_H_

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or one of 255,0xFF,EOF
 */

/*  Special case tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
 *  white space
 *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
 *  .AND., .OR. , SEOF, 'illegal symbol',
 */
 
/*Maximum value for integer literals*/
#define INL_MAX 32767

/*REPLACE *ESN* and *ESR* WITH YOUR ERROR STATE NUMBER*/
#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */

/*REPLACE *CN* WITH YOUR COLUMN NUMBER */ 

#define TABLE_COLUMNS 8						
/*transition table - type of states defined in separate table */

int st_table[][TABLE_COLUMNS] = {
	/* State 0 */  {1, 6, 4, ES, ES, 9, ES, ER},
	/* State 1 */  {1, 1, 1, 2/*or KW - AS2*/, 3, ES, 2, 2},
	/* State 2 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 3 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 4 */	{ES/*or DIL - AS5*/, 4, 4, 7, 5, ES, 5, 5},
	/* State 5 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 6 */	{ES, 6, ES, 7, 5, ES, 5, 5},
	/* State 7 */	{8, 7, 7, 8, 8, ES, 8, 8},
	/* State 8 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 9 */	{9, 9, 9, 9, 9, 10, 9, ER},
	/* State 10 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 11 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 12 */  {IS, IS, IS, IS, IS, IS, IS, IS} };


 
/* Accepting state table definition */
/*REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS*/
#define ASWR     1  /* accepting state with retract */
#define ASNR     2  /* accepting state with no retract */
#define NOAS     0  /* not accepting state */

int as_table[ ] = {NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASNR, ASNR, ASWR};

/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME.*/ 

Token aa_func02(char *lexeme); /*Variable Identifier - Arithmetic / Keyword - ASWR*/
Token aa_func03(char *lexeme); /*Variable Identifier - String - ASNR*/
Token aa_func05(char *lexeme); /*Decimal Integer Literal - ASWR*/
Token aa_func08(char *lexeme); /*Floating Point Literal - ASWR*/
Token aa_func10(char *lexeme); /*String Literal - ASNR (but might need to change this)*/
Token aa_func11(char *lexeme); /*ES - Error State (both 11 NR and 12 WR)*/

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */
PTR_AAF aa_table[] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	aa_func10,
	aa_func11,
	aa_func11
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table []=
	{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"   
	};

#endif
                     