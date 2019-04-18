/*
File name: parser.h
Compiler: MS Visual Studio 2017
Author: Jamie Harnum, #040898399
Course: CST8152 - Compilers, Lab Section: 13
Assignment: 3
Date: April 19, 2019
Professor: Sv. Ranev
Purpose: 
Function list: [list here all functions declared / defined in the file. do not include function parameter lists and return type - i.e. func(), not int func(int a).
		list must follow order of the functions in the file]
*/

#pragma once

#ifndef  PARSER_H_
#define PARSER_H_

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*Extern variables*/
extern int line;
extern char* kw_table[];
extern Buffer * str_LTBL;
extern Token malar_next_token();

/*Global variables and constants*/
static Token lookahead = { 0 };
int synerrno;
#define NO_ATTR -1
#define NUM_NEED_ATTR 4
int NEED_ATTR[] = { KW_T, LOG_OP_T, ART_OP_T, REL_OP_T };
enum KEYWORD { ELSE, FALSE, IF, PLATYPUS,
	READ, REPEAT, THEN, TRUE, WHILE, WRITE };

/*function prototypes*/
void parser(void);

void program(void);

void opt_statements(void);

void statements(void);

void statement(void);

void assignment_statement(void);

void assignment_expression(void);

void selection_statement(void);

void precondition(void);

void iteration_statement(void);

void input_statement(void);

void variable_list(void);

void opt_variable_list(void);

void variable_identifier(void);

void output_statement(void);

void write_opt(void);

void output_list(void);

void arithmetic_expression(void);

void unary_arithmetic_expression(void);

void additive_arithmetic_expression(void);

void next_additive_expression(void);

void multiplicative_arithmetic_expression(void);

void next_multiplicative_expression();

void primary_arithmetic_expression(void);

void string_expression(void);

void opt_concat_expression(void);

void primary_string_expression(void);

void conditional_expression(void);

void logical_or_expression(void);

void next_or_expression(void);

void logical_and_expression(void);

void next_and_expression(void);

void relational_expression(void);

void a_relational_comparison(void);

void primary_a_relational_expression(void);

void s_relational_comparison(void);

void primary_s_relational_expression(void);

void match(int pr_token_code, int pr_token_attribute);

void gen_incode(char* statement);

void syn_eh(int sync_token_code);

void syn_printe();

#endif