/*
File name: parser.c
Compiler: MS Visual Studio 2017
Author: Jamie Harnum, #040898399
Course: CST8152 - Compilers, Lab Section: 13
Assignment: 3
Date: April 19, 2019
Professor: Sv. Ranev
Purpose: [brief description of contents]
Function list: [list here all functions declared / defined in the file. do not include function parameter lists and return type - i.e. func(), not int func(int a).
		list must follow order of the functions in the file]
*/

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/* project header files */
#include "parser.h"
#ifndef TOKEN_H_
#include "token.h"
#endif
#ifndef TABLE_H_
//#include "table.h"
#endif
#ifndef BUFFER_H_
#include "buffer.h"
#endif

void parser(void) {
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}


void program(void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*FIRST(opt_statements) -> e, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), AVID_T, SVID_T*/
void opt_statements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); 
		break;
	case KW_T: 
		/*check for IF, WHILE, READ, WRITE*/ 
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
			break;
		}
	default: 
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*same as opt_statements but having an empty string is an error*/
void statements(void) {

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statement(); next_statements();
		break;
	case KW_T:
		/*check for IF, WHILE, READ, WRITE*/
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statement(); next_statements();
			break;
		}
	default: /*no further statements*/
		syn_printe();
	}
}

void next_statements() {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statement(); next_statements();
		break;
	case KW_T:
		/*check for IF, WHILE, READ, WRITE*/
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statement(); next_statements();
			break;
		}
	}
}
/*FIRST(statement) -> KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), AVID_T, SVID_T*/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: assignment_statement(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF) {
			selection_statement(); break;
		}
		else if (lookahead.attribute.get_int == WHILE) {
			iteration_statement(); break;
		}
		else if (lookahead.attribute.get_int == READ) {
			input_statement(); break;
		}
		else if (lookahead.attribute.get_int == WRITE) {
			output_statement(); break;
		}
	default: /*empty string - not permitted*/
		syn_printe();
	}
}

/*Assignment statement -> assignment_expression() = assignment_expression();
FIRST(assignment statement) == FIRST(assignment expression)*/
void assignment_statement(void) {
	assignment_expression(); match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Assignment statement parsed");
}



/*FIRST(assignment expression) -> AVID, SVID*/
void assignment_expression(void) {
	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); arithmetic_expression(); 
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T: match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR);  string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default: syn_printe();
	}
	
}

/*IF precondition ( conditional_expression ) THEN { opt_statements }
ELSE { opt_statements };*/
void selection_statement(void) {
	match(KW_T, IF); 
		precondition();
	match(LPR_T, NO_ATTR); 
		conditional_expression();
	match(RPR_T, NO_ATTR); 
	
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
		opt_statements(); 
	match(RBR_T, NO_ATTR);
	
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
		opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Selection statement parsed");
}

/*options: true or false*/
void precondition(void) {
	switch (lookahead.attribute.get_int) {
	case TRUE: match(KW_T, TRUE); break;
	case FALSE: match(KW_T, FALSE); break;
	default:
		syn_printe();
	}
}

/*WHILE precondition (conditional_expression) REPEAT { statements };*/
void iteration_statement(void) {
	match(KW_T, WHILE);
	precondition();
	match(LPR_T, NO_ATTR); conditional_expression(); match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR); statements(); match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Iteration statement parsed");
}

/*READ(variable_list);*/
void input_statement(void) {
	match(KW_T, READ); match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*at least one and possibly more variable identifiers, separated by commas*/
void variable_list(void) {
	variable_identifier(); opt_variable_list();
	gen_incode("PLATY: Variable list parsed");
}

/*FIRST()->COM_T
Zero or more possible variable identifiers*/
void opt_variable_list(void) {
	switch (lookahead.code) {
	case COM_T: match(COM_T, NO_ATTR); variable_identifier(); opt_variable_list();
	}
}

/*Identifier must be either SVID_T or AVID_T*/
void variable_identifier() {
	switch (lookahead.code) {
	case SVID_T: match(SVID_T, NO_ATTR); break;
	case AVID_T: match(AVID_T, NO_ATTR); break;
	default: syn_printe();
	}
}

/*WRITE (write_opt);*/
void output_statement(void) {
	match(KW_T, WRITE); match(LPR_T, NO_ATTR);
	write_opt();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*EITHER opt_variable_list); OR STR_T);*/
void write_opt(void) {
	if (lookahead.code == STR_T) {
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		return;
	} 

	switch (lookahead.code) {
	case SVID_T: match(SVID_T, NO_ATTR); opt_variable_list(); 
		gen_incode("PLATY: Variable list parsed"); break;
	case AVID_T: match(AVID_T, NO_ATTR); opt_variable_list(); 
		gen_incode("PLATY: Variable list parsed"); break;
	default: gen_incode("PLATY: Output list (empty) parsed");
	}
}

/*expressions may be urnary or additive depending on whether they are preceeded by a sign token*/
void arithmetic_expression(void) {

	if (lookahead.code == ART_OP_T) {
		unary_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
		return;
	}
	 
	switch (lookahead.code) {
	case AVID_T: case FPL_T: case INL_T: case LPR_T: additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	default: syn_printe();
	}
}

/*FIRST()-> plus or minus*/
void unary_arithmetic_expression(void) {
	if (lookahead.attribute.get_int == PLUS) {
		match(ART_OP_T, PLUS); additive_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
	}
	else if (lookahead.attribute.get_int == MINUS) {
		match(ART_OP_T, MINUS); additive_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
	}
	else {
		syn_printe();
	}
}

/*FIRST()->AVID_T, FPL_T, INL_T, (*/
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();
	next_additive_expression();
}

/*FIRST()-> PLUS, MINUS, AVID_T, FPL_T, INL_T, (, e*/
void next_additive_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T: 
		if (lookahead.attribute.get_int == PLUS) {
			match(ART_OP_T, PLUS); multiplicative_arithmetic_expression();  next_additive_expression();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
		else if (lookahead.attribute.get_int == MINUS) {
			match(ART_OP_T, MINUS); multiplicative_arithmetic_expression();
			next_additive_expression();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
		break;
	default: /*No more expressions left*/
		//TODO: check if it makes more sense to do incode here
		return;
	}
}

/*FIRST()-> AVID_T, FPL_T, INL_T, LPR_T*/
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression(); 
	next_multiplicative_expression();
}

/*FIRST()-> MULT, DIV, AVID_T, FPL_T, INL_T, LPR_T*/
void next_multiplicative_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.get_int == MULT) {
			match(ART_OP_T, MULT); primary_arithmetic_expression(); 
			next_multiplicative_expression();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
		else if (lookahead.attribute.get_int == DIV) {
			match(ART_OP_T, DIV); primary_arithmetic_expression();
			next_multiplicative_expression();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
		break;
	default: /*No more expressions left*/
		//TODO: check if it makes more sense to do incode here
		return;
	}
}

/*FIRST()-> AVID_T, FPL_T, INL_T, LPR_T*/
void primary_arithmetic_expression(void) {
	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR); break;
	case FPL_T: match(FPL_T, NO_ATTR); break;
	case INL_T: match(INL_T, NO_ATTR); break;
	case LPR_T: match(LPR_T, NO_ATTR); arithmetic_expression(); match(RPR_T, NO_ATTR);
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*FIRST()->SVID_T, STR_T*/
void string_expression(void) {
	primary_string_expression(); opt_concat_expression();
	gen_incode("PLATY: String expression parsed");
}

/*FIRST()-> SCC_OP_T (<<), e
must be at least one string token following a concat symbol*/
void opt_concat_expression(void) {
	switch (lookahead.code) {
	case SCC_OP_T: match(SCC_OP_T, NO_ATTR); primary_string_expression();
		opt_concat_expression(); break;
	default: /*no concatenation*/
		return;
	}
}

void primary_string_expression(void) {
	switch (lookahead.code) {
	case SVID_T: match(SVID_T, NO_ATTR); 
		gen_incode("PLATY: Primary string expression parsed");
		break;
	case STR_T: match(STR_T, NO_ATTR); 
		gen_incode("PLATY: Primary string expression parsed"); break;
	default: /*not optional*/
		syn_printe(); 
	}
}

/*FIRST()->AVID_T, FPL_T, INL_T, STR_T, SVID_T*/
//TODO: check where error messages get spit out. here?
void conditional_expression(void) {
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*FIRST()->AVID_T, FPL_T, INL_T, STR_T, SVID_T*/
void logical_or_expression(void) {
	logical_and_expression(); next_or_expression();
}

/*FIRST()-> .OR., e*/
void next_or_expression(void) {
	switch (lookahead.code) {
	case LOG_OP_T: if (lookahead.attribute.get_int == OR) {
		match(LOG_OP_T, OR); logical_and_expression(); next_or_expression();
		gen_incode("PLATY: Logical OR expression parsed");
	} break;
	default: /*empty, no further or expressions*/;
	}
}

/*FIRST()->AVID_T, FPL_T, INL_T, STR_T, SVID_T*/
void logical_and_expression(void) {
	relational_expression(); next_and_expression();
}

/*FIRST()-> .AND., e*/
void next_and_expression(void) {
	switch (lookahead.code) {
	case LOG_OP_T: if (lookahead.attribute.get_int == AND) {
		match(LOG_OP_T, AND); relational_expression(); next_and_expression();

		gen_incode("PLATY: Logical AND expression parsed");
	} break;
	default: /*empty, no further and expressions*/;
	}
}

//TODO check if should still print parsed if error
/*FIRST()-> AVID_T, FPL_T, INL_T, STR_T, SVID_T*/
void relational_expression(void) {
	switch (lookahead.code) {
	case AVID_T: case FPL_T: case INL_T: primary_a_relational_expression();
		a_relational_comparison(); 
		break;
	case STR_T: case SVID_T: primary_s_relational_expression();
		s_relational_comparison();
		break;
	default: /*non-optional*/
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*FIRST()-> REL_OP_T ==, <>, >, <*/
void a_relational_comparison(void) {
	if (lookahead.code != REL_OP_T) {
		syn_printe();
		return;
	}

	switch (lookahead.attribute.get_int) {
	case EQ: match(REL_OP_T, EQ); break;
	case NE: match(REL_OP_T, NE); break;
	case GT: match(REL_OP_T, GT); break;
	case LT: match(REL_OP_T, LT); break;
	default: syn_printe(); return; /*don't continue if there's no operator attribute*/
	}

	primary_a_relational_expression();
}

/*FIRST()->AVID_T, FPL_T, INL_T*/
void primary_a_relational_expression(void) {
	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR); break;
	case FPL_T: match(FPL_T, NO_ATTR); break;
	case INL_T: match(INL_T, NO_ATTR); break;
	default: syn_printe(); /*not optional*/
	}

	gen_incode("PLATY: Primary a_relational expression parsed");

}

/*FIRST()-> REL_OP_T ==, <>, >, <*/
void s_relational_comparison(void) {
	if (lookahead.code != REL_OP_T) {
		syn_printe();
		return;
	}
	switch (lookahead.attribute.get_int) {
	case EQ: match(REL_OP_T, EQ); break;
	case NE: match(REL_OP_T, NE); break;
	case GT: match(REL_OP_T, GT); break;
	case LT: match(REL_OP_T, LT); break;	
	}

	primary_s_relational_expression();
}

void primary_s_relational_expression() {
	primary_string_expression();

	gen_incode("PLATY: Primary s_relational expression parsed");
}

void match(int pr_token_code, int pr_token_attribute) {
	
	/*If its not a match, call the error handler and return*/
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	} 

	if (lookahead.code == SEOF_T) {
		return;
	}

	if (pr_token_attribute != NO_ATTR) {
		/*check the appropriate types of attribute for the code*/
		
		if (lookahead.attribute.int_value != pr_token_attribute) {
			syn_eh(pr_token_code);
			return;
		}
		
	}

	lookahead = malar_next_token();

	/*Check if next token is an error*/
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		++synerrno;
		return;
	}

}

void gen_incode(char* statement) {
	printf("%s\n", statement);
}

void syn_eh(int sync_token_code) {
	syn_printe();
	++synerrno;

	while (1) {

		/*If the tokens don't match, check if the lookahead is the end of file before looping*/
		if (lookahead.code == SEOF_T && sync_token_code != lookahead.code) {
				exit(synerrno);
		}

		/*implied: this means sync_token_code is also == lookahead.code and SEOF_T*/
		if (lookahead.code == SEOF_T) {
			return;
		}

		/*if the tokens match, check for SEOF_T and return*/
		if (lookahead.code == sync_token_code) {
			lookahead = malar_next_token();
			return;
		}

		lookahead = malar_next_token();
		
	}
}

/* error printing function for Assignment 3 (Parser), W19
	Written by Prof Sv Ranev*/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/