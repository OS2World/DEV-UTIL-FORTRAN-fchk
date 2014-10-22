/*
    fortran.y:

	  Yacc grammar for Fortran program checker.  Uses the yylex()
	  in file FORLEX.C

*/

%{

/*
  fortran.c:

    Copyright (C) 1992 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it.  There is no warranty
    for this program.


	    This grammar is ANSI standard-conforming, except for:
		-- complex constant and a few other ambiguities needing
		   significant lookahead cannot be split across lines.

	    Extensions supported:
	        -- Case insensitive.
	 	-- Hollerith constants.
		-- Variable names may be longer than 6 characters.  Also
		   allows underscores and dollar signs in names.
		-- DO ... ENDDO and DO WHILE loop forms allowed.
		-- NAMELIST supported.
		-- TYPE and ACCEPT I/O statements allowed.
		-- Tabs are permitted in input, and (except in character data)
		   expand into blanks up to the next column equal to 1 mod 8.
		-- Type declarations INTEGER*2, REAL*8, etc. are allowed.
		-- IMPLICIT NONE allowed.
	 */

/*  Author: R. Moniot
 *  Date:   August 1988
 *  Last revision: July 1993
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#include "symtab.h"

	/* The following section is for use with bison-derived
	   parser.  Define alloca to be malloc for those cases
	   not covered by the cases covered there.  The ifdefs
	   are those in the skeleton parser with includes removed */
#ifdef AIXC	/* IBM RS/6000 xlc compiler does it this way */
#pragma alloca
#endif
#ifndef alloca
#ifdef __GNUC__
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#else /* Not sparc */
#ifdef MSDOS
#endif /* MSDOS */
#endif /* Not sparc.  */
#endif /* Not GNU C.  */
#define alloca malloc
#endif /* alloca now defined.  */

#ifndef YYDEBUG	/* If not declared otherwise... */
int yydebug;	/* declare yydebug to satisfy extern in ftnchek.c */
#ifdef DEVELOPMENT
#define YYDEBUG 1		/* For development it is handy */
#else
#define YYDEBUG 0
#endif
#endif

#ifdef DEVELOPMENT
#define DEBUG_PARSER
#endif

PRIVATE int current_datatype,	/* set when parse type_name or type_stmt */
    current_size_is_adjustable, /* set in CHARACTER declarations */
    current_size_is_expression, /* set in CHARACTER declarations */
    control_item_count;	/* count of items in control_info_list */
int io_internal_file,	/* Flag for catching misuse of internal files */
    io_list_directed,	/* Flag for use in processing io control lists */
    io_warning_given;		/* to prevent multiple warnings */
			/* Flag shared with forlex for lexing hints */
int stmt_sequence_no;   /* set when parsing, reset to 0 at end_stmt */

PRIVATE long current_typesize;	/* for type*len declarations: value of len */
PRIVATE char *current_len_text;	/* for type*len declarations: text of len */

PRIVATE Token save_token;	/* Holds token shared by productions */

extern unsigned prev_stmt_line_num; /* shared with advance */

unsigned true_prev_stmt_line_num;	/* shared with symtab.c */

PRIVATE int
    current_module_hash = -1,	/* hashtable index of current module name */
    current_module_type,
    executable_stmt=FALSE,
    prev_stmt_class=0,		 /* flags for lexer */
    prev_goto=FALSE,
    goto_flag=FALSE;	/* if unconditional GOTO was encountered */

int 
    complex_const_allowed=FALSE, /* for help in lookahead for these */
    in_assignment_stmt=FALSE,
    inside_format=FALSE,	/* when inside parens of FORMAT  */
    integer_context=FALSE;	/* says integers-only are to follow */

				/* Defns of private functions */

PROTO(PRIVATE Token * add_tree_node,( Token *node, Token *left, Token *right ));
PROTO(PRIVATE Token * append_token,( Token *tlist, Token *t ));
PROTO(PRIVATE void check_stmt_sequence,( Token *t, int seq_num ));
PROTO(PRIVATE void do_binexpr,( Token *l_expr, Token *op, Token *r_expr,
			Token *result ));
PROTO(PRIVATE int do_bounds_type,( Token *t1, Token *t2, Token *t3 ));
PROTO(PRIVATE void do_unexpr,( Token *op, Token *expr, Token *result ));
PROTO(PRIVATE Token * empty_token,( Token *t ));
PROTO(PRIVATE void END_processing,( Token *t ));
PROTO(PRIVATE void init_io_ctrl_list,( void ));
#ifdef DEBUG_PARSER
PROTO(PRIVATE void print_exprlist,( char *s, Token *t ));
PROTO(PRIVATE void print_comlist,( char *s, Token *t ));
#endif

		/* Uses of Token fields for nonterminals: */
/* NOTE: As of Aug 1994 these are undergoing revision to separate the
         use of class, subclass fields */
/*
  1. dim_bound_lists: dimensioning info for arrays:
       token.class = no. of dimensions,  --> TOK_dims
       token.subclass = no. of elements  --> TOK_elts
  2. expressions
       token.value.integer = hash index (of identifier)
       token.class = type_byte = storage_class << 4 + datatype  --> TOK_type
       token.subclass = flags: CONST_EXPR, LVALUE_EXPR, etc.    --> TOK_flags
  3. common variable lists
       token.subclass = flag: COMMA_FLAG used to handle extra/missing commas
								--> TOK_flags
  4. substring_interval
       token.class = start index  --> TOK_start
       token.subclass = end index --> TOK_end
*/


%}

%token tok_identifier
%token tok_array_identifier
%token tok_label
%token tok_integer_const
%token tok_real_const
%token tok_dp_const
%token tok_complex_const
%token tok_dcomplex_const
%token tok_logical_const
%token tok_string
%token tok_hollerith
%token tok_edit_descriptor
%token tok_letter
%token tok_relop	/* .EQ. .NE. .LT. .LE. .GT. .GE. */
%token tok_AND
%token tok_OR
%token tok_EQV
%token tok_NEQV
%token tok_NOT
%token tok_power	/*   **   */
%token tok_concat	/*   //   */
%token tok_ACCEPT
%token tok_ASSIGN
%token tok_BACKSPACE
%token tok_BLOCK
%token tok_BLOCKDATA
%token tok_BYTE
%token tok_CALL
%token tok_CHARACTER
%token tok_CLOSE
%token tok_COMMON
%token tok_COMPLEX
%token tok_CONTINUE
%token tok_DATA
%token tok_DIMENSION
%token tok_DO
%token tok_DOUBLE
%token tok_DOUBLECOMPLEX
%token tok_DOUBLEPRECISION
%token tok_DOWHILE
%token tok_ELSE
%token tok_ELSEIF
%token tok_END
%token tok_ENDDO
%token tok_ENDFILE
%token tok_ENDIF
%token tok_ENTRY
%token tok_EQUIVALENCE
%token tok_EXTERNAL
%token tok_FILE
%token tok_FORMAT
%token tok_FUNCTION
%token tok_GO
%token tok_GOTO
%token tok_IF
%token tok_IMPLICIT
%token tok_INCLUDE
%token tok_INQUIRE
%token tok_INTEGER
%token tok_INTRINSIC
%token tok_LOGICAL
%token tok_NAMELIST
%token tok_NONE
%token tok_OPEN
%token tok_PARAMETER
%token tok_PAUSE
%token tok_POINTER
%token tok_PRECISION
%token tok_PRINT
%token tok_PROGRAM
%token tok_READ
%token tok_REAL
%token tok_RETURN
%token tok_REWIND
%token tok_SAVE
%token tok_STOP
%token tok_SUBROUTINE
%token tok_THEN
%token tok_TO
%token tok_TYPE
%token tok_WHILE
%token tok_WRITE

%token tok_illegal  /* Illegal token unused in grammar: induces syntax error */

%token tok_empty    /* For empty tokens used to fill gaps in expr trees */

%token EOS	127	/* Character for end of statement.  */

%nonassoc tok_relop

%left REDUCE ')'	/* Used at unit_io to force a reduction */


%%
	/*  The following grammar is based on the ANSI manual, diagrams
	 *  of section F.  Numbers in the comments refer to the diagram
	 *  corresponding to the grammar rule.
	 */


/* 1-5 */

prog_body	:	stmt_list
		|	/* empty file */
		;

stmt_list	:	stmt_list_item
		|	stmt_list stmt_list_item
		;


stmt_list_item	:	ordinary_stmt
			{
				/* Create id token for prog if unnamed. */
			  if(current_module_hash == -1) {
			    implied_id_token(&($1),unnamed_prog);
			    def_function(
					 type_PROGRAM,	/* type */
					 size_DEFAULT,	/* size */
					 (char *)NULL,	/* size text */
					 &($1),		/* name */
					 (Token*)NULL);	/* args */
			    current_module_hash =
			      def_curr_module(&($1));
			    current_module_type = type_PROGRAM;
			  }

					/* Handle END statement */
			  if(curr_stmt_class == tok_END) {
			    if(prev_stmt_class != tok_RETURN)
			      do_RETURN(current_module_hash,&($1));
			    END_processing(&($$));
			    goto_flag = prev_goto = FALSE;
			  }
			  prev_stmt_class = curr_stmt_class;
			  integer_context = FALSE;
			  true_prev_stmt_line_num = $$.line_num;
			}
 		|	include_stmt
		|	EOS	/* "sticky" EOF for needed delay */
		;

			/* Statements: note that ordering by category
			   of statement is not enforced in the grammar
			   but is deferred to semantic processing.
			 */

ordinary_stmt	:	stmt
		|	end_stmt
		;

stmt		:	tok_label unlabeled_stmt
			{
#ifdef CHECK_LABELS
			  def_label(&($1));
#endif
			  if(executable_stmt)
			    prev_goto = goto_flag;
			}
		|	unlabeled_stmt
			{
			  if(executable_stmt) {
			    if(prev_goto)
				syntax_error($1.line_num, NO_COL_NUM,
					"No path to this statement");
			    prev_goto = goto_flag;
			  }
			}
		;

unlabeled_stmt	:	subprogram_header
			{
			    exec_stmt_count = 0;
			    executable_stmt = FALSE;
			}
		|	specification_stmt
			{
			    executable_stmt = FALSE;
			}
		|	executable_stmt
			{	/* handle statement functions correctly */
			  if(is_true(STMT_FUNCTION_EXPR, $1.TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN) {
			    stmt_sequence_no = SEQ_STMT_FUN;
			    executable_stmt = FALSE;
			  }
			  else {
			    stmt_sequence_no = SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			  }
			}
		|	restricted_stmt
			{
			    stmt_sequence_no = SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			}
		|	error EOS
			{
			    executable_stmt = TRUE;
			    if(stmt_sequence_no == 0)
			      stmt_sequence_no = SEQ_HEADER;
			    complex_const_allowed = FALSE; /* turn off flags */
			    inside_format=FALSE;
			    integer_context = FALSE;
			    in_assignment_stmt = FALSE;
			    $$.line_num = prev_stmt_line_num; /* best guess */
			    yyerrok; /* (error message already given) */
			}
		;

subprogram_header:	prog_stmt
			{
			    current_module_type = type_PROGRAM;
			}
		|	function_stmt
			{
			    current_module_type = type_SUBROUTINE;
			}
		|	subroutine_stmt
			{
			    current_module_type = type_SUBROUTINE;
			}
		|	block_data_stmt
			{
			    current_module_type = type_BLOCK_DATA;
			}
		;

end_stmt	:	unlabeled_end_stmt
		|	tok_label unlabeled_end_stmt
		;

unlabeled_end_stmt:	tok_END EOS
		;

include_stmt	:	tok_INCLUDE tok_string EOS
 			{
#ifdef ALLOW_INCLUDE
			  if(f77_include) {
			      nonstandard($1.line_num,$1.col_num);
			  }
 			  open_include_file($2.value.string,$1.line_num);
#else
			  syntax_error($1.line_num,$1.col_num,
				"statement not permitted");
#endif
 			}
 		;

/* 5,6 */
		/* Note that stmt_function_stmt is not distinguished from
		   assignment_stmt, but assign (label to variable) is.
		   Also, format_stmt w/o label is accepted here.
		   ANSI standard for statement sequencing is enforced here. */
specification_stmt:	anywhere_stmt
			{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				stmt_sequence_no = SEQ_IMPLICIT;
			     }
			}
		|	parameter_stmt
			{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				   stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     else if(stmt_sequence_no > SEQ_SPECIF) {
			       check_stmt_sequence(&($1),SEQ_SPECIF);
			     }
			}
		|	implicit_stmt
			{
			  check_stmt_sequence(&($1),SEQ_IMPLICIT);
			}
		|	data_stmt
			{
			     if(stmt_sequence_no < SEQ_STMT_FUN) {
				stmt_sequence_no = SEQ_STMT_FUN;
		 	     }
			}
		|	specif_stmt
			{
			  check_stmt_sequence(&($1),SEQ_SPECIF);
			}
		;

anywhere_stmt	:	entry_stmt
			{
			     goto_flag = prev_goto = FALSE;
			}
		|	format_stmt
		;

specif_stmt	:	dimension_stmt
		|	equivalence_stmt
		|	common_stmt
		|	namelist_stmt
		|	type_stmt
		|	external_stmt
		|	intrinsic_stmt
		|	save_stmt
		|       pointer_stmt
		;


/* 7 */
executable_stmt:		/* Allowed in logical IF */
			transfer_stmt
			{
			    goto_flag=TRUE;
			}
		|	nontransfer_stmt
			{
			    goto_flag=FALSE;
			}
		;

transfer_stmt	:	unconditional_goto
		|	assigned_goto
		|	arithmetic_if_stmt
		|	stop_stmt
		|	return_stmt
		;

nontransfer_stmt:	assignment_stmt
		|	assign_stmt
		|	computed_goto	/* fallthru allowed */
		|	continue_stmt
		|	pause_stmt
		|	read_stmt
		|	accept_stmt
		|	write_stmt
		|	print_stmt
		|       type_output_stmt
		|	rewind_stmt
		|	backspace_stmt
		|	endfile_stmt
		|	open_stmt
		|	close_stmt
		|	inquire_stmt
		|	call_stmt
		;

restricted_stmt:		/* Disallowed in logical IF */
			restricted_nontransfer_stmt
			{
			    goto_flag=FALSE;
			}
		|	else_or_endif_stmt
			{
			    prev_goto = goto_flag =FALSE;
			}
		;

restricted_nontransfer_stmt:
			logical_if_stmt
		|	block_if_stmt
		|	do_stmt
			{	/* Flag DO w/o label or DO WHILE forms here */
			  if(is_true(NONSTD_USAGE_FLAG,$1.TOK_flags))
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard($1.line_num,$1.col_num);
#else
			    syntax_error($1.line_num,$1.col_num,
				    "Nonstandard syntax");
#endif
			}

		|	enddo_stmt
			{
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard($1.line_num,$1.col_num);
#else
			    syntax_error($1.line_num,$1.col_num,
				    "Nonstandard syntax");
#endif
			}
		;

else_or_endif_stmt:	else_if_stmt
		|	else_stmt
		|	end_if_stmt
		;

/* 8 */
prog_stmt	:	tok_PROGRAM {check_seq_header(&($1));}
				 symbolic_name EOS
			{
			     def_function(
					  type_PROGRAM,	/* type */
					  size_DEFAULT,	/* size */
					  (char *)NULL,	/* size text */
					  &($3),	/* name */
					  (Token*)NULL);/* args */
			     current_module_hash =
			       def_curr_module(&($3));
			}
		;

			/* Note that function & subroutine entry not
			 * distinguished in this grammar.
			 */
/* 9 */
entry_stmt	:	tok_ENTRY symbolic_name EOS
			{
			  do_ENTRY(&($2),(Token*)NULL
				   ,current_module_hash);
			}
		|	tok_ENTRY symbolic_name '(' dummy_argument_list ')' EOS
			{
			  do_ENTRY(&($2),&($4)
				   ,current_module_hash);
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("entry stmt",&($4));
#endif
			}
		;

/* 10 */
function_stmt	:	unlabeled_function_stmt
		;

unlabeled_function_stmt
		:	typed_function_handle symbolic_name EOS
			{
			     if(f77_function_noparen) {
				nonstandard($2.line_num,
			     (unsigned)($2.col_num+strlen(token_name($2))));
				msg_tail(": parentheses required");
			     }
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      &($2),
				      (Token*)NULL);
			 current_module_hash=
			   def_curr_module(&($2));
			}
		|	typed_function_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      &($2),
				      &($4));
			 current_module_hash=
			   def_curr_module(&($2));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&($4));
#endif
			}
		|	plain_function_handle symbolic_name EOS
			{
			     if(f77_function_noparen) {
				nonstandard($2.line_num,
			      (unsigned)($2.col_num+strlen(token_name($2))));
				msg_tail(": parentheses required");
			     }
			 def_function(
				      type_UNDECL,
				      size_DEFAULT,
				      (char *)NULL,
				      &($2),
				      (Token*)NULL);
			 current_module_hash=
			   def_curr_module(&($2));
			}
		|	plain_function_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			 def_function(
				      type_UNDECL,	/* type */
				      size_DEFAULT,	/* size */
				      (char *)NULL,	/* size text */
				      &($2),		/* name */
				      &($4));		/* args */
			 current_module_hash=
			   def_curr_module(&($2));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&($4));
#endif
			}
		;

typed_function_handle:	type_name function_keyword
		;

plain_function_handle:	function_keyword
		;

function_keyword:	tok_FUNCTION
			{
			  check_seq_header(&($1));
			}
		;

type_name	:	arith_type_name
		|	plain_char_type_name
		|	char_type_name
		;


/* 11 not present: see 9 */

/* 12 */
subroutine_stmt	:	unlabeled_subroutine_stmt
		;

unlabeled_subroutine_stmt
		:	subroutine_handle symbolic_name EOS
			{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       &($2),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($2));
			}
		|	subroutine_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       &($2),
				       &($4));
			  current_module_hash=
			    def_curr_module(&($2));
#ifdef DEBUG_PARSER
			  if(debug_parser)
			    print_exprlist("subroutine stmt",&($4));
#endif
			}
		;

subroutine_handle:	tok_SUBROUTINE
			{
			  check_seq_header(&($1));
			}
		;

dummy_argument_list:	/* empty */
			{
			    $$.next_token = (Token*)NULL;
			}
		|	non_empty_arg_list
		;

non_empty_arg_list:	dummy_argument
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	non_empty_arg_list ',' dummy_argument
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

dummy_argument	:	symbolic_name
			{
			     def_arg_name(&($1));
			     primary_id_expr(&($1),&($$));
			}
		|	'*'
			{
			     $$.TOK_type = type_byte(class_LABEL,type_LABEL);
			     $$.size = size_DEFAULT;
			     $$.TOK_flags = 0;
			     $$.left_token = (Token *)NULL;
			}
		;

/* 13 not present: see 9 */

/* 14 */
block_data_stmt	:	block_data_handle EOS
			{
				  /* form name %DATnn */
			  ++block_data_number;
			  (void)sprintf(unnamed_block_data+4,"%02d",
					block_data_number%100);
			  implied_id_token(&($$),unnamed_block_data);

			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       &($$),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($$));
			}
		|	block_data_handle symbolic_name EOS
			{
			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       &($2),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($2));
			}
		;

block_data_handle:	tok_BLOCK tok_DATA
			{
			  check_seq_header(&($2));
			}
		|	tok_BLOCKDATA
			{
			  check_seq_header(&($1));
			}

		;
/* 15 */
dimension_stmt	:	tok_DIMENSION array_declarator_list EOS
		;

array_declarator_list:	array_declarator
		|	array_declarator_list ',' array_declarator
		;

/* 16 */
array_declarator:	symbolic_name '(' dim_bound_list ')'
			{
			     def_array_dim(&($1),&($3));
			}
		;

dim_bound_list	:	dim_bound_item      /* token class = no. of dimensions,
					       subclass = no. of elements */
			{
			     $$.TOK_dims = 1;
			     $$.TOK_elts = $1.TOK_elts;
			     $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	dim_bound_list ',' dim_bound_item
			{
			     $$.TOK_dims = $1.TOK_dims + 1; /* one more dimension */
			     $$.TOK_elts = $1.TOK_elts * $3.TOK_elts;
			     $$.next_token = append_token($1.next_token,&($3));
			}
		;

dim_bound_item	:	dim_bound_expr
			{
			      if( datatype_of($1.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$1.TOK_flags) )
				$$.TOK_elts = $1.value.integer;
			      else
				$$.TOK_elts = 0;
			}
		|	dim_bound_expr ':' dim_bound_expr
			{	/* avoid getting 0 - 0 + 1 = 1 if bounds nonconstant */
			      if( datatype_of($1.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$1.TOK_flags)
				 && datatype_of($3.TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$3.TOK_flags) )
				$$.TOK_elts = $3.value.integer - $1.value.integer + 1;
			      else
				$$.TOK_elts = 0;

			      $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		|	'*'
			{
			     $$.TOK_elts = 0;
			     $$.left_token = (Token *)NULL;
			}
		|	dim_bound_expr ':' '*'
			{
			     $$.TOK_elts = 0;
			     $3.left_token = (Token *)NULL;
			     $$.left_token = add_tree_node(&($2),&($1),&($3));
			}
		;

/* 17 */
equivalence_stmt:	tok_EQUIVALENCE {equivalence_flag = TRUE;}
			equivalence_list EOS {equivalence_flag = FALSE;}
		;

equivalence_list:	'(' equivalence_list_item ')'
		|	equivalence_list ',' '(' equivalence_list_item ')'
		;

equivalence_list_item:	equiv_entity ',' equiv_entity
			{
			  equivalence(&($1), &($3));
			}
		|	equivalence_list_item ',' equiv_entity
			{
			  equivalence(&($1), &($3));
			}
		;

/* 17 */
equiv_entity	:	symbolic_name
			{
			     def_equiv_name(&($1));
			}
		|	array_equiv_name
			{
			     def_equiv_name(&($1));
			}
		|	substring_equiv_name
			{
			     def_equiv_name(&($1));
			}
		;

array_equiv_name:	symbolic_name '(' subscript_list ')'
				/* should check */
		;

substring_equiv_name:	symbolic_name substring_interval
		|	array_equiv_name substring_interval
		;

/* 19 */
common_stmt	:	tok_COMMON common_variable_list EOS
			{
			     implied_id_token(&($$),blank_com_name);
			     def_com_block(&($$), &($2));
			     if(is_true(COMMA_FLAG,$2.TOK_flags))
			   	syntax_error(
					     $2.line_num,$2.col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&($2));
#endif

			}
		|	tok_COMMON common_block_list EOS
			{
			     if(is_true(COMMA_FLAG,$2.TOK_flags))
				syntax_error(
					     $2.line_num,$2.col_num,
					     "trailing comma");

			}
		|	tok_COMMON common_variable_list common_block_list EOS
			{
			     implied_id_token(&($$),blank_com_name);
			     def_com_block(&($$),&($2));
			     if(is_true(COMMA_FLAG,$3.TOK_flags))
				syntax_error(
					     $3.line_num,$3.col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&($2));
#endif
			}
		;

	/*  The following defns allow trailing commas and missing commas in
	    order to tolerate the optional comma before /blockname/.  The
	    token TOK_flags holds comma status to allow errors to be caught. */
common_block_list:	labeled_common_block
			{
			     $$.TOK_flags = $1.TOK_flags;
			}
		|	common_block_list labeled_common_block
			{
			     $$.TOK_flags = $2.TOK_flags;
			     $$.line_num = $2.line_num;
			     $$.col_num = $2.col_num;
			}
		;

labeled_common_block:	common_block_name common_variable_list
			{
			     def_com_block(&($1),&($2));
			     $$.TOK_flags = $2.TOK_flags;
			     $$.line_num = $2.line_num;
			     $$.col_num = $2.col_num;
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("labeled common",&($2));
#endif
			}
		;

common_block_name:	'/' symbolic_name '/'
			{
			     $$ = $2;
			}

		|	'/'  '/'		/* block with no name */
			{
			     implied_id_token(&($$),blank_com_name);
			}
		|	tok_concat		/* "//" becomes this */
			{
			     implied_id_token(&($$),blank_com_name);
			}
		;

common_variable_list:	common_list_item
			{
			    $$.TOK_flags = $1.TOK_flags;
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	common_variable_list common_list_item
			{
			    if(!is_true(COMMA_FLAG,$1.TOK_flags))
				syntax_error(
					$2.line_num,$2.col_num-1,
					"missing comma");
			    $$.TOK_flags = $2.TOK_flags;
			    $$.line_num = $2.line_num;
			    $$.col_num = $2.col_num;
			    $$.next_token = append_token($1.next_token,&($2));
			}
		;

common_list_item:	common_entity
			{			   /* no comma */
			     $$.TOK_flags = $1.TOK_flags;
			     make_false(COMMA_FLAG,$$.TOK_flags);
			}
		|	common_entity ','
			{			   /* has comma */
			     $$.TOK_flags = $1.TOK_flags;
			     make_true(COMMA_FLAG,$$.TOK_flags);
   			}
		;

common_entity	:	symbolic_name
			{
			     def_com_variable(&($1));
			     primary_id_expr(&($1),&($$));
			}
		|	array_declarator
			{
			     def_com_variable(&($1));
			     primary_id_expr(&($1),&($$));
			}
		;


/* NAMELIST : Not Standard
   Syntax is:
	NAMELIST /group/ var [,var...] [[,] /group/ var [,var...]...]
*/

namelist_stmt	:	tok_NAMELIST namelist_list EOS
			{
			    if(is_true(COMMA_FLAG,$2.TOK_flags))
				syntax_error($2.line_num,
				 (unsigned)($2.col_num+strlen(token_name($2))),
					"trailing comma");
			    if(f77_namelist) {
				nonstandard($1.line_num,$1.col_num);
			    }
			}
		;

namelist_list	:	namelist_decl
		|	namelist_list namelist_decl
			{
			    $$ = $2;
			}
		;

namelist_decl	:	namelist_name namelist_var_list
			{
			     def_namelist(&($1),&($2));
			     $$ = $2;
			}
		;

namelist_name	:	'/' symbolic_name '/'
			{
			    $$ = $2;
			}
		;

namelist_var_list:	namelist_item
			{
			     $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	namelist_var_list namelist_item
			{
			    if(!is_true(COMMA_FLAG,$1.TOK_flags))
				syntax_error(
					$2.line_num,$2.col_num-1,
					"missing comma");
			    $$.TOK_flags = $2.TOK_flags;
			    $$.line_num = $2.line_num;
			    $$.col_num = $2.col_num;
			    $$.next_token = append_token($1.next_token,&($2));
			}
		;

namelist_item	:	symbolic_name
			{			   /* no comma */
			     def_namelist_item(&($1));
			     primary_id_expr(&($1),&($$));
			     make_false(COMMA_FLAG,$$.TOK_flags);
			}
		|	symbolic_name ','
			{			   /* has comma */
			     def_namelist_item(&($1));
			     primary_id_expr(&($1),&($$));
			     make_true(COMMA_FLAG,$$.TOK_flags);
			}
		;

/* 20 */
type_stmt	:	arith_type_name arith_type_decl_list EOS
		|	plain_char_type_name char_type_decl_list EOS
		|	char_type_name char_type_decl_list EOS
		|	char_type_name ',' char_type_decl_list EOS
		;

arith_type_name	:	sizeable_type_name
			{
			  current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			}
				/* Allow *len to modify some arith types */
		|	sizeable_type_name '*' nonzero_unsigned_int_const
			{
			    current_typesize = $3.value.integer;
			    current_len_text = NULL;
#if 0 /* defunct feature */
			    if(local_wordsize > 0) {
			      /*  recognize REAL*2w as DOUBLE PRECISION */
			      if(current_datatype == type_REAL
				 && $3.value.integer == type_size[type_DP])
				current_datatype = type_DP;
			      /*  recognize COMPLEX*4w as DOUBLE COMPLEX */
			      if(current_datatype == type_COMPLEX
				 && $3.value.integer==type_size[type_DCOMPLEX])
				current_datatype = type_DCOMPLEX;
			    }
#endif
			     if(f77_typesize) {
				nonstandard($3.line_num,$3.col_num);
			     }
			}
				/* Other type disallow *len modifier */
		|	unsizeable_type_name
		;

sizeable_type_name:	tok_INTEGER
			{
			     current_datatype = type_INTEGER;
			     integer_context = TRUE;
			}
		|	tok_REAL
			{
			     current_datatype = type_REAL;
			     integer_context = TRUE;
			}
		|	tok_COMPLEX
			{
			     current_datatype = type_COMPLEX;
			     integer_context = TRUE;
			}
		|	tok_LOGICAL
			{
			     current_datatype = type_LOGICAL;
			     integer_context = TRUE;
			}
		;

unsizeable_type_name:	tok_DOUBLE tok_PRECISION
			{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			}
		|	tok_DOUBLEPRECISION
			{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			}
		|	tok_DOUBLE tok_COMPLEX
			{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     if(f77_double_complex) {
				nonstandard($2.line_num,$2.col_num);
			     }
			}
		|	tok_DOUBLECOMPLEX
			{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     if(f77_double_complex) {
				nonstandard($1.line_num,$1.col_num);
			     }
			}
		|	tok_BYTE /* treate BYTE as a form of integer for now */
			{
			     current_datatype = type_INTEGER;
			     current_typesize = 1;
			     current_len_text = NULL;
			     if(f77_byte)
			       nonstandard($1.line_num,$1.col_num);
			}
		;

plain_char_type_name:	tok_CHARACTER
			{
			     current_datatype = type_STRING;
			     current_typesize = 1;
			     current_len_text = NULL;
			     current_size_is_adjustable = 0;
			     current_size_is_expression = 0;
			     integer_context = TRUE;
			}
		;

char_type_name	:	plain_char_type_name '*' len_specification
			{
			     current_typesize = $3.value.integer;
			     current_size_is_adjustable = $3.size_is_adjustable;
			     current_size_is_expression = $3.size_is_expression;
				/* Save length spec text if expression */
			     if(current_size_is_expression) {
			       if($3.left_token == NULL)
				 current_len_text = new_tree_text(&($3));
			       else
				 current_len_text = new_tree_text($3.left_token);
			     }
			     else
			       current_len_text = NULL;
			}
		;

arith_type_decl_list:	arith_type_decl_item
		|	arith_type_decl_list ',' arith_type_decl_item
		;

arith_type_decl_item:	symbolic_name
			{
			     declare_type(&($1),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			}
		|	array_declarator
			{
			     declare_type(&($1),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			}
		;

char_type_decl_list:	char_type_decl_item
		|	char_type_decl_list ',' char_type_decl_item
		;

char_type_decl_item:	symbolic_name
			{
			     $1.size_is_adjustable = current_size_is_adjustable;
			     $1.size_is_expression = current_size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			}
		|	symbolic_name '*' len_specification
			{
			     $1.size_is_adjustable = $3.size_is_adjustable;
			     $1.size_is_expression = $3.size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  $3.value.integer,
					  new_tree_text(
					     $3.left_token == NULL?
					     &($3): $3.left_token )
					  );
			}
		|	array_declarator
			{
			     $1.size_is_adjustable = current_size_is_adjustable;
			     $1.size_is_expression = current_size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			}
		|	array_declarator '*' len_specification
			{
			     $1.size_is_adjustable = $3.size_is_adjustable;
			     $1.size_is_expression = $3.size_is_expression;
			     declare_type(&($1),
					  current_datatype,
					  $3.value.integer,
					  new_tree_text(
					     $3.left_token == NULL?
					     &($3): $3.left_token )
					  );
			}
   		;

/* 21 */
				/* implicit_flag helps is_keyword's work */
implicit_handle	:	tok_IMPLICIT {implicit_flag=TRUE;}
		;

implicit_stmt	:	implicit_handle implicit_decl_list EOS
			{
			    implicit_flag=FALSE;
			    if(implicit_none) {
				syntax_error($1.line_num,$1.col_num,
				     "conflicts with IMPLICIT NONE");
			    }
			    else {
				implicit_type_given = TRUE;
			    }
			}
				/* IMPLICIT NONE statement */
		|	implicit_handle tok_NONE EOS
			{
			    implicit_flag=FALSE;
				if(implicit_type_given) {
				    syntax_error($1.line_num,$1.col_num,
					 "conflicts with IMPLICIT statement");
				}
				else {
				    if(f77_implicit_none)
				      nonstandard($2.line_num,$2.col_num);
				    implicit_none = TRUE;
				}
			}
		;

implicit_decl_list:	implicit_decl_item
		|	implicit_decl_list ',' {initial_flag = TRUE;}
				       implicit_decl_item
		;

		/* implicit_letter_flag tells lexer to treat letters as letters,
			   not as identifiers */
implicit_decl_item:	type_name '('  {implicit_letter_flag = TRUE;}
				letter_list ')'  {implicit_letter_flag = FALSE;}
		;

letter_list	:	letter_list_item
		|	letter_list ',' letter_list_item
		;

letter_list_item:	tok_letter
			{
			  int c1 = (int)$1.subclass;

			  if( (f77_dollarsigns && c1=='$')
			   || (f77_underscores && c1=='_') ) {
			    nonstandard($1.line_num,$1.col_num);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c1);
			}
		|	tok_letter '-' tok_letter
			{
			  int c1 = (int)$1.subclass,
			      c2 = (int)$3.subclass;

			  if( (f77_dollarsigns && (c1 == '$' || c2 == '$'))
			   || (f77_underscores && (c1 == '_' || c2 == '_')))
			  {
			    if(!isalpha(c1))
			      nonstandard($1.line_num,$1.col_num);
			    else
			      nonstandard($3.line_num,$3.col_num);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c2);
			}
		;


/* 22 */
len_specification:	'(' '*' ')'
			{
			     $2.left_token = (Token *)NULL;
			     $$.value.integer = size_ADJUSTABLE;
			     $$.size_is_adjustable = 1;
			     $$.size_is_expression = 0;
				/* Store as a parenthesized expr tree */
			     $$.left_token = add_tree_node(&($1),&($2),
							  (Token*)NULL);
			}
		|	nonzero_unsigned_int_const
			{
			     $$.value.integer = $1.value.integer;
			     $$.size_is_adjustable = 0;
			     $$.size_is_expression = 0;
			}
		|	'(' int_constant_expr ')'
			{
			     $$ = $2;
			     $$.size_is_adjustable = 0;
			     $$.size_is_expression = 1;
			     if( $$.value.integer <= 0 ){
			       warning($2.line_num,$2.col_num,
					"invalid length specification");
			       msg_tail(": substituting 1");
			       $$.value.integer = 1;
			     }
			     $$.left_token = add_tree_node(&($1),&($2),
							  (Token*)NULL);
			}
		;

/* 23 */
parameter_stmt	:	tok_PARAMETER '(' parameter_defn_list ')' EOS
		|	tok_PARAMETER parameter_defn_list  EOS
			{
			    if(f77_param_noparen)
				nonstandard($1.line_num,$1.col_num);
			}
   		;

parameter_defn_list:	parameter_defn_item
		|	parameter_defn_list ',' parameter_defn_item
		;

parameter_defn_item:	symbolic_name {complex_const_allowed = TRUE;}
				'=' parameter_expr
			{
			     def_parameter(&($1),&($4));
			     primary_id_expr(&($1),&($1));
			     assignment_stmt_type(&($1),&($3),&($4));
			     complex_const_allowed = FALSE;
			}
		;

/* 24 */
external_stmt	:	tok_EXTERNAL external_name_list EOS
		;

external_name_list:	symbolic_name
			{
			     def_ext_name(&($1));
			}
		|	external_name_list ',' symbolic_name
			{
			     def_ext_name(&($3));
			}
		;

/* 25 */
intrinsic_stmt	:	tok_INTRINSIC intrinsic_name_list EOS
		;

intrinsic_name_list:	symbolic_name
			{
			     def_intrins_name(&($1));
			}
		|	intrinsic_name_list ',' symbolic_name
			{
			     def_intrins_name(&($3));
			}
		;

        /* constructs for POINTER(pointer=pointee) statement */
pointer_stmt    :       tok_POINTER pointer_item_list EOS
		{
		  if(f77_cray_pointers)
		    nonstandard($1.line_num,$1.col_num);
		}
		;

pointer_item_list:      pointer_item
		|       pointer_item_list ',' pointer_item
		;

pointer_item    :       '(' pointer_name ',' pointee_name ')'
		;

pointer_name    :       symbolic_name
			{
			     declare_type(&($1),type_INTEGER,local_wordsize,
					  NULL );
			}
		;

pointee_name    :       symbolic_name
		        {
				/* Suppress set/used warnings since
				   often is accessed only via pointer */
		             use_lvalue(&($1));
		             use_variable(&($1));
		        }
		|       array_declarator
		        {
		             use_lvalue(&($1));
		             use_variable(&($1));
		        }
		;

/* 26 */
save_stmt	:	tok_SAVE EOS
			{
			  global_save = TRUE;
			}
		|	tok_SAVE save_list EOS
		;

save_list	:	save_item
		|	save_list ',' save_item
		;

save_item	:	symbolic_name
			{
			     save_variable(&($1));
			}
		|	'/' symbolic_name '/'
			{
/***			     def_com_block(&($2),(Token*)NULL);***/
			     save_com_block(&($2));
			}
		;

/* 27 */
data_stmt	:	tok_DATA data_defn_list EOS
   		;

data_defn_list	:	data_defn_item
		|	data_defn_list data_defn_item
		|	data_defn_list ',' data_defn_item
		;

data_defn_item	:	data_defn_assignee_list '/'
				{complex_const_allowed=TRUE;}
					data_value_list
				{complex_const_allowed=FALSE;}  '/'
		;

data_defn_assignee_list
		:	data_defn_assignee
		|	data_defn_assignee_list ',' data_defn_assignee
		;

data_defn_assignee:	lvalue
			{
			     use_lvalue(&($1));
			}
		|	data_implied_do_list
		;

data_value_list:	data_value
		|	data_value_list ',' data_value
		;

data_value	:	data_constant_value
		|	data_repeat_factor '*' data_constant_value
		;

data_repeat_factor:	nonzero_unsigned_int_const
		|	symbolic_name
			{
			     use_parameter(&($1));
			}
		;

data_constant_value:	data_constant
		|	symbolic_name
			{
			     use_parameter(&($1));
			}
		;


data_dlist	:	data_dlist_item
		|	data_dlist ',' data_dlist_item
		;

data_dlist_item	:	array_element_lvalue
			{
			     use_lvalue(&($1));
			}
		|	data_implied_do_list
		;

data_implied_do_list:  '(' data_dlist ',' symbolic_name
				'=' data_do_loop_bounds ')'
			{
			    use_implied_do_index(&($4));
			}
		;

data_do_loop_bounds:	int_constant_expr ',' int_constant_expr
		| int_constant_expr ',' int_constant_expr ',' int_constant_expr
		;


/* 29 */
assignment_stmt	:	lvalue '=' {complex_const_allowed = TRUE;
				    in_assignment_stmt = TRUE;} expr
			{
			  if( ! (is_true(LVALUE_EXPR,$1.TOK_flags)
			       || is_true(STMT_FUNCTION_EXPR,$1.TOK_flags) )) {
			    syntax_error($1.line_num,$1.col_num,
					 "left side is not assignable");
			  }
			  else {
			    assignment_stmt_type(&($1),&($2),
					&($4));
			  }
			  complex_const_allowed = FALSE;
			  in_assignment_stmt = FALSE;
			}
				 EOS
			{
				/* Clear u-b-s flags spuriously set */
			  if(is_true(STMT_FUNCTION_EXPR, $1.TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN)
			     stmt_function_stmt(&($1));
		        }
		;

lvalue		:	variable_name
		|	array_element_lvalue
		|	substring_lvalue
		|	stmt_function_handle
		;


/* array-element_lvalue is at 88 */

assign_stmt	:    	tok_ASSIGN pre_label label tok_TO variable_name EOS
			{
			    do_ASSIGN(&($5));
			}
		;


/* 31 */
unconditional_goto:	goto pre_label label EOS
		;

/* 32 */
computed_goto	:	goto '(' goto_list ')' integer_expr EOS
		|	goto '(' goto_list ')' ',' integer_expr EOS
		;

/* 33 */
assigned_goto	:	goto symbolic_name EOS
			{
			     do_assigned_GOTO(&($2));
			}
		|	goto symbolic_name '(' goto_list ')' EOS
			{
			     do_assigned_GOTO(&($2));
			}
		|	goto symbolic_name ',' '(' goto_list ')' EOS
			{
			     do_assigned_GOTO(&($2));
			}
		;

goto		:	tok_GOTO
			{
			    integer_context=TRUE;
			}
		|	tok_GO tok_TO
			{
			    integer_context=TRUE;
			}
		;

goto_list	:	pre_label label
		|	goto_list ',' pre_label label
		;

/* 34 */
arithmetic_if_stmt:	if_handle pre_label label ',' pre_label label
				 ',' pre_label label EOS
			{
			  int t=datatype_of($1.class);
			  if(t != type_INTEGER && t != type_REAL
			     && t != type_DP && t != type_ERROR ) {
			    syntax_error($1.line_num,$1.col_num,
		  "integer, real, or double precision expression required");
			  }
			}
		;

/* 35 */
logical_if_stmt	:	if_handle executable_stmt
			{
			  int t=datatype_of($1.TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error($1.line_num,$1.col_num,
					  "logical expression required");
			}
		;

/* 36 */
block_if_stmt	:	if_handle tok_THEN EOS
			{
			  int t=datatype_of($1.TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error($1.line_num,$1.col_num,
					  "logical expression required");
			}
		;

if_handle	:	tok_IF '(' {complex_const_allowed = TRUE;}  expr ')'
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;	/* for is_keyword */
			    $$ = $4; /* Inherit expr for type checking above */
			}
		;

/* 37 */
else_if_stmt	:	tok_ELSE block_if_stmt
		|	tok_ELSEIF '(' {complex_const_allowed = TRUE;} expr ')'
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;
			}
			tok_THEN EOS
		;

/* 38 */
else_stmt	:	tok_ELSE EOS
		;

/* 39 */
end_if_stmt	:	tok_ENDIF EOS
		|	tok_END tok_IF EOS
		;

/* 40 */
			/* Allow VAX/VMS extensions:
			   DO [label [,]] var = expr , expr [,expr]
			   DO [label [,]] WHILE ( expr )
			      ...
			   ENDDO
			*/

do_stmt		:	do_handle variable_name
				'=' do_loop_bounds EOS
			{
			  if( ! is_true(LVALUE_EXPR,$2.TOK_flags) ) {
			    syntax_error($2.line_num,$2.col_num,
					 "index is not assignable");
			  }
			  else {
			     use_lvalue(&($2));
			     use_variable(&($2));
			  }

				/* Check for non-integer DO index or bounds */
			     if(datatype_of($2.TOK_type) == type_INTEGER
				&& datatype_of($4.TOK_type) != type_INTEGER)
			       warning($3.line_num,$3.col_num,
				  "type mismatch between DO index and bounds");

			     else if(datatype_of($2.TOK_type) != type_INTEGER)
			       if(datatype_of($4.TOK_type) != type_INTEGER) {
				 if(port_real_do)
				   nonportable($4.line_num,$4.col_num,
					       "non-integer DO loop bounds");
			       }
			       else {
				 if(trunc_real_do_index)
				   warning($2.line_num,$2.col_num,
					   "DO index is not integer");
			       }
			}
		|	do_handle tok_WHILE '('
				{complex_const_allowed=TRUE;} expr ')' EOS
			{
			    if(is_true(ID_EXPR,$5.TOK_flags)){
				use_variable(&($5));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			}
		|	tok_DOWHILE '('
				{complex_const_allowed=TRUE;} expr ')' EOS
			{
			    if(is_true(ID_EXPR,$4.TOK_flags)){
				use_variable(&($4));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			}
		;

do_handle	:	tok_DO pre_label label
		|	tok_DO pre_label label ','
		|	tok_DO pre_label
			{
			    make_true(NONSTD_USAGE_FLAG,$$.TOK_flags);
			    integer_context=FALSE;
			}
		;

do_loop_bounds	:	int_real_dp_expr ',' int_real_dp_expr
			{
			    $$.TOK_type=do_bounds_type(&($1),&($3),&($3));
			}
		|   int_real_dp_expr ',' int_real_dp_expr ',' int_real_dp_expr
			{
			    $$.TOK_type=do_bounds_type(&($1),&($3),&($5));
			}
		;

enddo_stmt	:	tok_END tok_DO EOS
		|	tok_ENDDO EOS
		;

/* 41 */
continue_stmt	:	tok_CONTINUE EOS
		;

/* 42 */
stop_stmt	:	tok_STOP stop_info EOS
		;

/* 43 */
pause_stmt	:	tok_PAUSE stop_info EOS
		;

stop_info	:	/* empty */
		|	tok_integer_const
		|	symbolic_name
			{
			     use_variable(&($1));
			}
		|	tok_string
		;

/* 44 */
write_stmt	:	write_handle
				{complex_const_allowed = FALSE;} EOS
		|	write_handle io_list
				{complex_const_allowed = FALSE;} EOS
		;

write_handle	:	tok_WRITE {init_io_ctrl_list();}
				'(' control_info_list ')'
				{complex_const_allowed = TRUE;}
		;

/* 45 */
		/* Note that parenthesized format_id's will end up in
		   control_info_list. Disambiguation left to semantic phase.
		   This is why we need the optional comma */
read_stmt	:	read_handle '(' control_info_list ')' EOS
		|	read_handle '(' control_info_list ')' io_list EOS
		|	read_handle '(' control_info_list ')' ',' io_list EOS
		|	read_handle format_id EOS
		|	read_handle format_id ',' io_list EOS
		;
read_handle	:	tok_READ {init_io_ctrl_list();}
		;

accept_stmt	:	tok_ACCEPT format_id EOS
			{
			    if(f77_accept_type)
				nonstandard($1.line_num,$1.col_num);
			}
		|	tok_ACCEPT format_id ',' io_list EOS
			{
			    if(f77_accept_type)
				nonstandard($1.line_num,$1.col_num);
			}
		;

/* 46 */
print_stmt	:	tok_PRINT format_id EOS
   		|	tok_PRINT format_id ','
				{complex_const_allowed = TRUE;} io_list
				{complex_const_allowed = FALSE;}  EOS
		;

type_output_stmt:	tok_TYPE format_id EOS
			{
			    if(f77_accept_type)
				nonstandard($1.line_num,$1.col_num);
			}
   		|	tok_TYPE format_id ','
				{complex_const_allowed = TRUE;} io_list
				{complex_const_allowed = FALSE;}  EOS
			{
			    if(f77_accept_type)
				nonstandard($1.line_num,$1.col_num);
			}
		;

/* 47 */
control_info_list:	control_info_item
			{
			    ++control_item_count;
			}
		|	control_info_list ',' control_info_item
			{
			    ++control_item_count;
			    if(! io_warning_given) {
			      if( io_internal_file ) {
				if( (curr_stmt_class == tok_WRITE ||
				     curr_stmt_class == tok_READ) &&
				    io_list_directed ) {
				  if(f77_internal_list_io) {
				    nonstandard($3.line_num,$3.col_num);
	    msg_tail(": internal file cannot be used with list-directed I/O");
				  }
				  io_warning_given = TRUE;
				}
			      }
			    }
			}
		;

	/* Note that unit id is not distinguished from format id
	   by the grammar. Use sequence no. to tell which is which.
	 */
control_info_item:	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			}
		|	unit_id
			{
			    if( $1.class == '*' ) {
			      if(control_item_count == 1) /* format id */
				{
				  io_list_directed = TRUE;
				}
			    }
			    else if( is_true(ID_EXPR,$1.TOK_flags)){

					/* Handle special cases */
				if(control_item_count == 0 &&
				 datatype_of($1.TOK_type) == type_STRING) {
					/* unit id=char variable is
					   an internal file.  I/O goes in
					   and out of the variable. */
				  io_internal_file = TRUE;
				  if(curr_stmt_class == tok_WRITE) {
				    use_lvalue(&($1));
				  }
				}

					/* format id=namelist means
					   I/O with variables of namelist. */
				else if( control_item_count == 1 &&
				 datatype_of($1.TOK_type) == type_NAMELIST) {
				    ref_namelist(&($1),curr_stmt_class);
				}

					/* Handle use of variable */
				use_variable(&($1));
			    }
			}
		;

			/* OPEN stmt needs its own control list defn to
			   allow for VMS READONLY and similar keywords.
			   Special prodn for unit_id as optional 1st item
			   needed to avoid reduce/reduce conflict with
			   later-occurring symbolic_name items.   */
open_info_list	:	unit_id
			{
			    if( $1.class != '*'
			       && is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    ++control_item_count;
			}
		|	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			    ++control_item_count;
			}
		|	open_info_list ',' open_info_item
			{
			    ++control_item_count;
			}
		;

open_info_item	:	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			}
		|	symbolic_name	/* NOSPANBLOCKS, READONLY or SHARED */
			{
			    use_special_open_keywd(&($1));
			}
		;

/* 48 */
io_list		:	io_item
		|	io_list ',' io_item
		;

io_item		:	expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				if( curr_stmt_class == tok_READ ||
				    curr_stmt_class == tok_ACCEPT )
				    use_lvalue(&($1));
				else
				    use_variable(&($1));
			    }
			}
		|	io_implied_do_list
		;

/* 49 */
io_implied_do_list:	'(' io_list ',' variable_name '=' do_loop_bounds ')'
			{
			  if( ! is_true(LVALUE_EXPR,$4.TOK_flags) ) {
			    syntax_error($4.line_num,$4.col_num,
					 "index is not assignable");
			  }
			  else {
			     use_implied_do_index(&($4));
			  }
			}
		;

/* 50 */
open_stmt	:	tok_OPEN {init_io_ctrl_list();}
				 '(' open_info_list ')' EOS
		;

/* 51 */
close_stmt	:	tok_CLOSE {init_io_ctrl_list();}
				'(' control_info_list ')' EOS
		;

/* 52 */
inquire_stmt	:	tok_INQUIRE {init_io_ctrl_list();}
				'(' control_info_list ')' EOS
		;

/* 53 */
backspace_stmt	:	backspace_handle unit_id EOS
			{
			    if( $2.class != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			}
		|	backspace_handle '(' control_info_list ')' EOS
		;
backspace_handle:	tok_BACKSPACE {init_io_ctrl_list();}
		;

/* 54 */
endfile_stmt	:	endfile_handle unit_id EOS
			{
			    if( $2.class != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			}
		|	endfile_handle '(' control_info_list ')' EOS
		;
endfile_handle	:	tok_ENDFILE {init_io_ctrl_list();}
		|	tok_END tok_FILE {init_io_ctrl_list();}
		;

/* 55 */
rewind_stmt	:	rewind_handle unit_id EOS
			{
			    if( $2.class != '*'
			       && is_true(ID_EXPR,$2.TOK_flags)){
				use_variable(&($2));
			    }
			}
		|	rewind_handle '(' control_info_list ')' EOS
		;
rewind_handle	:	tok_REWIND {init_io_ctrl_list();}
		;


/* 56 */
		/* "expr" causes shift/reduce conflict on ')' between
		   red'n  unit_id: expr_  and shift  primary: ( expr_ ).
		   Use "associativity" rule to force reduction */
unit_id		:	expr		%prec REDUCE
		|	'*'
		;

/* 57 */
format_id	:	char_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				 use_variable(&($1));
			    }
			}
		|	'*'
		;

/* 58,59 */
format_stmt	:	tok_FORMAT {inside_format=TRUE;} '(' format_spec ')' EOS
			{
			  inside_format=FALSE;
			}
		;

/* 60-69 */
format_spec	:		/* EMPTY */
		|	nonempty_format_spec
		;


nonempty_format_spec:	fmt_spec_item
		|	nonempty_format_spec fmt_spec_item
		;

fmt_spec_item	:	repeatable_fmt_item
		|	unrepeatable_fmt_item
		|	fmt_item_separator
		;

repeatable_fmt_item:	'(' nonempty_format_spec ')'
		|	tok_edit_descriptor
		;

unrepeatable_fmt_item:	tok_string
		|	tok_hollerith
		|	repeat_spec
		|	variable_fmt_item
		;

fmt_item_separator:	','
		|	'/'
		|	tok_concat	/* since lexer spots "//" */
		|	':'
		|	'.'		/* Occurs when variable w.d is used */
		|	nonstandard_fmt_item
			{
			  if(f77_format_dollarsigns)
			     nonstandard($1.line_num,$1.col_num);
			}
		;

nonstandard_fmt_item: '$'	/* VMS uses this */
		;

repeat_spec	:	tok_integer_const
		|	'-' tok_integer_const	/* for kP descriptor */
		|	'+' tok_integer_const	/* for +kP descriptor */
		;

		/* VMS-style variable format size or repeat spec*/
variable_fmt_item:	'<' {inside_format=FALSE;} integer_expr
				{inside_format=TRUE;} '>'
			{
			  if(f77_variable_format)
			     nonstandard($1.line_num,$1.col_num);
			}
		;

/* 70 handle only: complete defn handled as assignment stmt */

stmt_function_handle:	scalar_name '(' stmt_function_dummy_list ')'
			{
			  check_stmt_sequence(&($1),SEQ_STMT_FUN);

				def_stmt_function(&($1),&($3));
					/* make token info */
				primary_id_expr(&($1),&($$));
#ifdef DEBUG_PARSER
				if(debug_parser)
				  print_exprlist("stmt function",&($3));
#endif
			}
		;

stmt_function_dummy_list: /* empty list */
			{
			    $$.next_token = (Token*)NULL;
			}
		| nonempty_stmt_fun_dummy_list
		;

nonempty_stmt_fun_dummy_list:	  stmt_function_dummy_arg
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	  nonempty_stmt_fun_dummy_list ','
					stmt_function_dummy_arg
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

stmt_function_dummy_arg:  variable_name	/* for now: later, handle correctly */
		;

/* 71 */
call_stmt	:	call_handle
			{
			     call_subr(&($1),(Token*)NULL);
			     complex_const_allowed = FALSE;
			} EOS

		|	call_handle '(' ')'
			{
			     call_subr(&($1),(Token*)NULL);
			     complex_const_allowed = FALSE;
			} EOS

		|	call_handle '(' subr_arg_list ')'
			{
			     call_subr(&($1),&($3));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("call stmt",&($3));
#endif
			     complex_const_allowed = FALSE;
			} EOS
		;

call_handle	:	tok_CALL symbolic_name
			{
			     complex_const_allowed = TRUE;
			     $$ = $2;
			}
		;
subr_arg_list:		subr_arg
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			    $$.left_token = (Token *)NULL;
			}
		|	subr_arg_list ',' subr_arg
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

subr_arg	:	expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				 use_actual_arg(&($1));
				 use_variable(&($1));
			    }
			}
		|	'*' pre_label label
			{
			  $$ = $3;
			  $$.left_token = (Token *)NULL;
			}
		;

/* 72 */
return_stmt	:	tok_RETURN EOS
			{
			     do_RETURN(current_module_hash,&($1));
			}
		|	tok_RETURN integer_expr EOS
			{
			     do_RETURN(current_module_hash,&($1));
			}
		;

/* 73 */
function_reference:	fun_or_substr_handle '(' fun_arg_list ')'
			{
				   /* restore context */
				if(!is_true(COMPLEX_FLAG,$1.TOK_flags))
				  complex_const_allowed=FALSE;
				if(is_true(IN_ASSIGN,$1.TOK_flags))
				  in_assignment_stmt = TRUE;

				  /* Change empty arg list to no arg list */
				if($3.next_token == NULL)
				  call_func(&($1),(Token *)NULL);
				else
				  call_func(&($1),&($3));
							/* make token info */
				func_ref_expr(&($1),&($3),&($$));
				/* Substitute empty token for null arglist */
				$$.left_token = add_tree_node(
						   &($2),&($1),
						   ($3.next_token == NULL?
						    empty_token(&($3)) :
						    $3.next_token) );
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("function",&($3));
#endif
			}
		;

fun_or_substr_handle:	scalar_name
			{
			  if(complex_const_allowed)/* save context */
			    make_true(COMPLEX_FLAG,$$.TOK_flags);
			  complex_const_allowed=TRUE;
			  if(in_assignment_stmt)
			    make_true(IN_ASSIGN,$$.TOK_flags);
			  in_assignment_stmt = FALSE;
			}
		;
fun_arg_list	:	/* empty */
			{
				$$.class = 0;
				$$.next_token = (Token *)NULL;
				$$.left_token = (Token *)NULL;
			}
		|	nonempty_fun_arg_list
		;

nonempty_fun_arg_list:	expr
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			    $$.left_token = (Token *)NULL;
			}
		|	nonempty_fun_arg_list ',' expr
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}

/* 74 not present: type checking not done at this level */

/* 75 was constant_expr, but only used by PARAMETER */
parameter_expr	:	/* arith, char, or logical */ expr
			{
			  int t=datatype_of($1.TOK_type);
			  if( t != type_ERROR){
			    if( ! is_const_type(t) ) {
			      syntax_error($1.line_num,$1.col_num,
		      "arithmetic, char, or logical expression expected");
			    }
			    else {
			      if( !is_true(PARAMETER_EXPR,$1.TOK_flags) ) {
				syntax_error($1.line_num,$1.col_num,
					   "constant expression expected");
			      }
			    /* Here we allow, with some warnings, expr
			       containing intrins func or **REAL in
			       PARAMETER defn. */
			      else if( !is_true(CONST_EXPR,$1.TOK_flags) ) {
				if(f77_param_intrinsic) {
				  nonstandard($1.line_num,$1.col_num);
				  msg_tail(
			 "intrinsic function or **REAL in PARAMETER defn");
				}
			      }
			    }
			  }
			}
		;

/* 76 following the text of the standard, not the diagrams */
expr		:	log_expr
			{
				/* Fix it up in case it is used in expr list */
			  $$.next_token = (Token *) NULL;
#ifdef DEBUG_PARSER
			    if(debug_parser) {
				(void)fprintf(list_fd,
					"\nexpr: class=0x%x subclass=0x%x",
					$1.class,
					$1.subclass);
			    }
#endif
			}
		;

log_expr	:	log_disjunct

		|	expr tok_EQV log_disjunct
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		|	expr tok_NEQV log_disjunct
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_disjunct	:	log_term

		|	log_disjunct tok_OR log_term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_term	:	log_factor

		|	log_term tok_AND log_factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_factor	:	log_primary

		|	tok_NOT log_primary
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		;

log_primary	:	arith_expr

		|	log_primary tok_relop log_primary
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;


arith_expr	:	term

		|	'-' term
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		|	'+' term
			{
			    do_unexpr(&($1),&($2),&($$));
			}
		|	arith_expr '+' term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		|	arith_expr '-' term
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

term		:	factor

		|	term '/' factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			    if(div_check &&
			       !is_true(CONST_EXPR,$3.TOK_flags)){
				warning($2.line_num,$2.col_num,
					"Possible division by zero");
			    }
			}
		|	term '*' factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

factor		:	char_expr

		|	char_expr tok_power factor
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

char_expr	:	primary

		|	char_expr tok_concat primary
			{
			    do_binexpr(&($1),&($2),&($3)
					 ,&($$));
			}
		;

primary		:	variable_name

		|	array_element_name

		|	function_reference

		|	substring_name

		|	literal_const
			{
			    $$.TOK_flags = 0;
			    $$.left_token = (Token *)NULL;
			    make_true(CONST_EXPR,$$.TOK_flags);
			    make_true(PARAMETER_EXPR,$$.TOK_flags);
			    make_true(LIT_CONST,$$.TOK_flags);
			    make_true(EVALUATED_EXPR,$$.TOK_flags);
			}
		|	'(' expr ')'
			{
			    $$ = $2;
				/* (identifier) becomes a non-identifier */
			    if(is_true(LVALUE_EXPR,$2.TOK_flags)) {
				if(pretty_parens) {
				  ugly_code($2.line_num,$2.col_num,
					  "Extraneous parentheses");
				}
				use_variable(&($2));
				make_false(LVALUE_EXPR,$$.TOK_flags);
				make_false(ARRAY_ID_EXPR,$$.TOK_flags);
				make_false(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				make_false(ID_EXPR,$$.TOK_flags);
			    }
				/* (expr) becomes tree node with root = '(' */
			    $$.left_token = add_tree_node(&($1),&($2),
							  (Token*)NULL);
			}
		;

				/* Literal constants are numbers, strings
				   holleriths, and logical constants */
literal_const	:	numeric_const
			{
			    /* (class is set in numeric_const productions) */
			    $$.size = size_DEFAULT;
			}
		|	tok_string
			{
			    $$.TOK_type = type_byte(class_VAR,type_STRING);
			    /* (size is set in get_string) */
			}
		|	tok_hollerith
			{
			    $$.TOK_type = type_byte(class_VAR,type_HOLLERITH);
			    /* (size is set in get_hollerith) */
			    if(port_hollerith) {
				warning($1.line_num,$1.col_num,
				"hollerith constant may not be portable");
			    }
			}
		|	tok_logical_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_LOGICAL);
			    $$.size = size_DEFAULT;
			}
		;

numeric_const	:	tok_integer_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_INTEGER);
			}
		|	tok_real_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_REAL);
			}
		|	tok_dp_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_DP);
			}
		|	tok_complex_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_COMPLEX);
			}
		|	tok_dcomplex_const
			{
			    $$.TOK_type = type_byte(class_VAR,type_DCOMPLEX);
			}
		;

/* 77 */
integer_expr	:	/* integer */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    if(datatype_of($1.TOK_type) != type_INTEGER) {
				syntax_error(
					$1.line_num,$1.col_num,
					"expression must be integer type");
			    }
			}
		;

/* 78 */
int_real_dp_expr:	/* integer, real, or double */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    {
				int t=datatype_of($1.TOK_type);
				    if(t != type_INTEGER && t != type_REAL
					&& t != type_DP ) {
					syntax_error(
					  $1.line_num,$1.col_num,
		"expression must be integer, real, or double precision type");
			    	    }
			    }
			}
		;

/* 79 absent */

/* 80 */
int_constant_expr:	/* integer const */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }
			    if( ! is_true(CONST_EXPR,$1.TOK_flags) ) {
				syntax_error(
					$1.line_num,$1.col_num,
					"constant expression expected");
			    }
			    else {
			      if(datatype_of($1.TOK_type) != type_INTEGER){
				syntax_error(
					$1.line_num,$1.col_num,
					"integer expression expected");
			      }
			      else {
				$$.value.integer = int_expr_value(&($1));
			      }
			    }
			}
		;

/* 81 */
dim_bound_expr	:       /* integer */  arith_expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				use_variable(&($1));
			    }

			    if( datatype_of($1.TOK_type) != type_INTEGER ){
				syntax_error(
					$1.line_num,$1.col_num,
					"integer dimension expected");
				$$.value.integer = 0;
			    }
			    else {
			      if( is_true(EVALUATED_EXPR,$1.TOK_flags) )
				$$.value.integer =
				  int_expr_value(&($1));
			      else		/* must be dummy */
				$$.value.integer = 0;
			    }
			}
		;

/* 82-85 absent: no type checking here */
/* 86-87 absent: see 76 */

/* 88 */
array_element_lvalue:	array_name '(' subscript_list ')'
			{
				ref_array(&($1),&($3));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array lvalue",&($3));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,$$.TOK_flags);
				make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				$$.left_token = add_tree_node(
						   &($2),&($1),$3.next_token);
				$$.next_token = (Token *) NULL;
			}
		;

array_element_name:	array_name '(' subscript_list ')'
			{
				ref_array(&($1),&($3));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array",&($3));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,$$.TOK_flags);
				make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
				$$.left_token = add_tree_node(
						   &($2),&($1),$3.next_token);
				$$.next_token = (Token *) NULL;
			}
		;

subscript_list	:	subscript
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	subscript_list ',' subscript
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		     ;

subscript	:	expr
			{
			    if(is_true(ID_EXPR,$1.TOK_flags)){
				 use_variable(&($1));
			    }
				/* check subscript exprs for integer type */
			    if(datatype_of($1.TOK_type) != type_INTEGER)
			      if(trunc_real_subscript)
			         warning($1.line_num,$1.col_num,
					 "subscript is not integer");
			}
		;

/* 89 */
substring_name	:	fun_or_substr_handle  substring_interval
			{
				   /* restore status of complex flag */
			    if(!is_true(COMPLEX_FLAG,$1.TOK_flags))
				  complex_const_allowed=FALSE;
				/* set flag to keep more than just id for
				   arg list text */
			    if(is_true(ID_EXPR,$1.TOK_flags))
			       make_true(ARRAY_ELEMENT_EXPR,$$.TOK_flags);
			    $$.size=substring_size(&($1),&($2));
			    $$.left_token = add_tree_node(
					       &save_token,&($1),&($2));
			    $$.next_token = (Token *) NULL;
			}

		|	function_reference  substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			    $$.left_token = add_tree_node(
					       &save_token,&($1),&($2));
			    $$.next_token = (Token *) NULL;
			}

		|	array_element_name substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			    $$.left_token = add_tree_node(
					       &save_token,&($1),&($2));
			    $$.next_token = (Token *) NULL;
			}
		;

substring_lvalue:	scalar_name substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			}
		|	array_element_lvalue substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			}
		;

			/* substring interval: limits go into
			   TOK_start, TOK_end.  */

substring_interval:	'(' ':' ')'
			{
			    $$.TOK_start=1;
			    $$.TOK_end=0; /* 0 means LEN */

			    save_token = $1; /* Save the paren for tree node */
			    $$.left_token =
			      add_tree_node(&($2),
				     empty_token(&($1)),empty_token(&($3)));
				/* Nullify next_token so it looks like
				   a tokenlist */
			    $$.next_token = (Token *)NULL;
			}

		  |	'(' substr_index_expr ':' ')'
			{
			    $$.TOK_start=$2.value.integer;
			    $$.TOK_end=0; /* 0 means LEN */

			    save_token = $1; /* Save the paren for tree node */
			    $$.left_token =
			      add_tree_node(&($3),&($2),empty_token(&($4)));
			    $$.next_token = (Token *)NULL;
			}
		  |	'(' ':' substr_index_expr ')'
			{
			    $$.TOK_start=1;
			    $$.TOK_end=$3.value.integer;

			    save_token = $1; /* Save the paren for tree node */
			    $$.left_token =
			      add_tree_node(&($2),empty_token(&($1)),&($3));
			    $$.next_token = (Token *)NULL;
			}
		  |	'(' substr_index_expr ':' substr_index_expr ')'
			{
			    $$.TOK_start=$2.value.integer;
			    $$.TOK_end=$4.value.integer;

			    save_token = $1; /* Save the paren for tree node */
			    $$.left_token =
			      add_tree_node(&($3),&($2),&($4));
			    $$.next_token = (Token *)NULL;
			}
		  ;

substr_index_expr:	arith_expr
			{
			  if(is_true(ID_EXPR,$1.TOK_flags)){
			    use_variable(&($1));
			  }
				/* check validity and replace nonconst
				   value by size_UNKNOWN. */
			  if(is_true(CONST_EXPR,$1.TOK_flags)) {
			    if( ($$.value.integer=int_expr_value(&($1))) < 1) {
			      syntax_error($1.line_num,$1.col_num,
					   "invalid substring index");
			    }
			  }
			  else  /* (no longer need ID hash index) */
			    $$.value.integer=size_UNKNOWN;
			}
		;

/* 90-98 absent: name categories not distinguished */

/* 99 */
variable_name	:	scalar_name
		|	array_name
		;

scalar_name	:	tok_identifier
			{
			    ref_variable(&($1));
			    primary_id_expr(&($1),&($$));
			}
		;

array_name	:	tok_array_identifier
			{
			    ref_variable(&($1));
			    primary_id_expr(&($1),&($$));
			}
		;


/* symbolic_name refers to a name without making it into an id expr */
symbolic_name	:	tok_identifier
		|	tok_array_identifier
		;

/* 100 */
data_constant	:	numeric_const
		|	'-' numeric_const
		|	'+' numeric_const
		|	tok_logical_const
   		|	tok_string
		|	tok_hollerith
		;

/* 101-102 absent */

/* 103 */
nonzero_unsigned_int_const:
			tok_integer_const
			{
			  if($1.value.integer == 0) {
			    warning($1.line_num,$1.col_num,
				    "nonzero integer expected");
			    msg_tail(": substituting 1");
			    $$.value.integer = 1;
			  }
			  $$.left_token = (Token *)NULL;
			}
		;

/* 104-109 absent: lexer handles these */
	/* pre_label prepares for an expected label by setting flag
	   so that lexer won't look for E-format number.  All grammar
	   rules that have "label" precede it with "pre_label" */
pre_label	:	/* NOTHING */
			{
			    integer_context=TRUE;
			}
		;

/* 110 */
label		:	tok_integer_const
			{
				integer_context=FALSE;
				$$.TOK_type = type_byte(class_LABEL,type_LABEL);
				$$.size = size_DEFAULT;
				$$.TOK_flags = 0;
			}
		;

/* 111-116 absent: lexer handles these */

%%

void
init_parser(VOID)			/* Initialize various flags & counters */
{
	initial_flag = TRUE;	/* set flag for keyword test */
	implicit_flag=FALSE;	/* clear flags for IMPLICIT stmt */
	implicit_letter_flag = FALSE;
	implicit_type_given = FALSE;
	implicit_none = FALSE;
	global_save = FALSE;
	prev_token_class = EOS;
	complex_const_allowed = FALSE;
	stmt_sequence_no = 0;
	true_prev_stmt_line_num = 0;
}

				/* Handle unary expressions: link
				   into a tree and propagate type.
				 */
PRIVATE void
#if HAVE_STDC
do_unexpr(Token *op, Token *expr, Token *result)
#else /* K&R style */
do_unexpr(op,expr,result)
     Token *op,*expr,*result;
#endif /* HAVE_STDC */
{
  unexpr_type(op,expr,result);

  result->left_token = add_tree_node(op, expr, (Token*)NULL);
}
				/* Handle binary expressions: link
				   into a tree and propagate type.
				 */
PRIVATE void
#if HAVE_STDC
do_binexpr(Token *l_expr, Token *op, Token *r_expr, Token *result)
#else /* K&R style */
do_binexpr(l_expr,op,r_expr,result)
     Token *l_expr,*op,*r_expr,*result;
#endif /* HAVE_STDC */
{
  binexpr_type(l_expr,op,r_expr,result); /* Propagate the type */

  result->left_token = add_tree_node(op, l_expr, r_expr);
}


			/* Changes a token to empty and replaces
			   src_text by null string, value by 0.  Other
			   info (line, col, etc.)  unchanged. */

PRIVATE Token *
#if HAVE_STDC
empty_token(Token *t)
#else /* K&R style */
empty_token(t)
     Token *t;
#endif /* HAVE_STDC */
{
#ifdef DEBUG_EMPTY_TOKEN
  static char *nullstring="(empty)"; /* for debugging.  */
#else
  static char *nullstring=""; /* for operation.  */
#endif
  t->class = tok_empty;
  t->subclass = 0;
  t->value.integer = 0;
  t->left_token = (Token *) NULL;
  t->src_text = nullstring;

  return t;
}

		/* Propagate non-integer type if any of DO loop
		   bounds are non-integer. */
PRIVATE int
#if HAVE_STDC
do_bounds_type(Token *t1, Token *t2, Token *t3)
#else /* K&R style */
do_bounds_type(t1,t2,t3)
     Token *t1, *t2, *t3;
#endif /* HAVE_STDC */
{
  int result_type;
       if(datatype_of(t1->TOK_type) != type_INTEGER)result_type = t1->TOK_type;
  else if(datatype_of(t2->TOK_type) != type_INTEGER)result_type = t2->TOK_type;
  else if(datatype_of(t3->TOK_type) != type_INTEGER)result_type = t3->TOK_type;
  else result_type = t1->TOK_type;
  return result_type;
}


/* Debugging routine: prints the expression list of various productions */
#ifdef DEBUG_PARSER
PRIVATE void
print_exprlist(s,t)
	char *s;
	Token *t;
{

	(void)fprintf(list_fd,"\n%s arglist: ",s);

	if(t == NULL)
		(void)fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name[datatype_of(t->TOK_type)]);
		  if( is_true(ID_EXPR,t->TOK_flags) )
			(void)fprintf(list_fd,"(%s) ",token_name(*t));
	    }
	}
}

PRIVATE void
print_comlist(s,t)
	char *s;
	Token *t;
{

	(void)fprintf(list_fd,"\n%s varlist: ",s);

	if(t == NULL)
		(void)fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name[datatype_of(t->TOK_type)]);
		  if( is_true(ID_EXPR,t->TOK_flags) )
			(void)fprintf(list_fd,"(%s) ",token_name(*t));
		}
	  }
}
#endif

/* After having parsed prog_stmt, function_stmt, subroutine_stmt,
   block_data_stmt, the stmt_sequence_no is set to the value SEQ_HEADER.
*/

void
#if HAVE_STDC
check_seq_header(Token *t)
#else /* K&R style */
check_seq_header(t)
     Token *t;
#endif /* HAVE_STDC */
{
	if(stmt_sequence_no >= SEQ_HEADER) {
	   syntax_error( (t == (Token *) NULL? line_num: t->line_num),
			NO_COL_NUM,
			"missing END statement inserted");
	   msg_tail( (t == (Token *) NULL? "at end of file":
		      "prior to statement") );

	   END_processing(t);
	}
	stmt_sequence_no = SEQ_HEADER;
}

PRIVATE void
#if HAVE_STDC
check_stmt_sequence(Token *t, int seq_num)
#else /* K&R style */
check_stmt_sequence(t,seq_num)
     Token *t;
     int seq_num;
#endif /* HAVE_STDC */
{
    if(stmt_sequence_no > seq_num) {
	 syntax_error(t->line_num, NO_COL_NUM,
		      "Statement out of order.");
    }
    else {
	stmt_sequence_no = seq_num;
    }
}

PRIVATE void
init_io_ctrl_list(VOID)
{
  control_item_count = 0;
  io_internal_file = FALSE;
  io_list_directed = FALSE;
  io_warning_given = FALSE;
}


	/* After having parsed end_stmt, common block lists and
	   subprogram argument lists are copied over into global symbol
	   table, the local symbol table is printed out and then cleared,
	   and stmt_sequence_no is set to zero for start of next module.
	*/

PRIVATE void
#if HAVE_STDC
END_processing(Token *t)
#else /* K&R style */
END_processing(t)
	Token *t;
#endif /* HAVE_STDC */
{
  ++tot_module_count;
  if(current_module_hash != -1) {
        if(exec_stmt_count == 0 &&
	   current_module_type != type_BLOCK_DATA) {
	  warning(t == (Token *)NULL? line_num: t->line_num, NO_COL_NUM,
		  "Module contains no executable statements");
	}
	if(do_list && t != (Token *)NULL)
	    (void)flush_line_out(t->line_num);
	check_loose_ends(current_module_hash);
	process_lists(current_module_hash);
	debug_symtabs();
	print_loc_symbols(current_module_hash);
	init_symtab();
  }
  exec_stmt_count = 0;
  stmt_sequence_no = 0;
  current_module_hash = -1;
  implicit_type_given = FALSE;
  implicit_none = FALSE;
  true_prev_stmt_line_num = 0;
  integer_context = FALSE;
  global_save = FALSE;
}

		/* Routine to create a node for an expr tree.  Returns
		   a pointer to the newly created node.
		 */
PRIVATE Token *
#if HAVE_STDC
add_tree_node(Token *node, Token *left, Token *right)
#else /* K&R style */
add_tree_node(node,left,right)
     Token *node,*left,*right;
#endif /* HAVE_STDC */
{
  Token *new_node, *new_left, *new_right;

  new_node=new_token();

  *new_node = *node;		/* Make a permanent copy of root */

		/* Add the children.  If child's left_token pointer is
		   null, then that expression is a primary.  Otherwise
		   it is the root node of a subtree.
		 */
  if(left->left_token == (Token *)NULL) {
    new_left=new_token();
    *new_left = *left;			/* Copy primary to permanent space */
  }
  else {
    new_left = left->left_token;	/* No copying needed in this case */
  }

  if(right == (Token *)NULL) {
    new_right = (Token *)NULL;		/* No right child */
  }
  else if(right->left_token == (Token *)NULL
	  || node->class == '(') { /* Paren means right child is expr list */
    new_right=new_token();
    *new_right = *right;		/* Copy primary to permanent space */
  }
  else {
    new_right = right->left_token;	/* No copying needed in this case */
  }

  new_node->left_token = new_left;	/* Link children onto the new root */
  new_node->next_token = new_right;
  return new_node;
}

		/* Routine to add token t to the front of a token list. */
PRIVATE Token *
#if HAVE_STDC
append_token(Token *tlist, Token *t)
#else /* K&R style */
append_token(tlist,t)
     Token *tlist, *t;
#endif /* HAVE_STDC */
{
	Token *tcopy;

	tcopy=new_token();

	*tcopy = *t;		/* make permanent copy of token */
	tcopy->next_token = tlist; /* link it onto front of list */
	return tcopy;		/* return it as new tlist */
}
