/* symtab.c:

Contains formerly separate modules:
   I. Symtab: symbol table maintenance routines.
  II. Hash:  hash table functions: hash(), kwd_hash(), rehash()
 III. Intrins: handles recognition & data typing of intrinsic functions.


    Copyright (C) 1992 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


*/

/*
  I. Symtab


		Symbol table routines for Fortran program checker.

	  Shared functions defined:


	   call_func(id,arg)	 Handles function invocations.
	   call_subr(id,arg)	 Handles CALL statements.
	   declare_type(id,datatype,size) Handles TYPE statements.
	   def_arg_name(id)	 Handles func/subr argument lists.
	   def_array_dim(id,arg) Handles dimensioning declarations.
	   def_com_block(id)	 Handles common blocks and SAVE stmts.
	   def_com_variable(id)	 Handles common block lists.
       int def_curr_module(id)	 Identifies symbol as current module.
     	   def_equiv_name(id)	 Initializes equivalence list items.
	   def_ext_name(id)	 Handles external lists.
	   def_function(datatype,size,size_text,id,args)
	   		Installs function name in global table.
	   def_intrins_name(id)  Handles intrinsic lists.
	   def_parameter(id,value) Handles parameter_defn_item
	   def_stmt_function(id) Declares a statement function.
	   do_ASSIGN(id)	 Handles ASSIGN stmts.
	   do_assigned_GOTO(id)	 Handles assigned GOTO.
	   do_ENTRY(id,args,hashno) Processes ENTRY statement.
	   do_RETURN(hashno,keyword) Processes RETURN statement.
	   equivalence(id1,id2)	 equivalences two variables
       int get_type(symt)	 Finds out data type of symbol, or uses implicit
				 typing to establish its type.
       int get_size(symt,type)	 Finds out size of symbol's datatype.
	unsigned hash_lookup(s)	 Looks up identifier in hashtable.
	   init_globals()	 Initializes global symbol info.
	   init_symtab()	 Clears local symbol table & removes locals
				 from stringspace. Also restores default
				 implicit data typing.
 Gsymtab* install_global(t,datatype,storage_class) Installs indentifier in
				global symbol table.
 Lsymtab* install_local(t,datatype,storage_class) Installs indentifier in
				local symbol table.
ArgListHeader* make_arg_array(t) Converts list of tokens into list of
				 type-flag pairs.
ArgListHeader* make_dummy_arg_array(t) Converts list of tokens into list of
				 type-flag pairs.
ArgListHeader* make_arrayless_alist() Sets up argument list header for
				EXTERNAL decl or subprog as actual arg.

ComListHeader* make_com_array(t) Converts list of common block tokens into
				 list of dimen_info-type pairs.
	   process_lists()	 Places pointer to linked list of arrays in
				 global symbol table
	   ref_array(id,subscrs) Handles array references
	   ref_variable(id)	 Handles accessing variable name.
	   set_implicit_type(type,size,c1,c2) Processes IMPLICIT statement.
	   stmt_function_stmt(id) Finishes processing stmt func defn.
    char * token_name(t)	 Returns ptr to token's symbol's name.
	   use_actual_arg(id)	 Handles using a variable as actual arg.
	   use_io_keyword(id_keywd,id_val,class) Handles i/o control specifier.
	   use_len_arg(id)	 Handles arguments passed to LEN.
	   use_lvalue(id)	 Handles assignment to a variable.
	   use_parameter(id)	 Handles data_constant_value &
				 data_repeat_factor.
	   use_variable(id)	 Sets used-flag for a variable used in expr.

*/

/*  private functions defined:
 arg_count(t)		Counts the number of arguments in a token list.
 call_external(symt,id,arg)	places token list of args into local symtab
 check_intrins_args(arg, defn) Checks call seq of intrinsic functions
 check_stmt_function_args(symt,id,arg)  ditto for statement functions
 find_intrinsic()		Looks up intrinsic functions in table
 find_io_keyword()		Looks up i/o control spec keywords
 reverse_tokenlist(t)		Reverses a linked list of tokens
 make_TL_head();		Initializes a tokenlist header
 new_tokhead();			Allocates space for a tokenlist header
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#define SYMTAB
#include "ftnchek.h"
#include "symtab.h"
#include "tokdefs.h"

#ifdef DEVELOPMENT		/* for maintaining the program */
#define DEBUG_SIZES
#endif

PRIVATE long
  parameter_count;	/* Count of parameters for keeping them in order */

PRIVATE StrSpace *
curr_loc_strspace;		/* Ptr to current local string space struct */

PRIVATE StrSpace *
curr_srctextspace;		/* Ptr to current token string space struct */

PRIVATE ParamInfoSpace *
curr_paraminfospace;		/* Ptr to current param info space struct */

PRIVATE TokHeadSpace *
curr_tokheadspace;		/* Ptr to current TokHeadSpace struct */

PRIVATE TokenSpace *
curr_tokspace;			/* Ptr to current TokenSpace struct */

PRIVATE PtrSpace *
curr_ptrspace;			/* Ptr to current PtrSpace struct */


PROTO(PRIVATE unsigned arg_count,( Token *t ));
PROTO(PRIVATE void call_external,( Lsymtab *symt, Token *id, Token *arg ));
PROTO(PRIVATE void check_intrins_args,( Token *id, Token *arg ));
PROTO(PRIVATE void check_stmt_function_args,( Lsymtab *symt, Token *id, Token *arg ));
PROTO(PRIVATE int cp_tok_src_text,( char *s, Token *t, int max ));
PROTO(PRIVATE int cp_tree_src_text,( char *s, Token *t, int max ));
PROTO(PRIVATE int cp_list_src_text,( char *s, Token *t, int max ));
PROTO(PRIVATE Lsymtab* install_local,( int h, int datatype, int storage_class ));
PROTO(PRIVATE ArgListHeader * make_arg_array,( Token *t ));
PROTO(PRIVATE void make_arg_names,( Token *tlist, ArgListHeader *alhead,
			  ArgListHeader *prev_alhead ));
PROTO(PRIVATE ComListHeader * make_com_array,( Token *t ));
PROTO(PRIVATE void make_com_names,( Token *tlist, ComListHeader *clhead,
			  ComListHeader *prev_clhead ));
PROTO(PRIVATE ArgListHeader * make_arrayless_alist,( void ));
PROTO(PRIVATE ArgListHeader * make_dummy_arg_array ,( Token *t ));
PROTO(PRIVATE TokenListHeader * make_TL_head,( Token *t ));
PROTO(PRIVATE char * new_local_string,( char *s ));
PROTO(PRIVATE ParamInfo * new_param_info,( void ));
PROTO(PRIVATE TokenListHeader * new_tokhead,( void ));
PROTO(PRIVATE Token * reverse_tokenlist,( Token *t ));
PROTO(PRIVATE void use_function_arg,( Token *id ));
PROTO(PRIVATE void use_len_arg,( Token *id ));
PROTO(PRIVATE unsigned long kwd_hash,( char *s ));
PROTO(PRIVATE void set_intrinsic_numargs, ( char *name, int choice ));
PROTO(PRIVATE IntrinsInfo * find_intrinsic,( char *s ));
PROTO(PRIVATE int find_io_keyword,( char *s ));

PROTO(PRIVATE ArgListElement * new_arglistelement,( unsigned count ));
PROTO(PRIVATE ArgListHeader * new_arglistheader,( void ));
PROTO(PRIVATE ComListElement * new_comlistelement,( unsigned count ));
PROTO(PRIVATE ComListHeader * new_comlistheader,( void ));

#ifdef DEBUG_SIZES
PROTO(extern void print_sizeofs,( void ));	/* in symtab.c */
#endif



PRIVATE unsigned
#if HAVE_STDC
arg_count(Token *t)            /* Counts the number of arguments in a token list */
#else /* K&R style */
arg_count(t)            /* Counts the number of arguments in a token list */
	Token *t;
#endif /* HAVE_STDC */
{
	unsigned count;
	count = 0;
	while(t != NULL){
		count++;
		t = t->next_token;
	}
	return(count);
}

			/* This routine handles the saving of arg lists which
			   is done by call_func and call_subr.  Also called
			   by def_namelist to save its variable list. */
PRIVATE void
#if HAVE_STDC
call_external(Lsymtab *symt, Token *id, Token *arg)
#else /* K&R style */
call_external(symt,id,arg)
	Lsymtab *symt;
	Token *id,*arg;
#endif /* HAVE_STDC */
{
       	TokenListHeader *TH_ptr;

		/* Insert the new list onto linked list of token lists */
      	TH_ptr= make_TL_head(id);

	TH_ptr->tokenlist = (arg == NULL ? NULL: arg->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;
#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\nSubprogram %s :: ",symt->name);
	  if(arg != NULL)
	    print_expr_list(arg->next_token);
	}
#endif
} /*call_external*/

void
#if HAVE_STDC
call_func(Token *id, Token *arg)	/* Process function invocation */
#else /* K&R style */
call_func(id,arg)	/* Process function invocation */
	Token *id, *arg;
#endif /* HAVE_STDC */
{
	int t, h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
	IntrinsInfo *defn;

	if( (symt = (hashtab[h].loc_symtab)) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
       	   symt->info.toklist = NULL;
	}

	t = datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

	if(storage_class_of(symt->type) == class_VAR) {
	    symt->type = type_byte(class_SUBPROGRAM,t);
	    symt->info.toklist = NULL;
	  }


		/* See if intrinsic.  If so, set flag, save info */
    if(!symt->external && !symt->intrinsic
		&& (defn = find_intrinsic(symt->name)) != NULL) {
			/* First encounter with intrinsic fcn: store info */
		symt->intrinsic = TRUE;
		symt->info.intrins_info = defn;
    }

		/* Update set/used status of variables in arg list.  This
		   is deferred to now to allow intrinsics to be treated
		   as pure functions regardless of pure_function flag. */

	if(arg != NULL) {
	    Token *a=arg;
	    int nonpure,	/* flag if function may modify arg */
	        i_len;		/* special handling for intrinsic LEN */
	    if(symt->intrinsic) {
	      nonpure = symt->info.intrins_info->intrins_flags&I_NONPURE;
	      i_len = symt->info.intrins_info->intrins_flags&I_LEN;
	    }
	    else {
	      nonpure = ! pure_functions;
	      i_len = FALSE;
	    }

			/* Token list is in reverse order.  Restore
			   args to original order. */
	    arg->next_token = reverse_tokenlist(arg->next_token);

  	    while( (a=a->next_token) != NULL) {
	      if(is_true(ID_EXPR,a->TOK_flags)){
		if( nonpure ) {
			     /* Treat impure function like subroutine call */
		  use_actual_arg(a);
		  use_variable(a);
		}
		else {
		  if(i_len)
		    use_len_arg(a); /* LEN is sui generis */
		  else
			     /* Pure-function invocation checks u-b-s */
		    use_function_arg(a);
		}
	      }
	    }
	}

		/* If intrinsic, do checking now.  Otherwise, save arg list
		   to be checked later. */

    if(symt->intrinsic) {
			/* It is intrinsic: check it */
	check_intrins_args(id,arg);
    }
    else {		/* It is not intrinsic: install in global table */
      switch(storage_class_of(symt->type)) {
	case class_SUBPROGRAM:
	  symt->external = TRUE;
	  if((!symt->argument) && (gsymt=(hashtab[h].glob_symtab)) == NULL) {
		gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
		gsymt->info.arglist = NULL;
	  }
			/* store arg list in local table */
	  call_external(symt,id,arg);
	  break;
	case class_STMT_FUNCTION:
	  symt->external = TRUE;
	  check_stmt_function_args(symt,id,arg);
	  break;
      }
    }

    symt->used_flag = TRUE;
    symt->invoked_as_func = TRUE;

} /*call_func*/


void
#if HAVE_STDC
call_subr(Token *id, Token *arg)	/* Process call statements */
#else /* K&R style */
call_subr(id,arg)	/* Process call statements */
	Token *id, *arg;
#endif /* HAVE_STDC */
{
	int t, h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
#ifndef STANDARD_INTRINSICS
	IntrinsInfo *defn;
#endif
	if( (symt = (hashtab[h].loc_symtab)) == NULL){
	   symt = install_local(h,type_SUBROUTINE,class_SUBPROGRAM);
   	   symt->info.toklist = NULL;
	}


	t=datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

	if( (storage_class_of(symt->type) == class_VAR
	     || symt->external ) && t == type_UNDECL) {
		t = type_SUBROUTINE;
		symt->info.toklist = NULL;
	}
	symt->type = type_byte(class_SUBPROGRAM,t);

	/* Since nonstandard intrinsics include some subroutines,
	   see if it is in intrinsic list.  Or
	   if declared intrinsic, then accept it as such and
	   do checking now.  Otherwise, save arg list
	   to be checked later. */
#ifndef STANDARD_INTRINSICS
    if(!symt->external && !symt->intrinsic
		&& (defn = find_intrinsic(symt->name)) != NULL) {
			/* First encounter with intrinsic fcn: store info */
		symt->intrinsic = TRUE;
		symt->info.intrins_info = defn;
    }
#endif

			/* Token list is in reverse order.  Restore
			   args to original order. */
    if(arg != NULL)
	arg->next_token = reverse_tokenlist(arg->next_token);

    if(symt->intrinsic) {
			/* It is intrinsic: check it */
	check_intrins_args(id,arg);
    }
    else {		/* It is not intrinsic: install in global table */
	symt->external = TRUE;
	if((!symt->argument) && (gsymt=(hashtab[h].glob_symtab)) == NULL) {
		gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
		gsymt->info.arglist = NULL;
	}
			/* store arg list in local table */
	call_external(symt,id,arg);
    }

	symt->used_flag = TRUE;

}/*call_subr*/

		/* This routine catches syntax errors that have to
		   wait till END is seen.  At the moment, only looks if
		   CHARACTER*(*) declarations are put on the wrong thing.
		   Has to wait since can use it for ENTRY pt.
		   Also checks if things SAVED that shouldn't be.
		   Also fixes size_is_expression flags if IMPLICIT makes
		   the variable so.
		 */
void
#if HAVE_STDC
check_loose_ends(int curmodhash)
                        /* current_module_hash from fortran.y */
#else /* K&R style */
check_loose_ends(curmodhash)
     int curmodhash;    /* current_module_hash from fortran.y */
#endif /* HAVE_STDC */
{
  int i;
  for(i=0;i<loc_symtab_top;i++) {

				/* Catch illegal CHARACTER*(*) */
    if( datatype_of(loc_symtab[i].type) == type_STRING &&
	loc_symtab[i].size == size_ADJUSTABLE &&
       !(loc_symtab[i].argument ||
	   loc_symtab[i].parameter ||
	     loc_symtab[i].entry_point) ) {
      syntax_error(NO_LINE_NUM,NO_COL_NUM,loc_symtab[i].name);
      msg_tail("cannot be adjustable size in module");
      msg_tail(hashtab[curmodhash].name);
    }

				/* Catch unSAVEable SAVE */
    if(loc_symtab[i].saved &&
        (loc_symtab[i].common_var ||
	 loc_symtab[i].argument ||
	 loc_symtab[i].external ||
	 loc_symtab[i].parameter ||
	 loc_symtab[i].entry_point) ) {
      syntax_error(NO_LINE_NUM,NO_COL_NUM,loc_symtab[i].name);
      msg_tail("cannot be declared in SAVE statement in module");
      msg_tail(hashtab[curmodhash].name);
    }

			/* Common block misspelled in SAVE stmt will
			   show up as a SAVEd block with no elements */
    if(loc_symtab[i].saved &&
       datatype_of(loc_symtab[i].type) == type_COMMON_BLOCK &&
       loc_symtab[i].info.comlist == NULL) {
      warning(NO_LINE_NUM,NO_COL_NUM,loc_symtab[i].name);
      msg_tail(": no such common block in module");
      msg_tail(hashtab[curmodhash].name);
    }

			/* If IMPLICIT CHARACTER*(expr) is used, then
			   need to fix flag to reflect it. */
    if(datatype_of(loc_symtab[i].type) == type_UNDECL &&
       get_size_text(&loc_symtab[i],type_UNDECL) != NULL) {
      loc_symtab[i].size_is_expression = TRUE;
    }
  }
}

		/* check out consistency of intrinsic argument list */
PRIVATE
void
#if HAVE_STDC
check_intrins_args(Token *id, Token *arg)
#else /* K&R style */
check_intrins_args(id, arg)
	Token *id;
	Token *arg;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;
	IntrinsInfo *defn=symt->info.intrins_info;
	unsigned args_given = ((arg == NULL)?0:arg_count(arg->next_token));
	int numargs;
	unsigned short flags;
	Token *t;

	numargs = defn->num_args;
	flags = defn->intrins_flags;

			/* positive numargs: must agree */
	if( (numargs >= 0 && (args_given != numargs))
			/* 1 or 2 arguments allowed */
	 || (numargs == I_1or2 && (args_given != 1 && args_given != 2))
			/* numargs == -2: 2 or more */
	 || (numargs == I_2up && (args_given < 2))
			/* 0 or 1 argument allowed */
	 || (numargs == I_0or1 && (args_given != 0 && args_given != 1)) ){
		unsigned line_num,col_num;
		if(arg==NULL) {line_num=id->line_num; col_num=id->col_num;}
		else {line_num = arg->line_num; col_num = arg->col_num;}

		syntax_error(line_num,col_num,
		  "wrong number of arguments for intrinsic function");
		msg_tail(defn->name);
	}
#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\nIntrinsic %s :: ",defn->name);
	  if(arg != NULL)
	    print_expr_list(arg->next_token);
	}
#endif
	if(arg != NULL && numargs != 0) {

	  Token *prev_t,	/* one operand in type propagation  */
	         fake_op;	/* operator in binexpr_type call */

	  t = arg->next_token;
				/* Copy type & size info into result */
	  arg->class = t->class;
	  arg->subclass = t->subclass;
#ifndef TOK_type
	  arg->TOK_type = t->TOK_type;
#endif
#ifndef TOK_flags
	  arg->TOK_flags = t->TOK_flags;
#endif
	  arg->size = t->size;
	  prev_t = t;

	  while(t != NULL) {
	    if(intrins_arg_cmp(defn,t)) {
				/* Propagate data type thru the list.
				   Resulting type info is stored in
				   args token.  */
	      if(prev_t != t && ! (flags & I_MIXED_ARGS) ) {
				/* Set up a pretend expr term for binexpr */
		fake_op.class = ',';
		fake_op.line_num = prev_t->line_num;
		fake_op.col_num = prev_t->col_num;
		fake_op.src_text = ",";

		binexpr_type(prev_t,&fake_op,t,arg);
	      }
	      prev_t = t;
	    }
	    t = t->next_token;
	  }/* end while */

	}/* end arg != NULL */
}/* check_intrins_args */


PRIVATE
void
#if HAVE_STDC
check_stmt_function_args(Lsymtab *symt, Token *id, Token *arg)
#else /* K&R style */
check_stmt_function_args(symt,id,arg)
	Lsymtab *symt;
	Token *id,*arg;
#endif /* HAVE_STDC */
{
	unsigned n1,n2,n;
	int i;
	Token *t1,*t2;

	t1 = symt->info.toklist->tokenlist;
	t2 = ((arg==NULL)? NULL: arg->next_token);

	n1 = arg_count(t1);
	n2 = arg_count(t2);

	if(n1 != n2) {
	    syntax_error(id->line_num,id->col_num,
		"function invoked with incorrect number of arguments");
	}

	n = (n1 < n2? n1: n2);
	for(i=0; i<n; i++) {
#ifdef OLDSTUFF
	    if( t1->TOK_type != t2->TOK_type) {
		syntax_error(t2->line_num,t2->col_num,
		  "function argument is of incorrect datatype");
	    }
#else
	    stmt_fun_arg_cmp(symt,t1,t2);
#endif
	    t1 = t1->next_token;
	    t2 = t2->next_token;
	}
}

		/* Routines to copy src text strings from an
		   expression tree into a char array.  Given max
		   no. of chars (excl. nul) to transfer.  Result is
		   always nul-terminated.  Total no. of non-nul chars
		   stored is returned. */

PRIVATE int
#if HAVE_STDC
cp_tok_src_text(char *s, Token *t, int max)	/* Copies src text from a token */
             			/* The destination string */
              			/* Expression tree */
             			/* Max no. of chars to transfer (excl. nul)*/
#else /* K&R style */
cp_tok_src_text(s,t,max)	/* Copies src text from a token */
     char *s;			/* The destination string */
     Token *t;			/* Expression tree */
     int max;			/* Max no. of chars to transfer (excl. nul)*/
#endif /* HAVE_STDC */
{
  int i,j;

#ifndef LEX_RAWSTRINGS
  if( ! is_true(LIT_CONST,t->TOK_flags)
     || t->TOK_type != type_byte(class_VAR,type_STRING))
#endif
  {
    j=0;
#if 0 /* this needs to be done only for actual, not dummy arg */
    if(t->TOK_type == type_byte(class_LABEL,type_LABEL))
      s[j++] = '*';		/* for subroutine arg = *label  */
#endif
    for(i=0; j<max && t->src_text[i] != '\0'; i++) {
      s[j++] = t->src_text[i];
    }
  }

#ifndef LEX_RAWSTRINGS
  else {                        /* Strings must be undigested */
    int  quote_char;
    quote_char = t->src_text[0];
    for(i=j=0; j<max && t->src_text[i] != '\0'; i++) {
      s[j++] = t->src_text[i];
      if(i>0 && t->src_text[i] == quote_char) /* Double a quoted quote */
	if(j < max)
	  s[j++] = quote_char;
    }
    if(j < max)
      s[j++] = quote_char; /* Add the final quote */
  }
#endif
  s[j] = '\0';			/* Terminate with nul character */
  return j;			/* Return total xferred */
}

PRIVATE int
#if HAVE_STDC
cp_tree_src_text(char *s, Token *t, int max)	/* Copies src text from expr tree */
             			/* The destination string */
              			/* Expression tree */
             			/* Max number of chars to transfer (exc. nul)*/
#else /* K&R style */
cp_tree_src_text(s,t,max)	/* Copies src text from expr tree */
     char *s;			/* The destination string */
     Token *t;			/* Expression tree */
     int max;			/* Max number of chars to transfer (exc. nul)*/
#endif /* HAVE_STDC */
{
  int ncopied=0;
  if(t != NULL) {
    if(t->left_token == NULL) {	/* Primary */
      ncopied += cp_tok_src_text(s+ncopied,t,max-ncopied);
    }
    else {			/* Expr tree */
      if(t->next_token != (Token *)NULL) {

				/* binary subtree */
        ncopied += cp_tree_src_text(s+ncopied,t->left_token,max-ncopied);

		/* root node */
	ncopied += cp_tok_src_text(s+ncopied,t,max-ncopied);

        if(t->class == '(') {     /* Array, substring, or function ref */
	  ncopied += cp_list_src_text(s+ncopied,t->next_token,max-ncopied);
	  if(max-ncopied > 0) {
	    s[ncopied++] = ')'; /* Add left parenthesis */
	    s[ncopied] = '\0';
	  }
	}
	else {
	  ncopied += cp_tree_src_text(s+ncopied,t->next_token,max-ncopied);
	}
      }
      else {
				/* parent node */
        ncopied = cp_tok_src_text(s+ncopied,t,max-ncopied);

				/* unary subtree */
        ncopied += cp_tree_src_text(s+ncopied,t->left_token,max-ncopied);

        if(t->class == '(') {     /* Parenthesized subexpression */
	  if(max-ncopied > 0) {
	    s[ncopied++] = ')'; /* Add left parenthesis */
	    s[ncopied] = '\0';
	  }
	}
      }
    }
  }
  return ncopied;
}

PRIVATE int
#if HAVE_STDC
cp_list_src_text(char *s, Token *t, int max)	/* Copies text from a tokenlist */
             			/* The destination string */
              			/* Expression tree */
             			/* Max number of chars to transfer (exc. nul)*/
#else /* K&R style */
cp_list_src_text(s,t,max)	/* Copies text from a tokenlist */
     char *s;			/* The destination string */
     Token *t;			/* Expression tree */
     int max;			/* Max number of chars to transfer (exc. nul)*/
#endif /* HAVE_STDC */
{
  int ncopied=0;
  while( t != NULL) {
    if(t->left_token == NULL) {	/* Primary */
      ncopied += cp_tok_src_text(s+ncopied,t,max-ncopied);
    }
    else {
				/* Print tree at this point in list */
      ncopied += cp_tree_src_text(s+ncopied,t->left_token,max-ncopied);
    }
    t = t->next_token;
    if(t != NULL) {		/* If next one coming, print the comma */
      if(max-ncopied > 0) {    /* Parenthesized subexpression */
	s[ncopied++] = ',';
	s[ncopied] = '\0';
      }
    }
  }
  return ncopied;
}


void
#if HAVE_STDC
declare_type(Token *id, int datatype, long int size, char *size_text)
#else /* K&R style */
declare_type(id,datatype,size,size_text)
	Token *id;
	int datatype;
	long size;
	char *size_text;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;

	if( (symt) == NULL) {
	   symt = install_local(h,datatype,class_VAR);
	   symt->size = size;
	   symt->size_is_adjustable = id->size_is_adjustable;
	   symt->size_is_expression = id->size_is_expression;
	}
	else {           /* Symbol has been seen before: check it */

			/* Intrinsic: see if type is consistent */
	  if( symt->intrinsic ) {
	    IntrinsInfo *defn = symt->info.intrins_info;
	    int rettype = defn->result_type,
		argtype = defn->arg_type;
			/* N.B. this test catches many but not all errors */
	    if( (rettype != type_GENERIC && datatype != rettype)
	     || (rettype == type_GENERIC && !((1<<datatype) & argtype)) ){
		    warning(id->line_num,id->col_num,
				"Declared type ");
		    msg_tail(type_name[datatype]);
		    msg_tail(" is invalid for intrinsic function: ");
		    msg_tail(symt->name);
	      }
	  }

	  if(datatype_of(symt->type) != type_UNDECL) {
	      syntax_error(id->line_num,id->col_num,
		"Symbol redeclared: ");
	  	msg_tail(symt->name);
	  }
	  else {
			/* Now give it the declared type */
	      symt->type = type_byte(storage_class_of(symt->type),datatype);
	      symt->size = size;
	      symt->size_is_adjustable = id->size_is_adjustable;
	      symt->size_is_expression = id->size_is_expression;
	  }
	}

		/* If character type, save the source text of the size
		   specification.  If it is an array already
		   dimensioned, add size_text to tail of src.textvec,
		   otherwise place size_text in src.text if it is
		   character type, except for parameter, which
		   shouldn't happen.
		 */

	if( datatype_of(symt->type) == type_STRING ) {
	  if(symt->array_var) {
	    int i, dims = array_dims(symt->info.array_dim);
	    char **tvec = new_textvec(dims+1);

	    for(i=0; i<dims; i++)	/* Copy the old list over */
	      tvec[i] = symt->src.textvec[i];

	    tvec[dims] = size_text; /* Copy size text to new last element */

	    free_textvec(symt->src.textvec); /* Free the old list */

	    symt->src.textvec = tvec; /* Replace old list with new */
	  }
	  else if( ! symt->parameter ) {
	    symt->src.text = size_text;
	  }
	}

#ifdef DEBUG_EXPRTREES
	      if(debug_latest) {
		fprintf(list_fd,"\n      %s",type_table[datatype]);
		size_text = get_size_text(symt,0);
		if(size_text != NULL) {
		  fprintf(list_fd," * %s",size_text);
		}
		else {
		  if(symt->size != size_DEFAULT)
		  fprintf(list_fd," * %d",symt->size);
		}
		fprintf(list_fd," %s",symt->name);
	      }
#endif

			/* Under -port=long-string warn if char size > 255 */
	if(port_long_string) {
	  if(datatype == type_STRING && size > 255)
	    nonportable(id->line_num,id->col_num,
			"character variable length exceeds 255");
	}
}/*declare_type*/


void
#if HAVE_STDC
def_arg_name(Token *id)		/* Process items in argument list */
#else /* K&R style */
def_arg_name(id)		/* Process items in argument list */
#endif /* HAVE_STDC */

#if HAVE_STDC
#else /* K&R style */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	else {           /* Symbol has been seen before: check it */

	}
	symt->argument = TRUE;
}/*def_arg_name*/


void
#if HAVE_STDC
def_array_dim(Token *id, Token *arg)	/* Process dimension lists */
	               	     /* arg previously defined as int */
#else /* K&R style */
def_array_dim(id,arg)	/* Process dimension lists */
	Token *id,*arg;	     /* arg previously defined as int */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;


	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	else {           /* Symbol has been seen before: check it */
	   if(storage_class_of(symt->type) != class_VAR) {
	      syntax_error(id->line_num,id->col_num,
		"Entity cannot be dimensioned: ");
		msg_tail(symt->name);
	      return;
	   }
	}

	symt->array_var = TRUE;
	if(!equivalence_flag){      /* some checking should be done here */
	   if(symt->info.array_dim != 0)
	      syntax_error(id->line_num,id->col_num,
		"Array redimensioned");
	   else
	      symt->info.array_dim = array_dim_info(arg->TOK_dims,
						    arg->TOK_elts);

	}

		/* Save text of dimension exprs in a list of strings
		   in symtab entry.  If array is of type character,
		   the text of size expression is already in src.text,
		   and is saved at tail of the list of dim strings. */

	{
	  int i, dims=arg->TOK_dims,
	      is_char = (datatype_of(symt->type) == type_STRING);
	  char **tvec;
	  char *size_text=symt->src.text;
	  Token *t;
				/* Restore dim list to correct order */
	  arg->next_token = reverse_tokenlist(arg->next_token);

	  symt->src.textvec = tvec = new_textvec(is_char?dims+1:dims);

				/* Store dimension expr text in list */
	  for(i=0, t=arg->next_token; i<dims; i++, t=t->next_token) {
	    tvec[i] = ( t->left_token == NULL ?
		       new_tree_text(t):
		       new_tree_text(t->left_token) );
	  }
				/* If character type, store size expr
				   text in tail of list. */
	  if(is_char)
	    tvec[dims] = size_text;

#ifdef DEBUG_EXPRTREES
	  if(debug_latest) {
	    int type=datatype_of(symt->type);
	    fprintf(list_fd,"\n      %s",
		    (type == type_UNDECL)?"DIMENSION":type_table[type]);
	    if(is_char)
	      fprintf(list_fd," * %s",symt->src.textvec[dims]);

	    fprintf(list_fd," %s ( ",symt->name);
	    for(i=0; i<dims; i++) {
	      fprintf(list_fd,"%s",symt->src.textvec[i]);
	      if(i < dims-1)
		fprintf(list_fd," , ");
	    }
	    fprintf(list_fd," )");
	  }
#endif

	}

}/*def_array_dim*/


void
#if HAVE_STDC
def_com_block(Token *id, Token *comlist)	/* Process common blocks and save_stmt */
#else /* K&R style */
def_com_block(id,comlist)	/* Process common blocks and save_stmt */
	Token *id, *comlist;
#endif /* HAVE_STDC */

{
	int h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
   	TokenListHeader *TH_ptr;
	extern unsigned true_prev_stmt_line_num;/* set by fortran.y */

		/* Install name in global symbol table */
	if( (gsymt=hashtab[h].com_glob_symtab) == NULL) {
	   gsymt = install_global(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   gsymt->info.comlist = NULL;
	}


	if( (symt = hashtab[h].com_loc_symtab) == NULL){
	   symt = install_local(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   symt->info.toklist = NULL;
	}
	if(pretty_multiple_common) {

		/* Flag declarations of same block in separate statements
		   unless separated only by comments. Use front token
		   of previous tokenlist which is last token of decl. */
	  if(comlist != NULL && symt->info.toklist != NULL
	   && symt->info.toklist->tokenlist->line_num < true_prev_stmt_line_num) {
	    ugly_code(id->line_num,id->col_num,
		"Common block declared in more than one statement");
	  }
	}

		/* Insert the new list onto linked list of token lists */
	if(comlist != NULL) {
	  	/* Will be NULL only for SAVE, in which case skip */
	    TH_ptr= make_TL_head(id);

 	    TH_ptr->tokenlist = comlist->next_token;
	    TH_ptr->next = symt->info.toklist;
            symt->info.toklist = TH_ptr;
	}

   	symt->set_flag = TRUE;
	symt->used_flag = TRUE;
}/*def_com_block*/


void
#if HAVE_STDC
def_com_variable(Token *id)		/* Process items in common block list */
#else /* K&R style */
def_com_variable(id)		/* Process items in common block list */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	else {           /* Symbol has been seen before: check it */
	    if(symt->common_var) {
		syntax_error(id->line_num,id->col_num,
		     "Variable cannot be in two different common blocks");
	    }
	    else if(symt->entry_point || symt->parameter ||
		    symt->argument || symt->external || symt->intrinsic) {
		syntax_error(id->line_num,id->col_num,
		     "Item cannot be placed in common");
		return;
	    }
	    if(symt->size == size_ADJUSTABLE) {	/* CHARACTER *(*) */
	      syntax_error(id->line_num,id->col_num,
		    "Common variable cannot have adjustable size");
	      symt->size = 1;
	    }
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->common_var = TRUE; /* set the flag even if not legit */
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*def_com_variable*/


	/* This guy sets the flag in symbol table saying the id is the
	   current module.  It returns the hash code for later reference.
	 */
int
#if HAVE_STDC
def_curr_module(Token *id)
#else /* K&R style */
def_curr_module(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int hashno = id->value.integer;
	hashtab[hashno].loc_symtab->is_current_module = TRUE;

	return hashno;
}/*def_curr_module*/




void
#if HAVE_STDC
def_equiv_name(Token *id)		/* Process equivalence list elements */
#else /* K&R style */
def_equiv_name(id)		/* Process equivalence list elements */
	Token *id;
#endif /* HAVE_STDC */
{
  ref_variable(id);		/* Put it in symtab */
	/* No other action needed: processing of equiv pairs is
	   done by equivalence() */
}/*def_equiv_name*/



void
#if HAVE_STDC
def_ext_name(Token *id)		/* Process external lists */
#else /* K&R style */
def_ext_name(id)		/* Process external lists */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt = hashtab[h].loc_symtab) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
	   symt->info.toklist = NULL;
        }
	else {
			/* Symbol seen before: check it & change class */

	    if(storage_class_of(symt->type) == class_VAR) {
	      symt->info.toklist = NULL;
	    }
	    symt->type = type_byte(class_SUBPROGRAM,datatype_of(symt->type));
	}

	if(symt->intrinsic){
	    syntax_error(id->line_num,id->col_num,
		"Cannot declare same subprogram both intrinsic and external:");
	    msg_tail(symt->name);
	}
	else{
	    symt->external = TRUE;
	    if(!symt->argument){
	        TokenListHeader *TH_ptr;
		Gsymtab *gsymt;
		if( (gsymt=hashtab[h].glob_symtab) == NULL) {
	   	    gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
	   	    gsymt->info.arglist = NULL;
		}
		TH_ptr=make_TL_head(id);

		TH_ptr->external_decl = TRUE;
		TH_ptr->next = symt->info.toklist;
		symt->info.toklist = TH_ptr;
	     }
	  }
      symt->declared_external = TRUE;
}/*def_ext_name*/



void
#if HAVE_STDC
def_function(int datatype, long int size, char *size_text, Token *id, Token *args)
#else /* K&R style */
def_function(datatype,size,size_text,id,args)
#endif /* HAVE_STDC */
				/* Installs function or subroutine name */
#if HAVE_STDC
	                                  /* in global table */
#else /* K&R style */
	int datatype;                     /* in global table */
	long size;
	char *size_text;
	Token *id,*args;
#endif /* HAVE_STDC */
{
	int storage_class;
	int h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
	TokenListHeader *TH_ptr;
   	storage_class = class_SUBPROGRAM;

	if((gsymt = (hashtab[h].glob_symtab)) == NULL) {
			/* Symbol is new to global symtab: install it */
	  gsymt = install_global(h,datatype,storage_class);
	  gsymt->size = size;
	  gsymt->info.arglist = NULL;
	}
	else {
			/* Symbol is already in global symtab. Put the
			   declared datatype into symbol table. */
	  gsymt->type = type_byte(storage_class,datatype);
	  gsymt->size = size;
	}

   	if((symt = (hashtab[id->value.integer].loc_symtab)) == NULL) {
			/* Symbol is new to local symtab: install it.
			   Since this is the current routine, it has
			   storage class of a variable. */
	   symt = install_local(h,datatype,class_VAR);
	   symt->size = size;
	   symt->src.text = size_text;
	}
	if(! symt->entry_point)	/* seen before but not as entry */
	   symt->info.toklist = NULL;

				/* Restore args list to original order */
	if(args != NULL)
	  args->next_token = reverse_tokenlist(args->next_token);

		/* Insert the new list onto linked list of token lists */
   	TH_ptr=make_TL_head(id);

			/* If this is an implied PROGRAM statement it may
			   occur in an include file, which we do not want
			   to appear in diagnostic messages about it. */
	if(top_filename != current_filename && datatype == type_PROGRAM) {
	  TH_ptr->filename = top_filename;
	  TH_ptr->line_num = top_file_line_num;
	}

	TH_ptr->tokenlist = (args == NULL ? NULL: args->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;

	symt->entry_point = TRUE;

		/* library mode: set the flag so no complaint will
		   be issued if function never invoked.  Also, set
		   used_flag if this is a main program, for same reason. */
	if(library_mode)
		symt->library_module = TRUE;
	if(datatype == type_PROGRAM) {
		symt->used_flag = TRUE;
#ifdef VCG_SUPPORT		/* Get name of file containing main module */
		main_filename = top_filename;
#endif
	}

}/*def_function*/



void
#if HAVE_STDC
def_intrins_name(Token *id)		/* Process intrinsic lists */
#else /* K&R style */
def_intrins_name(id)		/* Process intrinsic lists */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt = hashtab[h].loc_symtab) == NULL){
	   symt = install_local(h,type_UNDECL,class_SUBPROGRAM);
	   symt->info.toklist = NULL;
        }
	else {
			/* Symbol seen before: check it & change class */
	  if(storage_class_of(symt->type) == class_VAR) {
	    symt->info.toklist = NULL;
	  }

	  symt->type = type_byte(class_SUBPROGRAM,datatype_of(symt->type));
	}

		/* Place info about intrinsic datatype in local symtab.
		   If not found, it will be treated as external.
		 */

	if(symt->external){
	    syntax_error(id->line_num,id->col_num,
	       "Cannot declare same subprogram both intrinsic and external:");
	    msg_tail(symt->name);
	}
	else{
	  IntrinsInfo *defn;
	  symt->declared_intrinsic = TRUE;
	  if( (defn=find_intrinsic(symt->name)) == NULL ) {
	     warning(id->line_num,id->col_num,
			"Unknown intrinsic function: ");
	     msg_tail(symt->name);
	     msg_tail("Treated as if user-defined");
				/* Here treat as if EXTERNAL declaration */
	     def_ext_name(id);
	     return;
	   }
	   else {
			/* Found in info table: set intrins flag and store
			   pointer to definition info. */
	     symt->intrinsic = TRUE;
	     symt->info.intrins_info = defn;
	   }
	}
	symt->declared_external = TRUE;
}/*def_intrins_name*/

void
#if HAVE_STDC
def_namelist(Token *id, Token *list)		/* Process NAMELIST declaration */
#else /* K&R style */
def_namelist(id,list)		/* Process NAMELIST declaration */
     Token *id,*list;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;
	extern unsigned true_prev_stmt_line_num;/* set by fortran.y */

	if( (symt=hashtab[h].loc_symtab) == NULL) {
				/* First encounter: install in local symtab */
	  symt = install_local(h,type_NAMELIST,class_NAMELIST);
	  symt->info.toklist = NULL;
	}
	else if(pretty_multiple_namelist) {

		/* Flag declarations of same namelist in separate statements
		   unless separated only by comments. Use front token
		   of previous tokenlist which is last token of decl. */
	  if((symt->info.toklist != NULL) && 
	    (symt->info.toklist->tokenlist->line_num < true_prev_stmt_line_num)) {
	    ugly_code(id->line_num,id->col_num,
		"Namelist declared in more than one statement");
	  }
	}

	call_external(symt,id,list); /* attach list to symt->info.toklist */

}/*def_namelist*/


void
#if HAVE_STDC
def_namelist_item(Token *id)		/* Process NAMELIST list elements */
#else /* K&R style */
def_namelist_item(id)		/* Process NAMELIST list elements */
	Token *id;
#endif /* HAVE_STDC */
{
  ref_variable(id);		/* Put it in symtab */
}/*def_namelist_name*/


#ifdef CHECK_LABELS
void				/* stub for future statement-label handler */
def_label(lab)				/* ARGSUSED0 */
     Token *lab;
{
}
#endif

void
#if HAVE_STDC
def_parameter(Token *id, Token *val)	/* Process parameter_defn_item */
#else /* K&R style */
def_parameter(id,val)	/* Process parameter_defn_item */
	Token *id,*val;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}

	symt->set_flag = TRUE;
	symt->parameter = TRUE;
	symt->info.param = new_param_info();
	symt->info.param->seq_num = ++parameter_count;

		/* Integer parameters: save value in symtab entry.  Other
		   types not saved.  Need these since used in array dims */
	switch(get_type(symt)) {
		case type_INTEGER:
			symt->info.param->value.integer = int_expr_value(val);
#ifdef DEBUG_PARAMETERS
if(debug_latest)
(void)fprintf(list_fd,"\nPARAMETER %s = %d",
	      symt->name,symt->info.param->value.integer);
#endif
			break;
			/* Character parameter: if declared adjustable
			   i.e. *(*) then inherit size of const */
		case type_STRING:
			if(symt->size == size_ADJUSTABLE
			   && datatype_of(val->TOK_type) == type_STRING)
			  symt->size = val->size;
			symt->info.param->value.string = char_expr_value(val);
			break;
		case type_REAL:
		case type_DP:
		case type_COMPLEX:
			symt->info.param->value.dbl = float_expr_value(val);
		default:
			break;
	}

			/* Save the source text of value for declaration */

	symt->info.param->src_text = new_tree_text(
		(val->left_token == NULL?
			val:			/* Primary */
			val->left_token)	/* Expr tree */
	        );

#ifdef DEBUG_EXPRTREES
	if(debug_latest) {
	  fprintf(list_fd,"\n      PARAMETER ( %s = %s ) ",
		  symt->name,
		  symt->info.param->src_text);
	}
#endif

}/*def_parameter*/



void    	       /* Installs statement function name in local table */
#if HAVE_STDC
def_stmt_function(Token *id, Token *args)
#else /* K&R style */
def_stmt_function(id, args)
	Token *id, *args;
#endif /* HAVE_STDC */
{
	int t,h=id->value.integer;
	Lsymtab *symt;
   	TokenListHeader *TH_ptr;

   	if((symt = (hashtab[h].loc_symtab)) == NULL) {
			/* Symbol is new to local symtab: install it. */

	   symt = install_local(h,type_UNDECL,class_STMT_FUNCTION);
	   symt->info.toklist = NULL;
	}
	else {
	  if(storage_class_of(symt->type) == class_VAR) {
	    symt->info.toklist = NULL;
	  }
	}

		/* Restore args to original order for sake of checking phase */
	if(args != NULL)
	  args->next_token = reverse_tokenlist(args->next_token);

		/* Save dummy arg list in symbol table */
    	TH_ptr= make_TL_head(id);

	TH_ptr->tokenlist = (args == NULL ? NULL: args->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;

	t=datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

		/* check, check, check ... */
	if(storage_class_of(symt->type) == class_VAR)
	   symt->type = type_byte(class_STMT_FUNCTION,t);

	symt->external = TRUE;
}/*def_stmt_function*/




void
#if HAVE_STDC
do_ASSIGN(Token *id)		/* Process ASSIGN statement */
#else /* K&R style */
do_ASSIGN(id)		/* Process ASSIGN statement */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	else {
	   if(get_type(symt) != type_INTEGER) {
	      syntax_error(id->line_num,id->col_num,
		"Variable must be an integer: ");
	      msg_tail(symt->name);
	   }
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->set_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }
}/*do_ASSIGN*/




void
#if HAVE_STDC
do_assigned_GOTO(Token *id)		/* Process assigned_goto */
#else /* K&R style */
do_assigned_GOTO(id)		/* Process assigned_goto */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	else {
	   if(get_type(symt) != type_INTEGER) {
	      syntax_error(id->line_num,id->col_num,
		"Variable must be an integer: ");
	      msg_tail(symt->name);
	   }
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag)
	   equiv->used_before_set = TRUE;
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*do_assigned_GOTO*/





void
#if HAVE_STDC
do_ENTRY(Token *id, Token *args, int hashno)	/* Processes ENTRY statement */
#else /* K&R style */
do_ENTRY(id,args,hashno)	/* Processes ENTRY statement */
	Token *id,*args;
	int hashno;
#endif /* HAVE_STDC */
{
	int datatype;
	if(hashno == -1) {	/* -1 signifies headerless program */
	    datatype = type_PROGRAM;
	}
	else {
	    datatype = datatype_of(hashtab[hashno].loc_symtab->type);
	}
	switch(datatype) {
	    case type_PROGRAM:
	    case type_BLOCK_DATA:
	    case type_COMMON_BLOCK:
	        syntax_error(id->line_num,NO_COL_NUM,
			"You cannot have an entry statement here");
		break;
	    case type_SUBROUTINE:	/* Subroutine entry */
		def_function(type_SUBROUTINE,size_DEFAULT,(char *)NULL,
			     id,args);
		break;
	    default:		/* Function entry */
		def_function(type_UNDECL,size_DEFAULT,(char *)NULL,
			     id,args);
		break;
	}
}/*do_ENTRY*/




	/* This routine checks whether a RETURN statement is valid at
	   the present location, and if it is, looks for possible
	   failure to assign return value of function.
	*/
void
#if HAVE_STDC
do_RETURN(int hashno, Token *keyword)
	           	/* current module hash number */
	               	/* tok_RETURN, or tok_END if implied RETURN */
#else /* K&R style */
do_RETURN(hashno,keyword)
	int hashno;	/* current module hash number */
	Token *keyword;	/* tok_RETURN, or tok_END if implied RETURN */
#endif /* HAVE_STDC */
{
	int i,datatype;
	if(hashno == -1) {	/* -1 signifies headerless program */
	    datatype = type_PROGRAM;
	}
	else {
	    datatype = datatype_of(hashtab[hashno].loc_symtab->type);
	}
	switch(datatype) {
	    case type_PROGRAM:
	    case type_BLOCK_DATA:
		if(keyword->class == tok_RETURN)
		    syntax_error(keyword->line_num,keyword->col_num,
		    	"You cannot have a RETURN statement here!");
		break;
	    case type_SUBROUTINE:	/* Subroutine return: OK */
		break;
	    default:		/* Function return: check whether entry
				   points have been assigned values. */
		for(i=0; i<loc_symtab_top; i++) {
		    if(storage_class_of(loc_symtab[i].type) == class_VAR
			&& loc_symtab[i].entry_point
			&& ! loc_symtab[i].set_flag ) {
			    warning(keyword->line_num,keyword->col_num,
					loc_symtab[i].name);
			    msg_tail("not set when RETURN encountered");
		    }
		}
		break;
	}

}/*do_RETURN*/

void
#if HAVE_STDC
equivalence(Token *id1, Token *id2)
#else /* K&R style */
equivalence(id1,id2)
     Token *id1, *id2;
#endif /* HAVE_STDC */
{
	int h1=id1->value.integer, h2=id2->value.integer;
	Lsymtab *symt1,*symt2,*temp;

		/* install the variables in symtab if not seen before */
	if( (symt1=hashtab[h1].loc_symtab) == NULL) {
	   symt1 = install_local(h1,type_UNDECL,class_VAR);
	}
	if( (symt2=hashtab[h2].loc_symtab) == NULL) {
	   symt2 = install_local(h2,type_UNDECL,class_VAR);
	}
			/* Check for legality.  Ought to do complementary
			   checks elsewhere.
			 */
	if(symt1 == symt2
	   || symt1->parameter || symt2->parameter
	   || symt1->entry_point || symt2->entry_point
	   || symt1->argument || symt2->argument
	   || symt1->external || symt2->external) {

		syntax_error(id1->line_num,id1->col_num,
			     "illegal to equivalence these");
	}
		/* now swap equiv_links so their equiv lists are united */
	else {
	    temp = symt1->equiv_link;
	    symt1->equiv_link = symt2->equiv_link;
	    symt2->equiv_link = temp;
	}

		/* If either guy is in common, both are in common */
	if(symt1->common_var || symt2->common_var) {
	    Lsymtab *equiv=symt1;
	    do {
		equiv->common_var = TRUE;
		equiv = equiv->equiv_link;
	    } while(equiv != symt1);
	}
}

int
#if HAVE_STDC
get_size(Lsymtab *symt, int type)			/* ARGSUSED1 */
#else /* K&R style */
get_size(symt,type)			/* ARGSUSED1 */
#endif /* HAVE_STDC */
		    /* Returns size of symbol if explicitly declared
		       or declared using IMPLICIT type*size statement.
		       Otherwise returns size_DEFAULT. */
#if HAVE_STDC
              			/* Evaluated datatype: not used at present */
#else /* K&R style */
     Lsymtab *symt;
     int type;			/* Evaluated datatype: not used at present */
#endif /* HAVE_STDC */
{
  int datasize=symt->size;
  int datatype = datatype_of(symt->type);
  if(datatype != type_UNDECL) /* Declared? */
    return datasize;		/* if declared, use it */
  else {
    int first_char=(int)symt->name[0];

    if(first_char == '$')  first_char = 'Z'+1;
    if(first_char == '_')  first_char = 'Z'+2;

    return implicit_size[first_char - 'A'];
  }
}

char *
#if HAVE_STDC
get_size_text(Lsymtab *symt, int type)		/* ARGSUSED1 */
              			/* Evaluated datatype: not used at present */
#else /* K&R style */
get_size_text(symt,type)		/* ARGSUSED1 */
     Lsymtab *symt;
     int type;			/* Evaluated datatype: not used at present */
#endif /* HAVE_STDC */
{
  int datatype = datatype_of(symt->type);
  if(datatype != type_UNDECL)
				/* Declared: use text in symtab entry */
    if(symt->array_var)
      return symt->src.textvec[array_dims(symt->info.array_dim)];
    else
      if(symt->parameter)
	return NULL;
      else
	return symt->src.text;

  else {
				/* Undeclared: use implicit value */
    int first_char=(int)symt->name[0];

    if(first_char == '$')  first_char = 'Z'+1;
    if(first_char == '_')  first_char = 'Z'+2;

    return implicit_len_text[first_char - 'A'];
  }
}

int
#if HAVE_STDC
get_type(Lsymtab *symt)	/* Returns data type of symbol, using implicit if necessary */
#else /* K&R style */
get_type(symt)	/* Returns data type of symbol, using implicit if necessary */
	Lsymtab *symt;
#endif /* HAVE_STDC */
{
	int datatype = datatype_of(symt->type);

	if(datatype != type_UNDECL)	/* Declared? */
	   return datatype;		/*   Yes: use it */
	else if(storage_class_of(symt->type) == class_SUBPROGRAM
	     && !symt->invoked_as_func )
				/* Function never invoked: assume subr */
	   return type_SUBROUTINE;
	else if (symt->invoked_as_func && symt->intrinsic)
	{
	    IntrinsInfo *defn;

	    defn = find_intrinsic(symt->name);
	    if (defn != (IntrinsInfo *)NULL)
		return defn->result_type;
	}

	/* Fell through, so type must be determined by first letter of name */

	{
	  int first_char=(int)symt->name[0];
#ifdef ALLOW_DOLLARSIGNS
	  if(first_char == '$')  first_char = 'Z'+1;
#endif
#ifdef ALLOW_UNDERSCORES
	  if(first_char == '_')  first_char = 'Z'+2;
#endif

	   return implicit_type[first_char - 'A'];
	}
}/*get_type*/


	/* hash_lookup finds identifier in hashtable and returns its
	   index.  If not found, a new hashtable entry is made for it,
	   and the identifier string s is copied to local stringspace.
	*/
unsigned
#if HAVE_STDC
hash_lookup(char *s)
#else /* K&R style */
hash_lookup(s)
	char *s;
#endif /* HAVE_STDC */
{
        unsigned h;
	unsigned long hnum;

	hnum = hash(s);

	while(h = hnum%HASHSZ, hashtab[h].name != NULL
	          && strcmp(hashtab[h].name,s) != 0) {
			  hnum = rehash(hnum);	/* Resolve clashes */
	}

	if(hashtab[h].name == NULL) {
		    hashtab[h].name = new_local_string(s);
		    hashtab[h].loc_symtab = NULL;
		    hashtab[h].glob_symtab = NULL;
		    hashtab[h].com_loc_symtab = NULL;
		    hashtab[h].com_glob_symtab = NULL;
        }
	return h;
}/*hash_lookup*/

void
init_tables(VOID)			/* Allocates table space */
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
	if( ((loc_symtab=(Lsymtab*)calloc(LOCSYMTABSZ,sizeof(Lsymtab)))
		== (Lsymtab*)NULL) ||
	    ((glob_symtab=(Gsymtab*)calloc(GLOBSYMTABSZ,sizeof(Gsymtab)))
		== (Gsymtab*)NULL) ||
	    ((hashtab=(HashTable*)calloc(HASHSZ,sizeof(HashTable)))
		== (HashTable*)NULL)
	  ) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for tables");
	}
#endif
}

void
init_globals(VOID)                	/* Clears the global symbol table */
{
  glob_symtab_top = 0;	/* Neither of these stmts is really needed. */
  glob_strings_used = 0;
}/*init_globals*/



void
init_symtab(VOID)                     /* Clears the local symbol table */
{
	int i,h;
	unsigned long hnum;

	loc_symtab_top = 0;	/* Clear local symtab */

	curr_loc_strspace = &lstrspace;
	loc_str_top = 0;	/* Reset storage area for local strings */
	extra_locstrspace = 0;

	curr_srctextspace = &srctextspace;
	srctextspace_top = 0;	/* Reset storage area for token text */
	extra_srctextspace = 0;

	curr_tokspace = &tokspace;
	token_space_top = 0;	/* Reset storage for tokens in lists & trees */
	extra_tokspace = 0;

	curr_paraminfospace = &paraminfospace;
 	param_info_space_top = 0;/* Reset storage for parameter info structs */
	extra_paraminfospace = 0;

	curr_tokheadspace = &tokheadspace;
 	token_head_space_top = 0;/* Reset storage for tokenlist headers */
	extra_tokheadspace = 0;

	curr_ptrspace = &ptrspace;
	ptrspace_top = 0;	/* Reset storage for array dim textvecs */
	extra_ptrspace = 0;

	parameter_count = 0;

		      /* Clears the hash table */
	for(i=0;i<HASHSZ;i++) {
	    hashtab[i].name = NULL;
	    hashtab[i].loc_symtab = NULL;
	    hashtab[i].com_loc_symtab = NULL;
	    hashtab[i].glob_symtab = NULL;
	    hashtab[i].com_glob_symtab = NULL;
	}

		      /* Re-establishes global symbols */
	for(i=0;i<glob_symtab_top;i++) {
	    hnum = hash(glob_symtab[i].name);
	    while (h=hnum % HASHSZ, hashtab[h].name != NULL
	       && strcmp(hashtab[h].name,glob_symtab[i].name) != 0 ) {
	       hnum = rehash(hnum);
	    }
	    hashtab[h].name = glob_symtab[i].name;
	    if(storage_class_of(glob_symtab[i].type) == class_COMMON_BLOCK)
		hashtab[h].com_glob_symtab = &(glob_symtab[i]);
	    else
		hashtab[h].glob_symtab = &(glob_symtab[i]);

	}

		      /* Restores implicit typing to default values.
		         Note: 27 is '$', 28 is '_' which are default REAL */
	{
		int c;
		for( c=0; c<=('Z'-'A'+2); c++ ) {
	    	    implicit_type[c] = type_REAL;
		    implicit_size[c] = size_DEFAULT;
		    implicit_len_text[c] = NULL;
		}
		for( c='I'-'A'; c <= 'N'-'A'; c++ )
		    implicit_type[c] = type_INTEGER;
	}
}/*init_symtab*/



Gsymtab*
#if HAVE_STDC
install_global(int h, int datatype, int storage_class)	/* Install a global symbol */
	      			/* hash index */
#else /* K&R style */
install_global(h,datatype,storage_class)	/* Install a global symbol */
	int h;			/* hash index */
	int datatype,storage_class;
#endif /* HAVE_STDC */
{
	Gsymtab *gsymt = &glob_symtab[glob_symtab_top];

	if(glob_symtab_top == GLOBSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in global symbol table\n\
Recompile me with larger GLOBSYMTABSZ value\n"
#else
"out of space in global symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {
			/* Store symtab pointer in hash table */
	    if(storage_class == class_COMMON_BLOCK)
		hashtab[h].com_glob_symtab = gsymt;
	    else
		hashtab[h].glob_symtab = gsymt;

	    clear_symtab_entry(gsymt);

	 		/* Duplicate copy of string into global stringspace */
	    gsymt->name = new_global_string(hashtab[h].name);

			/* Set symtab info fields */
	    gsymt->type = type_byte(storage_class,datatype);
	    gsymt->size = type_size[datatype];
	    if(storage_class == class_COMMON_BLOCK)
		gsymt->info.comlist = NULL;
	    else
		gsymt->info.arglist = NULL;

	    gsymt->link.child_list = NULL;

	    ++glob_symtab_top;
	}
	return (gsymt);
}/*install_global*/


PRIVATE Lsymtab*
#if HAVE_STDC
install_local(int h, int datatype, int storage_class)	/* Install a local symbol */
	      			/* hash index */
#else /* K&R style */
install_local(h,datatype,storage_class)	/* Install a local symbol */
	int h;			/* hash index */
	int datatype,storage_class;
#endif /* HAVE_STDC */
{
	Lsymtab *symt = &loc_symtab[loc_symtab_top];
	if(loc_symtab_top == LOCSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in local symbol table\n\
Recompile me with larger LOCSYMTABSZ value\n"
#else
"out of space in local symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {
	    if(storage_class == class_COMMON_BLOCK)
		hashtab[h].com_loc_symtab = symt;
	    else
		hashtab[h].loc_symtab = symt;

	    clear_symtab_entry(symt);
	    symt->name = hashtab[h].name;
	    symt->info.array_dim = 0;

		      /* Set symtab info fields */
	    symt->type = type_byte(storage_class,datatype);
	    symt->size = type_size[datatype];
	    symt->src.text = NULL;
	    symt->equiv_link = symt;	/* equivalenced only to self */
	    if(incdepth > 0)
	      symt->defined_in_include = TRUE;
	    ++loc_symtab_top;
	}
	return symt;
}/*install_local*/


		/* Get value specified by an integer-expression token.
		   This will be either an identifier, which should be a
		   parameter whose value is in the symbol table, or else
		   an expression token as propagated by exprtype.c
		   routines, with value stored in the token.
		*/
int
#if HAVE_STDC
int_expr_value(Token *t)
#else /* K&R style */
int_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(!is_true(EVALUATED_EXPR,t->TOK_flags)) {/* something bogus */
				/* warn if error message not already given */
    if(is_true(PARAMETER_EXPR,t->TOK_flags))
      warning(t->line_num,t->col_num,
	      "Constant not evaluated: value of 0 assumed");
  }
  else {
	if( is_true(ID_EXPR,t->TOK_flags) ) {
		/* Identifier: better be a parameter */
	    int h=t->value.integer;
	    Lsymtab *symt = hashtab[h].loc_symtab;
	    if(symt == NULL || !(symt->parameter) ) {
		syntax_error(t->line_num,t->col_num,
			"symbolic constant required");
	    }
	    else {
		return symt->info.param->value.integer;
	    }
	}
		/* Otherwise, it is a const or expr, use token.value.integer */
	else {
	    return t->value.integer;
	}
  }
				/* Unsuccessful: return value of 0 */
  return 0;
}/*int_expr_value*/

DBLVAL
#if HAVE_STDC
float_expr_value(Token *t)
#else /* K&R style */
float_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(is_true(LIT_CONST,t->TOK_flags))
    return t->value.dbl;
  else
    return (DBLVAL)0;		/* float values are not propagated */
}

char *
#if HAVE_STDC
char_expr_value(Token *t)
#else /* K&R style */
char_expr_value(t)
	Token *t;
#endif /* HAVE_STDC */
{
  if(is_true(LIT_CONST,t->TOK_flags))
    return t->value.string;
  else
    return NULL;		/* char values are not propagated */
}
	/* Following routine converts a list of tokens into a list of type-
	   flag pairs. */

PRIVATE ArgListHeader *
#if HAVE_STDC
make_arg_array(Token *t)
	         		/* List of tokens */
#else /* K&R style */
make_arg_array(t)
	Token *t;		/* List of tokens */
#endif /* HAVE_STDC */
{
	int i;
	unsigned count;
	Token *s;
	ArgListElement *arglist;
	ArgListHeader *alhead;

	count = arg_count(t);
	if(((alhead=new_arglistheader())
		 		 == (ArgListHeader *) NULL) ||
	  (count != 0 &&
          ((arglist=new_arglistelement(count))
				 == (ArgListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for argument list");
	}
	s = t;
	for(i=0; i<count; i++){  /* Here we fill array. */

	    arglist[i].type = s->TOK_type; /* use evaluated type, not symt */
	    arglist[i].size = s->size;
			/* Keep track of array and external declarations */
	    if( is_true(ID_EXPR,s->TOK_flags) ){
		int h = s->value.integer;
		Lsymtab *symt = hashtab[h].loc_symtab;
		if( (arglist[i].info.array_dim = symt->info.array_dim) == 0)
				/* change scalars to 0 dims, size 1 */
		  arglist[i].info.array_dim = array_dim_info(0,1);
		arglist[i].array_var = symt->array_var;
		arglist[i].declared_external = symt->declared_external;
	    }
	    else {
		arglist[i].info.array_dim = array_dim_info(0,1);
		arglist[i].array_var = FALSE;
		arglist[i].declared_external = FALSE;
	    }

	    arglist[i].array_element =
		arglist[i].array_var && !is_true(ARRAY_ID_EXPR,s->TOK_flags);

	    if( is_true(LVALUE_EXPR,s->TOK_flags) ){
		arglist[i].is_lvalue = TRUE;
			/* is_true(f,x) yields 0 or non-0: convert to 0 or 1 */
		arglist[i].set_flag =
			is_true(SET_FLAG,s->TOK_flags)? TRUE: FALSE;
		arglist[i].assigned_flag =
			is_true(ASSIGNED_FLAG,s->TOK_flags)? TRUE: FALSE;
		arglist[i].used_before_set =
			is_true(USED_BEFORE_SET,s->TOK_flags)? TRUE: FALSE;
	    }
	    else {	/* it is an expression or constant, not an lvalue */
		arglist[i].is_lvalue = FALSE;
		arglist[i].set_flag = TRUE;
		arglist[i].assigned_flag = FALSE;
		arglist[i].used_before_set = FALSE;
	    }
	    s = s->next_token;
	}
	alhead->numargs = (short)count;
	alhead->is_defn = FALSE;
	alhead->is_call = TRUE;
	alhead->external_decl = FALSE;
	alhead->actual_arg = FALSE;

        if (count == 0)
		alhead->arg_array = NULL;
	else
		alhead->arg_array = arglist;
	return(alhead);
}/* make_arg_array */

PRIVATE void
#if HAVE_STDC
make_arg_names(Token *tlist, ArgListHeader *alhead, ArgListHeader *prev_alhead)
#else /* K&R style */
make_arg_names(tlist, alhead, prev_alhead)
     Token *tlist;
     ArgListHeader *alhead, *prev_alhead;
#endif /* HAVE_STDC */
{
	int h, i, n, prev_n;
	Token *s;
#ifdef KEEP_ARG_NAMES
	char *name;
	char expr_text[MAXEXPRTEXT+2]; /* big enough for 1 extra */
#else
	static char expr[]="expr", 	/* text strings to use */
	            var[]="var";
#endif
	ArgListElement *arglist, *prev_arglist;

	n = alhead->numargs;
	if(n > 0) {
	  arglist = alhead->arg_array;
	  if(prev_alhead != NULL) {
	    prev_n = prev_alhead->numargs;
	    prev_arglist = prev_alhead->arg_array;
	  }
	  for(i=0, s=tlist; i<n; i++, s=s->next_token) {
				/* Use symtab name for id's but note that
				   array elements come thru with ID_EXPR
				   true but want to use expr tree for them.*/
	    if(is_true(ID_EXPR,s->TOK_flags)
		    && !is_true(ARRAY_ELEMENT_EXPR,s->TOK_flags)) {
#ifdef KEEP_ARG_NAMES
	      h = s->value.integer;
	      name = hashtab[h].loc_symtab->name;
#else
	      name = var;
#endif
	    }
	    else {				/* expression */
#ifdef KEEP_ARG_NAMES
	      int ncopied;
	      ncopied = cp_tree_src_text(expr_text,
			(s->left_token == NULL?
				s:			/* Primary */
				s->left_token),	/* Expr tree */
			MAXEXPRTEXT+1);
	      if(ncopied > MAXEXPRTEXT)	/* Longer than the limit: */
					/* replace tail by dots   */
		(void)strcpy(expr_text+MAXEXPRTEXT-2,"..");
	      name = expr_text;
#else
	      arglist[i].name = expr;
#endif
	    }
#ifdef KEEP_ARG_NAMES
				/* Try to avoid allocating space again */
	    if(prev_alhead != NULL && i < prev_n
		 && strcmp(name,prev_arglist[i].name) == 0) {
	      name = prev_arglist[i].name;
	    }
	    else if(is_true(ID_EXPR,s->TOK_flags)
		    && !is_true(ARRAY_ELEMENT_EXPR,s->TOK_flags)) {
	      if(hashtab[h].glob_symtab != NULL) {
		name = hashtab[h].glob_symtab->name;
	      }
	      else if(hashtab[h].com_glob_symtab != NULL) {
		name = hashtab[h].com_glob_symtab->name;
	      }
	      else			/* No luck: put it into global space */
		name = new_global_string(name);
	    }
	    else
	      name = new_global_string(name);
#endif
	    arglist[i].name = name;
	  }
	}
}

	/* Following routine converts a list of common block tokens
	    into a list of dimen_info-type pairs. */

PRIVATE ComListHeader *
#if HAVE_STDC
make_com_array(Token *t)
	         		/* List of tokens */
#else /* K&R style */
make_com_array(t)
	Token *t;		/* List of tokens */
#endif /* HAVE_STDC */
{
	Token *s;
	Lsymtab *symt;
	int h, i;
	unsigned count;
	ComListHeader *clhead;
	ComListElement *comlist;

	count = arg_count(t);
	if(((clhead=new_comlistheader())
		 == (ComListHeader *) NULL) ||
	  (count != 0 &&
 	  ((comlist=new_comlistelement(count))
		 == (ComListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for common list");
	}
	s = t;
	for(i=0; i<count; i++){
	   h = s->value.integer;
	   symt = hashtab[h].loc_symtab;
	   comlist[i].name = NULL; /* names are added later by make_com_list */
	   if( (comlist[i].dimen_info = symt->info.array_dim) == 0)
				/* change scalars to 0 dims, size 1 */
	     comlist[i].dimen_info = array_dim_info(0,1);
       	   comlist[i].type = get_type(symt);
	   comlist[i].size = get_size(symt,(int)comlist[i].type);
	   comlist[i].used = symt->used_flag;
	   comlist[i].set = symt->set_flag;
	   comlist[i].used_before_set = symt->used_before_set;
	   comlist[i].assigned = symt->assigned_flag;
	   if (comlist[i].used)
		clhead->any_used = TRUE;
	   if (comlist[i].set)
		clhead->any_set = TRUE;
	   s = s->next_token;
	}
	clhead->numargs = (short)count;
	if (count == 0)
		clhead->com_list_array = NULL;
	else
		clhead->com_list_array = comlist;
	return(clhead);
} /* make_com_array */

PRIVATE void
#if HAVE_STDC
make_com_names(Token *tlist, ComListHeader *clhead, ComListHeader *prev_clhead)
#else /* K&R style */
make_com_names(tlist, clhead, prev_clhead)
     Token *tlist;
     ComListHeader *clhead, *prev_clhead;
#endif /* HAVE_STDC */
{
	int h, i, n, prev_n;
	Token *s;
	ComListElement *comlist, *prev_comlist;
	char *name;
	comlist = clhead->com_list_array;

	n = clhead->numargs;
	if(prev_clhead != NULL) {
	  prev_n = prev_clhead->numargs;
	  prev_comlist = prev_clhead->com_list_array;
	}

	for(i=0, s=tlist; i<n; i++, s=s->next_token) {
	  h = s->value.integer;
	  name = hashtab[h].loc_symtab->name;

		/* Try to avoid allocating new global space for name:
		   Check if the variable matches a global name
		   (unlikely) or name of corresponding variable in
		   previous declaration of same block used the same
		   name (likely), and if so, re-use the global string.
		   Otherwise allocate new space in global table.  */

	  if(prev_clhead != NULL && i < prev_n
	     && strcmp(name,prev_comlist[i].name) == 0) {
	    name = prev_comlist[i].name;
	  }
	  else if(hashtab[h].glob_symtab != NULL) {
	    name = hashtab[h].glob_symtab->name;
	  }
	  else if(hashtab[h].com_glob_symtab != NULL) {
	    name = hashtab[h].com_glob_symtab->name;
	  }
	  else			/* No luck: put it into global space */
	    name = new_global_string(name);

	  comlist[i].name = name;
	}
}

PRIVATE ArgListHeader *
#if HAVE_STDC
make_dummy_arg_array (Token *t)
	         		/* List of tokens */
#else /* K&R style */
make_dummy_arg_array (t)
	Token *t;		/* List of tokens */
#endif /* HAVE_STDC */
{
	int i;
	unsigned count;
	Token *s;
	ArgListElement *arglist;
	ArgListHeader *alhead;

	count = arg_count(t);
	if(((alhead=new_arglistheader())
			 == (ArgListHeader *) NULL) ||
	  (count != 0 &&
          ((arglist=new_arglistelement(count))
			== (ArgListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for dummy argument list");
	}
	s = t;
	for(i=0; i<count; i++){
	    if( is_true(ID_EXPR,s->TOK_flags) ){
	        int implied_type;
		int h = s->value.integer;
		Lsymtab *symt = hashtab[h].loc_symtab;
		if( (arglist[i].info.array_dim = symt->info.array_dim) == 0)
				/* change scalars to 0 dims, size 1 */
		  arglist[i].info.array_dim = array_dim_info(0,1);
		implied_type = get_type(symt);
		arglist[i].type = type_byte(storage_class_of(symt->type),
						implied_type);
		arglist[i].size = get_size(symt,implied_type);
		arglist[i].is_lvalue = TRUE;
		arglist[i].set_flag = symt->set_flag;
		arglist[i].assigned_flag = symt->assigned_flag;
		arglist[i].used_before_set = symt->used_before_set;
		arglist[i].array_var = symt->array_var;
		arglist[i].array_element = FALSE;
		arglist[i].declared_external = symt->declared_external;
	    }
	    else {	/* It is a label */
		arglist[i].info.array_dim = 0;
		arglist[i].type = s->TOK_type;
		arglist[i].size = 0;
		arglist[i].is_lvalue = FALSE;
		arglist[i].set_flag = FALSE;	/* Don't currently do labels */
		arglist[i].assigned_flag = FALSE;
		arglist[i].used_before_set = FALSE;
		arglist[i].array_var = FALSE;
		arglist[i].array_element = FALSE;
		arglist[i].declared_external = FALSE;
	    }
	    s = s->next_token;
	}
	alhead->numargs = (short)count;
	alhead->is_defn = TRUE;
	alhead->is_call = FALSE;
	alhead->external_decl = FALSE;
	alhead->actual_arg = FALSE;

        if (count == 0)
		alhead->arg_array = NULL;
	else
		alhead->arg_array = arglist;
	return(alhead);
}/* make_dummy_arg_array */


	/* This routine makes an empty argument list: used for
	   EXTERNAL declarations of subprograms. */
PRIVATE ArgListHeader *
make_arrayless_alist(VOID)
{
	ArgListHeader *alhead;

	if(((alhead=new_arglistheader())
		 		 == (ArgListHeader *) NULL) ) {
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for external decl");
	}

	alhead->numargs = 0;
	alhead->is_defn = FALSE;
	alhead->is_call = FALSE;
	alhead->arg_array = NULL;

	return(alhead);
}/* make_arrayless_arglist */

PRIVATE TokenListHeader *	/* Initializes a tokenlist header */
#if HAVE_STDC
make_TL_head(Token *t)
#else /* K&R style */
make_TL_head(t)
     Token *t;
#endif /* HAVE_STDC */
{
	TokenListHeader *TH_ptr;
	TH_ptr = new_tokhead();
	TH_ptr->line_num = t->line_num;
	TH_ptr->top_line_num = (current_filename == top_filename?
				t->line_num: top_file_line_num);
  	TH_ptr->filename = current_filename;
				/* Clear all the flags */
	TH_ptr->external_decl = FALSE;
	TH_ptr->actual_arg = FALSE;
	TH_ptr->tokenlist = NULL;
	TH_ptr->next = NULL;

  return TH_ptr;
}

#ifndef T_ALLOC

	/* This routine allocates permanent space for argument list
	   elements in chunks for efficiency.  It returns a pointer to
	   space for count consecutive elements. */

PRIVATE ArgListElement *
#if HAVE_STDC
new_arglistelement(unsigned int count)
#else /* K&R style */
new_arglistelement(count)
     unsigned count;
#endif /* HAVE_STDC */
{
  static unsigned long arglistspace_bot=0;
  static ArgListElement *arglist_space=NULL;

  arglist_element_used += count;	/* For -resources */

  if(arglistspace_bot < count) {
    unsigned long numalloc = (count > ARGLISTELTSZ? count: ARGLISTELTSZ);
    arglist_space=(ArgListElement *)calloc(numalloc,sizeof(ArgListElement));
    if(arglist_space == (ArgListElement *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for argument list");
      return (ArgListElement *)NULL; /*NOTREACHED*/
    }
    arglistspace_bot = numalloc;
  }
				/* Slots are allocated from top down */
  arglistspace_bot -= count;
  return arglist_space + arglistspace_bot;
}

	/* This routine allocates permanent space for argument list
	   headers in chunks for efficiency.  Returns a pointer to
	   space for one header. */

PRIVATE ArgListHeader *
new_arglistheader(VOID)
{
  static unsigned long arglistheadspace_bot=0;
  static ArgListHeader *arglisthead_space;

  arglist_head_used++;

  if(arglistheadspace_bot < 1) {
    arglisthead_space=
      (ArgListHeader *)calloc(ARGLISTHEADSZ,sizeof(ArgListHeader));
    if(arglisthead_space == (ArgListHeader *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for argument list header");
      return (ArgListHeader *)NULL; /*NOTREACHED*/
    }
    arglistheadspace_bot = ARGLISTHEADSZ;
  }
				/* Slots are allocated from top down */
  return arglisthead_space + (--arglistheadspace_bot);
}

	/* Returns pointer to space for count consecutive common list
	   elements. */

PRIVATE ComListElement *
#if HAVE_STDC
new_comlistelement(unsigned int count)
#else /* K&R style */
new_comlistelement(count)
     unsigned count;
#endif /* HAVE_STDC */
{
  static unsigned long comlistspace_bot=0;
  static ComListElement *comlist_space=NULL;

  comlist_element_used += count;	/* For -resources */

  if(comlistspace_bot < count) {
    unsigned long numalloc = (count > COMLISTELTSZ? count: COMLISTELTSZ);
    comlist_space=(ComListElement *)calloc(numalloc,sizeof(ComListElement));
    if(comlist_space == (ComListElement *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for common block list");
      return (ComListElement *)NULL; /*NOTREACHED*/
    }
    comlistspace_bot = numalloc;
  }
				/* Slots are allocated from top down */
  comlistspace_bot -= count;
  return comlist_space + comlistspace_bot;
}

	/* Returns pointer to space for one common block header */

PRIVATE ComListHeader *
new_comlistheader(VOID)
{
  static unsigned long comlistheadspace_bot=0;
  static ComListHeader *comlisthead_space;

  comlist_head_used++;

  if(comlistheadspace_bot < 1) {
    comlisthead_space=
      (ComListHeader *)calloc(COMLISTHEADSZ,sizeof(ComListHeader));
    if(comlisthead_space == (ComListHeader *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for common block header");
      return (ComListHeader *)NULL; /*NOTREACHED*/
    }
    comlistheadspace_bot = COMLISTHEADSZ;
  }
				/* Slots are allocated from top down */
  return comlisthead_space + (--comlistheadspace_bot);
}


#endif /*T_ALLOC*/


		/* this routine allocates room in global stringspace
		   (top down) for string s, and copies it there. */
char *
#if HAVE_STDC
new_global_string(char *s)
#else /* K&R style */
new_global_string(s)
	char *s;
#endif /* HAVE_STDC */
{
  static unsigned long glob_str_bot = 0;
  static char *glob_strspace;

  int count = strlen(s) + 1;	/* no. of chars needed including final nul */

  glob_strings_used += count;	/* keep track for -resource */

  if(glob_str_bot < count) {
    unsigned long numalloc = (count > STRSPACESZ? count: STRSPACESZ);
    glob_strspace = (char *)calloc(numalloc,sizeof(char));
    if(glob_strspace == (char *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for global strings");
      return (char *)NULL; /*NOTREACHED*/
    }
    glob_str_bot = numalloc;
  }

  glob_str_bot -= count;    /*pre-decrement*/
  return strcpy(glob_strspace+glob_str_bot,s);
}/*new_global_string*/

		/* Allocate space for string s in local string space
		   (bottom up), and copy it there. */
PRIVATE char *
#if HAVE_STDC
new_local_string(char *s)
#else /* K&R style */
new_local_string(s)
	char *s;
#endif /* HAVE_STDC */
{
  int count = strlen(s) + 1;	/* No. of chars needed including final nul */
  int orig_top = loc_str_top;
  loc_str_top += count;
  if(loc_str_top > STRSPACESZ) {
    StrSpace *new_loc_strspace;
    new_loc_strspace = (StrSpace *)malloc(sizeof(StrSpace));
    if(new_loc_strspace == (StrSpace *)NULL) {
      oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		   "Cannot alloc space for local strings");
      return (char *)NULL; /*NOTREACHED*/
    }
    else {
      new_loc_strspace->next = (StrSpace *)NULL;
      curr_loc_strspace->next = new_loc_strspace;
    }
    curr_loc_strspace = curr_loc_strspace->next;
    extra_locstrspace += orig_top; /* Remember amount used so far */
    orig_top = 0;
    loc_str_top = count;
  }
  return strcpy(curr_loc_strspace->strspace+orig_top,s);
}/* new_local_string */

PRIVATE ParamInfo *
new_param_info(VOID)		/* Allocates space for parameter info field */
{
  if(param_info_space_top == PARAMINFOSPACESZ) {
    if(curr_paraminfospace->next == (ParamInfoSpace *)NULL) {
      ParamInfoSpace *new_paraminfospace;
      if( (new_paraminfospace = (ParamInfoSpace *)malloc(sizeof(ParamInfoSpace)))
	 == (ParamInfoSpace *)NULL) {
	oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		     "Cannot alloc space for parameter info");
	return (ParamInfo *)NULL;	/*NOTREACHED*/
      }
      else {
	new_paraminfospace->next =  (ParamInfoSpace *)NULL;
	curr_paraminfospace->next = new_paraminfospace;
      }
    }
    curr_paraminfospace = curr_paraminfospace->next;
    extra_paraminfospace += PARAMINFOSPACESZ;
    param_info_space_top = 0;
  }
  return curr_paraminfospace->paraminfospace + param_info_space_top++;

}

void
#if HAVE_STDC
free_textvec(char **p)		/*ARGSUSED*/
#else /* K&R style */
free_textvec(p)		/*ARGSUSED*/
     char **p;
#endif /* HAVE_STDC */
{
	/* No action necessary since all the space is freed in
	   a lump at end of processing module */
}

char **
#if HAVE_STDC
new_textvec(int n)		/* Allocates space for array of n char ptrs */
#else /* K&R style */
new_textvec(n)		/* Allocates space for array of n char ptrs */
     int n;
#endif /* HAVE_STDC */
{
  int orig_top = ptrspace_top;
  ptrspace_top += n;

  if( ptrspace_top > PTRSPACESZ) {
    if(curr_ptrspace->next == (PtrSpace *)NULL) {
      PtrSpace *new_ptrspace;
      if( (new_ptrspace = (PtrSpace *)malloc(sizeof(PtrSpace)))
	 == (PtrSpace *)NULL) {
	oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		     "Cannot alloc space for pointers to text");
	return (char **)NULL; /*NOTREACHED*/
      }
      else {
	new_ptrspace->next = (PtrSpace *)NULL;
	curr_ptrspace->next = new_ptrspace;
      }
    }
    curr_ptrspace = curr_ptrspace->next;
    extra_ptrspace += orig_top;
    orig_top = 0;
    ptrspace_top = n;
  }
  return curr_ptrspace->ptrspace + orig_top;
}

				/* Routine to allocate space for
				   a string containing source text
				   of a token. */

char *
#if HAVE_STDC
new_src_text_alloc(int size)
              			/* length counting nul */
#else /* K&R style */
new_src_text_alloc(size)
     int size;			/* length counting nul */
#endif /* HAVE_STDC */
{
  int orig_top = srctextspace_top;
  srctextspace_top += size;

  if(srctextspace_top > STRSPACESZ) {
    StrSpace *new_srctextspace;
    new_srctextspace = (StrSpace *)malloc(sizeof(StrSpace));
    if(new_srctextspace == (StrSpace *)NULL) {
      oops_message(OOPS_FATAL,line_num,col_num,
		   "Cannot alloc space for token text");
      return (char *)NULL; /*NOTREACHED*/
    }
    else {
      new_srctextspace->next = (StrSpace *)NULL;
      curr_srctextspace->next = new_srctextspace;
    }
    curr_srctextspace = curr_srctextspace->next;
    extra_srctextspace += orig_top; /* Remember amount used so far */
    orig_top = 0;
    srctextspace_top = size;
  }

  return curr_srctextspace->strspace + orig_top;
}

				/* Tokens that are 1 char long have their
				   src_text stored in this array, indexed
				   by their codes.  Avoids duplication of
				   strings, wasting space.
				 */
PRIVATE char onechar_text[2*(MAX_CHAR_CODE+1)];

				/* Routine to get space for string
				   containing source text of token
				   and copy it to there.
				 */

char *
#if HAVE_STDC
new_src_text(char *s, int len)
             			/* string (final nul not needed) */
             			/* length not counting nul */
#else /* K&R style */
new_src_text(s,len)
     char *s;			/* string (final nul not needed) */
     int len;			/* length not counting nul */
#endif /* HAVE_STDC */
{
  int i;
  char *new_s;
				/* If it is a single char, it goes
				   into the special array.  Otherwise
				   allocate space for it. */
  if(len <= 1)
    new_s = &onechar_text[s[0]*2];
  else
    new_s = new_src_text_alloc(len+1);

  for(i=0; i<len; i++)		/* copy string to new space */
    new_s[i] = s[i];
  new_s[i] = '\0';

  return new_s;
}

		/* Copy expr token src text into local stringspace. */

#define MAXTREETEXT (20*72+1)	/* Enough space for any f77 expression. */
PRIVATE char tree_text_space[MAXTREETEXT];

char *
#if HAVE_STDC
new_tree_text(Token *t)
#else /* K&R style */
new_tree_text(t)
     Token *t;
#endif /* HAVE_STDC */
{
  (void) cp_tree_src_text(tree_text_space, t, MAXTREETEXT-1);
  return new_local_string(tree_text_space);
}


PRIVATE TokenListHeader *
new_tokhead(VOID)
{
  if(token_head_space_top == TOKHEADSPACESZ) {
    if(curr_tokheadspace->next == (TokHeadSpace *)NULL) {
      TokHeadSpace *new_tokheadspace;
      if( (new_tokheadspace = (TokHeadSpace *)malloc(sizeof(TokHeadSpace)))
	 == (TokHeadSpace *)NULL) {
	oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		     "Cannot alloc space for token list header");
	return (TokenListHeader *)NULL;	/*NOTREACHED*/
      }
      else {
	new_tokheadspace->next =  (TokHeadSpace *)NULL;
	curr_tokheadspace->next = new_tokheadspace;
      }
    }
    curr_tokheadspace = curr_tokheadspace->next;
    extra_tokheadspace += TOKHEADSPACESZ;
    token_head_space_top = 0;
  }
  return curr_tokheadspace->tokheadspace + token_head_space_top++;
}

Token *
new_token(VOID)			/* Returns pointer to space for a token */
{
  if(token_space_top == TOKENSPACESZ) {
	/* When token space is used up, go to the next.  If none, then
	   allocate a new one.  The memory is never deallocated, since
	   it will likely be needed again later.  So token space structs
	   are linked into a list. */
    if(curr_tokspace->next == (TokenSpace *)NULL) {
      TokenSpace *new_tokspace;
      if( (new_tokspace = (TokenSpace *)malloc(sizeof(TokenSpace)))
	 == (TokenSpace *)NULL) {
	oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
		     "Cannot alloc space for tokens");
	return (Token *)NULL; /*NOTREACHED*/
      }
      else {
	new_tokspace->next =  (TokenSpace *)NULL;
	curr_tokspace->next = new_tokspace;
      }
    }
    curr_tokspace = curr_tokspace->next;
    extra_tokspace += TOKENSPACESZ; /* Keep track of how much for -resource */
    token_space_top = 0;
  }
  return curr_tokspace->tokenspace + token_space_top++;
}


	/* note_filename():  This routine is called by main prog to give
	   symbol table routines access to current input file name, to be
	   stored in function arg list headers and common list headers, for
	   the use in diagnostic messages. Since filenames are from argv,
	   they are permanent, so pointer is copied, not the string.
	*/
void
#if HAVE_STDC
note_filename(char *s)
#else /* K&R style */
note_filename(s)
	char *s;
#endif /* HAVE_STDC */
{
	current_filename = s;
	top_filename = s;
}/* note_filename */

#ifdef DEBUG_EXPRTREES		/* Routines to print out expr tree src text */
void
print_src_text(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_tok_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}

void
print_expr_tree(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_tree_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}

void
print_expr_list(t)
     Token *t;
{
  char textbuf[256];
  (void) cp_list_src_text(textbuf,t,sizeof(textbuf)-1);
  fprintf(list_fd,"%s",textbuf);
}
#endif


void
#if HAVE_STDC
process_lists(int curmodhash)  /* Places pointer to linked list of arrays in
			      global symbol table */
#else /* K&R style */
process_lists(curmodhash)  /* Places pointer to linked list of arrays in
			      global symbol table */
#endif /* HAVE_STDC */

#if HAVE_STDC
	                   /* current_module_hash from fortran.y */
#else /* K&R style */
	int curmodhash;    /* current_module_hash from fortran.y */
#endif /* HAVE_STDC */
{
	int i, h;
	unsigned long hnum;
	Gsymtab *curr_gsymt;

	Gsymtab *gsymt;
	TokenListHeader *head_ptr;

	if( (curr_gsymt=
	     (curmodhash == -1) ? NULL:hashtab[curmodhash].glob_symtab)
	   == NULL) {
	  oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
		  "module not in global symtab:");
	  oops_tail(hashtab[curmodhash].name);
	}
	else {
	  if(curr_gsymt->internal_entry) {/* protect ourself */
	    warning(NO_LINE_NUM,NO_COL_NUM,
		    "entry point redefined as module");
	    msg_tail(curr_gsymt->name);
	    msg_tail(": previous definition overridden");
	    curr_gsymt->link.child_list = NULL;
	  }
	  curr_gsymt->internal_entry = FALSE;
	}

	for (i=0; i<loc_symtab_top; i++){
				/* Skip things which are not true externals */
	    if(loc_symtab[i].argument || loc_symtab[i].intrinsic ||
		   loc_symtab[i].array_var)
		      continue;

	    head_ptr = loc_symtab[i].info.toklist;

	    hnum=hash(loc_symtab[i].name);
	    while(h=hnum%HASHSZ,hashtab[h].name != NULL
		 && strcmp(hashtab[h].name,loc_symtab[i].name)!=0){
		      hnum = rehash(hnum);      /* Resolve clashes */
	    }

	    switch (storage_class_of(loc_symtab[i].type)){
		    case class_COMMON_BLOCK:
			if(head_ptr != NULL) {
if((gsymt=hashtab[h].com_glob_symtab) == NULL) {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
		 "common block not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
			Token *tok_ptr;
                        ComListHeader *c;

				/* First we link up possibly multiple
				   declarations of the same common block
				   in this module into one big list */
		    	while (tok_ptr = head_ptr->tokenlist,
			       (head_ptr = head_ptr->next) != NULL){
			    while(tok_ptr->next_token != NULL){
			        tok_ptr = tok_ptr->next_token;
			    }
			    tok_ptr->next_token = head_ptr->tokenlist;
			}
				/* Original token list is in reverse order.
				   Reverse it so order is correct. */
			head_ptr = loc_symtab[i].info.toklist;
			head_ptr->tokenlist =
			  reverse_tokenlist(head_ptr->tokenlist);

				/* Keep a copy for use by makedecls */
			loc_symtab[i].src.toklist = head_ptr;

				/* Now make it into array for global table */
		        c=make_com_array(head_ptr->tokenlist);
			c->module = curr_gsymt;
			c->filename = head_ptr->filename;
			c->topfile = top_filename;
			c->line_num = head_ptr->line_num;
			c->top_line_num = head_ptr->top_line_num;
			c->saved = global_save || loc_symtab[i].saved;

				/* add names to com list */
			make_com_names(head_ptr->tokenlist,
				       c,gsymt->info.comlist);

                        c->next = gsymt->info.comlist;
			gsymt->info.comlist = c;

		/* Replace token list by comlist for check_mixed_common */
			loc_symtab[i].info.comlist = c;
}
			}/* end if(head_ptr != NULL) */

		        break;	/* end case class_COMMON_BLOCK */


			/* Are we inside a function or subroutine? */
		    case class_VAR:
		       if(loc_symtab[i].entry_point) {
if((gsymt=hashtab[h].glob_symtab) == NULL) {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
    "subprog not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
                          ArgListHeader *a;
			  int implied_type;

				/* Make each token list into an array of
				   args for global table */
			  while (head_ptr != NULL){
			     a=make_dummy_arg_array(head_ptr->tokenlist);
			     make_arg_names(head_ptr->tokenlist,
					   a,gsymt->info.arglist);
			     implied_type = get_type(&(loc_symtab[i]));
			     a->type = type_byte(
			         class_SUBPROGRAM,implied_type);
			     a->size = get_size(&(loc_symtab[i]),implied_type);
			     a->module = curr_gsymt;
			     a->filename = head_ptr->filename;
			     a->topfile = top_filename;
			     a->line_num = head_ptr->line_num;
			     a->top_line_num = head_ptr->top_line_num;

			     a->next = gsymt->info.arglist;
			     gsymt->info.arglist = a;
			/* store arglist in local symtab for project file */
			     loc_symtab[i].info.arglist = a;
			     head_ptr = head_ptr->next;
		          }/* end while (head_ptr != NULL) */

			  if(loc_symtab[i].set_flag)
			         gsymt->set_flag = TRUE;
			  if(loc_symtab[i].used_flag)
			         gsymt->used_flag = TRUE;
			  if(loc_symtab[i].declared_external)
				 gsymt->declared_external = TRUE;
			  if(loc_symtab[i].library_module)
				 gsymt->library_module = TRUE;
			  gsymt->defined = TRUE;
			  if(gsymt != curr_gsymt) {
			    gsymt->internal_entry = TRUE;
			    gsymt->link.module = curr_gsymt;
			  }
}
			}/* end if(loc_symtab[i].entry_point) */
		    	break; /* end case class_VAR */

                    case class_SUBPROGRAM:
if((gsymt=hashtab[h].glob_symtab) == NULL) {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
    "subprog not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
                        ArgListHeader *a;
			int implied_type;
			while (head_ptr != NULL){
			  if(head_ptr->external_decl || head_ptr->actual_arg)
			    a=make_arrayless_alist();
			  else {
			    a=make_arg_array(head_ptr->tokenlist);
			    make_arg_names(head_ptr->tokenlist,
					   a,gsymt->info.arglist);
			  }
			  implied_type = get_type(&(loc_symtab[i]));
			  a->type = type_byte(
			         class_SUBPROGRAM,implied_type);
			  a->size = get_size(&(loc_symtab[i]),implied_type);
			  a->module = curr_gsymt;
			  a->filename = head_ptr->filename;
			  a->topfile = top_filename;
			  a->line_num = head_ptr->line_num;
			  a->top_line_num = head_ptr->top_line_num;
			  a->external_decl = head_ptr->external_decl;
			  a->actual_arg = head_ptr->actual_arg;

			  a->next = gsymt->info.arglist;
			  gsymt->info.arglist = a;
		/* put arglist into local symtab for project file use */
			  loc_symtab[i].info.arglist = a;
			  head_ptr = head_ptr->next;
		        }
			if(loc_symtab[i].used_flag)
			        gsymt->used_flag = TRUE;
if(debug_glob_symtab)
(void)fprintf(list_fd,"\nmodule %s local used=%d global used=%d",
gsymt->name,loc_symtab[i].used_flag,gsymt->used_flag);
}
				/* Add this guy to linked list of children,
				   unless never actually used. */
			if(loc_symtab[i].used_flag) {
			  ChildList *node=
			    (ChildList *)calloc(1,sizeof(ChildList));
			  node->child = gsymt;
			  node->next = curr_gsymt->link.child_list;
			  curr_gsymt->link.child_list = node;
			}

			break;/* end case class_SUBPROGRAM*/

                    case class_NAMELIST:
			if(head_ptr != NULL) {
			  Token *tok_ptr;

				/* Link up possibly multiple
				   declarations of the same namelist
				   in this module into one big list */
			  while (tok_ptr = head_ptr->tokenlist,
			       (head_ptr = head_ptr->next) != NULL){
			    while(tok_ptr->next_token != NULL){
			        tok_ptr = tok_ptr->next_token;
			    }
			    tok_ptr->next_token = head_ptr->tokenlist;
			  }
				/* Original token lists are in reverse order.
				   Reverse it so order is correct. */
			head_ptr = loc_symtab[i].info.toklist;
			head_ptr->tokenlist =
			  reverse_tokenlist(head_ptr->tokenlist);
			}
				/* Keep a copy for use by makedecls */
			loc_symtab[i].src.toklist = head_ptr;


			break;/* end case class_NAMELIST*/
	    }/* end switch */

        }/* end for (i=0; i<loc_symtab_top; i++) */

}/* process_lists */


void
#if HAVE_STDC
ref_array(Token *id, Token *subscrs)   /* Array reference: install in symtab */
#else /* K&R style */
ref_array(id,subscrs)   /* Array reference: install in symtab */
	Token *id, *subscrs;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;

				/* Restore subscripts to original order */
	subscrs->next_token = reverse_tokenlist(subscrs->next_token);

	if(symt == NULL){
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
		       "undeclared variable has dim info:");
	   oops_tail(hashtab[h].name);
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	else{    /* check that subscrs match dimension info */


	  if(arg_count(subscrs->next_token)!=array_dims(symt->info.array_dim)){
	      syntax_error(subscrs->line_num,subscrs->col_num,
			"array");
	      msg_tail(symt->name);
	      msg_tail("referenced with wrong no. of subscripts");
	  }
	}

}/* ref_array */

void
#if HAVE_STDC
ref_namelist(Token *id, int stmt_class)
#else /* K&R style */
ref_namelist(id,stmt_class)
     Token *id;
     int stmt_class;
#endif /* HAVE_STDC */
{
	Token *t;
	TokenListHeader *toklist;
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;
	if(symt == NULL){
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
			"undeclared identifier is a namelist:");
	   oops_tail(hashtab[h].name);
	   symt = install_local(h,type_NAMELIST,class_NAMELIST);
	   symt->info.toklist = NULL;
	}

			/* Go thru token list of namelist variables,
			   setting flags appropriately. */
	toklist = symt->info.toklist;
	if (toklist != NULL){
	    t = toklist->tokenlist;
	    while(t != NULL){
	        if(stmt_class == tok_READ)
		  use_lvalue(t);
		else
		  use_variable(t);
		t = t->next_token;
	    }
	}
}

void
#if HAVE_STDC
ref_variable(Token *id)	/* Variable reference: install in symtab */
#else /* K&R style */
ref_variable(id)	/* Variable reference: install in symtab */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;

	if( hashtab[h].loc_symtab == NULL) {
	   (void) install_local(h,type_UNDECL,class_VAR);
	}

}/*ref_variable*/

		/* this guy reverses a tokenlist and returns a pointer
		   to the new head. */
PRIVATE Token *
#if HAVE_STDC
reverse_tokenlist(Token *t)
#else /* K&R style */
reverse_tokenlist(t)
	Token *t;
#endif /* HAVE_STDC */
{
	Token *curr,*next,*temp;

	if(t == NULL)
	    return t;

	curr = t;
	next = curr->next_token;
	while(next != NULL) {
		temp = next->next_token;
		next->next_token = curr;
		curr = next;
		next = temp;
	}
	t->next_token = NULL;		/* former head is now tail */
	return curr;			/* curr now points to new head */
}

void
#if HAVE_STDC
save_com_block(Token *id)	/* Process SAVEing of a common block */
	          	/* N.B. Legality checking deferred to END */
#else /* K&R style */
save_com_block(id)	/* Process SAVEing of a common block */
	Token *id;	/* N.B. Legality checking deferred to END */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

			/* N.B. SAVE does not create a global table entry */
	if( (symt = hashtab[h].com_loc_symtab) == NULL){
	   symt = install_local(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   symt->info.toklist = NULL;
	}

	if(symt->saved) {
	  syntax_error(id->line_num,id->col_num,
		       "redundant SAVE declaration");
	}
	else
	  symt->saved = TRUE;
}

void
#if HAVE_STDC
save_variable(Token *id)	/* Process SAVEing of a variable */
	          	/* N.B. Legality checking deferred to END */
#else /* K&R style */
save_variable(id)	/* Process SAVEing of a variable */
	Token *id;	/* N.B. Legality checking deferred to END */
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}

	if(symt->saved) {
	  syntax_error(id->line_num,id->col_num,
		       "redundant SAVE declaration");
	}
	else {		/* set flags for all equivalenced vars */
	  Lsymtab *equiv=symt;
	  do{
	    equiv->saved = TRUE;
	    equiv = equiv->equiv_link;
	  } while(equiv != symt);
	}
}

	/* Following routine sets the implicit typing of characters in
	   range c1 to c2 to the given type. */
void
#if HAVE_STDC
set_implicit_type(int type, long int size, char *len_text, int c1, int c2)
	         		/* Data type of IMPLICIT declaration */
                  		/* Type size or size_DEFAULT if not given */
	               		/* Source text of length spec */
	       			/* First character of range */
	       			/* Last character of range */
#else /* K&R style */
set_implicit_type(type,size,len_text,c1,c2)
	int type;		/* Data type of IMPLICIT declaration */
        long size;		/* Type size or size_DEFAULT if not given */
	char *len_text;		/* Source text of length spec */
	int c1;			/* First character of range */
	int c2;			/* Last character of range */
#endif /* HAVE_STDC */
{
	int c;
#ifdef ALLOW_DOLLARSIGNS
	  if(c1 == '$')  c1 = 'Z'+1;
	  if(c2 == '$')  c2 = 'Z'+1;
#endif
#ifdef ALLOW_UNDERSCORES
	  if(c1 == '_')  c1 = 'Z'+2;
	  if(c2 == '_')  c2 = 'Z'+2;
#endif
	if(c2 < c1) {
		yyerror("IMPLICIT range must be in alphabetical order");
	}
	else {
		/* Fill in the lookup table for the given range of chars */
	  for(c=c1; c<=c2; c++) {
		implicit_type[c-'A'] = type;
		implicit_size[c-'A'] = size;
		implicit_len_text[c-'A'] = len_text;
	  }
	}
}/*set_implicit_type*/


		/* Finish processing statement function.
		   Clears all used-before-set flags of ordinary
		   variables. Reason: statement functions are processed
		   like assignment to an array element, setting ubs flags.
		   At this point, no valid setting of ubs flags should
		   be possible, so clearing them will elim false messages.*/
void
#if HAVE_STDC
stmt_function_stmt(Token *id)			/* ARGSUSED0 */
               			/* Not used at present */
#else /* K&R style */
stmt_function_stmt(id)			/* ARGSUSED0 */
     Token *id;			/* Not used at present */
#endif /* HAVE_STDC */
{
    int i;
    for(i=0; i<loc_symtab_top; i++) {
	if(storage_class_of(loc_symtab[i].type) == class_VAR &&
	   ! loc_symtab[i].parameter )
	  loc_symtab[i].used_before_set = FALSE;
    }
}/*stmt_function_stmt(id)*/

char *
#if HAVE_STDC
token_name(Token t)
#else /* K&R style */
token_name(t)
	Token t;
#endif /* HAVE_STDC */
{
	return hashtab[t.value.integer].name;
}/*token_name*/




void
#if HAVE_STDC
use_actual_arg(Token *id)	/* like use_lvalue except does not set assigned_flag */
#else /* K&R style */
use_actual_arg(id)	/* like use_lvalue except does not set assigned_flag */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(h,type_UNDECL,class_VAR);
	}
	else {
			/* If an external other than an intrinsic, set up
			   tokenlist for "call".  If intrinsic, check
			   legality of this usage.) */
	  if(storage_class_of(symt->type) == class_SUBPROGRAM) {
	    if(symt->intrinsic) {
	      IntrinsInfo *defn = symt->info.intrins_info;
	      if( !(symt->declared_intrinsic) ) {
		warning(id->line_num,id->col_num,
				defn->name);
		msg_tail("not declared INTRINSIC");
	      }
	      if( (defn->intrins_flags&I_NOTARG) ) {
		syntax_error(id->line_num,id->col_num,
				defn->name);
		msg_tail("intrinsic function cannot be a subprogram argument");
	      }
	    }
	    else {		/* External subprogram as actual arg */
	      TokenListHeader *TH_ptr;
	      TH_ptr= make_TL_head(id);

	      TH_ptr->actual_arg = TRUE;
	      TH_ptr->next = symt->info.toklist;
	      symt->info.toklist = TH_ptr;
	    }
	  }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->set_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_actual_arg*/


PRIVATE void
#if HAVE_STDC
use_function_arg(Token *id)	/* Like use_variable but invokes use_actual_arg
			   if id is an external (subprogram) passed as
			   arg of a function. This routine is used when
			   pure_functions flag is set. */
#else /* K&R style */
use_function_arg(id)	/* Like use_variable but invokes use_actual_arg
			   if id is an external (subprogram) passed as
			   arg of a function. This routine is used when
			   pure_functions flag is set. */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}

	if(storage_class_of(symt->type) == class_SUBPROGRAM)
	  use_actual_arg(id);
	else
	  use_variable(id);

}/*use_function_arg*/

void
#if HAVE_STDC
use_implied_do_index(Token *id)
#else /* K&R style */
use_implied_do_index(id)
	Token *id;
#endif /* HAVE_STDC */
{
		/* Like use_lvalue and use_variable but clears ubs flag.
	           This is because we cannot handle used-before-set
		   properly in this case, and the odds are that ubs
		   was set in the preceding I/O list. */
	int h=id->value.integer;
	Lsymtab *symt;

	use_lvalue(id);
	use_variable(id);
	symt=hashtab[h].loc_symtab;

	symt->used_before_set = FALSE;
}/*use_implied_do_index*/


	/* use_io_keyword handles keyword=value fields in i/o control lists */

#include "iokeywds.h"

void
#if HAVE_STDC
use_io_keyword(Token *keyword, Token *value, int stmt_class)
#else /* K&R style */
use_io_keyword(keyword,value,stmt_class)
     Token *keyword,*value;
     int stmt_class;
#endif /* HAVE_STDC */
{
    int i, k, stmt_flag=0, type_flag, setit,useit;
    int hkey=keyword->value.integer;
    extern int io_internal_file, io_list_directed;/* shared with fortran.y */

		/* Convert statement_class (a token class) into
		   a bit flag compatible with io_keywords table. */
    for(i=0; i<NUM_IO_STMTS; i++) {
	if(local_class[i].stmt_class == stmt_class) {
	    stmt_flag = local_class[i].stmt_flag;
	    break;
	}
    }
    if(stmt_flag == 0) {
      oops_message(OOPS_NONFATAL,keyword->line_num,keyword->col_num,
	"not an i/o statement class:");
      (void)fprintf(stderr,"%d",stmt_class);
      return;
    }
		/* Convert value datatype into
		   a bit flag compatible with io_keywords table.
		   Note that '*' is handled by using type_UNDECL */
    if(value->class == '*')
	type_flag = STAR;
    else
	type_flag = (1<<datatype_of(value->TOK_type));

				/* Look up keyword in table*/
    k = find_io_keyword(hashtab[hkey].name);

		/* Not found or nonstandard: issue warning.  Note
		   that not-found is also nonstandard.  All nonstandard
		   I/O keywds are VMS extensions, warn if allow_vms_io=0
		 */
    if(io_keywords[k].nonstandard
#ifdef ALLOW_VMS_IO /* special VMS case: OPEN(...,NAME=str,...) */
       || (io_keywords[k].special && stmt_flag==OP)
#endif /*ALLOW_VMS_IO*/
	   ) {
		/* If nonstandard and -f77 flag given, issue warning */
	if(f77_vms_io) {
	  nonstandard(keyword->line_num,keyword->col_num);
	}


	if(io_keywords[k].name == NULL) {
	    if(f77_vms_io) {
	      /* abbrev warning if nonstd message given */
		msg_tail(": unrecognized keyword");
	    }
	    else {
		warning(keyword->line_num,keyword->col_num,
		"Unrecognized keyword");
	    }
	    msg_tail(hashtab[hkey].name);
	    msg_tail("--  Ftnchek may process incorrectly");
	}
    }

	/* If label expected, switch integer const to label */
    if( (LAB & io_keywords[k].allowed_types)
       &&  (type_flag == INT && is_true(LIT_CONST,value->TOK_flags))) {
	type_flag = LAB;
    }

	/*  Now check it out */


		/* Check if keyword is allowed with statement */

    if(!(stmt_flag & io_keywords[k].allowed_stmts)) {
	syntax_error(keyword->line_num,keyword->col_num,
		     "keyword illegal in this context");
	return;
    }

		/* Check if the type is OK */

    if( !(type_flag & io_keywords[k].allowed_types) ) {
	syntax_error(value->line_num,value->col_num,
		     "control specifier is incorrect type");
	return;
    }


	/* Now handle usage */

				/* internal file?: WRITE(UNIT=str,...) */
    if(stmt_flag == WR && type_flag == CHR
	    && io_keywords[k].allowed_types == UID) {
	setit = TRUE;
	useit = FALSE;
    }
				/* INQUIRE: set it if inquire_set flag true */
    else if(stmt_flag == INQ && io_keywords[k].inquire_set) {
	setit = TRUE;
	useit = FALSE;
    }
     /* otherwise use use/set flags in table */
    else {
	useit = io_keywords[k].implies_use;
	setit = io_keywords[k].implies_set;
    }

				/* Keep note if format is '*' */
    if(value->class == '*' && io_keywords[k].allowed_types == FID ) {
      io_list_directed = TRUE;
    }

			/* Handle NML=namelist */
    if(type_flag == NML){
      ref_namelist(value,stmt_class);
    }
			/* Update usage status if a variable. */
    if( is_true(ID_EXPR,value->TOK_flags)) {
	if(useit) {
	    use_variable(value);
	}
	if(setit) {
	    use_lvalue(value);
	}
		/* Character variable as unit id = internal file */
	if(type_flag == CHR && io_keywords[k].allowed_types == UID) {
	  io_internal_file = TRUE;
	}
    }
    else if(setit) {		/* if value is set, must be an lvalue */
	    syntax_error(value->line_num,value->col_num,
			 "variable required");
	    return;
    }
}


		/* Handle VMS OPEN keywords that have no =value */
void
#if HAVE_STDC
use_special_open_keywd(Token *id)
#else /* K&R style */
use_special_open_keywd(id)
     Token *id;
#endif /* HAVE_STDC */
{
#ifdef ALLOW_VMS_IO
  int i;
  char *id_name= hashtab[id->value.integer].name;

  for(i=0; i<NUM_SPECIAL_OPEN_KEYWDS; i++) {
    if(strcmp(id_name,special_open_keywds[i]) == 0) {
				/* found: report nonstandard if requested */
      if(f77_vms_io)
	nonstandard(id->line_num,id->col_num);
      return;
    }
  }
#endif/*ALLOW_VMS_IO*/
				/* not found or not VMS: report error */
  syntax_error(id->line_num,id->col_num,
	       "Illegal control-list item");
}

PRIVATE void
#if HAVE_STDC
use_len_arg(Token *id)		/* Set the use-flag of arg to intrinsic LEN. */
#else /* K&R style */
use_len_arg(id)		/* Set the use-flag of arg to intrinsic LEN. */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}

    {		/* set flags for all equivalenced vars.  Do not set
		   the used-before-set flag since LEN argument does
		   not need to be defined. */
      Lsymtab *equiv=symt;
      do{
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_len_arg*/

void
#if HAVE_STDC
use_lvalue(Token *id)	/* handles scalar lvalue */
#else /* K&R style */
use_lvalue(id)	/* handles scalar lvalue */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;
	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(h,type_UNDECL,class_VAR);
	}
	else {
	  /*   check match to previous invocations and update  */
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->set_flag = TRUE;
	equiv->assigned_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_lvalue*/



void                    /* Process data_constant_value & data_repeat_factor */
#if HAVE_STDC
use_parameter(Token *id)
#else /* K&R style */
use_parameter(id)
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}
	if(! symt->parameter) {
		syntax_error(id->line_num,id->col_num,
			"must be a parameter");
		symt->parameter = TRUE;
	}

	if(! symt->set_flag) {
	   symt->used_before_set = TRUE;
	}
	symt->used_flag = TRUE;

}/*use_parameter*/


void
#if HAVE_STDC
use_variable(Token *id)		/* Set the use-flag of variable. */
#else /* K&R style */
use_variable(id)		/* Set the use-flag of variable. */
	Token *id;
#endif /* HAVE_STDC */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(h,type_UNDECL,class_VAR);
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) {
	   equiv->used_before_set = TRUE;
	}
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*use_variable*/


/*  End of symtab.c */

/*

 II. Hash

*/

/*    hash.c:
 	performs a hash function

This was formerly a separate file.

*/

extern int sixclash;	/* flag to check clashes in 1st 6 chars of name */

unsigned long
#if HAVE_STDC
hash(char *s)
#else /* K&R style */
hash(s)
    char *s;
#endif /* HAVE_STDC */
{
    unsigned long sum = 0, wd;
    int i = 0,j;

    int n = strlen(s);
    if(sixclash && n > 6) n = 6;

    while (i < n) {
         wd = 0;
         for(j=1; j <= sizeof(long) && i < n; i++,j++) {
            wd += (unsigned long)(s[i] & 0xff) << (sizeof(long) - j) * 8;}

	sum ^= wd;}
    return sum;
}

		/* Same as hash() but always uses full length of keyword.
		   To keep the keyword table clash-free on any machine,
		   packs only 4 bytes per word even if long is bigger */
PRIVATE unsigned long
#if HAVE_STDC
kwd_hash(char *s)
#else /* K&R style */
kwd_hash(s)
    char *s;
#endif /* HAVE_STDC */
{
    unsigned long sum = 0, wd;
    int i = 0,j;

    int n = strlen(s);

    while (i < n) {
         wd = 0;
         for(j=1; j <= 4 && i < n; i++,j++) {
            wd += (unsigned long)(s[i] & 0xff) << (4 - j) * 8;}

	sum ^= wd;}
    return sum;
}



/*    rehash.c
        performs a rehash for resolving clashes.
*/

#ifdef COUNT_REHASHES
unsigned long rehash_count=0;
#endif

unsigned long
#if HAVE_STDC
rehash(long unsigned int hnum)
#else /* K&R style */
rehash(hnum)
    unsigned long hnum;
#endif /* HAVE_STDC */
{
#ifdef COUNT_REHASHES
    rehash_count++;
#endif
    return hnum+1;
}


/*  End of hash */


/*

III. Intrins

*/

/* intrinsic.c:

	Handles datatyping of intrinsic functions.
*/


	/* File intrinsic.h contains information from Table 5, pp. 15-22
	   to 15-25 of the standard.  Note: num_args == -1 means 1 or 2 args,
	   num_args == -2 means 2 or more args.  Value of arg_type is the OR
	   of all allowable types (I, R, etc. as defined above).  Value of
	   result_type is type returned by function (type_INTEGER, etc.).
	   If result_type is type_GENERIC, function type is same as arg type.
	*/


PRIVATE IntrinsInfo intrinsic[]={
#include "intrins.h"
};

#define NUM_INTRINSICS (sizeof(intrinsic)/sizeof(intrinsic[0]))

#define EMPTY 255

PRIVATE unsigned char intrins_hashtab[INTRINS_HASHSZ];

/*    init_intrins_hashtab:
                 Initializes the intrinsic hash table by clearing it to EMPTY
                 and then hashes all the intrinsic names into the table.
*/

unsigned long
init_intrins_hashtab(VOID)
{
    unsigned i,h;
    unsigned long hnum;
    unsigned long numclashes=0;

    for(h=0;h<INTRINS_HASHSZ;h++) {
           intrins_hashtab[h] = EMPTY;
    }
    for(i=0; i < NUM_INTRINSICS; i++) {
	   hnum = kwd_hash(intrinsic[i].name);
	   while(h=hnum%INTRINS_HASHSZ, intrins_hashtab[h] != EMPTY) {
		hnum = rehash(hnum);
		numclashes++;
	   }
	   intrins_hashtab[h] = i;
    }
    return numclashes;
}


		/* Function called by main to alter intrinsic table for
		   user-selected options respecting RAND and IARGC.
		 */
void
#if HAVE_STDC
set_intrinsic_options(int option)
#else /* K&R style */
set_intrinsic_options(option)
     int option;
#endif /* HAVE_STDC */
{
				/* RAND and IRAND use tens digit */
    set_intrinsic_numargs("RAND",(option/10)%10);
    set_intrinsic_numargs("IRAND",(option/10)%10);
				/* IARGC uses hundreds digit */
    set_intrinsic_numargs("IARGC",(option/100)%10);
}

#if HAVE_STDC
PRIVATE void set_intrinsic_numargs(char *name, int choice)
                		/* Name of function to fix up */
                		/* 0 = none, 1 = one, 2 = either */
#else /* K&R style */
PRIVATE void set_intrinsic_numargs(name,choice)
     char *name;		/* Name of function to fix up */
     int choice;		/* 0 = none, 1 = one, 2 = either */
#endif /* HAVE_STDC */
{
  IntrinsInfo *defn;

  if(choice == 2)		/* Convert digit to num_args equivalent */
    choice = I_0or1;

  defn = find_intrinsic(name);
  if(defn != (IntrinsInfo *)NULL) {
    defn->num_args = choice;
  }
}


	/* Function to look up an intrinsic function name in table.
	   If found, returns ptr to table entry, otherwise NULL.
	*/
PRIVATE IntrinsInfo *
#if HAVE_STDC
find_intrinsic(char *s)
	        			/* given name */
#else /* K&R style */
find_intrinsic(s)
	char *s;			/* given name */
#endif /* HAVE_STDC */
{
	unsigned i, h;
	unsigned long hnum;

	hnum = kwd_hash(s);
	for(;;) {
	  h=hnum%INTRINS_HASHSZ;
	  if( (i=intrins_hashtab[h]) == EMPTY )
	    break;		/* Not found */

				/* Something found: see if a match */
	  if( strcmp(s,intrinsic[i].name) == 0  &&
	      ((intrinsic[i].intrins_flags&(I_EXTRA|I_VMS|I_UNIX))==0 ||
	       ((intrinsic[i].intrins_flags&I_EXTRA) && intrinsic_set != 0) ||
	       ((intrinsic[i].intrins_flags&I_UNIX) && intrinsic_set == 2) ||
	       ((intrinsic[i].intrins_flags&I_VMS) && intrinsic_set == 3)
	      ) ) {

	    return &intrinsic[i];
	  }
	  else {		/* No match: try next */
	    hnum = rehash(hnum);
	  }
	}
				/* Not an intrinsic function */
	return (IntrinsInfo *)NULL;
}

	/* find_io_keyword looks up an i/o keyword in io_keywords
	   table and returns its index.  Uses simple linear search
	   since not worth hash overhead.  If not found, returns
	   index of last element of list, which is special. */
PRIVATE int
#if HAVE_STDC
find_io_keyword(char *s)
             			/* given name */
#else /* K&R style */
find_io_keyword(s)
     char *s;			/* given name */
#endif /* HAVE_STDC */
{
    int i;
    for(i=0; io_keywords[i].name != NULL; i++) {
	if(strcmp(io_keywords[i].name, s) == 0) {
	    break;
	}
    }
    return i;
}



#ifdef DEBUG_SIZES
void print_sizeofs()			/* For development: print sizeof for
				   various data structures */
{
#ifdef __STDC__
#define PrintObjSize(OBJ) (void)fprintf(list_fd,#OBJ " size = %d\n",sizeof(OBJ))
#else			/* K&R form */
#define PrintObjSize(OBJ) (void)fprintf(list_fd,"OBJ size = %d\n",sizeof(OBJ))
#endif
  PrintObjSize(char *);
  PrintObjSize(Token);
  PrintObjSize(Lsymtab);
  PrintObjSize(Gsymtab);
  PrintObjSize(HashTable);
  PrintObjSize(ArgListHeader);
  PrintObjSize(ArgListElement);
  PrintObjSize(ComListHeader);
  PrintObjSize(ComListElement);
  PrintObjSize(TokenListHeader);
  PrintObjSize(InfoUnion);
  PrintObjSize(IntrinsInfo);
  PrintObjSize(ParamInfo);
  PrintObjSize(ChildList);
}
#endif
