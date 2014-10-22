/* plsymtab.c:

		Routines associated with printing of local symbol table info

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.

	Shared functions defined:

		debug_symtabs()	Prints debugging info about symbol tables.
		print_loc_symbols(curmodhash) Prints local symtab info.

	Private functions defined:
		has_nonalnum()	  True if string has non-alphanumeric char
		sort_symbols()	  Sorts the list of names of a given category.
		swap_symptrs()	  Swaps a pair of pointers.
		check_flags()     Outputs messages about used-before-set etc.
		check_mixed_common() checks common for nonportable mixed type
		print_symbols(sym_list,n,do_types) Prints symbol lists.
		print_variables(sym_list,n)  Prints variable symbol table
		find_sixclashes() Finds variables with the same first 6 chars.
		identify_module(mod_name) Prints module name and file name.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "ftnchek.h"
#define PLSYMTAB
#include "symtab.h"

				/* Declarations of local functions */

PROTO(PRIVATE void sort_positions,( Lsymtab *sp[], int n ));
PROTO(PRIVATE void sort_symbols,( Lsymtab *sp[], int n ));
PROTO(PRIVATE void swap_symptrs,( Lsymtab **x_ptr, Lsymtab **y_ptr ));
PROTO(PRIVATE void identify_module,( char *mod_name ));
PROTO(PRIVATE int has_nonalnum,( char *s ));
PROTO(PRIVATE int print_symbols,( FILE *fd, Lsymtab *sym_list[], int n, int
			  do_types ));
PROTO(PRIVATE int print_variables,( Lsymtab *sym_list[], int n ));
PROTO(PRIVATE int print_var_type,( FILE *fd, Lsymtab *symt ));
PROTO(PRIVATE int find_sixclashes,( Lsymtab *list[] ));
#ifdef DEBUG_SYMTABS
PROTO(PRIVATE void print_arg_array,( ArgListHeader *arglist ));
PROTO(PRIVATE void print_com_array,( ComListHeader *cmlist ));
PROTO(PRIVATE void print_tokenlist,( TokenListHeader *toklist ));
#endif
PROTO(PRIVATE int make_sym_list,( Lsymtab *sym_list[], int (*select)(Lsymtab
							      *sym_entry) ));
PROTO(PRIVATE void check_mixed_common,( FILE *fd, Lsymtab *sym_list[], int n ));
PROTO(PRIVATE void check_flags,( Lsymtab *list[], int n, unsigned used,
			 unsigned set, unsigned ubs, char *msg, char
			 *mod_name ));

PROTO(PRIVATE void append_char_to_fragment,( int c ));
PROTO(PRIVATE void append_string_to_fragment,( char *s ));
PROTO(PRIVATE void append_expr_text_to_fragment,( char *s ));
PROTO(PRIVATE void make_declarations,( Lsymtab *sym_list[], char *mod_name ));
PROTO(PRIVATE void maybe_print_module_header,( void ));
PROTO(PRIVATE void new_fragment,( void ));
PROTO(PRIVATE void print_blanks,( int nblanks ));
PROTO(PRIVATE void print_common_decls,( Lsymtab *sym_entry ));
PROTO(PRIVATE void print_empty_comment_line,( void ));
PROTO(PRIVATE void print_equivalence_decls,( Lsymtab *sym_entry ));
PROTO(PRIVATE int count_undeclared_variables,( Lsymtab *sym_entry ));
PROTO(PRIVATE void print_list_decls,( Lsymtab *sym_list[], int n, char
			      *header, char *list_type_name ));
PROTO(PRIVATE int print_list_name,( char *list_type_name, char *name ));
PROTO(PRIVATE void print_declaration_class,( Lsymtab *sym_list[], int n, char
				     *header ));
PROTO(PRIVATE void print_one_list_decls,( Lsymtab *sym_entry, char
				  *list_type_name, char **pheader, int
				  *pnd ));
PROTO(PRIVATE void print_parameter_statement,( Lsymtab *symt ));
PROTO(PRIVATE void print_selected_declarations,( Lsymtab *sym_list[], int n,
					 int the_type, char
					 *type_name, char **pheader ));
PROTO(PRIVATE int print_type_name,( int the_type, char *type_name, int
			    the_size, Lsymtab *symt ));
PROTO(PRIVATE int select_arguments,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_commons,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_externals_by_name,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_externals_by_type,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_intrinsics_by_name,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_intrinsics_by_type,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_locals,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_common_blocks,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_namelists,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_parameters,( Lsymtab *sym_entry ));
PROTO(PRIVATE int select_statement_functions,( Lsymtab *sym_entry ));
PROTO(PRIVATE int sf3_internal_name,( Lsymtab *sym_entry ));



PRIVATE void
#if HAVE_STDC
sort_positions(Lsymtab **sp, int n) /* sort a given list by sequence num instead of name */
#else /* K&R style */
sort_positions(sp,n) /* sort a given list by sequence num instead of name */
    Lsymtab *sp[];
    int n;
#endif /* HAVE_STDC */
{
    int i,j,swaps;

    for (i = 0; i < n; i++)
    {
	swaps = 0;
	for (j = n-1; j >= i+1; j--)
	{
	    if ( sp[j-1]->info.param->seq_num > sp[j]->info.param->seq_num )
	    {
		swap_symptrs(&sp[j-1], &sp[j]);
		swaps ++;
	    }
	}
	if(swaps == 0)
	    break;
    }
}


PRIVATE void
#if HAVE_STDC
sort_symbols(Lsymtab **sp, int n)      /* sorts a given list */
#else /* K&R style */
sort_symbols(sp,n)      /* sorts a given list */
	Lsymtab *sp[];
	int n;
#endif /* HAVE_STDC */
{
	int i,j,swaps;
	for(i=0;i<n;i++) {
	    swaps = 0;
	    for(j=n-1;j>=i+1;j--) {
		if((strcmp(sp[j-1]->name, sp[j]->name)) > 0) {
		   swap_symptrs(&sp[j-1], &sp[j]);
		   swaps ++;
		}
	    }
	    if(swaps == 0) break;
	}
}


PRIVATE void			/* swaps two pointers */
#if HAVE_STDC
swap_symptrs(Lsymtab **x_ptr, Lsymtab **y_ptr)
#else /* K&R style */
swap_symptrs(x_ptr,y_ptr)
	Lsymtab **x_ptr,**y_ptr;
#endif /* HAVE_STDC */
{
	Lsymtab *temp = *x_ptr;
	*x_ptr = *y_ptr;
	*y_ptr = temp;
}


/* Routine to print module name and file name just once in standard
   format is shared by print_loc_symbols, check_mixed_common and check_flags*/
PRIVATE int any_warnings;

PRIVATE void
#if HAVE_STDC
identify_module(char *mod_name)
#else /* K&R style */
identify_module(mod_name)
     char *mod_name;
#endif /* HAVE_STDC */
{
  if(do_symtab) {
    (void)fprintf(list_fd,"\nWarning: ");
  }
  else {
    if(any_warnings++ == 0) { /* 1st message of this module? */
      if(novice_help) {		/* Old-style format */
	(void)fprintf(list_fd,
		"\nWarning in module %s file %s:",
		mod_name,current_filename);
      }
      else {			/* Lint-style format */
	(void)fprintf(list_fd,
		"\n\"%s\" module %s: Warning:",
		current_filename,mod_name);
      }
    }
    (void)fprintf(list_fd,"\n   ");	/* Details go indented on next line */
  }
  ++warning_count;		/* Count these warnings too */
}


void
#if HAVE_STDC
print_loc_symbols(int curmodhash)
                    		/* hash entry of current module */
#else /* K&R style */
print_loc_symbols(curmodhash)
     int curmodhash;		/* hash entry of current module */
#endif /* HAVE_STDC */
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
    static Lsymtab **sym_list=(Lsymtab **)NULL;
#else
    Lsymtab *sym_list[LOCSYMTABSZ]; /* temp. list of symtab entries to print */
#endif
    int	mod_type,		/* datatype of this module */
	this_is_a_function;	/* flag for treating funcs specially */
    Lsymtab *module;	 	/* entry of current module in symtab */
    char *mod_name;		/* module name */
    int
	com_vars_modified=0,	/* count of common variables which are set */
	args_modified=0,	/* count of arguments which are set */
	imps=0,			/* count of implicitly declared identifiers */
	numentries;		/* count of entry points of module */

    if (dcl_fd == (FILE*)NULL)
	dcl_fd = stdout;

#ifdef DYNAMIC_TABLES
    if(sym_list == (Lsymtab **)NULL) { /* Initialize if not done before */
      if( (sym_list=(Lsymtab **)calloc(LOCSYMTABSZ,sizeof(Lsymtab *)))
	 == (Lsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for local symbol list");
      }
    }
#endif

    any_warnings=0;		/* for identify_module(mod_name); */

				/* Keep track of statement counts
				   for -resource  */
    tot_exec_stmt_count += exec_stmt_count;
    if(exec_stmt_count > max_exec_stmt_count)
	max_exec_stmt_count = exec_stmt_count;

			/* Keep track of symbol table and string usage */
    if(loc_symtab_top > max_loc_symtab) {
	max_loc_symtab = loc_symtab_top;
    }
    if(loc_str_top + extra_locstrspace > max_loc_strings) {
	max_loc_strings = loc_str_top + extra_locstrspace;
    }
    if(srctextspace_top + extra_srctextspace > max_srctextspace) {
      max_srctextspace = srctextspace_top + extra_srctextspace;
    }
    if(token_head_space_top + extra_tokheadspace > max_tokenlists) {
      max_tokenlists=token_head_space_top + extra_tokheadspace;
    }
    if(param_info_space_top + extra_paraminfospace > max_paraminfo) {
      max_paraminfo=param_info_space_top + extra_paraminfospace;
    }
    if(token_space_top + extra_tokspace > max_token_space) {
	max_token_space = token_space_top + extra_tokspace;
    }
    if(ptrspace_top + extra_ptrspace > max_ptrspace) {
      max_ptrspace = ptrspace_top + extra_ptrspace;
    }
			/* Global symbols only increase in number */
    max_glob_symtab = glob_symtab_top;


		/* Set up name & type, and see what kind of module it is */

	      module = hashtab[curmodhash].loc_symtab;

	      mod_name = module->name;
	      mod_type = get_type(module);

	      if(  mod_type != type_PROGRAM
		&& mod_type != type_SUBROUTINE
		&& mod_type != type_COMMON_BLOCK
		&& mod_type != type_BLOCK_DATA )
			this_is_a_function = TRUE;
	      else
			this_is_a_function = FALSE;

				/* Print name & type of the module */
    if(do_symtab) {
      int i;
      for(i=0,numentries=0;i<loc_symtab_top;i++) {
	if(loc_symtab[i].entry_point)
	  sym_list[numentries++] = &loc_symtab[i];
      }

	   if(numentries > 1) {
	      sort_symbols(sym_list,numentries);
	   }


	  (void)fprintf(list_fd,"\n\nModule %s:",mod_name);
	  if( this_is_a_function ) (void)fprintf(list_fd," func:");
	  (void)fprintf(list_fd," %4s",type_name[mod_type]);
			/* Print a * next to non-declared function name */
	  if(datatype_of(module->type) == type_UNDECL ) {
			(void)fprintf(list_fd,"*");
			imps++;
	  }
	  (void)fprintf(list_fd,"\n");


				/* Print Entry Points (skip if only one,
				   since it is same as module name) */
      if(do_symtab && numentries > 1) {
	      (void)fprintf(list_fd,"\nEntry Points\n");
	      (void) print_symbols(list_fd,sym_list,numentries,FALSE);
      }

			/* End of printing module name and entry points */
    }/*if(do_symtab)*/



				/* Print the externals */

    if(do_symtab) {
	int i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
	    if(storage_class_of(loc_symtab[i].type) == class_SUBPROGRAM) {
		  sym_list[n++] = &loc_symtab[i];
	    }
	}
	if(n != 0) {
	      sort_symbols(sym_list,n);

	      if (do_symtab)
	      {
		  (void)fprintf(list_fd,"\nExternal subprograms referenced:\n");
		  imps += print_symbols(list_fd,sym_list,n,TRUE);
	      }
	}

      }/*if(do_symtab)*/


				/* Print list of statement functions */
    if(do_symtab || check_ext_unused) {
	   int i,n;

	   for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(storage_class_of(loc_symtab[i].type) == class_STMT_FUNCTION){
		  sym_list[n++] = &loc_symtab[i];
	       }
	   }
	   if(n != 0) {
	      sort_symbols(sym_list,n);
	      if(do_symtab) {
		(void)fprintf(list_fd,"\nStatement functions defined:\n");
		imps += print_symbols(list_fd,sym_list,n,TRUE);
	      }
	      if(check_ext_unused) {
		check_flags(sym_list,n,0,1,0,
		 "Statement functions declared but never referenced",mod_name);
	      }
	    }
    }/*if(do_symtab)*/


				/* Print the common blocks */
    if(do_symtab || port_common_alignment || f77_mixed_common) {
	   int i,numblocks;

	   for(i=0,numblocks=0;i<loc_symtab_top;i++) {
	      if(storage_class_of(loc_symtab[i].type) == class_COMMON_BLOCK) {
		  sym_list[numblocks++] = &loc_symtab[i];
	      }
	   }

	   if(numblocks != 0) {
	      sort_symbols(sym_list,numblocks);
	      if(do_symtab) {
		  (void)fprintf(list_fd,"\nCommon blocks referenced:\n");
		  (void) print_symbols(list_fd,sym_list,numblocks,FALSE);
	      }
	      if(port_common_alignment || f77_mixed_common) {
		    check_mixed_common(list_fd,sym_list,numblocks);
	      }
	   }
     }/*if(do_symtab||port_common_alignment||f77_mixed_common)*/

				/* Print the namelists */
    if(do_symtab) {
	   int i,numlists;

	   for(i=0,numlists=0;i<loc_symtab_top;i++) {
	      if(storage_class_of(loc_symtab[i].type) == class_NAMELIST) {
		  sym_list[numlists++] = &loc_symtab[i];
	      }
	   }

	   if(numlists != 0) {
	      sort_symbols(sym_list,numlists);
	      if(do_symtab) {
		  (void)fprintf(list_fd,"\nNamelists defined:\n");
		  (void) print_symbols(list_fd,sym_list,numlists,FALSE);
	      }
	    }

    }/* End printing the namelists */
				/* Process the variables */

    if(do_symtab || var_usage_check) {
	int i,n;

	for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(storage_class_of(loc_symtab[i].type) == class_VAR
	       && (!loc_symtab[i].entry_point || this_is_a_function)) {
		  sym_list[n++] = &loc_symtab[i];
		  if(loc_symtab[i].argument && loc_symtab[i].set_flag) {
		    if(++args_modified <= 3)
			if(this_is_a_function && pure_functions) {
			    identify_module(mod_name);
			    (void)fprintf(list_fd,
				  "Function %s %s argument %s",
				  mod_name,
				  loc_symtab[i].assigned_flag?
					"modifies":"may modify",
				  loc_symtab[i].name);
			}
		  }
		  if(loc_symtab[i].common_var && loc_symtab[i].set_flag) {
		    if(++com_vars_modified <= 3)
			if(this_is_a_function && pure_functions) {
			    identify_module(mod_name);
			    (void)fprintf(list_fd,
				  "Function %s %s common variable %s",
				  mod_name,
				  loc_symtab[i].assigned_flag?
					"modifies":"may modify",
				  loc_symtab[i].name);
			}
		  }
	       }
	}
	if(args_modified > 3 || com_vars_modified > 3)
	  if(this_is_a_function && pure_functions)
	    (void)fprintf(list_fd,"\netc...");
	if(n != 0) {
	   sort_symbols(sym_list,n);

			/* Print the variables */

	   if(do_symtab) {
	      (void)fprintf(list_fd,"\nVariables:\n ");
	      imps += print_variables(sym_list,n);
	   }
	}
			/* Explain the asterisk on implicitly defined
			   identifiers.  Note that this message will
			   be given also if functions implicitly defined */
	if(do_symtab && imps != 0) {
	     (void)fprintf(list_fd,"\n* Variable not declared.");
	     (void)fprintf(list_fd," Type has been implicitly defined.\n");
	     ++warning_count;
	}

	if(var_usage_check) {
	  if(do_symtab || do_list)
	    (void)fprintf(list_fd,"\n");
	  if(check_var_unused) {
	    check_flags(sym_list,n,0,0,0,
		      "Variables declared but never referenced",mod_name);
	    check_flags(sym_list,n,0,1,0,
		      "Variables set but never used",mod_name);
	  }
	  if(check_var_set_used) {
	    check_flags(sym_list,n,1,0,1,
		      "Variables used before set",mod_name);
	    check_flags(sym_list,n,1,1,1,
		      "Variables may be used before set",mod_name);
	  }

	}/*end if(var_usage_check)*/

	if(do_symtab || do_list)
	  (void)fprintf(list_fd,"\n");

    }/* end if(do_symtab || var_usage_check) */

			/* List all undeclared vars & functions */
    if(decls_required || implicit_none) {
	int i,n;

	for(i=0,n=0;i<loc_symtab_top;i++) {
	    if(datatype_of(loc_symtab[i].type) == type_UNDECL
		&& ! loc_symtab[i].intrinsic /* omit intrinsics */
				/* omit subroutines called */
		&& (!loc_symtab[i].external || loc_symtab[i].invoked_as_func)
	       ) {
		sym_list[n++] = &loc_symtab[i];
	    }
	}
	if(n != 0) {
	    sort_symbols(sym_list,n);
	    identify_module(mod_name);
	    (void)fprintf(list_fd,
		    "Identifiers of undeclared type");
	    (void) print_symbols(list_fd,sym_list,n,FALSE);
	}
    }/*if(decls_required || implicit_none)*/

			/* Under -f77, list any nonstandard intrinsics used */
    if(f77_intrinsics) {
      int i,n;
      for(i=0,n=0;i<loc_symtab_top;i++) {
	if(storage_class_of(loc_symtab[i].type) == class_SUBPROGRAM
	   && loc_symtab[i].intrinsic &&
	   (loc_symtab[i].info.intrins_info->intrins_flags & I_NONF77)) {
	  sym_list[n++] = &loc_symtab[i];
	}
      }
      if(n != 0) {
	sort_symbols(sym_list,n);
	identify_module(mod_name);
	(void)fprintf(list_fd,"Nonstandard intrinsic functions referenced:\n");
	(void) print_symbols(list_fd,sym_list,n,FALSE);
      }
    }/*if(f77_standard)*/


		/* issue -f77 warning for identifiers
		   longer than 6 characters
		*/
    if(f77_long_names) {
	int i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(strlen(loc_symtab[i].name) > (unsigned)6)
		  sym_list[n++] = &loc_symtab[i];
	}

	if(n != 0) {

	   sort_symbols(sym_list,n);

	   identify_module(mod_name);
	   (void)fprintf(list_fd,
		   "Names longer than 6 chars (nonstandard):");
	   (void) print_symbols(list_fd,sym_list,n,FALSE);
	}
    }

	/* If -f77 flag given, list names with underscore or dollarsign */

    if(f77_underscores || f77_dollarsigns) {
	int i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
			/* Find all names with nonstd chars, but
			   exclude internal names like %MAIN */
	       if(has_nonalnum(loc_symtab[i].name) &&
		  loc_symtab[i].name[0] != '%')
		  sym_list[n++] = &loc_symtab[i];
	}

	if(n != 0) {

	   sort_symbols(sym_list,n);

	   identify_module(mod_name);

	   (void)fprintf(list_fd,
		   "Names containing nonstandard characters");
	   (void) print_symbols(list_fd,sym_list,n,FALSE);
	}
    }/*if(f77_underscores || f77_dollarsigns)*/

			/* Print out clashes in first six chars of name */
    if(sixclash) {
	 int n;
	 n = find_sixclashes(sym_list);
	 if(n != 0) {
	    sort_symbols(sym_list,n);
	    identify_module(mod_name);
	    (void)fprintf(list_fd,
		    "Identifiers which are not unique in first six chars");
	    (void) print_symbols(list_fd,sym_list,n,FALSE);
	 }/* end if(n != 0) */
    }/* end if(sixclash) */


		/* If portability flag was given, check equivalence
		   groups for mixed type. */
    if(port_mixed_equiv || port_mixed_size || local_wordsize==0) {
	int i,j,n;
	int imps=0;
	Lsymtab *equiv;

		/* scan thru table for equivalenced variables */
	for(i=0;i<loc_symtab_top;i++) {
	    if(storage_class_of(loc_symtab[i].type) == class_VAR
	       && loc_symtab[i].equiv_link != (equiv= &loc_symtab[i]) ){
		n=0;
		do {
		    if(equiv < &loc_symtab[i]) { /* skip groups done before */
			n=0;
			break;
		    }
		    sym_list[n++] = equiv;
		    equiv = equiv->equiv_link;
		} while(equiv != &loc_symtab[i]); /* complete the circle */
				/* Check for mixed types */
		if(n != 0) {
		    int mixed_type = FALSE, mixed_size = FALSE,
			mixed_default_size = FALSE;
		    int t1,t2,s1,s2,defsize1,defsize2;

		    t1 = get_type(sym_list[0]);
		    s1 = get_size(sym_list[0],t1);
		    defsize1 = (s1 == size_DEFAULT);
		    if(s1 == size_DEFAULT) s1 = type_size[t1];
		    for(j=1; j<n; j++) {
		      t2 = get_type(sym_list[j]);
		      s2 = get_size(sym_list[j],t2);
		      defsize2 = (s2 == size_DEFAULT);
		      if(s2 == size_DEFAULT) s2 = type_size[t2];
		      if( t1 == t2 ) {
			if( t1 != type_STRING ){
				/* Same non-char types: size must match */
			  if( s1 != s2 ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
		      }
		      else {/* Different types */
				/* It is nonportable to equivalence:
					 Real*8 to Double or
					 Complex*16 to DComplex */
			if(type_category[t1] == type_category[t2]) {
			  if( s1 != s2 ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
				/* It is standard and portable to equivalence:
					 Real to Complex or
					 Double to DComplex */
			else if(equiv_type[t1] == equiv_type[t2]) {
			  if( ((type_category[t1] == type_COMPLEX)?
				s1 != 2*s2: s2 != 2*s1) ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
			else {
			  mixed_type = TRUE;
			  break;
			}
		      }/*end else different types*/

		      t1 = t2;
		      s1 = s2;
		      defsize1 = defsize2;
		    }/*end for j*/

		    if( (mixed_type && port_mixed_equiv) ||
		       ((mixed_size || mixed_default_size) &&
			(port_mixed_size || local_wordsize==0)) )  {
			sort_symbols(sym_list,n);
			identify_module(mod_name);
			(void)fprintf(list_fd,
			       "Mixed %s equivalenced (not portable):",
				    mixed_type?"types":
				      mixed_size?"sizes":
				       "default and explicit size items");

			imps += print_symbols(list_fd,sym_list,n,TRUE);
		    }
		}
	    }
	}
	if(imps != 0) {
	     identify_module(mod_name);
	     (void)fprintf(list_fd,"* Variable not declared.");
	     (void)fprintf(list_fd," Type has been implicitly defined.\n");
	}

    }/*if(port_mixed_size/type)*/

    make_declarations(sym_list,mod_name);
}/* print_loc_symbols */


PRIVATE int
#if HAVE_STDC
has_nonalnum(char *s)	/* Returns TRUE if s contains a non-alphanumeric character
		   and -f77, or if it has $ or _ and that is not allowed  */
#else /* K&R style */
has_nonalnum(s)	/* Returns TRUE if s contains a non-alphanumeric character
		   and -f77, or if it has $ or _ and that is not allowed  */
   char *s;
#endif /* HAVE_STDC */
{
   while( *s != '\0' ) {
     if( (f77_dollarsigns && (*s) == '$')
      || (f77_underscores && (*s) == '_') )
       return TRUE;
     s++;
   }
   return FALSE;
}


     /* This routine prints symbol names neatly.  If do_types is true
	also prints types, with * next to implicitly
	typed identifiers, and returns count thereof. */

PRIVATE int
#if HAVE_STDC
print_symbols(FILE *fd, Lsymtab **sym_list, int n, int do_types)
#else /* K&R style */
print_symbols(fd,sym_list,n,do_types)
     FILE *fd;
     Lsymtab *sym_list[];
     int n;
     int do_types;
#endif /* HAVE_STDC */
{
     int i,col=0,len,implicits=0;

     (void)fprintf(fd,"\n");

     for(i=0;i<n;i++) {
	  len = strlen(sym_list[i]->name);/* len=actual length of name */
				/* Revise len to max(10,len)+extra 9=width
				   of field to be printed.  Adjust column
				   count to see where this will take us. */
	  col += len = (len <= 10? 10: len) + 9;
				/* If this will run past 78 start a new line */
	  if(col > 78) {
	    (void)fprintf(fd,"\n");
	    col = len;
	  }
	  (void)fprintf(fd,"%10s",sym_list[i]->name);/* Print the name in 10 cols */

	  if( do_types ) {	/* Optionally print the datatype */
	    if(sym_list[i]->intrinsic)
	      (void)fprintf(fd,": intrns ");
	    else {
	      (void)fprintf(fd,":");
	      (void) print_var_type(fd,sym_list[i]);
	      if(datatype_of(sym_list[i]->type) == type_UNDECL) {
		implicits++; /* Flag and count undeclareds */
		(void)fprintf(fd,"*");
	      }
	      else if(sym_list[i]->size == size_DEFAULT)
		(void)fprintf(fd," ");
	      (void)fprintf(fd,"  ");
	    }
	  }
	  else			/* Otherwise just 9 blanks */
	    (void)fprintf(fd,"%9s","");
     }

     (void)fprintf(fd,"\n");

     return implicits;

}/*print_symbols*/


	/* This routine prints the variables nicely, and returns
	    count of number implicitly defined.
	 */
PRIVATE int
#if HAVE_STDC
print_variables(Lsymtab **sym_list, int n)
#else /* K&R style */
print_variables(sym_list,n)
     Lsymtab *sym_list[];
     int n;
#endif /* HAVE_STDC */
{
     int i,implicits=0,adjustables=0;

     (void)fprintf(list_fd,"\n ");

     for(i=0; i<4; i++) {
	  (void)fprintf(list_fd,"%5sName Type Dims","");
		      /* 12345678901234567890 template for above*/
     }
     for(i=0; i<n; i++) {

	  if(i % 4 == 0)
	     (void)fprintf(list_fd,"\n");
	  else
	     (void)fprintf(list_fd," ");

	  (void)fprintf(list_fd,"%10s",sym_list[i]->name);
	  adjustables += print_var_type(list_fd,sym_list[i]);

			/* Print a * next to implicitly declared variables */
	  if(datatype_of(sym_list[i]->type) == type_UNDECL ) {
	    implicits++;
	    (void)fprintf(list_fd,"*");
	  }
	  else if(sym_list[i]->size == size_DEFAULT)
	    (void)fprintf(list_fd," "); /* print blank if no size or * */


			/* print no. of dimensions next to var name */
	  if(sym_list[i]->array_var) {
		(void)fprintf(list_fd," %ld",
			       array_dims(sym_list[i]->info.array_dim));
	  }
	  else {
		(void)fprintf(list_fd,"%2s","");
	  }
    }

    if(adjustables > 0)
      (void)fprintf(list_fd,"\nchar+ indicates adjustable size");
    (void)fprintf(list_fd,"\n");

    return implicits;

}/*print_variables*/


PRIVATE int
#if HAVE_STDC
print_var_type(FILE *fd, Lsymtab *symt)	/* Prints type name then size if explicit */
#else /* K&R style */
print_var_type(fd,symt)	/* Prints type name then size if explicit */
#endif /* HAVE_STDC */
			/* Returns 1 if adjustable size, else 0 */
#if HAVE_STDC
#else /* K&R style */
     FILE *fd;
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
  int adjustable=0;
  int t = get_type(symt);
  int s = get_size(symt,t);

	  (void)fprintf(fd," %4s",type_name[t]);

		/* Usually either size or * will be printed, and usually
		   size is 1 digit.  So mostly we print 1 column in
		   the next set of (void)fprintf's.  Output will be ragged
		   if size > 9 or implicit type has explicit size. */
	  if( s != size_DEFAULT ) {
	    if(t != type_STRING || s > 1)
	      (void)fprintf(fd,"%d",s);
	    else
	      if(s == size_ADJUSTABLE) {
		adjustable++;
		(void)fprintf(fd,"+");
	      }
	      else
		(void)fprintf(fd," ");
	  }
  return adjustable;
}


	/* Search thru local symbol table for clashes where identifiers
	   are not unique in 1st six characters. Return value =
	   number of clashes found, with pointers to symbol table
	   entries of clashers in array list. */
PRIVATE int
#if HAVE_STDC
find_sixclashes(Lsymtab **list)
#else /* K&R style */
find_sixclashes(list)
	Lsymtab *list[];
#endif /* HAVE_STDC */
{
	int i,h, clashes=0;
	int class;
	unsigned long hnum;

	for(i=0; i<loc_symtab_top; i++) {	/* Scan thru symbol table */
	    class = storage_class_of(loc_symtab[i].type);
	    hnum = hash( loc_symtab[i].name );
				/* First look for a clash of any kind.
				   (N.B. this loop will never quit if hash
				   table is full, but let's not worry) */
	    while( (h=hnum % HASHSZ), hashtab[h].name != (char *)NULL) {
		/* Now see if the clashing name is used locally and still
		   clashes at 6 chars.  Treat common blocks separately. */

	     if((class == class_COMMON_BLOCK &&
		  (
		   hashtab[h].com_loc_symtab != NULL
		   && strcmp( hashtab[h].name,loc_symtab[i].name) != 0
		   && strncmp(hashtab[h].name,loc_symtab[i].name,6) == 0
		  )
		)  ||
		 (class != class_COMMON_BLOCK &&
		  (
		   hashtab[h].loc_symtab != NULL
		   && strcmp( hashtab[h].name,loc_symtab[i].name) != 0
		   && strncmp(hashtab[h].name,loc_symtab[i].name,6) == 0
		  )
		 )
	       ) {
				/* If so, then i'th symbol is a clash */

			list[clashes++] = &loc_symtab[i];
			break;
		}
		else {
		    hnum = rehash(hnum);
		}
	    }
	}
	return clashes;
}


#ifdef DEBUG_SYMTABS
PRIVATE void
print_arg_array(arglist)	/* prints type and flag info for arguments */
	ArgListHeader *arglist;
{
	int i, count;
	ArgListElement *a;

	count = arglist->numargs;
	if(arglist->external_decl || arglist->actual_arg)
	  count = 0;
	a = arglist->arg_array;
	(void)fprintf(list_fd,"\nArg list in module %s file %s line %u:",
		arglist->module->name, arglist->filename, arglist->line_num);
	(void)fprintf(list_fd,"\n\tdef%d call%d ext%d arg%d",
		arglist->is_defn,
		arglist->is_call,
		arglist->external_decl,
		arglist->actual_arg);
	if(count == 0)
		(void)fprintf(list_fd,"\n(Empty list)");
	else {
	    for (i=0; i<count; i++) {
		(void)fprintf(list_fd,
			"\n\t%d %s: lv%d st%d as%d ub%d ar%d ae%d ex%d",
			i+1,
			type_name[datatype_of(a[i].type)],
				a[i].is_lvalue,
				a[i].set_flag,
				a[i].assigned_flag,
				a[i].used_before_set,
				a[i].array_var,
				a[i].array_element,
				a[i].declared_external);
		if(a[i].array_var)
		    (void)fprintf(list_fd,"(%ld,%ld)",
			array_dims(a[i].info.array_dim),
			array_size(a[i].info.array_dim) );
		(void)fprintf(list_fd,", ");
	    }
	}
}/* print_arg_array */
#endif

#ifdef DEBUG_SYMTABS
	       /* prints type and dimen info for common vars */
PRIVATE void
print_com_array(cmlist)
	ComListHeader *cmlist;
{
	int i, count;
	ComListElement *c;

	count = cmlist->numargs;
	c = cmlist->com_list_array;
	(void)fprintf(list_fd,"\nCom list in module %s file %s line %u:",
		cmlist->module->name, cmlist->filename, cmlist->line_num);
	(void)fprintf(list_fd,"\n\t");
	if(count == 0)
		(void)fprintf(list_fd,"(Empty list)");
	else {
	    for (i=0; i<count; i++){
		(void)fprintf(list_fd,"%s",type_name[datatype_of(c[i].type)]);
		if(c[i].dimen_info)
		    (void)fprintf(list_fd,":%ldD(%ld)",array_dims(c[i].dimen_info),
					   array_size(c[i].dimen_info));
		(void)fprintf(list_fd,", ");
	    }
	}
}/* print_com_array */
#endif


#if 0 /* debugging code not currently in use */
PRIVATE void
print_tokenlist(toklist)	/* prints list of token names or types */
	TokenListHeader *toklist;
{
	int numargs=0;
	Token *t;
	(void)fprintf(list_fd,"\n");
	if (toklist == NULL){
	    (void)fprintf(list_fd,"\t(No list)");
	}
	else {
	    t = toklist->tokenlist;
	    while(t != NULL){
		++numargs;
		(void)fprintf(list_fd," ");
		if ( is_true(ID_EXPR,t->TOK_flags) )
		    (void)fprintf(list_fd,"%s ",token_name(*t));
		else
		    (void)fprintf(list_fd,"%s ",
				  type_name[datatype_of(t->TOK_type)]);
		t = t->next_token;
	    }
	    if(numargs == 0)
		    (void)fprintf(list_fd,"\t(Empty list)");
	}
}/* print_tokenlist */
#endif


PRIVATE int
#if HAVE_STDC
make_sym_list(Lsymtab **sym_list, int (*select) (Lsymtab *))
#else /* K&R style */
make_sym_list(sym_list,select)
     Lsymtab *sym_list[];
     PROTO(int (*select),( Lsymtab *sym_entry ));
#endif /* HAVE_STDC */
{
    int i;
    int n;

    for (i = 0, n = 0; i < loc_symtab_top; ++i)
    {
	if (select(&loc_symtab[i]))
	    sym_list[n++] = &loc_symtab[i];
    }
    if (n > 0)
    {
	/* original PARAMETER statement order must be preserved so that
	   the expressions do not refer to as-yet-undefined parameter names */
	if (select == select_parameters)
	    sort_positions(sym_list,n);
	else
	    sort_symbols(sym_list,n);
    }
    return (n);
}


PRIVATE void
#if HAVE_STDC
check_mixed_common(FILE *fd, Lsymtab **sym_list, int n)
#else /* K&R style */
check_mixed_common(fd,sym_list,n)
     FILE *fd;
     Lsymtab *sym_list[];
     int n;
#endif /* HAVE_STDC */
{
    int i;
    for(i=0; i<n; i++) {
	ComListHeader *chead = sym_list[i]->info.comlist;
	ComListElement *clist;
	char *mod_name;
	int j,nvars;
	int has_char=FALSE,has_nonchar=FALSE;
	int prev_size = 0;
	  /* initialize to remove lint warning about use before definition */
	int this_size, this_type;

	if(chead == NULL)
	  continue;

	mod_name = chead->module->name;
	clist=chead->com_list_array;
	nvars = chead->numargs;

	for(j=0; j<nvars; j++) {

	   /* Check conformity to ANSI rule: no mixing char with other types */

	  if( (this_type=datatype_of(clist[j].type)) == type_STRING) {
	    has_char = TRUE;
	    this_size = 1;/* char type size is 1 for alignment purposes */
	  }
	  else { /* other types use declared sizes */
	    has_nonchar = TRUE;
	    if( (this_size=clist[j].size) == size_DEFAULT)
	      this_size = type_size[this_type];
	  }
	  if(has_char && has_nonchar) {
	    if(f77_mixed_common){
	      identify_module(mod_name);
	      (void)fprintf(fd,
		   "Common block %s line %u has mixed",
		   sym_list[i]->name,
		   chead->line_num);
	      (void)fprintf(fd,
		   "\n  character and non-character variables (nonstandard)");
	    }
	    break;
	  }

	/* Check that variables are in descending order of type size */

	 if(j > 0) {
	  if( this_size > prev_size ) {
	    if(port_common_alignment) {
	      identify_module(mod_name);
	      (void)fprintf(fd,
		    "Common block %s line %u has long data type",
		    sym_list[i]->name,
		    chead->line_num);
	      (void)fprintf(fd,
		    "\n  following short data type (may not be portable)");
	    }
	    break;
	  }
	 }
	 prev_size = this_size;
	}
    }
}


PRIVATE void
#if HAVE_STDC
check_flags(Lsymtab **list, int n, unsigned int used, unsigned int set, unsigned int ubs, char *msg, char *mod_name)
#else /* K&R style */
check_flags(list,n,used,set,ubs,msg,mod_name)
	Lsymtab *list[];
	int n;
	unsigned used,set,ubs;
	char *msg,*mod_name;
#endif /* HAVE_STDC */
{
	int matches=0,col=0,unused_args=0,i,len;
	unsigned pattern = flag_combo(used,set,ubs);

	for(i=0;i<n;i++) {
	    if( list[i]->common_var )	/* common vars are immune */
	       continue;
				/* for args, do only 'never used' */
	    if( list[i]->argument && pattern != flag_combo(0,0,0) )
	       continue;

#ifdef ALLOW_INCLUDE
				/* Skip variables 'declared but not used'
				   and parameters 'set but never used'
				   if defined in include file. */

	    if( list[i]->defined_in_include &&
	       ( pattern == flag_combo(0,0,0)
	       || (list[i]->parameter && pattern == flag_combo(0,1,0)) ) )
		continue;
#endif
			/*  function return val: ignore 'set but never used' */
	    if( list[i]->entry_point && pattern == flag_combo(0,1,0) )
		continue;

	    if(flag_combo(list[i]->used_flag,list[i]->set_flag,
	       list[i]->used_before_set) == pattern) {
		 if(matches++ == 0) {
		   identify_module(mod_name);
		   (void)fprintf(list_fd,
			    "%s:\n",
			    msg);
		 }
		 len = strlen(list[i]->name);
		 col += len = (len <= 10? 10: len) + 9;
		 if(col > 78) {
		   (void)fprintf(list_fd,"\n");
		   col = len;
		 }
		 (void)fprintf(list_fd,"%10s",list[i]->name);
				/* arg never used: tag with asterisk */
		 (void)fprintf(list_fd,"%-9s",
			 list[i]->argument? (++unused_args,"*") : "" );
	    }
	}
	if(unused_args > 0)
		(void)fprintf(list_fd,"\n  * Dummy argument");
	if(matches > 0)
		(void)fprintf(list_fd,"\n");
}


void
debug_symtabs(VOID) 	/* Debugging output: hashtable and symbol tables */
{
#ifdef DEBUG_SYMTABS
  if(debug_loc_symtab) {
    (void)fprintf(list_fd,"\n Debugging of local symbol table disabled");
    return;
  }

    if(debug_hashtab) {
	int i;
	(void)fprintf(list_fd,"\n\nContents of hashtable\n");
	for(i=0; i<HASHSZ; i++) {
	    if(hashtab[i].name != NULL) {
	      (void)fprintf(list_fd,"\n%4d %s",i,hashtab[i].name);
	      if(hashtab[i].loc_symtab != NULL)
		(void)fprintf(list_fd," loc %d",hashtab[i].loc_symtab-loc_symtab);
	      if(hashtab[i].glob_symtab != NULL)
		(void)fprintf(list_fd,
			" glob %d",hashtab[i].glob_symtab-glob_symtab);
	      if(hashtab[i].com_loc_symtab != NULL)
		(void)fprintf(list_fd,
			" Cloc %d",hashtab[i].com_loc_symtab-loc_symtab);
	      if(hashtab[i].com_glob_symtab != NULL)
		(void)fprintf(list_fd,
			" Cglob %d",hashtab[i].com_glob_symtab-glob_symtab);
	    }
	}
    }

    if(debug_glob_symtab) {
	int i;
	(void)fprintf(list_fd,"\n\nContents of global symbol table");

	for(i=0; i<glob_symtab_top; i++) {
	    (void)fprintf(list_fd,
		"\n%4d %s type 0x%x=%s,%s: ",
		i,
		glob_symtab[i].name,
		glob_symtab[i].type,
		class_name[storage_class_of(glob_symtab[i].type)],
		type_name[datatype_of(glob_symtab[i].type)]
	     );
	    (void)fprintf(list_fd,
      "usd%d set%d asg%d ubs%d lib%d int%d invf%d vis%d smw%d incl%d ext%d ",
		glob_symtab[i].used_flag,
		glob_symtab[i].set_flag,
		glob_symtab[i].assigned_flag,
		glob_symtab[i].used_before_set,
		glob_symtab[i].library_module,
		glob_symtab[i].internal_entry,
		glob_symtab[i].invoked_as_func,
		glob_symtab[i].visited,
		glob_symtab[i].visited_somewhere,
		glob_symtab[i].defined_in_include,
		glob_symtab[i].declared_external
		    );
	    switch(storage_class_of(glob_symtab[i].type)){
		case class_COMMON_BLOCK:{
		    ComListHeader *clist;
		    clist=glob_symtab[i].info.comlist;
		    while(clist != NULL){
			print_com_array(clist);
			clist = clist->next;
		    }
		    break;
		}
		case class_SUBPROGRAM:{
		    ArgListHeader *alist;
		    alist=glob_symtab[i].info.arglist;
		    while(alist != NULL){
			print_arg_array(alist);
			alist = alist->next;
		    }
		    break;
		}
	    }
	}
    }
#endif
}/* debug_symtabs*/


/*----------------Additions for declaration file output----------------*/

/* Originally written by Nelson H.F. Beebe before source text was
   saved in the symbol table.  Rewritten by R. Moniot to make use
   of said text. */

/* Only make_declarations() is used by the above routines */


PROTO(PRIVATE char * get_dimension_list,( Lsymtab *symt ));
PROTO(PRIVATE char * get_parameter_value,( Lsymtab *symt ));
PROTO(PRIVATE char * get_size_expression,( Lsymtab *symt ));


#if 0			/* This is how Beebe wrote it */
#define ACTUAL_SIZE(p)		(((p)->size == 0) ? \
				 std_size[the_type] : (p)->size)
#else
		/* This is what it has to be if IMPLICIT types supported */
#define ACTUAL_SIZE(p)		(get_size((p),sym_type))
#endif

#define DCL_FLAGS_DECLARATIONS				0x0001
#define DCL_FLAGS_ONLY_UNDECLARED			0x0002
#define DCL_FLAGS_COMPACT				0x0004
#define DCL_FLAGS_USE_CONTINUATIONS			0x0008
#define DCL_FLAGS_KEYWORDS_LOWERCASE			0x0010
#define DCL_FLAGS_VARIABLES_AND_CONSTANTS_LOWERCASE	0x0020
#define DCL_FLAGS_EXCLUDE_SFTRAN3_INTERNAL_VARIABLES	0x0040
#define DCL_FLAGS_ASTERISK_COMMENT_CHARACTER		0x0080
#define DCL_FLAGS_LOWERCASE_COMMENT_CHARACTER		0x0100
#define DCL_NO_ARRAY_DIMENSIONS				0x0200
#define COLUMN_WIDTH		13
#define DECLARE_ONLY_UNDECLARED() (make_dcls & DCL_FLAGS_ONLY_UNDECLARED)
#define DECLARE_COMPACT()	(make_dcls & DCL_FLAGS_COMPACT)
#define NO_CONTINUATION_LINES() (!(make_dcls & DCL_FLAGS_USE_CONTINUATIONS))
#define SF3_DECLARATIONS()	\
		(make_dcls & DCL_FLAGS_EXCLUDE_SFTRAN3_INTERNAL_VARIABLES)
#define ASTERISK_COMMENT_CHAR()	\
		(make_dcls & DCL_FLAGS_ASTERISK_COMMENT_CHARACTER)
#define KEYWORDS_LOWERCASE()	(make_dcls & DCL_FLAGS_KEYWORDS_LOWERCASE)
#define LOWERCASE_COMMENT_CHARACTER() \
		(make_dcls & DCL_FLAGS_LOWERCASE_COMMENT_CHARACTER)
#define VARIABLES_AND_CONSTANTS_LOWERCASE() \
		(make_dcls & DCL_FLAGS_VARIABLES_AND_CONSTANTS_LOWERCASE)
#define ARRAY_VARS_DIMENSIONED() \
		(!(make_dcls & DCL_NO_ARRAY_DIMENSIONS))

#ifndef FIRST_VARIABLE_COLUMN
#define FIRST_VARIABLE_COLUMN	26      /* to match Extended PFORT Verifier */
#endif
#define NEXT_COLUMN(column)	(FIRST_VARIABLE_COLUMN + \
				(((column) - FIRST_VARIABLE_COLUMN + \
				COLUMN_WIDTH - 1) / COLUMN_WIDTH)*COLUMN_WIDTH)

#define isaletter(C)    isalpha((int)(C))

	/* define isidletter to allow underscore and/or dollar sign  */
#define isidletter(C)    (isalpha((int)(C)) || (C) == '_' || (C) == '$')


#define makelower(C) (isupper((int)(C)) ? tolower((int)(C)) : (int)(C))
#define makeupper(C) (islower((int)(C)) ? toupper((int)(C)) : (int)(C))

PRIVATE char *begin_module;

#define MAX_STMT		(72 + 19*72 + 1) /* longest Fortran stmt */

PRIVATE char stmt_fragment[MAX_STMT];

PRIVATE char comment_char = 'C'; /* default value */

PRIVATE int std_size[] =	/* NB: depends on type_XXX order in symtab.h */
{
    0,					/* unknown */
    4,					/* INTEGER*4 */
    4,					/* REAL*4 */
    8,					/* DOUBLE PRECISION == REAL*8 */
    8,					/* COMPLEX*8 */
    16,					/* DOUBLE COMPLEX == COMPLEX*16 */
    4,					/* LOGICAL*4 */
    1					/* CHARACTER*1 == CHARACTER */
};

PRIVATE int
pos_fragment = 0;		/* cursor in stmt_fragment buffer */


PRIVATE void
#if HAVE_STDC
append_char_to_fragment(int c)
#else /* K&R style */
append_char_to_fragment(c)
int c;
#endif /* HAVE_STDC */
{
    if (pos_fragment < (MAX_STMT - 1))
	stmt_fragment[pos_fragment++] = c;
}


PRIVATE void
#if HAVE_STDC
append_string_to_fragment(char *s)
#else /* K&R style */
append_string_to_fragment(s)
char *s;
#endif /* HAVE_STDC */
{
    while (*s)
	append_char_to_fragment(*s++);
}

			/* Appends source text of an expression, up- or
			   down-casing the letters according to pref. */
PRIVATE void
#if HAVE_STDC
append_expr_text_to_fragment(char *s)
#else /* K&R style */
append_expr_text_to_fragment(s)
  char *s;
#endif /* HAVE_STDC */
{
    int quote_char, inside_quote;
    inside_quote = FALSE;
    for (; *s; ++s) {
      if(! inside_quote) {
	if(*s == '\'' || *s == '"') { /* Start of a quote */
	  inside_quote = TRUE;
	  quote_char = *s;
	}
	append_char_to_fragment(VARIABLES_AND_CONSTANTS_LOWERCASE()
				? makelower(*s) : makeupper(*s));
      }
      else {			/* inside quote */
	if(*s == quote_char) { /* End of quote (quoted quote_char is handled
				  as if consecutive strings) */
	  inside_quote=FALSE;
	}
	append_char_to_fragment(*s);
      }
    }
}


PRIVATE char *
#if HAVE_STDC
get_dimension_list(Lsymtab *symt)
#else /* K&R style */
get_dimension_list(symt)
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
    int n, dims;

		/* Get list of array dimensions from symbol table */

    new_fragment();

    append_char_to_fragment('(');

    dims = array_dims(symt->info.array_dim);
    for (n = 0; n < dims; ++n)
    {
	if (n > 0)
	    append_char_to_fragment(',');
	append_expr_text_to_fragment(symt->src.textvec[n]);
    }

    append_char_to_fragment(')');
    append_char_to_fragment('\0');

    return (&stmt_fragment[0]);
}




PRIVATE char *
#if HAVE_STDC
get_parameter_value(Lsymtab *symt)
#else /* K&R style */
get_parameter_value(symt)
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
    /* Construct parameter list "(NAME = value)" */

    new_fragment();
    append_char_to_fragment('(');

    append_expr_text_to_fragment(symt->name);

    append_string_to_fragment(" = ");

    append_expr_text_to_fragment(symt->info.param->src_text);

    append_char_to_fragment(')');
    append_char_to_fragment('\0');
    return (&stmt_fragment[0]);
}



PRIVATE char *
#if HAVE_STDC
get_size_expression(Lsymtab *symt)
#else /* K&R style */
get_size_expression(symt)
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
    /* Get a CHARACTER size expression from the symbol table */

    new_fragment();

    append_char_to_fragment('*');

    append_expr_text_to_fragment(get_size_text(symt,0));

    append_char_to_fragment('\0');

    return (&stmt_fragment[0]);
}

PRIVATE void
#if HAVE_STDC
make_declarations(Lsymtab **sym_list, char *mod_name)
#else /* K&R style */
make_declarations(sym_list,mod_name)
     Lsymtab *sym_list[];
     char *mod_name;
#endif /* HAVE_STDC */
{
    char *header;
    char begin[72+1+2+1];
    int len_current_filename = strlen(current_filename);

    if (!make_dcls)
	return;

    make_dcls |= DCL_FLAGS_DECLARATIONS; /* any non-zero value selects */

    if (LOWERCASE_COMMENT_CHARACTER())
	comment_char = 'c';
    else if (ASTERISK_COMMENT_CHAR())
	comment_char = '*';
    else
	comment_char = 'C';


    /* In the event there are no declarations to be output, we want
       the declaration file to be empty, because that reduces the
       number of files that the user has to deal with.  In fact, if it
       IS empty, it will be deleted on close.  Instead of printing the
       module header comment here, we point a global pointer at it,
       and then in the print_xxx() functions, print the header before
       the first declaration that is output.

       We also need to take care not be overwrite the begin[] array,
       which could happen if the module name or file name are
       exceptionally long.  We therefore take at most 8 characters
       from the start of the module name, and at most 12 (because 12 =
       8 + 1 + 3 for IBM PC DOS), from the END of the filename,
       discarding a long directory path prefix if necessary. */


    (void)sprintf(begin,
		  "%c====>Begin Module %-8s   File %-12s   %s\n%c\n",
		  comment_char,
		  mod_name,
		  (len_current_filename > 12) ?
			(current_filename + len_current_filename - 12) :
			current_filename,
		  DECLARE_ONLY_UNDECLARED() ?
			"Undeclared variables" : "All variables",
		  comment_char);
    begin_module = &begin[0];

    print_selected_declarations(sym_list,
				make_sym_list(sym_list,
					      select_intrinsics_by_name),
				type_ERROR, "INTRINSIC",
				(header = "Intrinsic functions", &header));
    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_intrinsics_by_type),
			    "Built-in functions");

    print_selected_declarations(sym_list,
				make_sym_list(sym_list,
					      select_externals_by_name),
				type_ERROR, "EXTERNAL",
				(header = "External functions", &header));
    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_externals_by_type),
			    (char*)NULL);

    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_statement_functions),
			    "Statement functions");

    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_parameters),
			    "Parameter variables");

    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_arguments),
			    "Argument variables");

    print_declaration_class(sym_list,
			    make_sym_list(sym_list,select_locals),
			    "Local variables");

    print_list_decls(sym_list,
			    make_sym_list(sym_list,select_common_blocks),
			    "Common blocks","COMMON");

    print_list_decls(sym_list,
			    make_sym_list(sym_list,select_namelists),
			    "Namelists","NAMELIST");

    if (begin_module == (char*)NULL) /* then need a trailer comment */
	(void)fprintf(dcl_fd,
		      "%c====>End Module   %-8s   File %-12s\n",
		      comment_char,
		      mod_name,
		      (len_current_filename > 12) ?
			    (current_filename + len_current_filename - 12) :
			    current_filename);

}



PRIVATE void
maybe_print_module_header(VOID)
{
    if (begin_module != (char*)NULL)
    {		/* print module header comment only once */
	(void)fputs(begin_module, dcl_fd);
	begin_module = (char*)NULL;
    }
}



PRIVATE void
new_fragment(VOID)
{
    pos_fragment = 0;
}



PRIVATE void
#if HAVE_STDC
print_blanks(int nblanks)
#else /* K&R style */
print_blanks(nblanks)
int	nblanks;
#endif /* HAVE_STDC */
{
    for ( ; nblanks > 0; --nblanks)
	(void)putc(' ',dcl_fd);
}

				/* Routine to print namelist and
				   common declarations. */

PRIVATE void
#if HAVE_STDC
print_common_decls(Lsymtab *sym_entry)
                        	/* COMMON block symbol table entry */
#else /* K&R style */
print_common_decls(sym_entry)
     Lsymtab *sym_entry;	/* COMMON block symbol table entry */
#endif /* HAVE_STDC */
{
    int h;
    int n;
    Token *t;

#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
    static Lsymtab **sym_list=(Lsymtab **)NULL;

    if(sym_list == (Lsymtab **)NULL) { /* Initialize if not done before */
      if( (sym_list=(Lsymtab **)calloc(LOCSYMTABSZ,sizeof(Lsymtab *)))
	 == (Lsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for local symbol list");
      }
    }
#else
    Lsymtab *sym_list[LOCSYMTABSZ]; /* temp. list of symtab entries to print */
#endif

    for (n = 0, t = sym_entry->src.toklist->tokenlist;
	 t != NULL;
	 t = t->next_token)
      {
	h = t->value.integer;
	sym_list[n++] = hashtab[h].loc_symtab;
      }

    if (n > 0)
    {
	sort_symbols(sym_list,n);
	print_declaration_class(sym_list, n, "Common variables");
    }
}


PRIVATE void
print_empty_comment_line(VOID)
{
    (void)putc(comment_char,dcl_fd);
    (void)putc('\n',dcl_fd);
}


PRIVATE void
#if HAVE_STDC
print_equivalence_decls(Lsymtab *sym_entry)
                        	/* COMMON block symbol table entry */
#else /* K&R style */
print_equivalence_decls(sym_entry)
     Lsymtab *sym_entry;	/* COMMON block symbol table entry */
#endif /* HAVE_STDC */
{
    int h;
    int n;
    Lsymtab *s;
    Token *t;

#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
    static Lsymtab **sym_list=(Lsymtab **)NULL;

    if(sym_list == (Lsymtab **)NULL) { /* Initialize if not done before */
      if( (sym_list=(Lsymtab **)calloc(LOCSYMTABSZ,sizeof(Lsymtab *)))
	 == (Lsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for local symbol list");
      }
    }
#else
    Lsymtab *sym_list[LOCSYMTABSZ]; /* temp. list of symtab entries to print */
#endif

    for (n = 0, t = sym_entry->src.toklist->tokenlist;
	 t != NULL;
	 t = t->next_token)
    {
	h = t->value.integer;
	for (s = hashtab[h].loc_symtab, s = s->equiv_link;
	     (s != NULL) && (s != hashtab[h].loc_symtab);
	     s = s->equiv_link)
	    sym_list[n++] = s;
    }

    if (n > 0)
    {
	sort_symbols(sym_list,n);
	print_declaration_class(sym_list, n,
				"Equivalenced common variables");
    }
}


PRIVATE int
#if HAVE_STDC
count_undeclared_variables(Lsymtab *sym_entry)
#else /* K&R style */
count_undeclared_variables(sym_entry)
     Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    int count, h;
    Token *t;
    Lsymtab *symt;

    for (count = 0, t = sym_entry->src.toklist->tokenlist;
	 t != NULL;
	 t = t->next_token)
    {			/* Loop over members */
	h = t->value.integer;
	symt = hashtab[h].loc_symtab;
	if (datatype_of(symt->type) == type_UNDECL)
	    count++;
    }
    return (count);
}


PRIVATE void
#if HAVE_STDC
print_list_decls(Lsymtab **sym_list, int n, char *header, char *list_type_name)
#else /* K&R style */
print_list_decls(sym_list, n, header, list_type_name)
     Lsymtab *sym_list[];
     int n;
     char *header;
     char *list_type_name;
#endif /* HAVE_STDC */
{
    int i, nd;

    if (DECLARE_ONLY_UNDECLARED() &&
	(strcmp(list_type_name,"NAMELIST") == 0)) /* These lists are always declared */
      return;

    nd = 0;
    for (i=0; i<n; i++)
    {					/* Loop over COMMON or NAMELIST lists */
	if (sym_list[i]->src.toklist != NULL)
	{
	    if (strcmp(list_type_name,"COMMON") == 0)
	    {				/* then COMMON list */
		if (!DECLARE_ONLY_UNDECLARED() ||
		    (DECLARE_ONLY_UNDECLARED() &&
		     (count_undeclared_variables(sym_list[i]) > 0)))
		{
		    print_common_decls(sym_list[i]);
		    if (!DECLARE_ONLY_UNDECLARED())
		        print_one_list_decls(sym_list[i], list_type_name,
					     &header, &nd);
		    print_equivalence_decls(sym_list[i]);
		}
	    }
	    else			/* must be NAMELIST list */
	        print_one_list_decls(sym_list[i], list_type_name, &header, &nd);
	}
    }

    if ((nd > 0) && (strcmp(list_type_name,"COMMON") != 0))
	print_empty_comment_line();
}
				/* routine to print COMMON or NAMELIST
				   name between slashes. */
PRIVATE int
#if HAVE_STDC
print_list_name(char *list_type_name, char *name)
#else /* K&R style */
print_list_name(list_type_name,name)
  char *list_type_name;
  char *name;
#endif /* HAVE_STDC */
{
    int column, len;
    char *p;

    maybe_print_module_header();

				/* Compact mode:   COMMON /blknam/
				   Padded mode:    COMMON / blknam /
				 */
    print_blanks(6);
    column = 6;

    for (p = list_type_name; *p; ++p, ++column)
	(void)putc(KEYWORDS_LOWERCASE() ? makelower(*p) : makeupper(*p),
		   dcl_fd);

    print_blanks(1);
    column++;

    (void)putc('/',dcl_fd);
    column++;

    if (!DECLARE_COMPACT())
      {
	print_blanks(1);
	column++;
      }
    len = 0;
    if (strcmp(name,blank_com_name) != 0) {
      for (p=name; *p; ++p, ++len)
	(void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
		   makelower(*p) : makeupper(*p),dcl_fd);
    }
    column += len;
    if (!DECLARE_COMPACT())
      {
	if (len <= 6)		/* Max standard length */
	  {
	    print_blanks(7-len); /* Print padding */
	    column += 7-len;
	  }
      }

    (void)putc('/',dcl_fd);
    column++;

    if (DECLARE_COMPACT())
    {
	print_blanks(1);
	column++;
    }
    else if (column < FIRST_VARIABLE_COLUMN)
    {
	print_blanks(FIRST_VARIABLE_COLUMN-column);
	column = FIRST_VARIABLE_COLUMN;
    }
    else  if (column == FIRST_VARIABLE_COLUMN)
    {
	print_blanks(1);
	column++;
	print_blanks(NEXT_COLUMN(column)-column);
	column = NEXT_COLUMN(column);
    }
    else
    {
	print_blanks(NEXT_COLUMN(column)-column);
	column = NEXT_COLUMN(column);
    }
    return column;
}


PRIVATE void
#if HAVE_STDC
print_declaration_class(Lsymtab **sym_list, int n, char *header)
#else /* K&R style */
print_declaration_class(sym_list, n, header)
     Lsymtab *sym_list[];
     int n;
     char *header;
#endif /* HAVE_STDC */
{
    int t;
    static int type_table[] =	/* table defining output declaration order */
    {			/* (alphabetical by type name) */
	type_STRING,
	type_COMPLEX,
	type_DCOMPLEX,
	type_DP,
	type_INTEGER,
	type_LOGICAL,
	type_REAL,
    };

    if (n > 0)
    {
	for (t = 0; t < sizeof(type_table)/sizeof(type_table[0]); ++t)
	    print_selected_declarations(sym_list, n, type_table[t],
					(char*)NULL, &header);
    }
}


PRIVATE void
#if HAVE_STDC
print_one_list_decls(Lsymtab *sym_entry, char *list_type_name, char **pheader, int *pnd)
#else /* K&R style */
print_one_list_decls(sym_entry, list_type_name, pheader, pnd)
     Lsymtab *sym_entry;
     char *list_type_name;
     char **pheader;
     int *pnd;
#endif /* HAVE_STDC */
{
    int column, need, next_column, nv;
    int ncontin;
    int h;
    Token *t;
    Lsymtab *symt;
    char *p;

    column = 0;
    ncontin = 0;		/* count of continuation lines */
    nv = 0;		/* count of variables in statement */
    for(t = sym_entry->src.toklist->tokenlist;
	t != NULL;
	t = t->next_token)
      {			/* Loop over members */
        h = t->value.integer;
        symt = hashtab[h].loc_symtab;
        if (column == 0)		/* at beginning of line, so */
          {			/* we need a type name */
            maybe_print_module_header();
            if ((*pheader != (char*)NULL) &&
                (strcmp(list_type_name,"COMMON") != 0))
              {				/* print header only once */
                (void)fprintf(dcl_fd,"%c     %s\n", comment_char,*pheader);
                print_empty_comment_line();
                *pheader = (char*)NULL; /* so we don't print it again */
              }
            column = print_list_name(list_type_name,sym_entry->name);
            nv = 0;		/* no variables yet in statement */
            ncontin = 0;
            ++(*pnd);			/* count declarations produced */
          }
        if (DECLARE_COMPACT())
          next_column = (nv==0?column:column + 2);
        else
          next_column = NEXT_COLUMN(nv==0?column:column + 2);
        need = (int)strlen(symt->name);
        if ((next_column + need) > 72)  /* then must start new line */
          {
            (void)putc('\n',dcl_fd);
            if (nv>0 && (strcmp(list_type_name,"COMMON") == 0) &&
                (NO_CONTINUATION_LINES() || ncontin == 19))
              {
                column = print_list_name(list_type_name,sym_entry->name);
                nv = 0;	/* no variables yet in statement */
                ncontin = 0;
              }
            else
              {
                print_blanks(5);
                (void)putc('x',dcl_fd);
                column = 6;
                if (DECLARE_COMPACT())
                  next_column = (nv==0?column:column + 2);
                else
                  next_column = NEXT_COLUMN(nv==0?column:column + 2);
                ++ncontin;
              }
          }
        if (nv > 0)		/* multiple variables */
          {
            (void)fputs(", ",dcl_fd);
            print_blanks(next_column - column - 2);
            column = next_column;
          }
        for (p = symt->name; *p; ++p)
          (void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
                     makelower(*p) : makeupper(*p),dcl_fd);

        column += need;
        nv++;			/* count variables */
      }
    if ((nv > 0) && (strcmp(list_type_name,"COMMON") == 0))
      {
    	if (column > 0)
              (void)putc('\n',dcl_fd);
          print_empty_comment_line();
          column = 0;
      }
    if (column > 0)
	(void)putc('\n',dcl_fd);
}


PRIVATE void
#if HAVE_STDC
print_parameter_statement(Lsymtab *symt)
#else /* K&R style */
print_parameter_statement(symt)
     Lsymtab *symt;
#endif /* HAVE_STDC */
{
    int column;
    int need;
    int i;

    column = print_type_name(type_ERROR,"PARAMETER",0,symt);
    need = strlen(get_parameter_value(symt));
    if ((column + need) > 72)	/* then too long to fit on current line */
    {
	(void)fputs("\n     x",dcl_fd);
	column = 6;
	if ((column + need) > 72)
	{	/* long parameter setting requires line break */
	    for (i = 0; stmt_fragment[i]; ++i)
	    {
		if (column == 72)
		{
		    (void)fputs("\n     x",dcl_fd);
		    column = 6;
		}
		(void)putc((int)stmt_fragment[i],dcl_fd);
		column++;
	    }
	}
	else
	    (void)fputs(stmt_fragment,dcl_fd);
    }
    else			/* fits on current line */
	(void)fputs(stmt_fragment,dcl_fd);
    (void)putc('\n',dcl_fd);
}


PRIVATE void
#if HAVE_STDC
print_selected_declarations(Lsymtab **sym_list, int n, int the_type, char *type_name, char **pheader)
#else /* K&R style */
print_selected_declarations(sym_list, n, the_type, type_name, pheader)
     Lsymtab *sym_list[];
     int n;
     int the_type;
     char *type_name;
     char **pheader;
#endif /* HAVE_STDC */
{
    int column, i, last_size, need, next_column, nt, nv, ncontin,
	raw_type, sym_type, sym_size;
    char *p;

    column = 0;
    last_size = 0;
    nt = 0;				/* count of type declaration stmts */
    nv = 0;				/* count of variables in statement */
    for (i = 0; i < n; ++i)
    {				/* loop over variables */
	raw_type = datatype_of(sym_list[i]->type);
	if (DECLARE_ONLY_UNDECLARED())
	{
	    if (raw_type != type_UNDECL)
		continue; /* want declarations only for undeclared vars */
	    if (sym_list[i]->external) /* and not for explicit EXTERNAL */
		continue;
	    if (sym_list[i]->intrinsic) /* and not for explicit INTRINSIC */
		continue;
	}
	sym_type = (raw_type == type_UNDECL) ?
	    get_type(sym_list[i]) : datatype_of(sym_list[i]->type);

	if ((the_type != type_ERROR) && (sym_type != the_type))
	    continue;

	sym_size = ACTUAL_SIZE(sym_list[i]);
	if ((nv > 0) && (sym_size != last_size))
	{	/* have new length modifier, so must start new declaration */
	    (void)putc('\n',dcl_fd);
	    nt++;		/* count type declaration statements */
	    column = 0;
	    ncontin = 0;
	    nv = 0;
	}
	if (column == 0)		/* at beginning of line, so */
	{				/* we need a type name */
	    maybe_print_module_header();
	    if (*pheader != (char*)NULL)
	    {				/* print header only once */
		(void)fprintf(dcl_fd,"%c     %s\n",comment_char,*pheader);
		print_empty_comment_line();
		*pheader = (char*)NULL;	/* so we don't print it again */
	    }
	    column = print_type_name(the_type,type_name, sym_size,
				     sym_list[i]);
	    last_size = sym_size;
	    nv = 0;		/* no variables yet in statement */
	    ncontin = 0;
	}
	if (DECLARE_COMPACT())
	    next_column = (nv==0?column:column + 2);
	else
	    next_column = NEXT_COLUMN(nv==0?column:column + 2);
	need = (int)strlen(sym_list[i]->name);

	if (sym_list[i]->array_var     /* leave space for "(...)" */
	    && ARRAY_VARS_DIMENSIONED())
	    need += strlen(get_dimension_list(sym_list[i]));

	if ((next_column + need) > 72)  /* then must start new declaration */
	{
	    (void)putc('\n',dcl_fd);
	    nt++;		/* count type declaration statements */
	    if (nv>0 && (NO_CONTINUATION_LINES() || ncontin == 19))
	      {
		column = print_type_name(the_type,type_name, sym_size,
				     sym_list[i]);
		ncontin = 0;
		nv = 0;		/* no variables yet in statement */
	      }
	    else
	      {
		print_blanks(5);
		(void)putc('x',dcl_fd);
		column = 6;
		if (DECLARE_COMPACT())
		  next_column = (nv==0?column:column + 2);
		else
		  next_column = NEXT_COLUMN(nv==0?column:column + 2);
		++ncontin;
	      }
	    last_size = sym_size;
	}
	if (nv > 0)		/* multiple variables */
	{
	    (void)fputs(", ",dcl_fd);
	    print_blanks(next_column - column - 2);
	    column = next_column;
	}
	for (p = sym_list[i]->name; *p; ++p)
	    (void)putc(VARIABLES_AND_CONSTANTS_LOWERCASE() ?
		       makelower(*p) : makeupper(*p),dcl_fd);
	if (sym_list[i]->array_var
	    && ARRAY_VARS_DIMENSIONED())
	    (void)fputs(stmt_fragment,dcl_fd);
	column += need;
	nv++;			/* count variables */
	if (sym_list[i]->parameter)
	{
	    (void)putc('\n',dcl_fd);
	    print_parameter_statement(sym_list[i]);
	    column = 0;
	    nt++;
	    nv = 0;
	}
    }
    if (column > 0)
    {
	(void)putc('\n',dcl_fd);
	nt++;			/* count type declaration statements */
    }
    if (nt > 0)
	print_empty_comment_line();
}


PRIVATE int
#if HAVE_STDC
print_type_name(int the_type, char *type_name, int the_size, Lsymtab *symt)
   	         		/* type_ERROR if type_name non-NULL */
    	           		/* non-NULL overrides type_table[] use */
#else /* K&R style */
print_type_name(the_type,type_name,the_size,symt)
int	the_type;		/* type_ERROR if type_name non-NULL */
char	*type_name;		/* non-NULL overrides type_table[] use */
int	the_size;
Lsymtab *symt;
#endif /* HAVE_STDC */
{				/* return value is last column printed */
    int column;
    char digits[sizeof("*18446744073709551616")]; /* big enough for 2^64 */
    char *p;
    char *size_expression;

    maybe_print_module_header();
    print_blanks(6);
    column = 6;

    for (p = (type_name == (char*)NULL) ? type_table[the_type] : type_name;
	 *p; ++p, ++column)
	(void)putc(KEYWORDS_LOWERCASE() ? makelower(*p) : makeupper(*p),
		   dcl_fd);
    if (symt != NULL) {
      if (((symt->size_is_adjustable && (the_type == type_STRING))) ||
	  (the_size == size_ADJUSTABLE)) /* happens only for CHARACTER*(*) */
	{
	    /* size_is_adjustable overrides the_size because def_parameter() */
	    /* in symtab.c replaced size_ADJUSTABLE with actual size. */
	    (void)fputs("*(*)",dcl_fd);
	    column += 4;
	}
      else if (symt->size_is_expression && (the_type == type_STRING))
	{
	    size_expression = get_size_expression(symt);
	    (void)fputs(size_expression,dcl_fd);
	    column += strlen(size_expression);
	}
      else if ((the_size > 0) &&
	       (the_type != type_ERROR) &&
	       (the_size != std_size[the_type]))
	{	/* supply length modifier for non-standard type sizes */
	    (void)sprintf(digits,"*%d",the_size);
	    (void)fputs(digits,dcl_fd);
	    column += strlen(digits);
	}
    }
    if (DECLARE_COMPACT())
    {
	print_blanks(1);
	column++;
    }
    else if (column < FIRST_VARIABLE_COLUMN)
    {
	print_blanks(FIRST_VARIABLE_COLUMN-column);
	column = FIRST_VARIABLE_COLUMN;
    }
    else  if (column == FIRST_VARIABLE_COLUMN)
    {
	print_blanks(1);
	column++;
	print_blanks(NEXT_COLUMN(column)-column);
	column = NEXT_COLUMN(column);
    }
    else
    {
	print_blanks(NEXT_COLUMN(column)-column);
	column = NEXT_COLUMN(column);
    }
    return (column);
}


PRIVATE int
#if HAVE_STDC
select_arguments(Lsymtab *sym_entry)
#else /* K&R style */
select_arguments(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a module argument) */
    if (sym_entry->declared_external ||
	sym_entry->invoked_as_func)
	return (0);
    else if (sym_entry->argument)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
select_commons(Lsymtab *sym_entry)
#else /* K&R style */
select_commons(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is in a COMMON block) */
    if (sym_entry->common_var)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
select_externals_by_name(Lsymtab *sym_entry)
#else /* K&R style */
select_externals_by_name(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is external and must appear in EXTERNAL declaration) */

    if (sym_entry->declared_intrinsic) /* must appear first, because symbols */
	return (0); /* can be both declared_intrinsic and declared_external*/
		    /* ??? is this a bug in ftnchek 2.7 ??? */
    else if (storage_class_of(sym_entry->type) == class_STMT_FUNCTION)
	return (0);
    else if (sym_entry->declared_external)
	return (1);
    else if (sym_entry->declared_intrinsic || sym_entry->intrinsic)
	return (0);
    else if (sym_entry->invoked_as_func)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
select_externals_by_type(Lsymtab *sym_entry)
#else /* K&R style */
select_externals_by_type(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is external and must appear in a type declaration) */
    if (storage_class_of(sym_entry->type) == class_STMT_FUNCTION)
	return (0);
    else if (sym_entry->declared_external)
	return (1);
    else if (sym_entry->declared_intrinsic)
	return (0);
    else if (sym_entry->intrinsic)
    {
	if (datatype_of(sym_entry->type) == type_UNDECL)
	{			/* user provided no type declaration */
	    if ((sym_entry->info.intrins_info)->result_type == type_GENERIC)
		return (0);	/* generics CANNOT have explicit type */
	    else
		return (1);	/* not generic, so has explicit type */
	}
	else			/* user supplied an explicit type */
	    return (1);
    }
    else if (sym_entry->invoked_as_func)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
select_intrinsics_by_name(Lsymtab *sym_entry)
#else /* K&R style */
select_intrinsics_by_name(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is intrinsic and must appear in INTRINSIC declaration) */
    if (sym_entry->declared_intrinsic)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
select_intrinsics_by_type(Lsymtab *sym_entry)
#else /* K&R style */
select_intrinsics_by_type(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is intrinsic and must appear in a type declaration) */
    if (sym_entry->intrinsic &&
	((sym_entry->info.intrins_info)->result_type == type_GENERIC))
	return (0);
    else
	return (select_intrinsics_by_name(sym_entry));
}


PRIVATE int
#if HAVE_STDC
select_locals(Lsymtab *sym_entry)
#else /* K&R style */
select_locals(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a local variable) */

    if (SF3_DECLARATIONS() && sf3_internal_name(sym_entry))
	return (0);
    else if (sym_entry->argument ||
	sym_entry->common_var ||
	sym_entry->declared_external ||
	sym_entry->declared_intrinsic ||
	sym_entry->entry_point ||
	sym_entry->external ||
	sym_entry->intrinsic ||
	sym_entry->invoked_as_func ||
	sym_entry->parameter)
	return (0);
    else
	return (1);
}


PRIVATE int
#if HAVE_STDC
select_common_blocks(Lsymtab *sym_entry)
#else /* K&R style */
select_common_blocks(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a COMMON block name) */
    if (storage_class_of(sym_entry->type) == class_COMMON_BLOCK)
	return (1);
    else
	return (0);
}

PRIVATE int
#if HAVE_STDC
select_namelists(Lsymtab *sym_entry)
#else /* K&R style */
select_namelists(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a NAMELIST name) */
    if (storage_class_of(sym_entry->type) == class_NAMELIST)
	return (1);
    else
	return (0);
}

PRIVATE int
#if HAVE_STDC
select_parameters(Lsymtab *sym_entry)
#else /* K&R style */
select_parameters(sym_entry)
    Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    /* return (symbol is a PARAMETER name) */
    if (sym_entry->parameter)
	return (1);
    else
	return (0);
}



PRIVATE int
#if HAVE_STDC
select_statement_functions(Lsymtab *sym_entry)
#else /* K&R style */
select_statement_functions(sym_entry)
     Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{
    if (storage_class_of(sym_entry->type) == class_STMT_FUNCTION)
	return (1);
    else
	return (0);
}


PRIVATE int
#if HAVE_STDC
sf3_internal_name(Lsymtab *sym_entry)
#else /* K&R style */
sf3_internal_name(sym_entry)
     Lsymtab *sym_entry;
#endif /* HAVE_STDC */
{    /* Return (symbol is an SFTRAN3 internal name). */
    char *p = sym_entry->name;

    /* The SFTRAN3 preprocessor uses internal names of the form NPRddd,
       NXdddd, N2dddd, and N3dddd, where d is a decimal digit. */

    if ((p[0] != 'N') || (strlen(p) != 6))
	return (0);
    switch (p[1])
    {
    case 'P':
	if ((p[2] == 'R') && isdigit(p[3]) && isdigit(p[4]) && isdigit(p[5]))
	    return (1);
	else
	    return (0);

    case 'X':                   /* fall through */
    case '2':                   /* fall through */
    case '3':
	if (isdigit(p[2]) && isdigit(p[3]) && isdigit(p[4]) && isdigit(p[5]))
	    return (1);
	else
	    return (0);

    default:
	return (0);
    }
}
