/* ftnchek.h:

	Common definitions for Fortran Program Checker

    Copyright (C) 1994 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.

*/

#define COPYRIGHT_DATE \
"Copyright (C) 1994, 1995, 1996 by Robert K. Moniot"
#define COPYRIGHT_NOTICE \
"This program is free software.  Permission is granted to\n\
modify it and/or redistribute it, retaining this notice.\n\
No guarantees accompany this software."
#define VERSION_NUMBER		"FTNCHEK Version 2.9 April 1996"
#define PATCHLEVEL		"Patch Level 0"
#define PROJECT_VERSION		"P2" /* Project file format version number */

		/* Define macro PROTO for declaring function prototypes.  If
		   compiler doesn't accept prototypes, use -DNO_PROTOTYPES.
		   The args in macro must be enclosed in parentheses.
		   Define macro VOID for declaring functions without
		   args.  Also define HAVE_STDC for declaring functions
		   in ANSI form or Kernighan-Ritchie form.  Each function
		   defn is in both forms, with #ifdef HAVE_STDC to select
		   between them.
		 */

#ifdef NO_PROTOTYPES
#define PROTO(TYPE_AND_NAME,ARGS) TYPE_AND_NAME()
#define VOID
#define HAVE_STDC	0
#else
#define PROTO(TYPE_AND_NAME,ARGS) TYPE_AND_NAME ARGS
#define VOID		void
#define HAVE_STDC	1
#endif

	/* The following system defines should be defined with the -D
	   (for UNIX) or /DEFINE (for VMS) compiler options in the makefile,
	   not here.  They are shown here so you know what to define.
	*/

/*#define VMS*/		/* Set flag for VAX/VMS   system-dependent defns. */
/*#define UNIX*/	/* Set flag for UNIX (ATT or BSD) defns. */
#ifdef __TURBOC__
#define MSDOS		/* Set flag for MSDOS (IBM PC) */
#define T_ALLOC		/* Specify workaround for malloc limits */
#endif

#ifdef MSDOS
#define NO_FLOATING_POINT	/* No need for floating point coprocessor */
#endif
			/* For portability, declare standard routines here. */
#if defined(__STDC__) || defined(__osf__)
#include <stdlib.h>
#else
PROTO(char *getenv,(char *));
PROTO(char *malloc,(unsigned));
PROTO(char *calloc,(unsigned,unsigned));
PROTO(void exit,(int));
#endif


/*************************************************************************
     Begin section of defines for system-dependent configuration.
**************************************************************************/


	/* set flag to allow options to start with '/' */
#ifndef OPTION_PREFIX_SLASH
#ifndef NO_OPTION_PREFIX_SLASH
#ifdef VMS
#define OPTION_PREFIX_SLASH
#endif
#ifdef MSDOS
#define OPTION_PREFIX_SLASH
#endif
#endif
#endif

			/* Define standard home directory for VMS.  This
			   is one place it looks for rc file.  You can
			   also define SPECIAL_HOMEDIR for other systems
			   such as MSDOS or Mac. */
#ifdef VMS
#ifndef SPECIAL_HOMEDIR
#define SPECIAL_HOMEDIR "SYS$DISK:[]"
#endif
#endif

	/* Define the Unix-style startup file name and a non-Unix-style
	   alternate.  Both are looked for on all systems. */

#ifndef UNIX_RC_FILE
#define UNIX_RC_FILE	".ftnchekrc"
#endif
#ifndef NONUNIX_RC_FILE
#define NONUNIX_RC_FILE	"ftnchek.ini"
#endif

		/* Define default source and output file extensions.  These
		 * can be overridden by defines on compiler commandline.
		 */
#ifndef DEF_SRC_EXTENSION
#ifdef VMS
#define DEF_SRC_EXTENSION ".for"		/* VMS default extension */
#endif
#ifdef MSDOS
#define DEF_SRC_EXTENSION ".for"		/* IBM PC default extension */
#endif
#endif /* DEF_SRC_EXTENSION */

#ifndef DEF_SRC_EXTENSION
#define DEF_SRC_EXTENSION ".f"		/* Unix and all others */
#endif
		/* define default list-file extension */
#ifndef DEF_LIST_EXTENSION
#define DEF_LIST_EXTENSION ".lis"
#endif
		/* define default project-file extension */
#ifndef DEF_PROJ_EXTENSION
#define DEF_PROJ_EXTENSION ".prj"
#endif
		/* define default declaration-file extension */
#ifndef DEF_DCL_EXTENSION
#define DEF_DCL_EXTENSION ".dcl"
#endif
		/* define default include-file extension */
#ifndef DEF_INC_EXTENSION
#define DEF_INC_EXTENSION DEF_SRC_EXTENSION
#endif
		/* define default declaration-file extension */
#ifndef DEF_VCG_EXTENSION
#define DEF_VCG_EXTENSION ".vcg"
#endif
		/* define project-file name for case of input from stdin */
#ifndef STDIN_PROJ_FILENAME
#define STDIN_PROJ_FILENAME "ftnchek.prj"
#endif

#ifndef ENV_PREFIX		/* prefix for option environment variables */
#define ENV_PREFIX "FTNCHEK_"
#endif

#ifndef ENV_INCLUDE_VAR
#define ENV_INCLUDE_VAR "INCLUDE" /* name of env variable for include dir */
#endif

#ifndef DEFAULT_INCLUDE_DIR
#ifdef UNIX
#define DEFAULT_INCLUDE_DIR "/usr/include"
#endif
#ifdef VMS
#define DEFAULT_INCLUDE_DIR "SYS$LIBRARY:"
#endif
#ifdef MSDOS
#define DEFAULT_INCLUDE_DIR "\\include"
#endif
#endif

/*************************************************************************
     End of section of defines for system-dependent configuration.

	Begin section of defines to control ftnchek's behavior
	      (syntax accepted, options supported, etc.)
**************************************************************************/




				/* The following macros define the default
				   value of the -source setting */

#ifndef DEC_TABS	/* DEC-style ugly tabbed source is off by default */
#define DEC_TABS 0
#endif
			/* VMS-style INCLUDE supports defaulting
			   extension, /NOLIST feature.  It is default
			   for VMS, not for other systems. */
#ifndef VMS_INCLUDE
#ifdef VMS
#define VMS_INCLUDE 1
#else
#define VMS_INCLUDE 0
#endif
#endif

#ifndef UNIX_BACKSLASH	/* UNIX backslash escape sequences in strings */
#define UNIX_BACKSLASH 0
#endif

			/* This is the default -source value */
#define DEF_SOURCE_FORMAT (DEC_TABS+2*VMS_INCLUDE+4*UNIX_BACKSLASH)


	/* VCG support is now standard. If you don't want it, you can
	   eliminate it by defining NO_VCG_SUPPORT.  You can customize
	   the graph options by changing the macro VCG_GRAPH_OPTIONS.
	   Include \n between options and after the last option.
	   (These options affect the graph as a whole.)  */

#ifndef NO_VCG_SUPPORT
#define VCG_SUPPORT
#ifndef VCG_GRAPH_OPTIONS
#define VCG_GRAPH_OPTIONS "color: lightgray\n"
#endif
#endif

	/* The following defines control the default status of
	   warnings controlled by -f77, -port, -pretty, and -trunc.
	 */

			/* -f77 options default to F77_ALL.  Define
			   STRICT_SYNTAX to make -f77=all the default.
			   Otherwise -f77=none is default.
			*/
#ifdef STRICT_SYNTAX
#define F77_ALL 1
#else
#define F77_ALL 0
#endif

			/* -portability options default to PORT_ALL.  Define
			   STRICT_PORTABILITY to make -port=all the default.
			   Otherwise -port=none is default.
			*/
#ifdef STRICT_PORTABILITY
#define PORT_ALL TRUE
#else
#define PORT_ALL FALSE
#endif

			/* -pretty options default to PRETTY_ALL.  Define
			   UGLY_IS_OK to make -pretty=none the default.
			   Otherwise -pretty=all is default.
			*/
#ifdef UGLY_IS_OK
#define PRETTY_ALL FALSE
#else
#define PRETTY_ALL TRUE
#endif

			/* -truncation options default to TRUNC_ALL.  Define
			   LAX_TRUNCATION to make -trunc=none the default.
			   Otherwise -trunc=all is default.
			*/
#ifdef LAX_TRUNCATION
#define TRUNC_ALL FALSE
#else
#define TRUNC_ALL TRUE
#endif


		/* If STANDARD_INTRINSICS is defined, only F77
		   standard intrinsic functions (plus double complex
		   flavors of standard ones) will be known to ftnchek.
		   Otherwise there are three classes of nonstandard
		   intrinsics: common, Unix flavor, and VMS flavor.
		   Common ones will be recognized if -intrinsic=
		   nonzero.  -intrinsic= 2 or 3 specify UNIX or VMS
		   rsptly, unless suppressed with NO_UNIX_INTRINSICS
		   or NO_VMS_INTRINSICS.  */

#ifndef STANDARD_INTRINSICS

#define NONSTD_INTRINSICS	/* Common nonstandard intrinsic functions */

#ifndef NO_UNIX_INTRINSICS
#define UNIX_INTRINSICS		/* UNIX intrinsic functions */
#endif
	/* Note: RAND syntax varies.  Define RAND_NO_ARG to make ftnchek
	   expect X=RAND(). By default ftnchek expects X=RAND(ISEED).
	 */

#ifndef NO_VMS_INTRINSICS
#define VMS_INTRINSICS		/* VMS intrinsic functions */
#endif

#endif /* not STANDARD_INTRINSICS */

			/* Define the default -intrinsic setting */

				/* Define which set to use */
#ifdef UNIX	/* Unix default: accept Unix intrinsics */
#define DEF_INTRINSIC_SET 2
#endif

#ifdef VMS	/* VMS default: accept VMS intrinsics */
#define DEF_INTRINSIC_SET 3
#endif

#ifndef DEF_INTRINSIC_SET	/* Neither one: use only common set */
#define DEF_INTRINSIC_SET 1
#endif

				/* Define which RAND form to use */
#ifdef RAND_NO_ARG	/*RAND() form*/
#define DEF_INTRINSIC_RAND 0
#else
#ifdef RAND_ONE_ARG	/*RAND(ISEED) form*/
#define DEF_INTRINSIC_RAND 1
#else				/* Allow either form */
#define DEF_INTRINSIC_RAND 2
#endif
#endif

				/* Define which IARGC form to use */
#ifdef IARGC_NO_ARG
#define DEF_INTRINSIC_IARG 0
#else
#ifdef IARGC_ONE_ARG
#define DEF_INTRINSIC_IARG 1
#else  /* default is to allow 0 or 1 */
#define DEF_INTRINSIC_IARG 2
#endif
#endif
				/* Now define the default setting from
				   the above choices. */
#define DEF_INTRINSIC_OPT (DEF_INTRINSIC_IARG*100 + DEF_INTRINSIC_RAND*10 \
			   + DEF_INTRINSIC_SET)



/*  Default BpW = 4 bytes per word, which matches many machines.
    This macro serves as the default value for -wordsize.
    If the Fortran code does not declare explicit sizes of
    numeric variables (e.g. REAL*8), then the value of -wordsize will
    not matter, since the table conforms to the standard in that
    sizeof(INTEGER)=sizeof(REAL), and sizeof(DOUBLE)=sizeof(COMPLEX)
    =2*sizeof(REAL).  If the code does declare explicit sizes of
    numeric types, then the value of -wordsize will matter if explicit
    and default sizes are expected to match.  If you want to
    suppress warnings of this kind, you may use the -wordsize setting
    or, to make it the default, change BpW to match
    your hardware.  Under the -portability option, explicit and
    default sizes never match: e.g. passing REAL*8 where DOUBLE
    PRECISION expected.  None of this applies to CHARACTER data:
    the default size (1) is well-defined, and the standard does
    not specify the ratio of sizeof(CHARACTER) to sizeof(REAL). */

#ifndef BpW
#define BpW 4	/* Bytes per Word: might want to use sizeof(float) instead */
#endif

		/* Define to tolerate embedded blanks in numeric consts unless
		   feature turned off by defining NO_BLANKS_IN_NUMBERS. */
#ifndef NO_BLANKS_IN_NUMBERS
#define BLANKS_IN_NUMBERS
#endif

				/* Default value of -wrap option */
#ifndef WRAP_COLUMN
#define WRAP_COLUMN 79		/* When to wrap error messages to next line */
#endif

		/* The following defines used to be optional, now they
		   are standard. If these defines are removed or
		   undef'd, the corresponding syntax will not be
		   recognized (resulting in parse errors).  I don't
		   recommend undefining them.  They are all warned about
		   by -f77 */

#define ALLOW_CRAY_POINTERS 1
#define ALLOW_DOLLARSIGNS 1
#define ALLOW_DO_ENDDO 1
#define ALLOW_INCLUDE 1
#define ALLOW_QUOTEMARKS 1
#define ALLOW_TYPELESS_CONSTANTS 1
#define ALLOW_UNDERSCORES 1
#define ALLOW_UNIX_BACKSLASH 1
#define ALLOW_UNIX_CPP 1
#define ALLOW_VMS_IO 1

	/* If INLINE_COMMENT_CHAR is not defined, ! comments will cause
	   ftnchek to choke.  F77_INLINE_COMMENT controls whether it
	   is handled but warned about.
	*/
#ifndef NO_INLINE_COMMENT
#define INLINE_COMMENT_CHAR '!' /* Inline comments starting with '!' */
#endif

#define KEEP_ARG_NAMES 1	/* option was formerly experimental */

/*************************************************************************
	 End section of defines to control ftnchek's behavior

	  Begin section to define limits, table sizes, etc.
**************************************************************************/




#ifndef MAXLINE
#define MAXLINE 132	/* Maximum input line length.  Ignores past this. */
#endif
#ifndef MAXIDSIZE
#define MAXIDSIZE 31	/* Longest identifier allowed */
#endif
#ifndef MAX_SRC_TEXT
#define MAX_SRC_TEXT (20*66) /* Longest text string of a token */
#endif
#ifndef MAX_CHAR_CODE
#define MAX_CHAR_CODE 255 /* Largest possible value of a char (8 bits here) */
#endif
#ifndef MAX_INCLUDE_DEPTH
#define MAX_INCLUDE_DEPTH 16	/* Max nesting depth of include files */
#endif
#ifndef MAXEXPRTEXT
#define MAXEXPRTEXT 15		/* length of expr text saved in arg lists */
#endif
#ifndef MAX_RC_LINE
#define MAX_RC_LINE 500		/* Max length of line in rc file */
#endif
#ifndef RC_COMMENT_CHAR
#define RC_COMMENT_CHAR '#'	/* Comments start with this or space  */
#endif
		/* Definitions of table sizes
		     Constraints:
			STRSPACESZ > 20*72 so max f77 statement will fit
			LOCSYMTABSZ+GLOBSYMTABSZ < HASHSZ so hashtab never full
		 */

#define KEYHASHSZ 195	/* Size of keyword hashtable -- do not change */
#define INTRINS_HASHSZ 326 /* Chosen to give few clashes -- change with care */

#ifdef SMALL_MACHINE		/* Use these for e.g. IBM PC */
#ifndef HASHSZ			/* Hint: pick one with no square factors */
#define HASHSZ 798     /* SMALL_MACHINE Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 4000 /* SMALL_MACHINE chunk size of string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 300 /* SMALL_MACHINE Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 400 /* SMALL_MACHINE Size of global symbol table */
#endif
#ifndef PARAMINFOSPACESZ
#define PARAMINFOSPACESZ 20 /* SMALL_MACHINE ParamInfo structs per chunk */
#endif
#ifndef TOKHEADSPACESZ
#define TOKHEADSPACESZ 50 /* SMALL_MACHINE TokenListHeaders per chunk */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 200 /* SMALL_MACHINE tokens per token space chunk */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ	300 /* SMALL_MACHINE argument headers per chunk */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ	1000 /* SMALL_MACHINE argument list elts per chunk */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ	200 /* SMALL_MACHINE common list headers per chunk */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ	1000 /* SMALL_MACHINE common elts per chunk */
#endif
#ifndef PTRSPACESZ
#define PTRSPACESZ	200  /* SMALL_MACHINE ptrs-to-arraydim per chunk */
#endif

#else  /* end if SMALL_MACHINE */

#ifdef LARGE_MACHINE		/* use these if space is no problem */
#ifndef HASHSZ		/* must be <= max int  */
#define HASHSZ 20930	 /* LARGE_MACHINE Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 10000 /* LARGE_MACHINE chunk size of string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 8000 /* LARGE_MACHINE Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 12000 /* LARGE_MACHINE Size of global symbol table */
#endif
#ifndef PARAMINFOSPACESZ
#define PARAMINFOSPACESZ 200 /* LARGE_MACHINE ParamInfo structs per chunk */
#endif
#ifndef TOKHEADSPACESZ
#define TOKHEADSPACESZ 500 /* LARGE_MACHINE TokenListHeaders per chunk */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 10000 /* LARGE_MACHINE tokens per token space chunk */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ	15000 /* LARGE_MACHINE argument headers per chunk */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ	50000	/* LARGE_MACHINE argument list elts per chunk */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ	10000 /* LARGE_MACHINE common list headers per chunk */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ	50000 /* LARGE_MACHINE common elts per chunk */
#endif
#ifndef PTRSPACESZ
#define PTRSPACESZ	2000 /* LARGE_MACHINE Max number of ptrs to arraydim */
#endif

#else		/* Defaults: Use these for average-size applications */

#ifndef HASHSZ
#define HASHSZ 2002	/* Default Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 10000 /* Default chunk size of string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 800 /* Default Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 1200 /* Default Size of global symbol table */
#endif
#ifndef PARAMINFOSPACESZ
#define PARAMINFOSPACESZ 50 /* Default ParamInfo structs per chunk */
#endif
#ifndef TOKHEADSPACESZ
#define TOKHEADSPACESZ 200 /* Default TokenListHeaders per chunk */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 1000 /* Default tokens per token space chunk */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ	1500 /* Default argument headers per chunk */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ	5000	/* Default argument list elts per chunk */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ	1000 /* Default common list headers per chunk */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ	4000 /* Default common elts per chunk */
#endif
#ifndef PTRSPACESZ
#define PTRSPACESZ	400 /* Default Max number of ptrs to arraydim */
#endif

#endif /* end if LARGE_MACHINE else */

#endif/*end if SMALL_MACHINE else*/

/*************************************************************************
	  End of section to define limits, table sizes, etc.

		From here down should not be altered.
**************************************************************************/



#define FALSE 0
#define TRUE 1

#define NO_COL_NUM ((unsigned)999)/* Impossible column number to suppress
				 * printing in error messages
				 */
#define NO_LINE_NUM ((unsigned)0)/* Ditto for line number to suppress flushing
				 * of line if error not in local context
				 */

#define OOPS_NONFATAL 0		/* Severity of "oops" messages */
#define OOPS_FATAL 1

/*************************************************************************
	    Shared variable and function defns start here
**************************************************************************/

#ifdef MAIN
#define SHARED		/* (nothing) */
#else
#define SHARED extern	/* Non-main routines declare shared vars extern */
#endif

#define PRIVATE static	/* For non-shared functions */

		/* Ftnchek has no need of floating-point calculations,
		   so to allow it to run on machines without coprocessor,
		   we neutralize the few floating-point operations.
		*/
#ifdef NO_FLOATING_POINT
typedef long DBLVAL;
#else
typedef double DBLVAL;
#endif

SHARED FILE
            *input_fd,  /* Input file */
            *list_fd,	/* Output file for listing */
	    *dcl_fd,	/* Output type declaration file */
#ifdef VCG_SUPPORT
	    *vcg_fd,	/* Output VCG graph description file */
#endif
	    *project_fd;/* Project file for symtab info summary */

SHARED char *current_filename,	/* name of current input file */
	    *top_filename;	/* name of toplevel parent input file */
SHARED int incdepth;

#ifdef VCG_SUPPORT
SHARED char *main_filename;	/* name of file containing main program */
#endif


		/* Declare variables for command line options */
#ifdef MAIN
#define OPT(Type,Name,Value) Type Name=Value
#else
#define OPT(Type,Name,Value) extern Type Name
#endif

				/* Define flag bits for call_tree_options */
					/* First three are a 2-bit option */
#define CALLTREE_PRINT		0x0001
#define CALLTREE_REFLIST	0x0002
#define CALLTREE_VCG		0x0003

#define CALLTREE_NOPRUNE	0x0004 /* expand each node, not "see above" */
#define CALLTREE_NOSORT		0x0008 /* program order, not alphabetized */
#define CALLTREE_WITHARGS	0x0010 /* include args of calls */

		/* These options are controlled by -f77 */
OPT(int,f77_20_continue,F77_ALL);
OPT(int,f77_accept_type,F77_ALL);
OPT(int,f77_byte,F77_ALL);
OPT(int,f77_cray_pointers,F77_ALL);
OPT(int,f77_common_subprog_name,F77_ALL);
OPT(int,f77_d_comment,F77_ALL);
OPT(int,f77_dec_tabs,F77_ALL);
OPT(int,f77_do_enddo,F77_ALL);
OPT(int,f77_dollarsigns,F77_ALL);
OPT(int,f77_double_complex,F77_ALL);
OPT(int,f77_format_dollarsigns,F77_ALL);
OPT(int,f77_format_extensions,F77_ALL);
OPT(int,f77_function_noparen,F77_ALL);
OPT(int,f77_implicit_none,F77_ALL);
OPT(int,f77_include,F77_ALL);
OPT(int,f77_inline_comment,F77_ALL);
OPT(int,f77_internal_list_io,F77_ALL);
OPT(int,f77_intrinsics,F77_ALL);
OPT(int,f77_long_names,F77_ALL);
OPT(int,f77_mixed_common,F77_ALL);
OPT(int,f77_mixed_expr,F77_ALL);
OPT(int,f77_namelist,F77_ALL);
OPT(int,f77_overlength,F77_ALL);
OPT(int,f77_param_intrinsic,F77_ALL);
OPT(int,f77_param_noparen,F77_ALL);
OPT(int,f77_quotemarks,F77_ALL);
OPT(int,f77_typeless_constants,F77_ALL);
OPT(int,f77_typesize,F77_ALL);
OPT(int,f77_underscores,F77_ALL);
OPT(int,f77_unix_backslash,F77_ALL);
OPT(int,f77_unix_cpp,F77_ALL);
OPT(int,f77_variable_format,F77_ALL);
OPT(int,f77_vms_io,F77_ALL);
		/* End of -f77 options */

		/* These options are controlled by -portability */
OPT(int,port_common_alignment,PORT_ALL);/* Common not in desc size order */
OPT(int,port_backslash,PORT_ALL); 	/* Backslash used in standard way */
OPT(int,port_real_do,PORT_ALL);		/* Non-integer DO loop bounds */
OPT(int,port_long_string,PORT_ALL);	/* Char string > 255 in length */
OPT(int,port_hollerith,PORT_ALL);	/* Hollerith (except in FORMAT) */
OPT(int,port_mixed_equiv,PORT_ALL);	/* Different types equivalenced */
OPT(int,port_mixed_size,PORT_ALL);	/* sized, nonsized types mixed */
OPT(int,port_tabs,PORT_ALL);		/* Tabs in source */
		/* End of -portability options */


		/* These options are controlled by -pretty */
OPT(int,pretty_multiple_common,PRETTY_ALL);/* COMMON decl in multiple stmts */
OPT(int,pretty_multiple_namelist,PRETTY_ALL);/* NAMELIST decl in multiple stmts */
OPT(int,pretty_parens,PRETTY_ALL);	/* Parentheses around a variable */
OPT(int,pretty_overlength,PRETTY_ALL);	/* Lines over 72 columns */
OPT(int,pretty_extra_space,PRETTY_ALL);	/* Spaces in identifiers */
OPT(int,pretty_no_space,PRETTY_ALL);    /* Space missing btw id and keyword */
OPT(int,pretty_contin,PRETTY_ALL);	/* Continuation mark after comment */
		/* End of -pretty options */

		/* These options are controlled by -truncation */
OPT(int,trunc_int_div_real,TRUNC_ALL);	/* Int/int --> real */
OPT(int,trunc_int_div_exponent,TRUNC_ALL);/* Int/int as exponentl */
OPT(int,trunc_int_div_zero,TRUNC_ALL);	/* Int/int const = 0 */
OPT(int,trunc_int_neg_power,TRUNC_ALL);	/* Int**(-int) */
OPT(int,trunc_real_subscript,TRUNC_ALL);/* Real array subscript */
OPT(int,trunc_real_do_index,TRUNC_ALL);	/* Real DO index with int bounds */
OPT(int,trunc_demotion,TRUNC_ALL);	/* High --> low precision */
OPT(int,trunc_promotion,TRUNC_ALL);	/* Low --> high precision */
OPT(int,trunc_sigfigs,TRUNC_ALL);	/* Sngl const overspecified */
		/* End of -truncation options */

	/* The following variables are controlled by command line options */

OPT(int,print_call_tree,FALSE);	/* Print the call tree */
OPT(int,do_check,TRUE);		/* For -nocheck option */
OPT(int,print_xref_list,FALSE);	/* Print subprogram cross-references */
OPT(int,decls_required,FALSE);	/* List all undeclared identifiers */
OPT(int,div_check,FALSE);	/* Check for possible division by zero */
OPT(int,ext_def_check,TRUE);	/* Check defined status of externals*/
OPT(int,help_screen,FALSE);	/* Print out help screen */
OPT(int,library_mode,FALSE);	/* Set used-flag for all modules in file */

#ifdef EOLSKIP
OPT(int,eol_is_space,TRUE);	/* Treat contd stmt linebreaks as space */
#endif

OPT(int,do_list,FALSE);		/* Listing flag */
OPT(int,novice_help,TRUE);	/* Extra help for novices */
OPT(int,make_project_file,FALSE);/* Save symtab defns in .prj file */
OPT(int,pure_functions,TRUE);	/* Assume functions are pure */
OPT(int,print_ref_list,FALSE);	/* Print reference (who-calls-who) list */
OPT(int,quiet,FALSE);		/* Less verbose output format */
OPT(int,sixclash,FALSE);	/* To check if names unique in 1st 6 chars */
OPT(int,print_topo_sort,FALSE);	/* Topological sort of modules */
OPT(int,do_symtab,FALSE);	/* For symbol table printout */
#ifdef VCG_SUPPORT
OPT(int,print_vcg_list,FALSE);	/* Print call graph in vcg format */
#endif
OPT(int,print_version,FALSE);	/* Print version number and quit */
OPT(int,volatile_flag,FALSE);	/* Assume volatile vars and comblocks */
		/* Debugging flags */
OPT(int,debug_latest,FALSE);	/* debug the latest addition */
OPT(int,debug_glob_symtab,FALSE);/* global symtab contents */
OPT(int,debug_parser,FALSE);	/* grammar debug via DBG statements */
OPT(int,debug_hashtab,FALSE);	/* hash table contents */
OPT(int,debug_loc_symtab,FALSE); /* local symtab contents */
OPT(int,show_resources,FALSE);	/* space avail and used */
OPT(int,debug_lexer,FALSE);	/* list of tokens as scanned */
#ifdef MAIN
extern int yydebug;		/* grammar debug via yydebug */
#endif
		/* Declare variables for commandline settings */
OPT(int,argcheck_strictness,3);	/* Strictness for checking args */
OPT(int,array_arg_check,3);/* Check array argument dims & size  */
OPT(int,call_tree_options,0);	/* Sum of: 1 (print it), */
				/* 2 (who-calls-who form), 4 (don't prune) */
				/* 8 (program order), 16 (VCG format) */
OPT(int,intrinsic_opt,DEF_INTRINSIC_OPT);/* 0=std, 1=common, 2=unix, 3=vms */
OPT(int,max_stmt_col,72);	/* End of statement field ( <= MAXLINE )*/
OPT(int,comcheck_strictness,3);	/* 0 (no check) to 3 (exact type & size) */
OPT(int,make_dcls,0);		/* sum of: 0 (none), 1 (all), */
				/* 2 (undeclared), 4 (compact), 8 (keywords */
				/* lowercase), 16 (variables and constants */
				/* lowercase), 32 (exclude sf3 internal */
				/* variables), 64 (use * comment starter), */
				/* 128 (use 'c' comment starter). */
OPT(int,source_format,DEF_SOURCE_FORMAT);/* Sum of: 1=DEC tabs, */
				/*   2=VMS style INCLUDE, 4=Unix backslash */
OPT(int,usage_check,333);	/* Print set/used/notset checks */
OPT(int,local_wordsize,BpW);	/* Bytes per word to use for default sizes */
OPT(int,given_wordsize,BpW);	/* User's request as per -wordsize=n  */
OPT(int,wrap_column,WRAP_COLUMN);/* For wrapping error messages */

			/* Secondary control vars based on usage_check */
OPT(int,var_usage_check,3);
OPT(int,com_usage_check,3);
OPT(int,ext_usage_check,3);

		/* Secondary variable for selecting intrinsic function set */
OPT(int,intrinsic_set,DEF_INTRINSIC_OPT%10);

			/* Shorthands for checking control settings */
#define check_args_off	(argcheck_strictness == 0)
#define check_args_number (argcheck_strictness&01)
#define check_args_type	(argcheck_strictness&02)
#define check_args_all	(argcheck_strictness == 3)
#define check_array_dims (array_arg_check&01) /* levels 1 and 3 */
#define check_array_size (array_arg_check&02) /* levels 2 and 3 */

#define check_var_set_used	(var_usage_check&01) /* levels 1 and 3 */
#define check_var_unused	(var_usage_check&02) /* levels 2 and 3 */
#define check_com_set_used	(com_usage_check&01) /* levels 1 and 3 */
#define check_com_unused	(com_usage_check&02) /* levels 2 and 3 */
#define check_ext_set_used	(ext_usage_check&01) /* levels 1 and 3 */
#define check_ext_unused	(ext_usage_check&02) /* levels 2 and 3 */

#define check_com_off	(comcheck_strictness == 0) /* no checking common */
#define check_com_lengths (comcheck_strictness >= 2) /* match lengths */
#define check_com_byname (comcheck_strictness == 3) /* match var by var */
#define check_com_tree (volatile_flag&&check_com_set_used) /* Check undef errors */
#define check_volatile_com (volatile_flag)/* Check not saved */


		/* Define macros for source format options */
#define dec_tabs	(source_format&0x1)	/* DEC-style tabbed source */
#define vms_include	(source_format&0x2)	/* VMS-style INCLUDE */
#define unix_backslash	(source_format&0x4)	/* Treat backslash as escape */


		/* Declare variables for commandline StrSettings */
OPT(char,*out_fname,(char *)NULL);	/* Output filename */
#ifdef ALLOW_INCLUDE
OPT(char,*include_path,(char *)NULL);	/* An include-file directory */
#endif
OPT(char,*f77_warn_list,(char *)NULL); /* Non F77 extensions to warn about */
OPT(char,*port_warn_list,(char *)NULL); /* Nonportable things to warn about */
OPT(char,*pretty_warn_list,(char *)NULL); /* Misleading things to warn about */
OPT(char,*trunc_warn_list,(char *)NULL); /* Truncation pitfalls to warn about */

SHARED unsigned
    line_num,		/* line num of current char */
    col_num,		/* column num of current char */
    next_line_num,	/* line num of lookahead char */
    next_col_num;	/* column num of lookahead char */

SHARED unsigned
    error_count,	/* Count of syntax error messages per file */
    warning_count;	/* Count of warning messages per file */

SHARED char *
    tab_filename;	/* Filename where there are tabs for -port */

		/* Resource usage information: */
SHARED unsigned long
    tot_line_count,	/* total source lines (stmts + comments) */
    tot_stmt_line_count,/* total source stmt lines  */
    tot_exec_stmt_count,/* count of executable stmts in program */
    tot_module_count,	/* count of modules in program */
    exec_stmt_count,	/* count of executable stmts in module */
    max_exec_stmt_count,/* max number of executable stmts in any module */
    max_loc_symtab,	/* number of local symtab entries used */
    max_glob_symtab,	/* number of global symtab entries used */
    max_loc_strings,	/* chars of local stringspace used */
    max_srctextspace,	/* chars of source text space used */
    max_paraminfo,	/* number of param info structs used */
    max_tokenlists,	/* number of tokenlists constructed */
    max_token_space,	/* number of tokens used */
    max_ptrspace,	/* number of pointers used */

    glob_strings_used,		/* chars of global stringspace used */
    arglist_element_used,	/* arg array elements used */
    arglist_head_used,		/* arg heads used (1 per call) */
    comlist_element_used,	/* com array elements used */
    comlist_head_used;		/* com heads used (1 per defn) */

SHARED int
    equivalence_flag,   /* true while parsing EQUIVALENCE statement */
    initial_flag,	/* true while only label or initial keywords read */
    implicit_flag,	/* true while parsing IMPLICIT statement */
    implicit_letter_flag, /* true while getting letters in IMPLICIT list */
    implicit_type_given,/* true if IMPLICIT type statement found */
    implicit_none,	/* true if IMPLICIT NONE statement found */
    prev_token_class,	/* token class of last-returned token */
    curr_stmt_class;	/* Token class of current stmt's leading token */

		/* Define linked-list structure for include-path list */
#ifdef ALLOW_INCLUDE
typedef struct IPNode {
  struct IPNode *link;		/* next path on the list */
  char *include_path;		/* one path (full directory name) */
} IncludePathNode;

SHARED IncludePathNode *include_path_list; /* header to the list */
#endif

		/* Declare routines shared with main module ftnchek.c */


	/* in exprtype.c */
PROTO(void init_typesizes, ( void ));

	/* in forlex.c */
PROTO(void finish_scan, ( void ));
PROTO(int flush_line_out, ( unsigned n ));
PROTO(void init_keyhashtab, ( void ));
PROTO(void init_scan, ( void ));
PROTO(void open_include_file, ( char *fname, unsigned include_line_num ));

	/* in fortran.y/fortran.c */
PROTO(void init_parser, ( void ));
PROTO(int yyparse,  ( void ));

	/* in ftnchek.c */
PROTO(void msg_tail, ( char *s ));
PROTO(void nonportable, ( unsigned lineno, unsigned colno, char *s ));
PROTO(void nonstandard, ( unsigned lineno, unsigned colno ));
PROTO(void oops_message, ( int severity, unsigned lineno, unsigned colno, char *s ));
PROTO(void oops_tail, ( char *s ));
PROTO(void print_a_line, ( FILE *fd, char *line, unsigned num ));
PROTO(void  syntax_error, ( unsigned lineno, unsigned colno, char *s ));
PROTO(void ugly_code, ( unsigned lineno, unsigned colno, char *s ));
PROTO(void warning, ( unsigned lineno, unsigned colno, char *s ));
PROTO(void yyerror, ( char *s ));

	/* in pgsymtab.c */
PROTO(void check_arglists, ( void ));
PROTO(void check_comlists, ( void ));
PROTO(void check_com_usage, ( void ));
PROTO(void visit_children, ( void ));

	/* in plsymtab.c */
PROTO(void debug_symtabs, ( void ));

	/* in project.c */
PROTO(void proj_file_out, ( FILE *fd ));
PROTO(void proj_file_in, ( FILE *fd ));

	/* in symtab.c */
PROTO(void init_tables, ( void ));
PROTO(void init_globals, ( void ));
PROTO(unsigned long init_intrins_hashtab, ( void ));
PROTO(void init_symtab, ( void ));
PROTO(void note_filename, ( char *s ));
PROTO(void set_intrinsic_options, ( int option ));

#ifdef DEBUG_SIZES
PROTO(void print_sizeofs, ( void ));
#endif


