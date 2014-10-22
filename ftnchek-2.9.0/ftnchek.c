/*  ftnchek.c:

	Main program for Fortran Syntax Checker.

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


	Top-level input/output is done here: opening and closing files,
	and printing error, warning, and informational messages.

	Shared functions defined:
		print_a_line()	Prints source code line.
		yyerror()	Error messages from yyparse and elsewhere.
		syntax_error()	Error messages with line and column num.
		warning()	Warning messages.
		nonportable()	Portability warnings.
		wrapup()	Look at cross references, etc.
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef DEVELOPMENT             /* For maintaining the program */
#define DEBUG_SIZES
#endif
#define MAIN
#include "ftnchek.h"

#ifdef VMS
#define unlink(s) remove(s)
#else
PROTO( int unlink,( const char *pathname ) );
#endif

				/* Define warn_option_list struct here */
typedef struct {
  char *name;
  int *flag;
  char *explanation;
} WarnOptionList;

typedef struct {
    char *name;
    char **strvalue;
    char *turnon, *turnoff;
    WarnOptionList *option_list;
    char *explanation;
} StrsettingList;


PROTO( char * add_ext,( char *s, char *ext ));

PROTO(PRIVATE char * append_extension,( char *s, char *ext, int mode ));

PROTO(PRIVATE void append_include_path,( char *new_path ));

PROTO(PRIVATE int cistrncmp,( char *s1, char *s2, unsigned n ));

PROTO(PRIVATE void error_summary,( char *fname ));

PROTO(PRIVATE void error_message,( unsigned lineno, unsigned colno, char *s,
			   char *tag ));

PROTO(PRIVATE void get_env_options,( void ));

PROTO(PRIVATE void get_rc_options,( void ));

PROTO(PRIVATE FILE *find_rc,( void ));

PROTO( int has_extension,( char *name, char *ext ));

PROTO(PRIVATE void lintstyle_error_message,( unsigned lineno, unsigned colno,
				     char *s, char *tag ));

PROTO(PRIVATE void list_options,( FILE *fd ));

PROTO(PRIVATE void list_warn_options,(WarnOptionList warn_option[]));

PROTO(int main,( int argc, char *argv[] ));

PROTO(PRIVATE void make_env_name,( char *env_name, char *option_name ));

PROTO(PRIVATE char * new_ext,( char *s, char *ext ));

PROTO(PRIVATE void oldstyle_error_message,( unsigned lineno, unsigned colno,
				    char *s, char *tag ));

PROTO(PRIVATE void open_outfile,( char *s ));

#ifdef DEBUG_SIZES
PROTO(extern void print_sizeofs,( void ));	/* in symtab.c */
#endif

PROTO(PRIVATE void print_version_number,( void ));

PROTO(PRIVATE void process_warn_string,
 ( char *warn_string, WarnOptionList warn_option[] ));

PROTO(PRIVATE int read_setting,( char *s, int *setvalue, char *name, int
			 minlimit, int maxlimit, int turnoff, int
			 turnon, int min_default_value, int
			 max_default_value ));

PROTO(PRIVATE void resource_summary,( void ));

PROTO(PRIVATE void set_option,( char *s, char *where ));

PROTO(PRIVATE void set_warn_option,
 ( char *s, WarnOptionList warn_option[] ));

PROTO(PRIVATE void src_file_in,( char *infile ));

PROTO(PRIVATE void turn_off_checks,( void ));

PROTO(PRIVATE void update_str_options,( StrsettingList *strset ));

PROTO(PRIVATE void wrapup,( void ));



PRIVATE int project_file_input;	/* true if input is from .prj file */

#define full_output	(do_list || do_symtab)

PRIVATE unsigned long intrins_clashes;	
				/* count of intrinsic hashtable clashes */
#ifdef COUNT_REHASHES
extern unsigned long rehash_count; /* count of calls to rehash() */
#endif

	/* Here we define the commandline options.  Most options are boolean
	   switchopts, with "no" prefix to unset them.  Others (called
	   settings) are numeric quantities, defined using "=num".
	   A third category (strsettings) are string quantities, eg filenames.
	   The argument "?" will cause list of options to be printed out.
	   For VMS, options can be prefixed with either "-" or "/",
	   but messages will use the canonical form.  Since VMS allows
	   options to be smushed together, end-of-option is signalled by
	   either NUL or the / of next option.
	 */

#ifdef OPTION_PREFIX_SLASH
#define OPT_PREFIX '/'	/* Canonical VMS prefix for commandline options */
#define END_OF_OPT( C )  ((C) == '\0' || (C) == '/')
#else
#define OPT_PREFIX '-'	/* Canonical Unix prefix for commandline options */
#define END_OF_OPT( C )  ((C) == '\0')
#endif

#define OPT_MATCH_LEN 3	/* Options are matched only in 1st 3 chars */
#define NUM_SWITCHES (sizeof(switchopt)/sizeof(switchopt[0]))
#define NUM_SETTINGS (sizeof(setting)/sizeof(setting[0]))
#define NUM_STRSETTINGS (sizeof(strsetting)/sizeof(strsetting[0]))

/*	Option definitions:
	   New options can be added to lists by inserting definition
	   here using same syntax as others, and declaring the variable
	   with OPT(type,name,default); in ftnchek.h.  No other changes
	   needed.
*/


		/* List of switches is defined first.  Each entry gives the
		   name and the corresponding flag variable to be set
		   or cleared.  See set_option() for processing of switches.

		   N.B. list_options() will suppress printing of any options
		   whose explanation starts with "debug" unless the -debug
		   switch was previously given.
		 */
PRIVATE struct {
    char *name;
    int *switchflag;
    char *explanation;
} switchopt[]={
	{"check",	&do_check,	"perform checking"},
	{"crossref",	&print_xref_list,"print call cross-reference list"},
	{"declare",	&decls_required,"list undeclared variables"},
	{"division",	&div_check,	"catch possible div by 0"},
	{"extern",	&ext_def_check,	"check if externals defined"},
	{"help",	&help_screen,	"print help screen"},
	{"library",	&library_mode,	"treat next files as library"},
#ifdef EOLSKIP
	{"linebreak",	&eol_is_space,	"treat linebreaks as space"},
#endif
	{"list",	&do_list,	"print program listing"},
	{"novice",	&novice_help,	"extra help for novices"},
	{"project",	&make_project_file,	"create project file"},
	{"pure",	&pure_functions,"functions have no side effects"},
	{"quiet",	&quiet,		"less verbose output"},
	{"reference",	&print_ref_list,"print who-calls-who reference list"},
	{"resources",	&show_resources,"show info on resource usage"},
	{"sixchar",	&sixclash,	"catch nonunique names"},
	{"sort",	&print_topo_sort,"prerequisite-order sort of modules"},
	{"symtab",	&do_symtab,	"print symbol table info"},
#ifdef VCG_SUPPORT
	{"vcg",		&print_vcg_list,"print call graph in vcg format"},
#endif
	{"version",	&print_version,	"print version number"},
	{"volatile",	&volatile_flag,	"assume volatile common blocks"},

	{"debug",	&debug_latest,	"debug latest code"},
	{"global",	&debug_glob_symtab,	"debug global symtab info"},
	{"grammar",	&debug_parser,	"debug printout in parser"},
	{"hashtable",	&debug_hashtab,	"debug printout of hashtable"},
	{"local",	&debug_loc_symtab,	"debug local symtab info"},
	{"tokens",	&debug_lexer,	"debug printout in lexer"},
	{"yydebug",	&yydebug,	"debug via yydebug"},
};


		/* List of settings is defined here. Each entry gives
		   the name, the corresponding variable, the range
		   of permitted values, the value for turning it off,
		   followed by brief explanation.
		   See set_option() for processing. */
PRIVATE struct {
    char *name;
    int *setvalue;
    int minlimit,maxlimit,turnoff,turnon,min_default_value,max_default_value;
    char *explanation;
} setting[]={
  {"arguments",	&argcheck_strictness, 0, 3, 0, 3, 0, 3,
			"check args: 0=none 1=number 2=type 3=all"},
  {"array",	&array_arg_check, 0, 3, 0, 3, 0, 3,
			"check array args: 0=none 1=dims 2=size 3=all"},
#ifdef VCG_SUPPORT
  {"calltree",	&call_tree_options, 0, 15, 0, 1, 1, 1,
			"print subprogram call graph: sum of:\
\n\t  1=print call graph in tree format\
\n\t  2=who-calls-who format\
\n\t  3=VCG format\
\n\t  4=do not prune routines printed earlier\
\n\t  8=keep program order, not alphabetic"},
#else
  {"calltree",	&call_tree_options, 0, 15, 0, 1, 1, 1,
			"print subprogram call graph: sum of:\
\n\t  1=print call graph in tree format\
\n\t  2=who-calls-who format\
\n\t  4=do not prune routines printed earlier\
\n\t  8=keep program order, not alphabetic"},
#endif
  {"columns",	&max_stmt_col,  72, MAXLINE, 72, MAXLINE, 72, MAXLINE,
			"max line length processed"},
  {"common",	&comcheck_strictness,  0, 3, 0, 3, 0, 3,
			"common check: 0=none 3=most strict"},
  {"intrinsic",	&intrinsic_opt,	0, 223, 0, DEF_INTRINSIC_OPT,
				0, 220+DEF_INTRINSIC_SET,
			"intrinsic function options: three digits:\n\
\tones digit=choice: 0=f77, 1=extra, 2=unix, 3=vms\n\
\ttens digit=RAND form: 0=no arg, 1=one arg, 2=either\n\
\thundreds digit=IARGC form: 0=no arg, 1=one arg, 2=either"},
  {"makedcls",  &make_dcls, 0, 1023, 0, 1, 1, 1,
			"make type declaration statements: sum of:\n\
\t  1=declarations\n\
\t  2=undeclared-only\n\
\t  4=compact\n\
\t  8=use-continuation-lines\n\
\t 16=keywords-lowercase\n\
\t 32=variables-and-constants-lowercase\n\
\t 64=exclude-sftran3-internal-variables\n\
\t128=asterisk-comment-character\n\
\t256=lowercase-comment-char\n\
\t512=no-array-dimensions"},
  {"source",	&source_format, 0, 7, 0, 7, 0, 7,
			"source format options: sum of:\n\
\t  1=DEC Fortran tab-format\n\
\t  2=VMS-style INCLUDE statement\n\
\t  4=UNIX-style backslash escape char"},
  {"usage",	&usage_check,	000, 333, 000, 333, 000, 333,
			"check usage: three digits:\n\
\t1st digit=subprogs, 2nd digit=common vars, 3rd digit=local vars\n\
\tdigit 0=no check, 1=used-not-defined 2=unused 3=all"},
  {"wordsize",	&given_wordsize, 0, 16, 0, BpW, 0, 16,
			"standard wordsize in bytes (0=no default)"},
  {"wrap",	&wrap_column, 0, 999, 0, WRAP_COLUMN, 0, 999,
			"width of page to wrap error messages"},
};

		/* Here define list of warning options.  These are set
		   or cleared by -[no]f77=list option.  Note that the variables
		   are FALSE if feature is ALLOWED, and TRUE if feature is
		   to be WARNED about.  List must be alphabetized or at
		   least options with matching prefix strings must be
		   adjacent. */
/*** (struct was declared above: repeated in comment here for reference)
WarnOptionList {
  char *name;
  int *flag;
  char *explanation;
};***/

PRIVATE WarnOptionList
 f77_warn_option[]={
  {
#if F77_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Fortran 77"},	/* Title for list */
  {"accept-type",	&f77_accept_type,
				"ACCEPT and TYPE I/O statements"},
  {"backslash",		&f77_unix_backslash,
				"Unix backslash escape in strings"},
  {"byte",		&f77_byte,
				"BYTE data type"},
  {"common-subprog-name",&f77_common_subprog_name,
				"Common block & subprog with same name "},
  {"continuation",	&f77_20_continue,
				"More than 19 continuation lines"},
  {"cpp",		&f77_unix_cpp,
				"Unix C preprocessor directives"},
  {"d-comment",		&f77_d_comment,
				"Debug comments starting with D"},
  {"dec-tab"	,	&f77_dec_tabs,
				"DEC Fortran tab-formatted source"},
  {"do-enddo",		&f77_do_enddo,
				"DO loop extensions"},
  {"double-complex",	&f77_double_complex,
				"Double complex datatype"},
  {"format-dollarsign",	&f77_format_dollarsigns,
				"$ control code in FORMAT"},
  {"format-edit-descr",	&f77_format_extensions,
				"Nonstandard edit descriptors"},
  {"function-noparen",	&f77_function_noparen,
				"FUNCTION defined without parens"},
  {"implicit-none",	&f77_implicit_none,
				"IMPLICIT NONE statement"},
  {"include",		&f77_include,
				"INCLUDE statement"},
  {"inline-comment",	&f77_inline_comment,
				"Inline comments starting with !"},
  {"internal-list-io",	&f77_internal_list_io,
				"List-directed I/O to internal file"},
  {"intrinsic",		&f77_intrinsics,
				"Nonstandard intrinsic functions"},
  {"long-line",		&f77_overlength,
				"Statements with code past 72 columns"},
  {"long-name",		&f77_long_names,
				"Identifiers over 6 chars"},
  {"mixed-common",	&f77_mixed_common,
				"Mixed char and nonchar data in common"},
  {"mixed-expr",	&f77_mixed_expr,
				"Nonstandard type combinations in exprs"},
  {"name-dollarsign",	&f77_dollarsigns,
				"$ in identifiers"},
  {"name-underscore",	&f77_underscores,
				"Underscores in variable names "},
  {"namelist",		&f77_namelist,
				"NAMELIST statement"},
  {"param-intrinsic",	&f77_param_intrinsic,
				"Intrinsics and **real in PARAMETER defns"},
  {"param-noparen",	&f77_param_noparen,
				"PARAMETER statement without parens"},
  {"pointer",		&f77_cray_pointers,
				"Cray pointer syntax"},
  {"quotemark",		&f77_quotemarks,
				"Strings delimited by \"quote marks\""},
  {"typeless-constant",	&f77_typeless_constants,
				"Typeless constants like Z'19AF"},
  {"type-size",		&f77_typesize,
				"Sized type declarations like REAL*8"},
  {"variable-format",	&f77_variable_format,
				"Variable format repeat spec or field size"},
  {"vms-io",		&f77_vms_io,
				"VMS Fortran I/O keywords"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};


PRIVATE WarnOptionList
 port_warn_option[]={
  {
#if PORT_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Portability"},	/* Title for list */
  {"backslash",		&port_backslash,
				"Backslash in standard-conforming strings"},
  {"common-alignment",	&port_common_alignment,
				"COMMON not in descending size order"},
  {"hollerith",		&port_hollerith,
				"Hollerith constants (except in FORMAT)"},
  {"long-string",	&port_long_string,
				"Strings over 255 chars long"},
  {"mixed-equivalence",	&port_mixed_equiv,
				"Different data types equivalenced"},
  {"mixed-size",	&port_mixed_size,
				"Default and explicit size types mixed"},
  {"real-do",		&port_real_do,
				"Non-integer DO loops"},
  {"tab",		&port_tabs,
				"Tabs in source code"},
  {(char *)NULL, (int *)NULL, (char *)NULL},
};

PRIVATE WarnOptionList
 pretty_warn_option[]={
  {
#if PRETTY_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Appearance"},	/* Title for list */
  {"embedded-space",	&pretty_extra_space,
				"Space in variable names"},
  {"continuation",	&pretty_contin,
				"Continuation mark following comment line"},
  {"long-line",		&pretty_overlength,
				"Lines over 72 columns"},
  {"missing-space",	&pretty_no_space,
				"Missing space between variable & keyword"},
  {"multiple-common",	&pretty_multiple_common,
				"COMMON declared in multiple stmts"},
  {"multiple-namelist",	&pretty_multiple_namelist,
				"NAMELIST declared in multiple stmts"},
  {"parentheses",	&pretty_parens,
				"Parentheses around a variable"},
  {(char *)NULL, (int *)NULL, (char *)NULL},

};

PRIVATE WarnOptionList
 trunc_warn_option[]={
  {
#if TRUNC_ALL
   "all"	 /* used by -help */
#else
   "none"
#endif
     , (int *)NULL,		"Truncation"},	/* Title for list */
  {"demotion",		&trunc_demotion,
				"higher precision truncated to lower"},
  {"int-div-exponent",	&trunc_int_div_exponent,
				"int/int used as exponent"},
  {"int-div-real",	&trunc_int_div_real,
				"int/int converted to real"},
  {"int-div-zero",	&trunc_int_div_zero,
				"int/int = constant 0 "},
  {"int-neg-power",	&trunc_int_neg_power,
				"int**(-int), usually equals 0"},
  {"promotion",		&trunc_promotion,
				"lower precision promoted to higher"},
  {"real-do-index",	&trunc_real_do_index,
				"real DO index with int bounds"},
  {"real-subscript",	&trunc_real_subscript,
				"real array subscript"},
  {"significant-figures",&trunc_sigfigs,
				"single precision const overspecified"},
  {(char *)NULL, (int *)NULL, (char *)NULL},

};

		/* List of strsettings is defined here. Each entry
		   gives the name of the corresponding string
		   variable, value to set if "=str" omitted, and brief
		   explanation.  See set_option() for processing. */

/*** (struct was declared above: repeated in comment here for reference)
StrsettingList {
    char *name;
    char **strvalue;
    char *turnon, *turnoff;
    WarnOptionList *option_list;
    char *explanation;
};***/

PRIVATE StrsettingList strsetting[]={
  {"f77",	&f77_warn_list,	"all", "none",
     f77_warn_option,
     "warn about non-F77 extensions"},
#ifdef ALLOW_INCLUDE
  {"include",	&include_path,  (char *)NULL, (char *)NULL,
     (WarnOptionList *)NULL,
     "include-file directory"},
#endif
  {"output",	&out_fname,	(char *)NULL, (char *)NULL,
     (WarnOptionList *)NULL,
     "output file name"},
  {"portability",&port_warn_list,"all,", "none",
     port_warn_option,
     "warn about portability problems"},
  {"pretty",	&pretty_warn_list,"all", "none",
     pretty_warn_option,
     "warn about deceiving appearances"},
  {"truncation",&trunc_warn_list,"all", "none",
     trunc_warn_option,
     "check for truncation pitfalls"},
};


PRIVATE int must_open_outfile=FALSE; /* Flag set to TRUE when out=name given */
PRIVATE int nocheck_given=FALSE; /* Keep track whether -nocheck was given */

PRIVATE char *dclfile;
PRIVATE int actioncount=0;
int
#if HAVE_STDC
main(int argc, char **argv)
#else /* K&R style */
main(argc,argv)
	int argc;
	char *argv[];
#endif /* HAVE_STDC */
{
	int iarg;
	int filecount=0;
	char *infile,*srcfile,*projfile;
	int prev_intrinsic_opt= DEF_INTRINSIC_OPT;

#ifdef VMS			/* VMS version: expand wildcards, etc. */
	shell_mung(&argc,&argv,1,NULL);
#endif

	list_fd = stdout;
	project_fd = (FILE *) NULL;
	error_count = 0;
	warning_count = 0;
	include_path_list = (IncludePathNode*) NULL;

	get_env_options();	/* Pick up options from environment */
	get_rc_options();	/* Pick up options from "rc" file */

	init_tables();		/* Initialize tables */
	init_keyhashtab();
	intrins_clashes = init_intrins_hashtab();
	init_globals();
	init_symtab();

	for(iarg=1; iarg < argc; iarg++) {

	  int argchar=0;/* location of start of option */
#ifdef OPTION_PREFIX_SLASH
	  do {			/* loop on flags within argv[iarg] */
#endif
	    if( argv[iarg][argchar] == '-'
#ifdef OPTION_PREFIX_SLASH
		 || argv[iarg][argchar] == '/'	/* Allow VMS /option form */
#endif
					 ) {
			/* Process flags here */

		set_option(&argv[iarg][argchar],"commandline");

			/* Handle -version, -help, or -f77=help */
		if(print_version) goto do_action;

		if(help_screen) goto do_action;

				/* Allow checking to be turned off */
		if(! do_check ) {
		  turn_off_checks();
		  do_check = TRUE; /* close the trapdoor */
		  nocheck_given = TRUE;	/* but remember it was done */
		}
				/* Derive usage options from -usage setting */
		var_usage_check = usage_check % 10;
		com_usage_check = (usage_check/10) % 10;
		ext_usage_check = (usage_check/100) % 10;

				/* intrinsic_set = last digit of -intrinsic */
		intrinsic_set = intrinsic_opt % 10;
				/* Other digits used to fix intrins table */
		if(intrinsic_opt != prev_intrinsic_opt) {
		  set_intrinsic_options(intrinsic_opt);
		  prev_intrinsic_opt = intrinsic_opt;
		}
	    }
	    else if(strcmp(&argv[iarg][argchar],"?") == 0) {
		    help_screen = TRUE;
		    goto do_action;
	    }/*end of processing options*/

	    else {	/* Process file arguments */
do_action:

		if( must_open_outfile )
		    open_outfile(out_fname);

		if(actioncount == 0) {
		  print_version_number();
		}
		++actioncount;	/* Cause exit w/o reading stdin below */

			/* Honor -version, -help and -f77=help options */
		if(print_version) {
		  print_version = FALSE;
		  continue;
		}

		if(help_screen) {
		  help_screen = FALSE;
		  list_options(list_fd);
		}
		else {	/* Process files here */
		    ++filecount;

		    srcfile = add_ext(&argv[iarg][argchar],DEF_SRC_EXTENSION);
		    projfile = new_ext(&argv[iarg][argchar],DEF_PROJ_EXTENSION);
		    dclfile =  new_ext(&argv[iarg][argchar],DEF_DCL_EXTENSION);
#ifdef VCG_SUPPORT
				/* Initialize main_filename to 1st file arg */
		    if(main_filename == (char *)NULL)
		      main_filename = argv[iarg];
#endif
				/* Project file mode: open source for reading
				   and .prj file for writing. */
		    if(make_project_file) {

		      infile = srcfile;

		      if( has_extension(infile,DEF_PROJ_EXTENSION) ) {
			(void)fprintf(stderr,
			 "Input from %s disallowed in project mode\n",infile);
			goto next_arg;
		      }

		      if( (input_fd = fopen(infile,"r")) == (FILE *)NULL ) {
			(void)fprintf(stderr,"Cannot open file %s\n",infile);
			goto next_arg;
		      }

		      project_fd = fopen(projfile,"w");
		      project_file_input = FALSE;
		    }
		    else {
			/* Non project file mode: if input file extension
			   given, use it.  Otherwise read project file
			   if it exists else read source file. */
		      if( &argv[iarg][argchar]==srcfile
		       || (input_fd = fopen(projfile,"r")) == (FILE *)NULL) {
			infile = srcfile;
			if( (input_fd = fopen(infile,"r")) == (FILE *)NULL ) {
			  (void)fflush(list_fd);
			  (void)fprintf(stderr,"Cannot open file %s\n",infile);
			  goto next_arg;
			}
			project_file_input =
			  has_extension(infile,DEF_PROJ_EXTENSION);
		      }
		      else {
			infile = projfile;
			project_file_input = TRUE;
		      }
		    }

		    /* now that we have a source file, try to open the 
		       declaration file */
		    dcl_fd = (make_dcls > 0 &&  ! project_file_input) ?
		      fopen(dclfile,"w") : (FILE*)NULL;

				/* Always print input .f file name.  If
				   verbose mode, print .prj file names too.
				 */
		    if(!quiet || !project_file_input)
		      (void)fprintf(list_fd,"\nFile %s:%s",
			      infile,
			      full_output?"\n":""
			      );

				/* In verbose mode, print .prj output
				   file name to stderr.  Always print
				   error message if couldn't open it. */
		    if( make_project_file ) {
		      if(project_fd != (FILE *)NULL) {
			if(!quiet) {
			  (void)fflush(list_fd);
			  (void)fprintf(stderr,
				  "\nProject file is %s\n",projfile);
			}
		      }
		      else {
			(void)fflush(list_fd);
			(void)fprintf(stderr,
				"\nCannot open %s for output\n",projfile);
		      }
		    }


				/* only has effect if done before 1st file*/
		    init_typesizes();

		    if(project_file_input) {

		        current_filename = projfile;
			proj_file_in(input_fd);

		    }
		    else {

		      src_file_in(infile);

		    }

		    (void) fclose(input_fd);
		}/*end processing file args*/
	      }
next_arg:
#ifdef OPTION_PREFIX_SLASH
				/* Here we allow /opts to be stuck together */
	    while(argv[iarg][++argchar] != '\0'
		 && argv[iarg][argchar] != '/') /* look for next opt */
	      continue;

	  } while(argv[iarg][argchar] != '\0'); /*end do-while*/
#else
	  continue;
#endif
	}	/* end for-loop on argument list */


				/* No files given: read stdin */
	if(actioncount == 0) {

		print_version_number();

		if( must_open_outfile )
		    open_outfile(out_fname);

		if(make_project_file) {
		      projfile = STDIN_PROJ_FILENAME;
		      if( (project_fd = fopen(projfile,"w")) == (FILE *)NULL) {
			(void)fflush(list_fd);
			(void)fprintf(stderr,
				"\nCannot open %s for output\n",projfile);
		      }
		      else {
			if(!quiet) {
			  (void)fflush(list_fd);
			  (void)fprintf(stderr,
				"\nProject file is %s\n",projfile);
			}
		      }
		}

		++filecount;
		input_fd = stdin;

		init_typesizes();

		src_file_in("std_input");
	}
	if(filecount > 0) {
	  wrapup();
	  (void)fprintf(list_fd,"\n");
	}

	if(show_resources)
	    resource_summary();

	exit(0);
	return 0;/*NOTREACHED*/
}

PRIVATE void
#if HAVE_STDC
src_file_in(char *infile)
                  		/* input filename */
#else /* K&R style */
src_file_in(infile)
     char *infile;		/* input filename */
#endif /* HAVE_STDC */
{
	note_filename(infile);

	init_scan();
	init_parser();

	(void) yyparse();

	finish_scan();

	if(make_project_file) {
		  proj_file_out(project_fd);
		  (void) fclose(project_fd);
	}

	if ((make_dcls > 0) && (dcl_fd != stdout))
	{
	    if (ftell(dcl_fd) == 0L)	/* delete an empty .dcl file */
		(void)unlink(dclfile);
	    (void) fclose(dcl_fd);
	}

	if(port_tabs && (tab_filename != (char *)NULL)) {
	  if(tab_filename != top_filename) {
	    nonportable(NO_LINE_NUM,NO_COL_NUM,
			"Included file");
	    msg_tail(tab_filename);
	  }
	  else {
	    nonportable(NO_LINE_NUM,NO_COL_NUM,
		      "File");
	  }
	  msg_tail("contains tabs");
	}

	error_summary(infile);
}

PRIVATE void
print_version_number(VOID)
{
  if((full_output || !quiet) && !print_version)
    (void)fprintf(list_fd,"\n");
  (void)fprintf(list_fd,"%s",VERSION_NUMBER);
  if(help_screen || print_version)
    (void)fprintf(list_fd," %s",PATCHLEVEL);
  if(full_output || !quiet || print_version)
    (void)fprintf(list_fd,"\n");
}

PRIVATE void
#if HAVE_STDC
error_summary(char *fname)		/* Print out count of errors in file */
#else /* K&R style */
error_summary(fname)		/* Print out count of errors in file */
	char *fname;
#endif /* HAVE_STDC */
{
	FILE *fd = list_fd;

	if(full_output ||
	   (!quiet && error_count+warning_count != 0))
	  (void)fprintf(fd,"\n");

	if(full_output || !quiet || error_count != 0)
	  (void)fprintf(fd,"\n %u syntax error%s detected in file %s",
			error_count, error_count==1? "":"s",
			fname);

	if(warning_count != 0)
		(void)fprintf(fd,"\n %u warning%s issued in file %s",
			warning_count, warning_count==1? "":"s",
			fname);

	if(full_output ||
	   (!quiet && error_count+warning_count != 0))
	  (void)fprintf(fd,"\n");

	error_count = 0;
	warning_count = 0;
}

void
#if HAVE_STDC
print_a_line(FILE *fd, char *line, unsigned int num)  /* Print source line with line number */
#else /* K&R style */
print_a_line(fd,line,num)  /* Print source line with line number */
	FILE *fd;
	char *line;
	unsigned num;
#endif /* HAVE_STDC */
{
	(void)fprintf(fd,"\n %6u ",num); /* Print line number */

#ifdef DEC_TABS
				/* Tab-formatted source lines: tab in
				   col 1-6 moves to col 7. */
	if(dec_tabs) {
	  int i,col;
	  for(i=0,col=1; col < 7 && line[i] != '\0'; i++) {
	    if(line[i] == '\t') {
	      do{
		(void)fprintf(fd," ");
	      } while(++col < 7);
	    }
	    else {
		(void)fprintf(fd,"%c",line[i]);
		++col;
	    }
	  }
	  (void)fprintf(fd,"%s",line+i);
	}
	else
#endif
	  (void)fprintf(fd,"%s",line);
}


void
#if HAVE_STDC
yyerror(char *s)
#else /* K&R style */
yyerror(s)
	char *s;
#endif /* HAVE_STDC */
{
	syntax_error(line_num,col_num,s);
}


void
#if HAVE_STDC
syntax_error(unsigned int lineno, unsigned int colno, char *s)		/* Syntax error message */
#else /* K&R style */
syntax_error(lineno,colno,s)		/* Syntax error message */
	unsigned lineno,colno;
	char *s;
#endif /* HAVE_STDC */
{
	++error_count;
	error_message(lineno,colno,s,"Error");
}

void
#if HAVE_STDC
warning(unsigned int lineno, unsigned int colno, char *s)		/* Print warning message */
#else /* K&R style */
warning(lineno,colno,s)		/* Print warning message */
	unsigned lineno,colno;
	char *s;
#endif /* HAVE_STDC */
{
	++warning_count;

	error_message(lineno,colno,s,"Warning");
}

void
#if HAVE_STDC
ugly_code(unsigned int lineno, unsigned int colno, char *s)		/* -pretty message */
#else /* K&R style */
ugly_code(lineno,colno,s)		/* -pretty message */
	unsigned lineno,colno;
	char *s;
#endif /* HAVE_STDC */
{
	++warning_count;

	error_message(lineno,colno,s,"Possibly misleading appearance");
}

void
#if HAVE_STDC
nonstandard(unsigned int lineno, unsigned int colno)
#else /* K&R style */
nonstandard(lineno,colno)
     unsigned lineno,colno;
#endif /* HAVE_STDC */
{
	++warning_count;
	error_message(lineno,colno,"Nonstandard syntax","Warning");
}

void
#if HAVE_STDC
nonportable(unsigned int lineno, unsigned int colno, char *s) /* Print warning about nonportable construction */
#else /* K&R style */
nonportable(lineno,colno,s) /* Print warning about nonportable construction */
	unsigned lineno,colno;
	char *s;
#endif /* HAVE_STDC */
{
	++warning_count;
	error_message(lineno,colno,s,"Nonportable usage");
}

/* error_message prints out error messages and warnings.  It
   now comes in two flavors.  If using lintstyle_error_message(),
   messages are produced in style like UNIX lint:

	"main.f", line nn, col nn: Error: your message here

   Otherwise messages by oldstyle_error_message in old ftnchek style:

	Error near line nn col nn file main.f: your message here

   At this time, oldstyle_error_message is used when -novice is
   in effect, lintstyle_error_message otherwise.
*/

PRIVATE int errmsg_col;
	/* Crude macro to give number of digits in line and column numbers.
	   Used by line wrap computation. */
#define NUM_DIGITS(n) ((n)<10?1:((n)<100?2:((n)<1000?3:(n)<10000?4:5)))

PRIVATE void
#if HAVE_STDC
error_message(unsigned int lineno, unsigned int colno, char *s, char *tag)
#else /* K&R style */
error_message(lineno,colno,s,tag)
	unsigned lineno,colno;
	char *s,*tag;
#endif /* HAVE_STDC */
{
  if(novice_help)
    oldstyle_error_message(lineno,colno,s,tag);
  else
    lintstyle_error_message(lineno,colno,s,tag);
}

PRIVATE void
#if HAVE_STDC
lintstyle_error_message(unsigned int lineno, unsigned int colno, char *s, char *tag)
#else /* K&R style */
lintstyle_error_message(lineno,colno,s,tag)
	unsigned lineno,colno;
	char *s,*tag;
#endif /* HAVE_STDC */
{
	int icol;
	extern unsigned prev_stmt_line_num; /* shared with advance.c */

	errmsg_col=1;		/* Keep track of line length */

			/* Print the character ^ under the column number.
			   But if colno == 0, error occurred in prior line.
			   If colno is NO_COL_NUM, then print message
			   without any column number given.
			 */

	if(lineno != NO_LINE_NUM) {
	    if(colno == NO_COL_NUM) {
		    /* colno == NO_COL_NUM means don't give column number.*/
		(void)flush_line_out(lineno);/* print line if not printed yet */
	    }
	    else if(colno != 0) {
			/* print line if not printed yet */
		if( flush_line_out(lineno) ) {
				/* If it was printed, put ^ under the col */
		    (void)fprintf(list_fd,"\n%8s","");

		    for(icol=1; icol<colno; icol++)
			(void)fprintf(list_fd," ");
		    (void)fprintf(list_fd,"^");
		}
	    }
	    else {		/* colno == 0 */
			/* print line if not printed yet */
		(void)flush_line_out(prev_stmt_line_num);
	    }
	}

	(void)fprintf(list_fd,"\n\"%s\"",current_filename);
	errmsg_col += 2+strlen(current_filename);

	if(lineno != NO_LINE_NUM) { /* nonlocal error-- don't flush */
	    if(colno == NO_COL_NUM) {
		(void)fprintf(list_fd,
		   ", near line %u",lineno);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	    else if(colno != 0) {
		(void)fprintf(list_fd,
		   ", line %u col %u",lineno,colno);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	    else {		/* colno == 0 */
		(void)fprintf(list_fd,
		   ", near line %u",prev_stmt_line_num);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	}

	(void)fprintf(list_fd,": %s:",tag); /* "Warning", "Error", etc. */
	errmsg_col += 3+strlen(tag);

	msg_tail(s); /* now append the message string */
}

				/* Our own style messages */
PRIVATE void
#if HAVE_STDC
oldstyle_error_message(unsigned int lineno, unsigned int colno, char *s, char *tag)
#else /* K&R style */
oldstyle_error_message(lineno,colno,s,tag)
	unsigned lineno,colno;
	char *s,*tag;
#endif /* HAVE_STDC */
{
	int icol;
	extern unsigned prev_stmt_line_num; /* shared with advance.c */

	errmsg_col=1;		/* Keep track of line length */

			/* Print the character ^ under the column number.
			   But if colno == 0, error occurred in prior line.
			   If colno is NO_COL_NUM, then print message
			   without any column number given.
			 */

	if(lineno == NO_LINE_NUM) { /* nonlocal error-- don't flush */
	  (void)fprintf(list_fd,"\n%s",tag);
	  errmsg_col += strlen(tag);
	}
	else {
	    if(colno == NO_COL_NUM) {
		    /* colno == NO_COL_NUM means don't give column number.*/
		(void)flush_line_out(lineno);/* print line if not printed yet */
		(void)fprintf(list_fd,
		   "\n%s near line %u",tag,lineno);
		errmsg_col += 11+NUM_DIGITS(lineno)+(unsigned)strlen(tag);
	    }
	    else if(colno != 0) {
			/* print line if not printed yet */
		if( flush_line_out(lineno) ) {
				/* If it was printed, put ^ under the col */
		    (void)fprintf(list_fd,"\n%8s","");

		    for(icol=1; icol<colno; icol++)
			(void)fprintf(list_fd," ");
		    (void)fprintf(list_fd,"^");
		}
		(void)fprintf(list_fd,
		   "\n%s near line %u col %u",tag,lineno,colno);
		errmsg_col += 16+NUM_DIGITS(lineno)+NUM_DIGITS(colno)
		  +(unsigned)strlen(tag);
	    }
	    else {		/* colno == 0 */
			/* print line if not printed yet */
		(void)flush_line_out(prev_stmt_line_num);
		(void)fprintf(list_fd,
		   "\n%s near line %u",tag,prev_stmt_line_num);
		errmsg_col += 11+NUM_DIGITS(lineno)+(unsigned)strlen(tag);
	    }
	}

	if(!full_output		/* If not listing, append file name */
	   || incdepth > 0){	/* Append include-file name if we are in one */
	  if(lineno == NO_LINE_NUM) { /* if no line no, preposition needed */
	    (void)fprintf(list_fd," in");
	    errmsg_col += 3;
	  }
	  (void)fprintf(list_fd," file %s",current_filename);
	  errmsg_col += 6+(unsigned)strlen(current_filename);
	}

	(void)fprintf(list_fd,":");
	errmsg_col++;

	msg_tail(s); /* now append the message string */
}

		/* msg_tail appends string s to current error message.
		   It prints one word at a time, starting a new line
		   when the message gets to be too long for one line.
		 */
void
#if HAVE_STDC
msg_tail(char *s)
#else /* K&R style */
msg_tail(s)
    char *s;
#endif /* HAVE_STDC */
{
	int wordstart,wordend,leading_skip,wordchars;

	(void)fprintf(list_fd," ");
	errmsg_col++;
	wordstart=0;
		/* Each iteration of loop prints leading space and the
		   nonspace characters of a word.  Loop invariant: wordstart
		   is index of leading space at start of word, wordend is
		   index of space char following word. */
	while(s[wordstart] != '\0') {
	  leading_skip = TRUE;
	  for(wordend=wordstart; s[wordend] != '\0'; wordend++) {
	    if(leading_skip) {	/* If skipping leading space chars */
	      if(!isspace(s[wordend]))
		leading_skip = FALSE; /* go out of skip mode at nonspace */
	    }
	    else {		/* If scanning word chars */
	      if(isspace(s[wordend]))
		break;		/* quit loop when space char found */
	    }
	  }
	  wordchars = wordend-wordstart;
				/* If word doesn't fit, wrap to next line */
	  if( wrap_column > 0 && (errmsg_col += wordchars) > wrap_column) {
	    (void)fprintf(list_fd,"\n");
	    errmsg_col = wordchars;
	  }
				/* Print the word */
	  while(wordstart < wordend) {
	    (void)putc(s[wordstart++],list_fd);
	  }
	}
}


void
#if HAVE_STDC
oops_message(int severity, unsigned int lineno, unsigned int colno, char *s)
#else /* K&R style */
oops_message(severity,lineno,colno,s)
	int severity;
	unsigned lineno,colno;
	char *s;
#endif /* HAVE_STDC */
{
	(void)fflush(list_fd);
	(void)fprintf(stderr,"\nOops");
	if(lineno != NO_LINE_NUM) {
	  (void)fprintf(stderr," at line %u",lineno);
	  if(colno != NO_COL_NUM)
	    (void)fprintf(stderr," at col %u",colno);
	}
	(void)fprintf(stderr," in file %s",current_filename);
	(void)fprintf(stderr," -- %s",s);
	if(severity == OOPS_FATAL) {
	  (void)fprintf(stderr,"\nFtnchek aborted\n");
	  (void) exit(1);
	}
}

void
#if HAVE_STDC
oops_tail(char *s)
#else /* K&R style */
oops_tail(s)
	char *s;
#endif /* HAVE_STDC */
{
	(void)fprintf(stderr," %s",s);
}

/*	get_env_options picks up any options defined in the
	environment.  A switch or setting is defined according to
	the value of an environment variable whose name is the switch
	or setting name (uppercased), prefixed by the string
	ENV_PREFIX (e.g.  FTNCHEK_).  For settings and strsettings,
	the value of the environment variable gives the value to be
	used.  For switches, the environment variable is set to "0" or
	"NO" to turn the switch off, or to any other value (including
	null) to turn it on.
*/

PRIVATE void
get_env_options(VOID)
{
	char env_option_name[32];
	char *value;
	int i;
	for(i=0; i<NUM_SWITCHES; i++) {
			/* Construct the env variable name for switch i */
	    make_env_name( env_option_name, switchopt[i].name);

			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {
		*(switchopt[i].switchflag) =
			!(strcmp(value,"0")==0 || strcmp(value,"NO")==0 );
	    }

	}

	for(i=0; i<NUM_SETTINGS; i++) {
			/* Construct the env variable name for setting i */
	    make_env_name( env_option_name, setting[i].name);
			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {
		if(read_setting(value, setting[i].setvalue, setting[i].name,
				setting[i].minlimit, setting[i].maxlimit,
				setting[i].turnon,
				setting[i].turnoff,
				setting[i].min_default_value,
				setting[i].max_default_value) != 0) {
		  (void)fflush(list_fd);
		  (void)fprintf(stderr,"Env setting garbled: %s=%s: ignored\n",
				env_option_name,value);
		}
	    }
	}


	for(i=0; i<NUM_STRSETTINGS; i++) {
			/* Construct the env variable name for setting i */
	    make_env_name( env_option_name, strsetting[i].name);
			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {

				/* setenv nothing or "1" or "YES" --> turnon*/
	      if(value[0] == '\0'
		 || cistrncmp(value,"1",strlen(value)) == 0
		 || cistrncmp(value,"yes",strlen(value)) == 0
		 ) {
		*(strsetting[i].strvalue) = strsetting[i].turnon;
	      }
	      else if(cistrncmp(value,"no",strlen(value)) == 0) {
		*(strsetting[i].strvalue) = strsetting[i].turnoff;
	      }
	      else {		/* Otherwise use the given value */
	        *(strsetting[i].strvalue) = value;
	      }

	      if( *(strsetting[i].strvalue) == (char *)NULL ) {
		(void)fflush(list_fd);
		(void)fprintf(stderr,
			 "Environment variable %s needs string value: ignored\n",
			 env_option_name);
	      }
	      else {
		update_str_options(&strsetting[i]);
	      }
	    }
	}
}

		/* Routine to concatenate ENV_PREFIX onto option name
		   and uppercase the result.
		*/
PRIVATE void
#if HAVE_STDC
make_env_name(char *env_name, char *option_name)
#else /* K&R style */
make_env_name( env_name, option_name)
	char *env_name, *option_name;
#endif /* HAVE_STDC */
{
    int i,c;

    (void)strcat(strcpy(env_name,ENV_PREFIX),option_name);
    for(i=sizeof(ENV_PREFIX)-1; (c=env_name[i]) != '\0'; i++) {
	if( islower(c) )
	    env_name[i] = toupper(c);
    }
}

		/* get_rc_options picks up options from an "rc" file.
		 */
PRIVATE void
get_rc_options(VOID)
{
  FILE *rc_fp;
  char rc_option_string[MAX_RC_LINE];
  int i;

  rc_option_string[0] = '-';

  if( (rc_fp = find_rc()) != (FILE *)NULL ) {
    for(;;) {
      if( fgets(rc_option_string+1,sizeof(rc_option_string)-1,rc_fp)
	 == (char *)NULL)
	break;
				/* Terminate line at start of comment.
				   This also changes final \n to \0. */
      for(i=1; rc_option_string[i] != '\0'; i++) {
	if(rc_option_string[i] == RC_COMMENT_CHAR ||
	   isspace(rc_option_string[i])) {
	  rc_option_string[i] = '\0';
	  break;
	}
      }
      if(i==1)			/* Skip blank line */
	continue;

      set_option(rc_option_string,"startup file");
    }
  }
}

		/* find_rc locates the "rc" file. */
PRIVATE FILE *
find_rc(VOID)
{
  FILE *fp;
  char fname[100];
  char *homedir=getenv("HOME");
			/* Look first for file in local directory */
  (void)strcpy(fname,UNIX_RC_FILE);
  if( (fp=fopen(fname,"r")) != (FILE *)NULL)
    return fp;

			/* Look for alternate name in local directory */
  (void)strcpy(fname,NONUNIX_RC_FILE);
  if( (fp=fopen(fname,"r")) != (FILE *)NULL)
    return fp;

			/* Allow local option of special home directory
			   for non-unix (usually VMS) systems. */
#ifdef SPECIAL_HOMEDIR
  if(homedir == (char *)NULL) {
    homedir = SPECIAL_HOMEDIR;
  }
#endif
			/* If not found, look in home directory */
  if(homedir != (char *)NULL) {
    (void)strcpy(fname,homedir);
#ifdef UNIX
    (void)strcat(fname,"/");
#endif
    (void)strcat(fname,UNIX_RC_FILE);
    if( (fp=fopen(fname,"r")) != (FILE *)NULL)
      return fp;

			/* If look for alternate name in home directory */
    (void)strcpy(fname,homedir);
#ifdef UNIX
    (void)strcat(fname,"/");
#endif
    (void)strcat(fname,NONUNIX_RC_FILE);
    if( (fp=fopen(fname,"r")) != (FILE *)NULL)
      return fp;
  }

  return (FILE *)NULL;		/* Not found: return NULL */
}


	/* set_option processes an option from command line.  Argument s is
	   the option string. First s is compared against boolean switches
	   from list in switchopt[].  If s matches switch string,
	   corresponding flag is set to TRUE.  If no match, then s is compared
	   to the same switches prefixed by "no", and if match is found, then
	   flag is set to FALSE.  Finally, special flags are handled.  If still
	   no match, an error message is generated.
	 */

PRIVATE void
#if HAVE_STDC
set_option(char *s, char *where)
	        		/* Option to interpret, including initial - */
	            		/* String to identify cmd line vs rc file */
#else /* K&R style */
set_option(s,where)
	char *s,		/* Option to interpret, including initial - */
	     *where;		/* String to identify cmd line vs rc file */
#endif /* HAVE_STDC */
{
	int i;
		/* look for noswitch flags first since otherwise
		   an option starting with no might take precedence */
	if(strncmp(s+1,"no",2) == 0) {
	    for(i=0; i<NUM_SWITCHES; i++) {
		if( strncmp(s+3,switchopt[i].name,OPT_MATCH_LEN) == 0) {
		    *(switchopt[i].switchflag) = FALSE;
		    return;
		}
	    }
	}

		/* -noswitch not found: look for nosetting flag */
	if(strncmp(s+1,"no",2) == 0) {
	    for(i=0; i<NUM_SETTINGS; i++) {
		if( strncmp(s+3,setting[i].name,OPT_MATCH_LEN) == 0) {
		    *(setting[i].setvalue) = setting[i].turnoff;
		    return;
		}
	    }
	}

				/* Next look for switches */
	for(i=0; i<NUM_SWITCHES; i++) {
	    if( strncmp(s+1,switchopt[i].name,OPT_MATCH_LEN) == 0) {
		*(switchopt[i].switchflag) = TRUE;
		return;
	    }
	}

		/* Handle settings of form "-opt=number" */
	for(i=0; i<NUM_SETTINGS; i++)
	    if( strncmp(s+1,setting[i].name,OPT_MATCH_LEN) == 0) {
		char *numstr;

		numstr = s + OPT_MATCH_LEN;
		while(++numstr, ! END_OF_OPT(*numstr) )
		{
		    if((*numstr == '=') || (*numstr == ':'))
		    {			/* Find the assignment operator */
			numstr++;
			break;
		    }
		}
		if(read_setting(numstr, setting[i].setvalue, setting[i].name,
				setting[i].minlimit, setting[i].maxlimit,
				setting[i].turnoff,
				setting[i].turnon,
				setting[i].min_default_value,
				setting[i].max_default_value) != 0) {
		  (void)fflush(list_fd);
		  (void)fprintf(stderr,"Setting garbled: %s: ignored\n",s);
		}
		return;
	    }


		/* Handle settings of form "-opt=string" */
	for(i=0; i<NUM_STRSETTINGS; i++) {
	    int is_a_turnoff=FALSE;

				/* First look for setting prefixed by "no"
				   if it allows turnon/turnoff. */
	    if( strsetting[i].turnoff != (char *)NULL &&
	       strncmp(s+1,"no",2) == 0 &&
	       strncmp(s+3,strsetting[i].name,OPT_MATCH_LEN) == 0) {
	      is_a_turnoff=TRUE;
	    }

	    if(is_a_turnoff ||
	       strncmp(s+1,strsetting[i].name,OPT_MATCH_LEN) == 0) {
		char *strstart;
		int numchars;

		strstart = s + (OPT_MATCH_LEN + 1);
		while( *strstart != '=' && *strstart != ':'
		      && ! END_OF_OPT(*strstart) )
			strstart++;	/* Find the = sign */
		if( END_OF_OPT(*strstart) ) {
				/* no = sign: use turnon/turnoff */
		  if(is_a_turnoff)
		    *(strsetting[i].strvalue) = strsetting[i].turnoff;
		  else
		    *(strsetting[i].strvalue) = strsetting[i].turnon;
		}
		else {		/* = sign found: use it but forbid -no form */
		    if(is_a_turnoff) {
		      (void)fflush(list_fd);
		      (void)fprintf(stderr,
			      "No string setting allowed for %s: ignored\n",s);
		      return;
		    }
		    ++strstart;	/* skip past the "=" */
				/* In VMS,MSDOS worlds, user might not leave
				   blank space between options.  If string
				   is followed by '/', must make a properly
				   terminated copy.  In any case, make a
				   copy in case this option comes from
				   the rc file. */
		    for(numchars=0;!END_OF_OPT(strstart[numchars]);numchars++)
		      continue;

		    *(strsetting[i].strvalue) = (char *)malloc(numchars+1);
		    (void)strncpy( *(strsetting[i].strvalue),
			       strstart,numchars);
		    (*(strsetting[i].strvalue))[numchars] = '\0';
		}

			/* Handle actions needed after new strsetting
			   is read. If it was a turn-on where turnon is
			   NULL, give a warning. */
		if( *(strsetting[i].strvalue) == (char *)NULL ) {
		  (void)fflush(list_fd);
		  (void)fprintf(stderr,
				"String setting missing: %s: ignored\n",s);
		}
		else {
		  update_str_options(&strsetting[i]);
		}

		return;
	    }

	}
		/* No match found: issue error message */

	(void)fflush(list_fd);
	(void)fprintf(stderr,"\nUnknown %s switch: %s\n",where,s);
}


	/* Routine to read integer setting from string s and check if valid */

PRIVATE int
#if HAVE_STDC
read_setting(char *s, int *setvalue, char *name, int minlimit, int maxlimit, int turnoff, int turnon, int min_default_value, int max_default_value)
#else /* K&R style */
read_setting(s, setvalue, name, minlimit, maxlimit, turnoff, turnon,
	     min_default_value,
	     max_default_value)
	char *s;
	int *setvalue;
	char *name;
	int minlimit, maxlimit,
	     turnon, turnoff,
	     min_default_value, max_default_value;
#endif /* HAVE_STDC */
{
	int given_val;

	if(strcmp(s,"NO")==0) {	/* -setting=no */
	  *(setvalue) = turnoff;
	}
	else if(END_OF_OPT(*s)) { /* -setting */
	  *(setvalue) = turnon;
	}
	else if(sscanf(s,"%d", &given_val) == 0) {
	    return -1;	/* error return: garbled setting */
	}
	else {		/* If outside limits, set to default */
	    int Ok=TRUE;
	    if(given_val < minlimit) {
		given_val = min_default_value;
		Ok = FALSE;
	    }
	    else if(given_val > maxlimit) {
		given_val = max_default_value;
		Ok = FALSE;
	    }

	    if(! Ok ) {
	        (void)fflush(list_fd);
		(void)fprintf(stderr,"\nSetting: %s",name);
		(void)fprintf(stderr," outside limits %d to %d",
				minlimit,maxlimit);
		(void)fprintf(stderr,": set to default %d\n",given_val);
	    }

	    *(setvalue) = given_val;
	}
	return 0;
}

			/* Handle actions needed to update things after
			   getting a non-null strsetting option.
			 */
PRIVATE void
#if HAVE_STDC
update_str_options(StrsettingList *strset)
#else /* K&R style */
update_str_options(strset)
  StrsettingList *strset;
#endif /* HAVE_STDC */
{

			/* Handle necessary action for  -out=listfile */
  if(strset->strvalue == &out_fname)
    must_open_outfile = TRUE;

				/* Update include path */
#ifdef ALLOW_INCLUDE
  if(strset->strvalue == &include_path) {
    append_include_path(include_path);
  }
#endif

				/* Handle warnings like -f77=list */
  if(strset->option_list != (WarnOptionList *)NULL) {
    process_warn_string(*(strset->strvalue), strset->option_list);
  }
}

#define MAX_OPT_LEN 32		/* Big enough to hold any option name */

				/* Process list of warn options.  Return
				   TRUE if "help" requested, else FALSE */
PRIVATE void
#if HAVE_STDC
process_warn_string(char *warn_string, WarnOptionList *warn_option)
                     		/* Names of options to set */
                                            /* array where options defined */
           			/* size of warn_option array */
#else /* K&R style */
process_warn_string( warn_string, warn_option )
     char *warn_string;		/* Names of options to set */
     WarnOptionList warn_option[]; /* array where options defined */
#endif /* HAVE_STDC */
{
  int i,c;
  char opt_buf[MAX_OPT_LEN+1];

  if(strcmp(warn_string,"help") == 0) { /* Print warning help screen */
    list_warn_options(warn_option);
    return;
  }
  else {
				/* Loop on warn options in string */
    while(!END_OF_OPT(*warn_string)) {
				/* Copy next warn option into buffer */
      for(i=0; !END_OF_OPT(*warn_string); ) {
	c = *warn_string++;
	if(c == ',' || c == ':') /* quit when reach next warn option */
	  break;
	if(i<MAX_OPT_LEN)
	  opt_buf[i++] = c;
      }
      opt_buf[i] = '\0';

      set_warn_option(opt_buf, warn_option );
    }
  }
  return;
}

			/* Routine to print list of warning options */
PRIVATE void
#if HAVE_STDC
list_warn_options(WarnOptionList *warn_option)
#else /* K&R style */
list_warn_options(warn_option)
     WarnOptionList warn_option[]; /* array of defns */
#endif /* HAVE_STDC */
{
  int i;

  ++actioncount;	/* Treat as an action so if no files, quit */

  (void)fprintf(list_fd,"\n%s Warning Options:",warn_option[0].explanation);
  for(i=1; warn_option[i].name != (char *)NULL; i++) {
    (void)fprintf(list_fd,"\n  %s [%s]: %s",
	    warn_option[i].name,
	    *(warn_option[i].flag)? "yes" : "no",
	    warn_option[i].explanation);
  }
  (void)fprintf(list_fd,"\nPrefix option with no- to turn off warning");
  (void)fprintf(list_fd,"\nSpecial keywords:");
  (void)fprintf(list_fd,"\n  %s: %s","help","Print this list");
  (void)fprintf(list_fd,"\n  %s: %s","all","Set all options");
  (void)fprintf(list_fd,"\n  %s: %s","none","Clear all options");
  (void)fprintf(list_fd,"\n");
}

			/* Routine to set warning options to given values */
PRIVATE void
#if HAVE_STDC
set_warn_option(char *s, WarnOptionList *warn_option)
#else /* K&R style */
set_warn_option(s, warn_option )
     char *s;
     WarnOptionList warn_option[];
#endif /* HAVE_STDC */
{
  int i, matchlen, offset;
  int value;

			/* Special keyword "all": set all options on */
  if(strcmp(s,"all") == 0) {
	for(i=1; warn_option[i].name != (char *)NULL; i++)
	  *(warn_option[i].flag) = TRUE;
	return;
  }
			/* Special keyword "none": set all options off */
  else if(strcmp(s,"none") == 0 ) {
	for(i=1; warn_option[i].name != (char *)NULL; i++)
	  *(warn_option[i].flag) = FALSE;
	return;
  }
  else {
				/* Look for "no-" prefix on option name */
    if(strncmp(s,"no-",strlen("no-")) == 0) {
      offset = strlen("no-");
      value = FALSE;
    }
    else {
      offset = 0;
      value = TRUE;
    }
				/* Go thru list to find a match at minimum
				   nonambiguous length. */
    for(i=1,matchlen=1; warn_option[i].name != (char *)NULL; i++) {
			/* Look for a match at current matchlen, then 
			  if found see if unique.  List must have names
			  with matching prefixes adjacent. */
      while(strncmp(s+offset,warn_option[i].name,matchlen) == 0) {
	if(warn_option[i+1].name == (char *)NULL ||
	   strncmp(s+offset,warn_option[i+1].name,matchlen) != 0) {
	  *(warn_option[i].flag) = value;
	  return;
	}
	else {
	  if(   s[offset+matchlen] == '\0'
	     || warn_option[i].name[matchlen] == '\0') {
	    (void)fflush(list_fd);
	    (void)fprintf(stderr,
		   "\nAmbiguous warning option: %s: ignored\n",s);
	    return;
	  }
	  ++matchlen;
	}
      }
    }
  }
  (void)fflush(list_fd);
  (void)fprintf(stderr,"\nUnknown warning option: %s: ignored\n",s);
  return;
}

	/* Routine to turn off all switches and numeric settings except
	   -word and -wrap.  The effect is as if -no had been given
	   for each switch and setting.  Useful when other features
	   like calltree are being used and checking is not needed.
	*/
PRIVATE void turn_off_checks(VOID)
{
	int save_wordsize=given_wordsize,
	    save_wrapcol=wrap_column,
	    save_source_format=source_format,
	    save_quiet=quiet;
	int i;

				/* Put all switches to FALSE */
	for(i=0; i<NUM_SWITCHES; i++) {
	  *(switchopt[i].switchflag) = FALSE;
	}

				/* Put all settings to turnoff value */
	for(i=0; i<NUM_SETTINGS; i++) {
	  *(setting[i].setvalue) = setting[i].turnoff;
	}

				/* Turn off warn lists */
	for(i=0; i<NUM_STRSETTINGS; i++) {
	  if( strsetting[i].option_list != (WarnOptionList *)NULL ) {
	    set_warn_option( strsetting[i].turnoff,
			      strsetting[i].option_list);
				/* Set strvalue so -help reports correctly */
	    *(strsetting[i].strvalue) = strsetting[i].turnoff;
	  }
	}

/* Restore the ones that aren't for checks */
	quiet=save_quiet;
	source_format=save_source_format;
	given_wordsize = save_wordsize;
	wrap_column = save_wrapcol;
}


PRIVATE void
#if HAVE_STDC
open_outfile(char *s)		/* open the output file for listing */
#else /* K&R style */
open_outfile(s)		/* open the output file for listing */
	char *s;
#endif /* HAVE_STDC */
{
	char *fullname;		/* given name plus extension */
	FILE *fd;

	must_open_outfile = FALSE;	/* Turn off the flag */

	if(s == (char *) NULL || *s == '\0') {
		return;		/* No filename: no action  */
	}

	fullname = add_ext(s,DEF_LIST_EXTENSION);
	(void)fflush(list_fd);
	if( (fd = fopen(fullname,"w")) == (FILE *)NULL) {
		(void)fprintf(stderr,"\nCannot open %s for output\n",fullname);
	}
	else {
		(void)fprintf(stderr,"\nOutput sent to file %s\n",fullname);
		list_fd = fd;
	}
}


PRIVATE void
#if HAVE_STDC
list_options(FILE *fd)/* List all commandline options, strsettings, and settings */
#else /* K&R style */
list_options(fd)/* List all commandline options, strsettings, and settings */
     FILE *fd;
#endif /* HAVE_STDC */
{
	int i;

			/* Print the copyright notice */
	(void)fprintf(fd,"\n%s",COPYRIGHT_DATE);
	(void)fprintf(fd,"\n%s\n",COPYRIGHT_NOTICE);

		/* Note: Headings say "default" but to be accurate they
		   should say "current value".  This would be confusing. */
	(void)fprintf(fd,"\nCommandline options [default]:");
	for(i=0; i<NUM_SWITCHES; i++) {

	  if( !debug_latest &&
	     strncmp(switchopt[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug switches unless debug mode */

	  (void)fprintf(fd,"\n    %c[no]%s",OPT_PREFIX,switchopt[i].name);
	  (void)fprintf(fd," [%s]",*(switchopt[i].switchflag)? "yes": "no");
	  (void)fprintf(fd,": %s",switchopt[i].explanation);
	}
		/* String settings follow switches w/o their own heading */
	for(i=0; i<NUM_STRSETTINGS; i++) {
	  if( !debug_latest &&
	     strncmp(strsetting[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug settings unless debug mode */

	  (void)fprintf(fd,"\n    %c%s=str ",OPT_PREFIX,strsetting[i].name);
			/* If strvalue has been given, list it.  Otherwise,
			   if this has an optionlist, the default value is
			   given as 'name' of option 0, which is the title
			   entry of the list.
			*/
	  (void)fprintf(fd,"[%s]",
		*(strsetting[i].strvalue)?
			*(strsetting[i].strvalue):
			strsetting[i].option_list != (WarnOptionList *)NULL?
			   strsetting[i].option_list[0].name:
			   "NONE");
	  (void)fprintf(fd,": %s",strsetting[i].explanation);
	  if( strsetting[i].option_list != (WarnOptionList *)NULL )
	    (void)fprintf(fd,"\n        Use %c%s=help for list of options",
#ifdef OPT_PREFIX_SLASH
			  '/',
#else
			  '-',
#endif
			  strsetting[i].name);
	}

	(void)fprintf(fd,"\nSettings (legal range) [default]:");
	for(i=0; i<NUM_SETTINGS; i++) {

	  if( !debug_latest &&
	     strncmp(setting[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug settings unless debug mode */

	  (void)fprintf(fd,"\n    %c%s=dd ",OPT_PREFIX,setting[i].name);
	  (void)fprintf(fd,"(%d to %d) ",setting[i].minlimit,
		  setting[i].maxlimit);
	  (void)fprintf(fd,"[%d]",*(setting[i].setvalue));
	  (void)fprintf(fd,": %s",setting[i].explanation);
	}

    (void)fprintf(fd,
	"\n(First %d chars of option name significant)\n",OPT_MATCH_LEN);
}


PRIVATE void
wrapup(VOID)	/* look at cross references, etc. */
{

	if(debug_hashtab || debug_glob_symtab)
	  debug_symtabs();

				/* Interpret -[no]ext option as synonym for
				   turning on[off] bit 1 in ext_usage_check.
				 */
	if(ext_def_check) {
	  if(nocheck_given)	/* -nocheck followed by -ext */
	    ext_usage_check |= 0x1;
	}
	else {
	  ext_usage_check &= 0x2; /* -noext given */
	}

				/* Sort out calltree options.  Don't forget
				   that reflist and vcg can be set by their
				   own convenience flags, if -call not given.
				*/
	if(call_tree_options > 0) {
	  print_call_tree = FALSE;
	  print_ref_list = FALSE;
#ifdef VCG_SUPPORT
	  print_vcg_list = FALSE;
#endif
	  switch( call_tree_options & 0x0003 ) { /* Low-order two bits => format */
	  case CALLTREE_REFLIST:
	    print_ref_list = TRUE;
	    break;
#ifdef VCG_SUPPORT
	  case CALLTREE_VCG:
	    print_vcg_list = TRUE;
	    break;
#endif
	  default:
	    print_call_tree = TRUE;
	    break;
	  }
	}
				/* VCG output file uses stem of file
				   containing main prog or 1st file on
				   command line. If none, output is to stdout.
				 */
#ifdef VCG_SUPPORT
	if(print_vcg_list) {
	  vcg_fd = (input_fd == stdin || main_filename == (char *)NULL)?
	    stdout :
	    fopen(new_ext(main_filename,DEF_VCG_EXTENSION) ,"w");
	}
#endif

	visit_children();	/* Make call tree & check visited status */
	check_com_usage();	/* Look for unused common stuff */
	check_comlists();	/* Look for common block mismatches */
	check_arglists();	/* Look for subprog defn/call mismatches */

#ifdef DEBUG_GLOBAL_STRINGS
	if(debug_latest)
	  print_global_strings();
#endif
}


#define MODE_DEFAULT_EXT 1
#define MODE_REPLACE_EXT 2
PRIVATE char *
#if HAVE_STDC
append_extension(char *s, char *ext, int mode)
#else /* K&R style */
append_extension(s,ext,mode)
     char *s,*ext;
     int mode;
#endif /* HAVE_STDC */
{
		/* MODE_DEFAULT_EXT: Adds extension to file name s if
		   none is present, and returns a pointer to the
		   new name.  If extension was added, space is allocated
		   for the new name.  If not, simply  returns pointer
		   to original name.  MODE_REPLACE_EXT: same, except given
		   extension replaces given one if any.
		*/
	int i,len;
	char *newname;
#ifdef OPTION_PREFIX_SLASH	/* set len=chars to NUL or start of /opt */
	for(len=0; s[len] != '\0' && s[len] != '/'; len++)
	  continue;
#else
	len=(unsigned)strlen(s);
#endif
		/* Search backwards till find the dot, but do not
		   search past directory delimiter
		*/
	for(i=len-1; i>0; i--) {
	    if(s[i] == '.'
#ifdef UNIX
	       || s[i] == '/'
#endif
#ifdef VMS
	       || s[i] == ']' || s[i] == ':'
#endif
#ifdef MSDOS
	       || s[i] == '\\' || s[i] == ':'
#endif
	       )
		break;
	}

	if(mode == MODE_REPLACE_EXT) {
	  if(s[i] == '.')	/* declare length = up to the dot */
	    len = i;
	  newname = (char *) malloc( (unsigned)(len+(unsigned)strlen(ext)+1) );
	  (void)strncpy(newname,s,len);
	  (void)strcpy(newname+len,ext);
	}
	else {			/* MODE_DEFAULT_EXT */
#ifdef OPTION_PREFIX_SLASH
		/* create new string if new ext or trailing /option */
	  if(s[i] != '.' || s[len] != '\0') {
	    if(s[i] != '.') {	/* no extension given */
	      newname = (char *) malloc( (unsigned)(len+
						    (unsigned)strlen(ext)+1) );
	      (void)strncpy(newname,s,len);
	      (void)strcpy(newname+len,ext);
	    }
	    else {		/* extension given but /option follows */
	      newname = (char *) malloc( (unsigned)(len+1) );
	      (void)strncpy(newname,s,len);
	    }
	  }
#else
	  if(s[i] != '.') {
	    newname = (char *) malloc( (unsigned)(len+
						  (unsigned)strlen(ext)+1) );
	    (void)strcpy(newname,s);
	    (void)strcat(newname,ext);
	  }
#endif
	  else {
	    newname = s;	/* use as is */
	  }
	}

	return newname;
}

		/* Adds default extension to source file name, replacing
		   any that is present, and returns a pointer to the
		   new name.  Space is allocated for the new name.
		*/
char *
#if HAVE_STDC
add_ext(char *s, char *ext)			/* adds default filename extension to s */
#else /* K&R style */
add_ext(s,ext)			/* adds default filename extension to s */
	char *s,*ext;
#endif /* HAVE_STDC */
{
  return append_extension(s,ext,MODE_DEFAULT_EXT);
}

PRIVATE char *
#if HAVE_STDC
new_ext(char *s, char *ext)
#else /* K&R style */
new_ext(s,ext)
	char *s,*ext;
#endif /* HAVE_STDC */
{
  return append_extension(s,ext,MODE_REPLACE_EXT);
}


PRIVATE int
#if HAVE_STDC
cistrncmp(char *s1, char *s2, unsigned int n)			/* case-insensitive strncmp */
#else /* K&R style */
cistrncmp(s1,s2,n)			/* case-insensitive strncmp */
     char *s1,*s2;
     unsigned n;
#endif /* HAVE_STDC */
{
  while( n != 0 &&
      (isupper(*s1)?tolower(*s1):*s1) == (isupper(*s2)?tolower(*s2):*s2) ) {
    if(*s1 == '\0')
      return 0;
    if(*s2 == '\0')
      break;
    ++s1; ++s2; --n;
  }
  return n==0? 0: *s1 - *s2;
}

int
#if HAVE_STDC
has_extension(char *name, char *ext)		/* true if name ends in ext */
#else /* K&R style */
has_extension(name,ext)		/* true if name ends in ext */
  char *name,*ext;
#endif /* HAVE_STDC */
{
  unsigned name_len, ext_len;
  int stem_len;
  ext_len = strlen(ext);

#ifdef VMS	/* shell_glob adds version number: filename.ext;1 */
  if(strrchr(name,';') != (char *)NULL) {
    name_len = strrchr(name,';') - name; /* distance to the semicolon */
  }
  else
#endif
    name_len=strlen(name);	/* distance to the null */

  stem_len = (unsigned)(name_len - ext_len); /* distance to the dot */

  if( stem_len >= 0 &&
     (name_len-stem_len) == ext_len &&
     cistrncmp(name+stem_len,ext,ext_len) == 0 )
    return TRUE;
  else
    return FALSE;
}

		/* Add an include directory path to list of paths */
#ifdef ALLOW_INCLUDE
PRIVATE void
#if HAVE_STDC
append_include_path(char *new_path)
#else /* K&R style */
append_include_path(new_path)
     char *new_path;
#endif /* HAVE_STDC */
{
  IncludePathNode *new_path_node, *p;
  if((new_path_node=(IncludePathNode *)malloc(sizeof(IncludePathNode)))
     ==(IncludePathNode *)NULL) {
    (void)fflush(list_fd);
    (void)fprintf(stderr,"\nmalloc error getting path list");
  }
  else {
    new_path_node->link = (IncludePathNode *)NULL;
    new_path_node->include_path = new_path;
				/* Append the new node at end of list */
    if((p=include_path_list) == (IncludePathNode *)NULL)
      include_path_list = new_path_node;
    else {
      while(p->link != (IncludePathNode *)NULL)
	p = p->link;
      p->link = new_path_node;
    }
  }
#ifdef DEBUG_INCLUDE_PATH	/* Print path as it grows */
  if(getenv("DEBUG")) {
    fprintf(list_fd,"\nINCLUDE path=");
    for(p=include_path_list; p != (IncludePathNode *)NULL; p=p->link) {
      fprintf(list_fd,"%s ",p->include_path);
    }
    fprintf(list_fd,"\n");
  }
#endif
}
#endif/*ALLOW_INCLUDE*/

PRIVATE void
resource_summary(VOID)
{
#ifdef DEBUG_SIZES
  if(debug_latest)
    print_sizeofs();	/* give sizeof various things */
#endif

  (void)fprintf(list_fd,
   "\n     Here are the amounts of ftnchek's resources that were used:\n");

  (void)fprintf(list_fd,
   "\nSource lines processed = %lu statement + %lu comment = %lu total",
		tot_stmt_line_count,
		tot_line_count-tot_stmt_line_count, /*tot_comment_line_count*/
		tot_line_count);

  (void)fprintf(list_fd,
   "\nTotal executable statements = %lu, max in any module = %lu",
		tot_exec_stmt_count,
		max_exec_stmt_count);

  (void)fprintf(list_fd,
   "\nTotal number of modules in program = %lu",
		tot_module_count);

  (void)fprintf(list_fd,
   "\nMax identifier name chars used = %lu local, %lu global, chunk size %lu",
			max_loc_strings,
			glob_strings_used,
			(unsigned long)STRSPACESZ);
  (void)fprintf(list_fd,
    "\nMax token text chars used = %lu, chunk size %lu ",
			max_srctextspace,
			(unsigned long)STRSPACESZ);
  (void)fprintf(list_fd,
    "\nMax local symbols used =  %lu out of %lu available",
			max_loc_symtab,
			(unsigned long)LOCSYMTABSZ);
  (void)fprintf(list_fd,
    "\nMax global symbols used = %lu out of %lu available",
			max_glob_symtab,
			(unsigned long)GLOBSYMTABSZ);
  (void)fprintf(list_fd,
    "\nMax number of parameter info fields used = %lu, chunk size = %lu",
			max_paraminfo,
			(unsigned long)PARAMINFOSPACESZ);
  (void)fprintf(list_fd,
    "\nMax number of tokenlists used = %lu, chunk size = %lu",
			max_tokenlists,
			(unsigned long)TOKHEADSPACESZ);
  (void)fprintf(list_fd,
    "\nMax token list/tree space used = %lu, chunk size = %lu",
			max_token_space,
			(unsigned long)TOKENSPACESZ);
  (void)fprintf(list_fd,
    "\nNumber of subprogram invocations = %lu totaling %lu args",
			arglist_head_used,
			arglist_element_used);
  (void)fprintf(list_fd,
    "\nArgument list header and element chunk sizes = %lu and %lu",
			(unsigned long)ARGLISTHEADSZ,
			(unsigned long)ARGLISTELTSZ);
  (void)fprintf(list_fd,
    "\nNumber of common block decls = %lu totaling %lu variables",
			comlist_head_used,
			comlist_element_used);
  (void)fprintf(list_fd,
    "\nCommon list header and element chunk sizes = %lu and %lu",
			(unsigned long)COMLISTHEADSZ,
			(unsigned long)COMLISTELTSZ);
  (void)fprintf(list_fd,
    "\nNumber of array dim ptrs used = %lu, chunk size = %lu",
			max_ptrspace,
			(unsigned long)PTRSPACESZ);

#ifdef DEBUG_SIZES
  (void)fprintf(list_fd,
    "\nIdentifier hashtable size = %6lu",
			(unsigned long)HASHSZ);
#ifdef KEY_HASH/* not used any more*/
  (void)fprintf(list_fd,
    "\nKeyword hashtable size = %6lu",
			(unsigned long)KEYHASHSZ);
#endif
#ifdef COUNT_REHASHES
  (void)fprintf(list_fd,
    "\nIdentifier rehash count = %6lu",
			rehash_count);
#endif
  (void)fprintf(list_fd,
    "\nIntrinsic function hashtable size=%6lu, clash count=%lu",
			(unsigned long)INTRINS_HASHSZ,
			intrins_clashes);
#endif /*DEBUG_SIZES*/

  (void)fprintf(list_fd,"\n\n");
}
