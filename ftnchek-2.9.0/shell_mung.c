/******************************************************************/

#ifdef VAXC			/* v1.2-004 */
#module SHELL_MUNG "Jym V1.2-005"       /* Only VAX C understands this. */
#else				/* v1.2-004 */
#define	TRUE		1	/* GNU C stdio.h doesn't define TRUE and FALSE*/
#define	FALSE		0
#endif

/* SHELL_MUNG.C
**=============================================================================
** Copyright (C) 1989 Jym Dyer (jym@wheaties.ai.mit.edu)
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 1, or (at your option)
** any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**-----------------------------------------------------------------------------
** Version:     V1.2-005
**-----------------------------------------------------------------------------
** Facility:    GNU
**-----------------------------------------------------------------------------
** Prefix:      SHELL_
**-----------------------------------------------------------------------------
** Abstract
** ~~~~~~~~
**  If you think "shell mung" is a step in the preparation of a good mung dahl,
** you're right, but in this context "mung" is the verb.  ("Mung" historically
** means "mung until no good," but one could easily interpret it to mean "mung
** until nearly gnu".  Or something to that effect.)
**  This file provides the shell_glob() and shell_mung() functions for the
** VMS/DCL/VAX C user.  The shell_glob() function "globs" (parses filespecs
** with wildcards and matches them with existing files).  The shell_mung()
** function provides GNU-shell-like IO redirection and globbing:  it changes
** `stderr', `stdin', and/or `stdout' to do the IO redirection; it mungs a
** VAX C program's main() function's `argc' and `argv' variables (using shell_
** glob()) to do the globbing; and it truncates `argv[0]' down to a filename
** and filetype or, if the file is a .EXE file, to just the filename.
**  This makes it easy to port many GNU utilities to VMS, as these utilities
** often expect the shell to have done the work that shell_mung() does.  It can
** also be used to port Un*x utilities as well, if anybody cares.
**-----------------------------------------------------------------------------
** Functions
** ~~~~~~~~~
** shell_glob()
** shell_mung()
** (static) truncate_argv_0()
**-----------------------------------------------------------------------------
** Environment
** ~~~~~~~~~~~
**  Must be linked with the VAX C RTL.  Intended for use with the DCL CLI on
** VMS; it would be pointless to use shell_mung() with the SHELL CLI, though
** shell_glob() might possibly come in handy.
**  Must be linked with xmalloc() and xrealloc() functions, which are readily
** available from GNU source code.  These are simply malloc() and realloc()
** functions that exit if there is an error getting memory.
**-----------------------------------------------------------------------------
** Author:      Jym Dyer (jym@wheaties.ai.mit) 8-Apr-1988
**-----------------------------------------------------------------------------
** Modifications
** ~~~~~~~~~~~~~
** 1.0-001 - Original version.  Inspired by the DECUS C getredirection()
**           function written by Martin Minow, Jerry Leichter, and Jym Dyer.
**           {Jym 8-Apr-1988}
** 1.1-002 - Added `shell_default' and use of same by shell_glob(), which
**           allows default filespecs to be used in globbing.
**           {Jym 9-Jan-1989}
** 1.2-003 - Made shell_glob() accept unglobbable filespecs.
**         - In addition to '-', '+' is now accepted as the start of an option.
**           {Jym 3-Jun-1989}
** 1.2-004 - Made changes to allow compilation by GNU C.
**	     {Hunter Goatley, goathunter@WKUVX1.BITNET, 12-SEP-1991}
** 1.2-005 - Added Andrew Greer's patch to fix problem parsing non-filespec
**	     arguments to options
**	     {Hunter Goatley, goathunter@WKUVX1.BITNET, 22-SEP-1991}
**	     {Andrew Greer, <Andrew.Greer@vuw.ac.nz>}
**=============================================================================
*/

/* -=- FILE INCLUSIONS -=- */

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <fab.h>
#include <nam.h>
#include <rmsdef.h>
#include <stsdef.h>

#ifndef VAXC				/* v1.2-004 */
#define _tolower(c)     ((c) >= 'A' && (c) <= 'Z' ? (c) | 0x20:(c))
#endif			/* Also changed tolower() calls to _tolower() */

/* -=- FORWARD DECLARATIONS -=- */

extern void *  xmalloc();
extern void *  xrealloc();

extern void  sys$exit();
extern int  sys$parse();
extern int  sys$search();

extern char **  shell_glob();
static char *  truncate_argv_0();

/* -=- GLOBAL VARIABLE DEFINITION -=- */

#ifdef VAXC				/* v1.2-004 */
globaldef char *  shell_default = NULL; /* Default filespec for shell_glob().*/
#else
char *  shell_default = NULL;		/* Default filespec for shell_glob().*/
#endif
                                        /* Initialized here to NULL, which
                                        ** means that no default filespec is
                                        ** used by shell_glob().
                                        */

/* -=- FUNCTIONS -=- */

/* shell_glob()
**-----------------------------------------------------------------------------
** Functional Description
** ~~~~~~~~~~~~~~~~~~~~~~
**  Given a filespec, this function parses that filespec out and searches for
** the file---or, if the filespec has wildcard characters in it, the files---
** associated with the filespec.  It returns a newly-allocated vector of file-
** specs that are also newly-allocated.  This function can be used to find only
** the first file that matches the filespec or all the files that match it.  In
** the latter case, this function will also provide a count of the files that
** were found.
**  Filespecs returned by this function are in lowercase, to approximate the
** "look and feel" of GNU.
**  This function is used by shell_mung(), and is also handy for general use.
**-----------------------------------------------------------------------------
** Usage
** ~~~~~
**  This function returns a vector---that is, a pointer to pointers.  Thus you
** will need to declare this function and a vector like this:
**
**      extern char **  shell_glob();
**
**      char **  filespec_v;
**
**  To find only the first match of a wildcarded filespec, or just to get the
** fully-parsed filespec of a given filespec, call shell_glob() with NULL as
** its second parameter.  For example, if you call it like this:
**
**      static int const  filespec = "*.c";
**      char **  filespec_v;
**              ...
**      filespec_v = shell_glob(filespec,NULL);
**
** The value in `filespec_v[0]' will be (for example) "dev:[dir]bar.c;".  Note
** that the character string pointer is the content of the vector, not the
** vector itself.
**  With this usage, `filespec_v' points to a pointer (actually an array of one
** pointer, which is the same thing).  When used to get a number of filespecs,
** it points to an array of pointers.
**  To get all the filespecs that match a filespec, use an address of an
** integer as shell_glob()'s second parameter, as in this example:
**
**      static int const  filespec = "*.c";
**      int  filespec_count;
**      char **  filespec_v;
**              ...
**      filespec_v = shell_glob(filespec,&filespec_count);
**
** The result of this would be (for example),
**
**      filespec_count = 3,
**      filespec_v[0] = "dev:[dir]bar.c",
**      filespec_v[1] = "dev:[dir]baz.c", and
**      filespec_v[2] = "dev:[dir]foo.c".
**
**  In either type of usage, both the vector and the character strings are in
** newly-allocated memory.
**  VMS globbing entails a feature that allows one to fill in parts of a file-
** spec with default values.  This feature can be used by shell_glob() using
** `shell_default', a global character string.  Use of this character string is
** optional, and it is initially set to NULL, meaning no default filespec is
** used.  To use it, you need to declare it like this:
**
**      globalref char *  shell_default;
**
**  The most frequent use of a default filespec in VMS is to provide a default
** filetype, as shown in this example:
**
**      static int const  filespec = "*.c";
**      int  filespec_count;
**      char **  filespec_v;
**              ...
**      shell_default = ".c";
**      filespec_v = shell_glob(filespec,&filespec_count);
**
** If `user_string' (which is, of course, a character string) is (for example)
** "foo*", a filespec without a filetype, shell_glob() will look for all files
** whose filespecs match "foo*.c".  If `user_string' were "foo*.h", a filespec
** that has a filetype, the default filespec ".c" is ignored and shell_glob()
** will look for all files whose filespecs match "foo*.h".
**  Default filespecs can include devices, directories, filenames, filetypes,
** and version numbers.  Wildcards can be used in filenames, filetypes, and
** version numbers.  To turn default filespecs off, just set `shell_default'
** back to NULL.
**  If the given filespec cannot be globbed, shell_glob() will simply return
** a filespec vector with an array of one pointer, pointing to the unglobbable
** given filespec.  The given filespec, therefore, should be something that
** will stick around for as long as you expect to use the filespec vector.  In
** particular, don't call shell_glob() from a function using an automatic
** character string variable of that function and expect the string to still be
** available when you've exited that function.
**-----------------------------------------------------------------------------
** Calling Sequence
** ~~~~~~~~~~~~~~~~
** filespec_v = shell_glob(given_filespec,filespec_count_p);
**-----------------------------------------------------------------------------
** Formal Arguments     Description
** ~~~~~~~~~~~~~~~~     ~~~~~~~~~~~
** given_filespec       Filespec "as given"---that is, as typed on the command
**                      line.  This is what we parse and search for.
** filespec_count_p     Pointer to an integer that is to receive a count of
**                      filespecs found that match `given_filespec'.  If NULL,
**                      indicates that we're only to search for the first file
**                      that matches `given_filespec'.
**-----------------------------------------------------------------------------
** Implicit Input       Description
** ~~~~~~~~~~~~~~       ~~~~~~~~~~~
** shell_default        This global variable is a character string of a default
**                      filespec.  If NULL, no default filespec is used.
**-----------------------------------------------------------------------------
** Implicit Outputs:    None
**-----------------------------------------------------------------------------
** Return Value
** ~~~~~~~~~~~~
**  Returns a vector of pointers to newly-parsed filespecs.
**-----------------------------------------------------------------------------
** Side Effects
** ~~~~~~~~~~~~
**  Memory is allocated for the filespec vector and for the filespecs that it
** points to.
**-----------------------------------------------------------------------------
*/
char **
shell_glob(given_filespec,filespec_count_p)
 char *  given_filespec;
 int *  filespec_count_p;
{
  /* --- LOCAL VARIABLES --- */

  register char *  character_p;         /* General-purpose character pointer,
                                        ** used to convert filespecs to
                                        ** lowercase.
                                        */
  register char  * expanded_filespec;	/* v1.2-004 */
                                        /* Buffer to hold the expanded filespec
                                        ** from a call to sys$parse().
                                        */
  register int  filespec_count = 0;     /* Count of filespecs that sys$search()
                                        ** finds.
                                        */
  register char **  filespec_v = NULL;  /* Vector of pointers to filespecs that
                                        ** sys$search() finds.  Allocated and
                                        ** reallocated as filespecs are found.
                                        */
  static char *our_path;                /* The default device and directory
                                        ** path.  Initialized the first time
                                        ** this function is called.
                                        */
  register char * resultant_filespec;	/* v1.2-004 */
                                        /* Buffer to hold a resultant filespec
                                        ** from a call to sys$search().
                                        */
  register int  status;                 /* Status codes returned from calls to
                                        ** sys$parse() and sys$search().
                                        */
  struct FAB  fab = cc$rms_fab;         /* RMS file access block.
                                        */
  struct NAM  nam = cc$rms_nam;         /* RMS name block.
                                        */

  /* --- INITIALIZE PATH --- */

  resultant_filespec = xmalloc(NAM$C_MAXRSS + 1);	/* v1.2-004 */
  expanded_filespec = xmalloc(NAM$C_MAXRSS + 1);	/* v1.2-004 */
  if (our_path == NULL)
   (char *) our_path = getenv("PATH");			/* v1.2-004 */
   /* our_path = getenv("PATH"); */

  /* --- INITIALIZE FAB AND NAM --- */

  if (shell_default != NULL)
  {
    fab.fab$l_dna = shell_default;
    fab.fab$b_dns = strlen(shell_default);
  }
  fab.fab$l_fna = given_filespec;
  fab.fab$b_fns = strlen(given_filespec);
  fab.fab$l_nam = &nam;
  nam.nam$l_esa = expanded_filespec;		/* v1.2-004 */
  nam.nam$b_ess = NAM$C_MAXRSS;
  nam.nam$l_rsa = resultant_filespec;		/* v1.2-004 */
  nam.nam$b_rss = NAM$C_MAXRSS;

  /* --- PARSE OUT THE GIVEN FILESPEC --- */

  if ((status = sys$parse(&fab)) == RMS$_NORMAL)
  {
    /* --- SEARCH FOR FILE OR FILES --- */

    while ((status = sys$search(&fab)) == RMS$_NORMAL)
    {
      filespec_v = ((filespec_v == NULL) ? xmalloc(sizeof (char *)) :
       xrealloc(filespec_v,(filespec_count + 1) * sizeof (char *)));

      nam.nam$l_rsa[nam.nam$b_rsl] = '\0';
      for (character_p = nam.nam$l_rsa; *character_p; ++character_p)
       *character_p = _tolower(*character_p);		/* v1.2-004 */

      if (strncmp(nam.nam$l_rsa,our_path,nam.nam$b_dev + nam.nam$b_dir) == 0)
      {
        filespec_v[filespec_count] = xmalloc(
          (int) (nam.nam$b_rsl - nam.nam$b_dev - nam.nam$b_dir) + 1
        );
        (void)strcpy(filespec_v[filespec_count],nam.nam$l_name);
      }
      else if (strncmp(nam.nam$l_rsa,our_path,nam.nam$b_dev) == 0)
      {
        filespec_v[filespec_count] = xmalloc(
          (int) (nam.nam$b_rsl - nam.nam$b_dev) + 1
        );
        (void)strcpy(filespec_v[filespec_count],nam.nam$l_dir);
      }
      else
      {
        filespec_v[filespec_count] = xmalloc((int) nam.nam$b_rsl + 1);
        (void)strcpy(filespec_v[filespec_count],nam.nam$l_rsa);
      }

      if (filespec_count_p == NULL)
       break;
      ++filespec_count;

    } /* sys$search() while */

    /* --- UPDATE FILESPEC COUNT AND RETURN FILESPEC VECTOR --- */

    if ((status == RMS$_NORMAL) || (status == RMS$_NMF))
    {
      if (filespec_count_p)
       *filespec_count_p = filespec_count;
      return filespec_v;
    }

  } /* sys$parse() if */

  /* --- RETURN VECTOR REFERRING TO GIVEN FILESPEC --- */

  /* If we get here, we either could not parse the given filespec or could
  ** not find it.  In such cases we simply return a vector that refers to
  ** the given filespec.
  */
  filespec_v = xmalloc(sizeof (char *));
  filespec_v[0] = given_filespec;
  if (filespec_count_p)
   *filespec_count_p = 1;
  return filespec_v;

} /* shell_glob() */

/* shell_mung()
**-----------------------------------------------------------------------------
** Functional Description
** ~~~~~~~~~~~~~~~~~~~~~~
**  Given a description of the command line that invoked a program from the
** main() function's `argc' and `argv' variables, this function changes those
** variables to reflect wildcard matching of filespecs in that command line
** and changes standard IO file pointers `stderr', `stdin', and `stdout' to
** reflect any IO redirection requested in that command line.
**-----------------------------------------------------------------------------
** Usage
** ~~~~~
**  Since shell_mung() is written for programs running VAX C programs under the
** DCL CLI on VMS, a call to shell_mung() should be predicated on these things:
**
**      #if defined(VAXC) && defined(VMS)
**        if (!shell$is_shell())
**         shell_mung(&argc,&argv,...,...);
**      #endif
**
** One should call shell_mung() before doing anything else---in particular,
** before anything is done with `argc', `argv', and parts of the standard IO
** library.
**  The first and second arguments to shell_mung() should always be addresses
** of `argc' and `argv', respectively.  The third and fourth arguments require
** a bit more thought.  Before they are explained, though, we have to define
** some terms.
**  A "parameter" is a position-dependent command line argument that isn't an
** IO redirection (those start with '<' or '>'), an option or an option value.
** An "option" is similar to a DCL qualifier, except that it starts with '-' or
** '+' instead of '/'.  A "value" is a string that follows certain options.
** Which options have values is something that varies from program to program.
** A value in DCL is indicated with a equal sign (e.g. /qualifier=value), but
** here it can either be concatenated to an option (e.g. -ovalue) or appear as
** an argument following the option (e.g. -o value).  "Globbing," as mentioned
** above, is to parse wildcarded filespecs and match them with existing files.
** A "mung" is a legume used in Indian cooking, often after being shelled.
**  The third argument to shell_mung() is the number of the parameter where we
** expect filespecs to appear---in other words, the parameters we expect to
** need globbing.  A GNU-shell-like command language interpreter will glob any
** argument that has a wildcard character in it, unless that argument is put in
** single quotes.  DCL doesn't glob arguments, and though you can put arguments
** in double quotes to pass them through exactly as typed, by the time VAX C
** has processed them into `argv' strings, the double quotes have been stripped
** off.  This is why we have to tell shell_mung() where to start globbing.  The
** assumption is made that unglobbable arguments (such as strings) will appear
** as parameters before globbable arguments (such as filespecs), an assumption
** that holds for most GNU utilities, probably because it's the only rational
** way to code utilities that accept multiple filespecs on the command line.
**  As an example, consider the following command line, where "fgrep" is a DCL
** command to run the GNU "fgrep" program:
**
**      $ fgrep -n xyzzy *.c *.h
**
** "xyzzy", "*.c", and "*.h" are the parameters.  For fgrep, the string to be
** searched for is always the first parameter ("xyzzy" in this case); the files
** to be searched always begin as a second parameter.  The "-n" is an option,
** which could be placed anywhere.  The following commands mean the same thing
** as the previous one:
**
**      $ fgrep xyzzy -n *.c *.h
**      $ fgrep xyzzy *.c -n *.h
**      $ fgrep xyzzy *.c *.h -n
**
** (It is, however, usually considered better form to type the options first.
** Some programs actually require that certain options appear before certain
** parameters.)  To indicate that we expect input filespecs to begin as the
** second parameter, we call shell_mung() with 2 as its third argument:
**
**      shell_mung(&argc,&argv,2,...);
**
** This changes `argc' and `argv' so that if the user types this:
**
**      $ fgrep -n xyzzy *.c
**
** It's as if the user had typed this (for example):
**
**      $ fgrep -n xyzzy dev:[dir]bar.c dev:[dir]baz.c dev:[dir]foo.c
**
** If all of the parameters are to be considered globbable filespecs, the third
** argument to shell_mung() should be 1 (indicating, of course, that globbing
** can start with the first parameter).  If no parameters are to be considered
** globbable, shell_mung()'s third argument should be 0.  If the user uses a
** negative number as the third argument, it is treated as 0.
**  The fourth argument is a list of options which are followed by values, with
** indications as to which values are to be globbed, and which option/value
** combinations are to be considered as replacements for parameters (more on
** this later).  The list is passed as a character string.
**  As an example, the "make" program has a "-f" option that expects a filespec
** as its value:
**
**      $ make [-f makefile] [target] [macros]...
**
** To indicate that we have a "-f" option followed by a globbable value, we put
** 'f' in shell_mung()'s fourth argument:
**
**      shell_mung(&argc,&argv,0,"f");
**
** When an option's value is globbed, only the first matching filespec is used
** to replace that value.  If, for example, "makefile.vms" and "makefile.xenix"
** are the only two files in your directory, and the user types one of these:
**
**      $ make -fmakefile.*
**      $ make -f makefile.*
**
** It's as if the user had typed one of these (respectively):
**
**      $ make -fdev:[dir]makefile.vms
**      $ make -f dev:[dir]makefile.vms
**
**  Options with unglobbable values must also be listed, with indications that
** they are unglobbable.  The "from" program is an example of this:
**
**      $ from [-s sender] [user]
**
** The "-s" option is followed by a username, not an input filespec.  Thus we
** put a '-' after the 's' in shell_mung()'s fourth argument:
**
**      shell_mung(&argc,&argv,0,"s-");
**
**  There are times when an option/value combination can replace a parameter.
** Using fgrep as an example again, its first parameter---the string to be
** searched for---can be replaced by the "-e" and "-f" options and their
** values.  The "-e" option is used when the string to be searched for starts
** with a '-' character and could be confused with an option, and the "-f"
** option is used when the string or strings to be searched for is in a file.
** We indicate such options by appending a '+' character to them in the string
** that is shell_mungs()'s fourth argument:
**
**      shell_mung(&argc,&argv,2,"e+-f+");
**
** (Note that the "-e" option also has a '-' character following it, since we
** don't want its value to be globbed.  When both '+' and '-' are used, the '+'
** should always be first.)  It should be noted that such option/value pairs
** should be typed early in the command line, in positions where the parameters
** they're replacing would have been.  For example, you would do this:
**
**      $ fgrep -f stringfile inputfile
**
** But this would be an invitation to disaster:
**
**      $ fgrep inputfile -f stringfile
**
**  Programs that use the getopt() library function to process options will
** help you decide which options to include in shell_mung()'s fourth argument.
** The third argument to getopt() is a list of options, and options that have
** values are followed by a ':' character.  Here's an example from the source
** to the GNU "grep" program:
**
**      while ((c = getopt(argc,argv,"0123456789A:B:CVbce:f:hilnsvwx")) != EOF)
**
** This tells us that the "-A", "-B", "-e", and "-f" options have values.  Of
** these, "-A", "-B", and "-e" have unglobbable values; and "-e" and "-f" can
** replace a parameter.  Thus, our call to shell_mung() for the GNU grep
** program looks like this:
**
**      shell_mung(&argc,&argv,2,"A-B-e+-f+");
**
**  The IO redirection capabilities provided are as follows:
**
**      <infile         Input comes from "infile".
**      >outfile        Output goes to "outfile".
**      >&outfile       Output and errors go to "outfile".
**      >>outfile       Output appended to "outfile".
**      >>&outfile      Output and errors appended to "outfile".
**
** They can be used anywhere on the command line, except between an option and
** its value.  Here are some examples:
**
**      $ fgrep xyzzy *.txt >&lines_with_xyzzy.lis
**      $ from -s flintstone >>accumulated_flintstone_missives.dat
**      $ my_mailer -d <mail_commands.com
**
** Some notes:  (1) The filespecs must be next to the redirection characters;
** they can't be separate arguments.  (2) For input redirection, the "infile"
** is globbed to the first matching filespec, just like an option's value.  (3)
** The "<<" redirection characters are not implemented.  Unlike shell scripts,
** VMS DCL command procedures define standard input as records that don't start
** with a '$' character, or aren't the record defined by the DECK command.
** Thus, "<<" redirection is irrelevant.  (4) The "!" redirection character as
** used in ">!", ">&!", ">>!", and ">>&!" is also not implemented.  VMS uses
** file versions, which makes it irrelevant.  (5) If a globbable argument turns
** out to be unglobbable, the new vector will simply point to the original
** argument.  (6) Pipes (as in the "|" and "|&" redirection characters) are not
** implemented.
**-----------------------------------------------------------------------------
** Calling Sequence
** ~~~~~~~~~~~~~~~~
** delta-arg-count = shell_mung(argc_p,argv_p,parameter_number,option_string);
**-----------------------------------------------------------------------------
** Formal Arguments     Description
** ~~~~~~~~~~~~~~~~     ~~~~~~~~~~~
** argc_p               Pointer to argument count (main()'s `argc').
** argv_p               Pointer to argument vector (main()'s `argv').
** parameter_number     Position among parameters where filespecs are expected.
** option_string        List of options that may be followed by filespecs.
**-----------------------------------------------------------------------------
** Implicit Inputs:     None
**-----------------------------------------------------------------------------
** Implicit Outputs:    None
**-----------------------------------------------------------------------------
** Return Value
** ~~~~~~~~~~~~
**  Returns the number of new arguments that have been added to the argument
** vector.  In other words, the difference between the new value of `argc_p'
** and its old value.
**-----------------------------------------------------------------------------
** Side Effects
** ~~~~~~~~~~~~
**  Function may cause the program to exit if it can't open files for error,
** input, and output redirection.  Uses global variable `vaxc$errno' for exit
** status.
**-----------------------------------------------------------------------------
*/
int
shell_mung(argc_p,argv_p,parameter_number,option_string)
 int *  argc_p;
 char ***  argv_p;
 int  parameter_number;
 char *  option_string;
{
  /* --- LOCAL VARIABLES --- */

  register char *  arg_p;               /* General-purpose argument pointer.
                                        */
  int  filespec_count;                  /* Number of filespecs in `filespec_v',
                                        ** returned from shell_glob().
                                        */
  char **  filespec_v;                  /* Vector of filespecs returned from
                                        ** shell_glob().
                                        */
  char  from_err_too = FALSE;           /* Flag set to indicate that error text
                                        ** will be redirected to a file along
                                        ** with the output text.
                                        */
  register int  i_new;                  /* Index to new pointers in `new_argv'.
                                        */
  register int  i_old;                  /* Index to old pointers in `argv_p'.
                                        */
  char **  new_argv;                    /* New argument vector, which `argv_p'
                                        ** will end up being replaced with.
                                        */
  int  new_argv_count;                  /* Count of pointers in `new_argv'.
                                        */
  int  output_fd;                       /* File descriptor for file being
                                        ** created as redirected output, for
                                        ** use with creat() and dup2().
                                        */
  int  parameter_count = 0;             /* Counts parameters---arguments that
                                        ** don't have '<' or '>' or '-' in
                                        ** front of them.  When and if the
                                        ** count reaches `parameter_number',
                                        ** we start attempting to parse the
                                        ** parameters as filespecs.
                                        */
  register char *  place_p;             /* Location of a character, as returned
                                        ** from strchr() or strrchr().
                                        */

  /* --- INITIALIZE THINGS --- */

  /* Allocate the new argument vector.
  */
  new_argv_count = *argc_p;
  new_argv = xmalloc(new_argv_count * sizeof (char *));

  /* Replace main()'s `argv[0]' with a GNU-like truncated version.
  */
  new_argv[0] = truncate_argv_0((*argv_p)[0]);

  /* --- TRAVERSE THE ARGUMENT VECTOR --- */

  for (i_new = i_old = 1; i_old < *argc_p; ++i_old)
   switch (*(arg_p = (*argv_p)[i_old]))
  {
    /* If it begins with '<', we're redirecting input.
    */
    case '<':
    filespec_v = shell_glob(++arg_p,NULL);
    if (freopen(*filespec_v,"r",stdin) == NULL)
    {
      perror(*filespec_v);
      sys$exit(vaxc$errno | STS$M_INHIB_MSG);
    }
    free(*filespec_v);
    free(filespec_v);
    --new_argv_count;
    break;

    /* If it begins with '>', we're redirecting output.  This comes in four
    ** varieties, as one can append output to an existing file and/or send
    ** error text out along with the output.  If we're told to append it (by
    ** two '>' characters), we try to access the filespec and append to it.
    ** If we can't access it, we proceed as if we were told to create one.
    ** We create an output file (which is also what we're told to do when we
    ** only get one '>' character) with "standard" VMS attributes.  If we're
    ** told to redirect error text as well (by the '&' character), we set
    ** `stderr' to refer to the same file.
    */
    case '>':
    if (*++arg_p == '>')
    {
      if (*++arg_p == '&')
      {
        from_err_too = TRUE;
        ++arg_p;
      }
      if (access(arg_p,2) == 0)
      {
        if (freopen(arg_p,"a",stdout) != NULL)
        {
          if (from_err_too == TRUE)
           stderr = stdout;
          --new_argv_count;
          break;
        }
        perror(arg_p);
        sys$exit(vaxc$errno | STS$M_INHIB_MSG);
      }
    }
    /* We get to this point if we didn't want to append output text to an
    ** existing file, or if an attempt to establish append access to an
    ** existing file has failed.
    */
    if ((from_err_too == FALSE) && (*arg_p == '&'))
    {
      from_err_too = TRUE;
      ++arg_p;
    }
    if (
      ((output_fd = creat(arg_p,0,"rat=cr","rfm=var","mrs=512")) != 1)
      && (dup2(output_fd,fileno(stdout)) != -1)
      && ((from_err_too == FALSE) || (dup2(output_fd,fileno(stderr)) != -1))
    )
    {
      --new_argv_count;
      break;
    }
    perror(arg_p);
    sys$exit(vaxc$errno | STS$M_INHIB_MSG);

    /* If it begins with '-' or '+', it's an option.  We check it against
    ** `option_string' to see if it's an option followed by an argument.
    */
    case '-':
    case '+':
    if (
      (option_string == NULL)
      || (((char *) place_p = strchr(option_string,(*(arg_p + 1)))) == 0)
    )
    {
      /* Option is not in `option_string'.  We copy it into `new_argv'.
      */
      new_argv[i_new++] = arg_p;
    }
    else
    {
      /* Option is in `option_string'.  First we check to see if its existence
      ** is to be thought of as introducing a parameter.  This is indicated by
      ** a '+' character following the option in `option_string'.
      */
      if (*++place_p == '+')
       ++parameter_count;
      else
       --place_p;

      /* Next we check to see if the string following it is to be considered an
      ** input filespec.  If a '-' character follows the option in `option_
      ** string' (or a '+' character following that option), that indicates
      ** that it's not to be considered as such.
      */
      if (*++place_p == '-')
      {
        new_argv[i_new++] = arg_p;
        if ((arg_p = (*argv_p)[++i_old]) == NULL)
            goto BREAKOUT;
        else					/* V1.2-005 */
            new_argv[i_new++] = arg_p;		/* V1.2-005 */
      }
      else
      {
        /* The string after the option is an input filespec, so we glob it.
        ** But we glob it to return only the first match (if it's a wildcard),
        ** not all possible matches.
        */
        if (*(arg_p + 2) == '\0')
        {
          new_argv = xrealloc(new_argv,(++new_argv_count * sizeof (char *)));
          new_argv[i_new++] = arg_p;
          if ((arg_p = (*argv_p)[++i_old]) == NULL)
           goto BREAKOUT;

          filespec_v = shell_glob(arg_p,NULL);
          new_argv[i_new++] = *filespec_v;
          free(filespec_v);
        }
        else
        {
          filespec_v = shell_glob((arg_p + 2),NULL);
          new_argv[i_new] = xmalloc(2 + strlen(*filespec_v) + 1);
          memcpy(new_argv[i_new],arg_p,2);
          (void)strcpy((new_argv[i_new++] + 2),*filespec_v);
          free(*filespec_v);
          free(filespec_v);
        }
      }
    }
    break;

    /* If we get here, the argument is a parameter.  If we're not concerned
    ** about parameters (that is, if `parameter_number' is zero or less), or
    ** if the parameter count is less than `parameter_number', we simply copy
    ** the parameter into the new argument vector.  Otherwise, the parameter
    ** and those following it are expected to be filespecs, and an attempt is
    ** made to parse them as such and put all resulting filespecs into the
    ** argument vector.
    */
    default:
    if ((parameter_number <= 0) || (++parameter_count < parameter_number))
     new_argv[i_new++] = arg_p;
    else
    {
      filespec_v = shell_glob(arg_p,&filespec_count);
      if (filespec_count == 1)
       new_argv[i_new++] = *filespec_v;
      else
      {
        new_argv_count += filespec_count - 1;
        new_argv = xrealloc(new_argv,(new_argv_count * sizeof (char *)));
        memcpy(&new_argv[i_new],filespec_v,(filespec_count * sizeof (char *)));
        i_new += filespec_count;
      }
      free(filespec_v);
    }
    break;

  } /* for/switch */

BREAKOUT:

  new_argv[i_new] = NULL;

  /* --- UPDATE ARGUMENT COUNT AND ARGUMENT VECTOR --- */

  *argc_p = i_new;
  *argv_p = new_argv;

  return i_new - i_old;

} /* shell_mung() */

/* -=- STATIC FUNCTION -=- */

/* truncate_argv_0
**-----------------------------------------------------------------------------
** Functional Description
** ~~~~~~~~~~~~~~~~~~~~~~
**  Programs written in VAX C, when run under VMS DCL, will have their entire
** filespec (i.e., the filespec of the running image) in the main() function's
** `argv[0]' variable.  While this makes sense from a VMS standpoint, it does
** not look very good in programs ported from GNU, which often use `argv[0]' as
** an error message prefix.
**  This function, which is called from shell_mung(), truncates the filespec in
** `argv[0]' to just the filename or, if the filetype is not ".EXE", to the
** filename and filetype.
**-----------------------------------------------------------------------------
** Calling Sequence
** ~~~~~~~~~~~~~~~~
** new_argv[0] = truncate_argv_0(given_filespec);
**-----------------------------------------------------------------------------
** Formal Argument      Description
** ~~~~~~~~~~~~~~~      ~~~~~~~~~~~
** given_filespec       The filespec to be truncated.  Assumed to be `argv[0]'
**                      or a pointer to the same thing `argv[0]' points to.
**-----------------------------------------------------------------------------
** Implicit Inputs:     None
**-----------------------------------------------------------------------------
** Implicit Outputs:    None
**-----------------------------------------------------------------------------
** Return Value
** ~~~~~~~~~~~~
**  Returns the truncated filespec, which is in newly-allocated memory.
**-----------------------------------------------------------------------------
** Side Effects
** ~~~~~~~~~~~~
**  Memory is allocated for the truncated filespec.
**-----------------------------------------------------------------------------
*/
static char *
truncate_argv_0(given_filespec)
 char *  given_filespec;
{
  /* --- LOCAL VARIABLES --- */

  register char *  character_p;         /* General-purpose character pointer,
                                        ** used to lowercase filespec.
                                        */
  char  * expanded_filespec;		/* v1.2-004 */
                                        /* Buffer to hold the expanded filespec
                                        ** from a call to sys$parse().
                                        */
  register char *  filespec_p;          /* Pointer to truncated filespec to be
                                        ** returned.
                                        */
  struct FAB  fab = cc$rms_fab;         /* RMS file access block for file.
                                        */
  struct NAM  nam = cc$rms_nam;         /* RMS name block for file.
                                        */

  /* --- INITIALIZE FABS AND NAMS --- */

  expanded_filespec = xmalloc(NAM$C_MAXRSS + 1);	/* v1.2-004 */
  fab.fab$l_fna = given_filespec;
  fab.fab$b_fns = strlen(given_filespec);
  fab.fab$l_nam = &nam;
  nam.nam$l_esa = expanded_filespec;			/* v1.2-004 */
  nam.nam$b_ess = NAM$C_MAXRSS;
  nam.nam$b_nop = NAM$M_SYNCHK;

  /* --- PARSE OUT THE GIVEN FILESPEC --- */

  sys$parse(&fab);

  /* --- NUL-TERMINATE AND LOWERCASE FILESPEC --- */

  *nam.nam$l_ver = '\0';

  for (character_p = nam.nam$l_esa; *character_p; ++character_p)
   *character_p = _tolower(*character_p);	/* v1.2-004 */

  /* --- RETURN TRUNCATED FILESPEC --- */

  if (strcmp(nam.nam$l_type,".exe"))
   filespec_p = xmalloc(nam.nam$b_name + nam.nam$b_type + 1);
  else
  {
    *nam.nam$l_type = '\0';
    filespec_p = xmalloc(nam.nam$b_name + 1);
  }

  (void)strcpy(filespec_p,nam.nam$l_name);
  return filespec_p;

} /* truncate_argv_0() */
