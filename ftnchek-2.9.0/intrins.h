/* intrins.h:
   List of intrinsic functions for use by find_intrinsic
   and check_intrins_args.  You may add locally available intrinsics
   to this list (order not important).

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


*/


	/* Define positional flags to allow specifying more
	   than one allowed type of argument for generics.
	 */

#define I   (1 << type_INTEGER)
#define R   (1 << type_REAL)
#define D   (1 << type_DP)
#define C   (1 << type_COMPLEX)
#define Z   (1 << type_DCOMPLEX)
#define L   (1 << type_LOGICAL)
#define STR (1 << type_STRING)

	/* Table contains: name, num_args, arg_type, result_type, flags.
	   Special num_args values are defined in symtab.h.

	   Flags: I_F77 if it is in Table 5 p. 15-24, I_NONF77 otherwise
		  I_MIXED_ARGS if arguments are not all of same type.
		  I_NONPURE if arg need not have defined value (LEN).
		  I_C_TO_R indicates complex -> real in generic cases
		      (ABS,IMAG,REAL).
		  I_SP_R indicates specific REAL result (REAL)
	          I_NOTARG if it is a generic with no specific meaning,
		      or if it is a type conversion, lexical relationship,
		      or min or max (cf. p. 15-3, sec. 15.3.2)
		  I_EXTRA indicates common nonstd function
		  I_VMS indicates VMS-specific function
		  I_UNIX indicates UNIX-specific function
	 */

{"INT", 	1,	I|R|D|C|Z,type_INTEGER,	I_F77|I_NOTARG},
{"IFIX",	1,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"IDINT",	1,	D,	type_INTEGER,	I_F77|I_NOTARG},
{"REAL",	1,	I|R|D|C|Z,type_GENERIC, I_F77|I_NOTARG|I_C_TO_R|I_SP_R},
{"FLOAT",	1,	I,	type_REAL,	I_F77|I_NOTARG},
{"SNGL",	1,	D,	type_REAL,	I_F77|I_NOTARG},
{"DBLE",	1,	I|R|D|C|Z,type_DP,	I_F77|I_NOTARG},
{"CMPLX",	I_1or2,	I|R|D|C|Z,type_COMPLEX,	I_F77|I_NOTARG},
{"ICHAR",	1,	STR,	type_INTEGER,	I_F77|I_NOTARG|I_ICHAR},
{"CHAR",	1,	I,	type_STRING,	I_F77|I_NOTARG|I_CHAR},
{"AINT",	1,	R|D,	type_GENERIC,	I_F77},
{"DINT",	1,	D,	type_DP,	I_F77},
{"ANINT",	1,	R|D,	type_GENERIC,	I_F77},
{"DNINT",	1,	D,	type_DP,	I_F77},
{"NINT",	1,	R|D,	type_INTEGER,	I_F77},
{"IDNINT",	1,	D,	type_INTEGER,	I_F77},
{"ABS", 	1,	I|R|D|C|Z,type_GENERIC,	I_F77|I_C_TO_R|I_ABS},
{"IABS",	1,	I,	type_INTEGER,	I_F77|I_ABS},
{"DABS",	1,	D,	type_DP,	I_F77},
{"CABS",	1,	C,	type_REAL,	I_F77},
{"MOD", 	2,	I|R|D,	type_GENERIC,	I_F77|I_MOD},
{"AMOD",	2,	R,	type_REAL,	I_F77},
{"DMOD",	2,	D,	type_DP,	I_F77},
{"SIGN",	2,	I|R|D,	type_GENERIC,	I_F77|I_SIGN},
{"ISIGN",	2,	I,	type_INTEGER,	I_F77|I_SIGN},
{"DSIGN",	2,	D,	type_DP,	I_F77},
{"DIM",		2,	I|R|D,	type_GENERIC,	I_F77|I_DIM},
{"IDIM",	2,	I,	type_INTEGER,	I_F77|I_DIM},
{"DDIM",	2,	D,	type_DP,	I_F77},
{"DPROD",	2,	R,	type_DP,	I_F77},
{"MAX",		I_2up,	I|R|D,	type_GENERIC,	I_F77|I_NOTARG|I_MAX},
{"MAX0",	I_2up,	I,	type_INTEGER,	I_F77|I_NOTARG|I_MAX},
{"AMAX1",	I_2up,	R,	type_REAL,	I_F77|I_NOTARG},
{"DMAX1",	I_2up,	D,	type_DP,	I_F77|I_NOTARG},
{"AMAX0",	I_2up,	I,	type_REAL,	I_F77|I_NOTARG},
{"MAX1",	I_2up,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"MIN", 	I_2up,	I|R|D,	type_GENERIC,	I_F77|I_NOTARG|I_MIN},
{"MIN0",	I_2up,	I,	type_INTEGER,	I_F77|I_NOTARG|I_MIN},
{"AMIN1",	I_2up,	R,	type_REAL,	I_F77|I_NOTARG},
{"DMIN1",	I_2up,	D,	type_DP,	I_F77|I_NOTARG},
{"AMIN0",	I_2up,	I,	type_REAL,	I_F77|I_NOTARG},
{"MIN1",	I_2up,	R,	type_INTEGER,	I_F77|I_NOTARG},
{"LEN", 	1,	STR,	type_INTEGER,	I_F77|I_LEN},
{"INDEX",	2,	STR,	type_INTEGER,	I_F77|I_INDEX},
{"AIMAG",	1,	C,	type_REAL,	I_F77},
{"CONJG",	1,	C,	type_COMPLEX,	I_F77},
{"SQRT",	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DSQRT",	1,	D,	type_DP,	I_F77},
{"CSQRT",	1,	C,	type_COMPLEX,	I_F77},
{"EXP",		1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DEXP",	1,	D,	type_DP,	I_F77},
{"CEXP",	1,	C,	type_COMPLEX,	I_F77},
{"LOG", 	1,	R|D|C|Z,type_GENERIC,	I_F77|I_NOTARG},
{"ALOG",	1,	R,	type_REAL,	I_F77},
{"DLOG",	1,	D,	type_DP,	I_F77},
{"CLOG",	1,	C,	type_COMPLEX,	I_F77},
{"LOG10",	1,	R|D,	type_GENERIC,	I_F77|I_NOTARG},
{"ALOG10",	1,	R,	type_REAL,	I_F77},
{"DLOG10",	1,	D,	type_DP,	I_F77},
{"SIN", 	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DSIN",	1,	D,	type_DP,	I_F77},
{"CSIN",	1,	C,	type_COMPLEX,	I_F77},
{"COS", 	1,	R|D|C|Z,type_GENERIC,	I_F77},
{"DCOS",	1,	D,	type_DP,	I_F77},
{"CCOS",	1,	C,	type_COMPLEX,	I_F77},
{"TAN", 	1,	R|D,	type_GENERIC,	I_F77},
{"DTAN",	1,	D,	type_DP,	I_F77},
{"ASIN",	1,	R|D,	type_GENERIC,	I_F77},
{"DASIN",	1,	D,	type_DP,	I_F77},
{"ACOS",	1,	R|D,	type_GENERIC,	I_F77},
{"DACOS",	1,	D,	type_DP,	I_F77},
{"ATAN",	1,	R|D,	type_GENERIC,	I_F77},
{"DATAN",	1,	D,	type_DP,	I_F77},
{"ATAN2",	2,	R|D,	type_GENERIC,	I_F77},
{"DATAN2",	2,	D,	type_DP,	I_F77},
{"SINH",	1,	R|D,	type_GENERIC,	I_F77},
{"DSINH",	1,	D,	type_DP,	I_F77},
{"COSH",	1,	R|D,	type_GENERIC,	I_F77},
{"DCOSH",	1,	D,	type_DP,	I_F77},
{"TANH",	1,	R|D,	type_GENERIC,	I_F77},
{"DTANH",	1,	D,	type_DP,	I_F77},
{"LGE", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LGT", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LLE", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},
{"LLT", 	2,	STR,	type_LOGICAL,	I_F77|I_NOTARG},

		/* DOUBLE COMPLEX intrinsics are included regardless
		   of NONSTD_INTRINSICS option, since they are essential
		   to support of this datatype.
		 */
{"DCMPLX",	I_1or2,	I|R|D|C|Z,type_DCOMPLEX,I_NONF77|I_NOTARG},
{"DCONJG",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"DIMAG",	1,	Z,	type_DP,	I_NONF77},
{"IMAG",	1,	C|Z,	type_GENERIC,	I_NONF77|I_NOTARG|I_C_TO_R},
{"DREAL",	1,	Z,	type_DP,	I_NONF77},
{"CDABS",	1,	Z,	type_DP,	I_NONF77},
{"ZABS",	1,	Z,	type_DP,	I_NONF77},
{"CDSQRT",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZSQRT",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"CDEXP",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZEXP",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"CDLOG",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZLOG",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"CDSIN",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZSIN",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"CDCOS",	1,	Z,	type_DCOMPLEX,	I_NONF77},
{"ZCOS",	1,	Z,	type_DCOMPLEX,	I_NONF77},

		/* DFLOAT has been available in almost all Fortran
                   implementations for decades, but curiously, was
                   omitted from the Fortran 66 and Fortran 77
                   standards.  A separate intrinsic is essential,
                   because DBLE(FLOAT()) will lose bits for integer
                   arguments larger than the REAL fraction size.  If
                   we don't include it here, declaration file output
                   will incorrectly type it as REAL instead of DOUBLE
                   PRECISION. */

{"DFLOAT",	1,	I,	type_DP,	I_NONF77},


#ifdef NONSTD_INTRINSICS

	/* Nonstandard but widely used intrinsics.  These follow both
	   VMS and AIX defns, so they are probably de facto standard.
	   Not included: specifics covered by a generic.
	   N.B. Argument checking is not tight for these: some
	   take arrays, 0 or 1 arguments, etc. that are not
	   handled by check_intrins_args().  Remarks are placed by
	   these cases.
	 */


		/* Bit test & Shift operations */
{"BTEST",	2,	I,	type_LOGICAL,	I_NONF77|I_EXTRA},
{"IAND",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IOR",		2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IBSET",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IBCLR",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IBITS",	3,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"IEOR",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"ISHFT",	2,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"ISHFTC",	3,	I,	type_INTEGER,	I_NONF77|I_EXTRA},
{"NOT",		1,	I,	type_INTEGER,	I_NONF77|I_EXTRA},

		/* Address-of function */
{"LOC",		1,I|R|D|C|Z|L|STR,type_INTEGER,	I_NONF77|I_EXTRA},

		/* Utility routines */
{"EXIT",       I_0or1,	I,	type_SUBROUTINE,I_NONF77|I_EXTRA},
#endif

		/* Unix only.  These are a selected subset of the F77
		   library routines listed in the USENIX manual section 3F.
		 */
#ifdef UNIX_INTRINSICS
{"ABORT",	1,	STR,	type_SUBROUTINE,I_NONF77|I_UNIX},
{"AND",		2,	I,	type_INTEGER,	I_NONF77|I_UNIX},
		/* I, then STR not enforced in GETARG. */
{"GETARG",	2,	I|STR,	type_SUBROUTINE,I_MIXED_ARGS|I_NONF77|I_UNIX},
{"GETENV",	2,	STR,	type_SUBROUTINE,I_NONF77|I_UNIX},
{"GMTIME",	2,	I,	type_SUBROUTINE,I_NONF77|I_UNIX},/*2nd arg array(9)*/
#ifdef IARGC_NO_ARG
{"IARGC",	0,	0,	type_INTEGER,	I_NONF77|I_UNIX},
#else
#ifdef IARGC_ONE_ARG
{"IARGC",	1,	I,	type_INTEGER,	I_NONF77|I_UNIX},
#else  /* default is to allow 0 or 1 */
{"IARGC",	I_0or1,	I,	type_INTEGER,	I_NONF77|I_UNIX},
#endif
#endif
{"LSHIFT",	2,	I,	type_INTEGER,	I_NONF77|I_UNIX},
{"LTIME",	2,	I,	type_SUBROUTINE,I_NONF77|I_UNIX},/*2nd arg array(9)*/
{"OR",		2,	I,	type_INTEGER,	I_NONF77|I_UNIX},
#ifdef RAND_NO_ARG	/*RAND() form*/
{"IRAND",	0,	0,	type_INTEGER,	I_NONF77|I_UNIX},
{"RAND",	0,	0,	type_REAL,	I_NONF77|I_UNIX},
#else
#ifdef RAND_ONE_ARG	/*RAND(ISEED) form*/
{"IRAND",	1,	I,	type_INTEGER,	I_NONF77|I_UNIX|I_NONPURE},
{"RAND",	1,	I,	type_REAL,	I_NONF77|I_UNIX|I_NONPURE},
#else				/* Allow either form */
{"IRAND",	I_0or1,	I,	type_INTEGER,	I_NONF77|I_UNIX|I_NONPURE},
{"RAND",	I_0or1,	I,	type_REAL,	I_NONF77|I_UNIX|I_NONPURE},
#endif
#endif
{"RSHIFT",	2,	I,	type_INTEGER,	I_NONF77|I_UNIX},
{"SRAND",	1,	I|R,	type_SUBROUTINE,I_NONF77|I_UNIX},/*AIX has this*/
{"SYSTEM",	1,	STR,	type_SUBROUTINE,I_NONF77|I_UNIX},
{"TIME",	I_0or1,	I,	type_INTEGER,	I_NONF77|I_UNIX},
{"XOR",		2,	I,	type_INTEGER,	I_NONF77|I_UNIX},
#endif

#ifdef VMS_INTRINSICS		/* VMS only */
{"DATE",	1,	STR,	type_SUBROUTINE,I_NONF77|I_VMS},
{"ERRSNS",	5,	I,	type_SUBROUTINE,I_NONF77|I_VMS},
{"IDATE",	3,	I,	type_SUBROUTINE,I_NONF77|I_VMS},
{"RAN",		1,	I,	type_REAL,	I_NONF77|I_VMS|I_NONPURE},
{"SECNDS",	1,	R,	type_REAL,	I_NONF77|I_VMS},
{"SIZEOF",	1,	I|R|D|C|Z|L|STR,type_INTEGER,	I_NONF77|I_VMS},
{"TIME",	1,	STR,	type_SUBROUTINE,I_NONF77|I_VMS},
#endif

#undef I
#undef R
#undef D
#undef C
#undef Z
#undef L
#undef STR
