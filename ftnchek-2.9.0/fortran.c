
/*  A Bison parser, made from fortran.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	tok_identifier	258
#define	tok_array_identifier	259
#define	tok_label	260
#define	tok_integer_const	261
#define	tok_real_const	262
#define	tok_dp_const	263
#define	tok_complex_const	264
#define	tok_dcomplex_const	265
#define	tok_logical_const	266
#define	tok_string	267
#define	tok_hollerith	268
#define	tok_edit_descriptor	269
#define	tok_letter	270
#define	tok_relop	271
#define	tok_AND	272
#define	tok_OR	273
#define	tok_EQV	274
#define	tok_NEQV	275
#define	tok_NOT	276
#define	tok_power	277
#define	tok_concat	278
#define	tok_ACCEPT	279
#define	tok_ASSIGN	280
#define	tok_BACKSPACE	281
#define	tok_BLOCK	282
#define	tok_BLOCKDATA	283
#define	tok_BYTE	284
#define	tok_CALL	285
#define	tok_CHARACTER	286
#define	tok_CLOSE	287
#define	tok_COMMON	288
#define	tok_COMPLEX	289
#define	tok_CONTINUE	290
#define	tok_DATA	291
#define	tok_DIMENSION	292
#define	tok_DO	293
#define	tok_DOUBLE	294
#define	tok_DOUBLECOMPLEX	295
#define	tok_DOUBLEPRECISION	296
#define	tok_DOWHILE	297
#define	tok_ELSE	298
#define	tok_ELSEIF	299
#define	tok_END	300
#define	tok_ENDDO	301
#define	tok_ENDFILE	302
#define	tok_ENDIF	303
#define	tok_ENTRY	304
#define	tok_EQUIVALENCE	305
#define	tok_EXTERNAL	306
#define	tok_FILE	307
#define	tok_FORMAT	308
#define	tok_FUNCTION	309
#define	tok_GO	310
#define	tok_GOTO	311
#define	tok_IF	312
#define	tok_IMPLICIT	313
#define	tok_INCLUDE	314
#define	tok_INQUIRE	315
#define	tok_INTEGER	316
#define	tok_INTRINSIC	317
#define	tok_LOGICAL	318
#define	tok_NAMELIST	319
#define	tok_NONE	320
#define	tok_OPEN	321
#define	tok_PARAMETER	322
#define	tok_PAUSE	323
#define	tok_POINTER	324
#define	tok_PRECISION	325
#define	tok_PRINT	326
#define	tok_PROGRAM	327
#define	tok_READ	328
#define	tok_REAL	329
#define	tok_RETURN	330
#define	tok_REWIND	331
#define	tok_SAVE	332
#define	tok_STOP	333
#define	tok_SUBROUTINE	334
#define	tok_THEN	335
#define	tok_TO	336
#define	tok_TYPE	337
#define	tok_WHILE	338
#define	tok_WRITE	339
#define	tok_illegal	340
#define	tok_empty	341
#define	EOS	127
#define	REDUCE	342

#line 9 "fortran.y"


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



#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		818
#define	YYFLAG		-32768
#define	YYNTBASE	102

#define YYTRANSLATE(x) ((unsigned)(x) <= 342 ? yytranslate[x] : 341)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,    98,     2,     2,     2,    90,
    89,    92,    99,    91,    95,    97,    94,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    93,     2,   100,
    96,   101,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,    87,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
    66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
    76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
    86,    88
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     3,     5,     8,    10,    12,    14,    16,    18,
    21,    23,    25,    27,    29,    31,    34,    36,    38,    40,
    42,    44,    47,    50,    54,    56,    58,    60,    62,    64,
    66,    68,    70,    72,    74,    76,    78,    80,    82,    84,
    86,    88,    90,    92,    94,    96,    98,   100,   102,   104,
   106,   108,   110,   112,   114,   116,   118,   120,   122,   124,
   126,   128,   130,   132,   134,   136,   138,   140,   142,   144,
   146,   148,   150,   152,   153,   158,   162,   169,   171,   175,
   182,   186,   193,   196,   198,   200,   202,   204,   206,   208,
   212,   219,   221,   222,   224,   226,   230,   232,   234,   237,
   241,   244,   246,   250,   252,   256,   261,   263,   267,   269,
   273,   275,   279,   280,   285,   289,   295,   299,   303,   305,
   307,   309,   314,   317,   320,   324,   328,   333,   335,   338,
   341,   345,   348,   350,   352,   355,   357,   360,   362,   364,
   368,   370,   373,   376,   380,   382,   385,   387,   390,   394,
   398,   402,   407,   409,   413,   415,   417,   419,   421,   423,
   426,   428,   431,   433,   435,   437,   441,   443,   447,   449,
   451,   453,   457,   459,   463,   465,   469,   471,   475,   479,
   481,   482,   487,   488,   494,   496,   500,   502,   506,   510,
   512,   516,   522,   526,   528,   532,   533,   538,   542,   544,
   548,   552,   554,   558,   562,   564,   568,   574,   576,   578,
   580,   583,   587,   589,   593,   595,   599,   603,   605,   608,
   612,   613,   614,   621,   623,   627,   629,   631,   633,   637,
   639,   643,   645,   647,   649,   651,   653,   657,   659,   661,
   669,   673,   679,   680,   681,   688,   690,   692,   694,   696,
   703,   708,   715,   723,   727,   734,   742,   744,   747,   750,
   755,   766,   769,   773,   774,   780,   783,   784,   785,   794,
   797,   800,   804,   810,   811,   819,   820,   827,   831,   836,
   839,   843,   849,   853,   856,   859,   863,   867,   868,   870,
   872,   874,   875,   879,   880,   885,   886,   892,   898,   905,
   913,   917,   923,   925,   929,   935,   939,   940,   941,   949,
   953,   954,   955,   963,   965,   969,   973,   975,   977,   981,
   985,   989,   991,   993,   997,   999,  1001,  1009,  1010,  1017,
  1018,  1025,  1026,  1033,  1037,  1043,  1045,  1049,  1055,  1057,
  1060,  1064,  1070,  1072,  1074,  1076,  1078,  1080,  1081,  1088,
  1089,  1091,  1093,  1096,  1098,  1100,  1102,  1106,  1108,  1110,
  1112,  1114,  1116,  1118,  1120,  1122,  1124,  1126,  1128,  1130,
  1132,  1135,  1138,  1139,  1140,  1146,  1151,  1152,  1154,  1156,
  1160,  1162,  1163,  1167,  1168,  1174,  1175,  1182,  1185,  1187,
  1191,  1193,  1197,  1200,  1204,  1209,  1211,  1212,  1214,  1216,
  1220,  1222,  1224,  1226,  1230,  1234,  1236,  1240,  1242,  1246,
  1248,  1251,  1253,  1257,  1259,  1262,  1265,  1269,  1273,  1275,
  1279,  1283,  1285,  1289,  1291,  1295,  1297,  1299,  1301,  1303,
  1305,  1309,  1311,  1313,  1315,  1317,  1319,  1321,  1323,  1325,
  1327,  1329,  1331,  1333,  1335,  1340,  1345,  1347,  1351,  1353,
  1356,  1359,  1362,  1365,  1368,  1372,  1377,  1382,  1388,  1390,
  1392,  1394,  1396,  1398,  1400,  1402,  1404,  1407,  1410,  1412,
  1414,  1416,  1418,  1419
};

static const short yyrhs[] = {   103,
     0,     0,   104,     0,   103,   104,     0,   105,     0,   111,
     0,    87,     0,   106,     0,   109,     0,     5,   107,     0,
   107,     0,   108,     0,   112,     0,   115,     0,   118,     0,
     1,    87,     0,   121,     0,   124,     0,   130,     0,   136,
     0,   110,     0,     5,   110,     0,    45,    87,     0,    59,
    12,    87,     0,   113,     0,   182,     0,   174,     0,   198,
     0,   114,     0,   123,     0,   278,     0,   138,     0,   143,
     0,   150,     0,   157,     0,   163,     0,   186,     0,   188,
     0,   195,     0,   190,     0,   116,     0,   117,     0,   218,
     0,   220,     0,   223,     0,   240,     0,   302,     0,   213,
     0,   217,     0,   219,     0,   239,     0,   241,     0,   248,
     0,   250,     0,   243,     0,   251,     0,   254,     0,   274,
     0,   270,     0,   272,     0,   264,     0,   266,     0,   268,
     0,   295,     0,   119,     0,   120,     0,   224,     0,   225,
     0,   233,     0,   238,     0,   228,     0,   231,     0,   232,
     0,     0,    72,   122,   336,    87,     0,    49,   336,    87,
     0,    49,   336,    90,   133,    89,    87,     0,   125,     0,
   126,   336,    87,     0,   126,   336,    90,   133,    89,    87,
     0,   127,   336,    87,     0,   127,   336,    90,   133,    89,
    87,     0,   129,   128,     0,   128,     0,    54,     0,   164,
     0,   167,     0,   168,     0,   131,     0,   132,   336,    87,
     0,   132,   336,    90,   133,    89,    87,     0,    79,     0,
     0,   134,     0,   135,     0,   134,    91,   135,     0,   336,
     0,    92,     0,   137,    87,     0,   137,   336,    87,     0,
    27,    36,     0,    28,     0,    37,   139,    87,     0,   140,
     0,   139,    91,   140,     0,   336,    90,   141,    89,     0,
   142,     0,   141,    91,   142,     0,   324,     0,   324,    93,
   324,     0,    92,     0,   324,    93,    92,     0,     0,    50,
   144,   145,    87,     0,    90,   146,    89,     0,   145,    91,
    90,   146,    89,     0,   147,    91,   147,     0,   146,    91,
   147,     0,   336,     0,   148,     0,   149,     0,   336,    90,
   327,    89,     0,   336,   331,     0,   148,   331,     0,    33,
   154,    87,     0,    33,   151,    87,     0,    33,   154,   151,
    87,     0,   152,     0,   151,   152,     0,   153,   154,     0,
    94,   336,    94,     0,    94,    94,     0,    23,     0,   155,
     0,   154,   155,     0,   156,     0,   156,    91,     0,   336,
     0,   140,     0,    64,   158,    87,     0,   159,     0,   158,
   159,     0,   160,   161,     0,    94,   336,    94,     0,   162,
     0,   161,   162,     0,   336,     0,   336,    91,     0,   164,
   169,    87,     0,   167,   171,    87,     0,   168,   171,    87,
     0,   168,    91,   171,    87,     0,   165,     0,   165,    92,
   338,     0,   166,     0,    61,     0,    74,     0,    34,     0,
    63,     0,    39,    70,     0,    41,     0,    39,    34,     0,
    40,     0,    29,     0,    31,     0,   167,    92,   181,     0,
   170,     0,   169,    91,   170,     0,   336,     0,   140,     0,
   172,     0,   171,    91,   172,     0,   336,     0,   336,    92,
   181,     0,   140,     0,   140,    92,   181,     0,    58,     0,
   173,   175,    87,     0,   173,    65,    87,     0,   177,     0,
     0,   175,    91,   176,   177,     0,     0,   129,    90,   178,
   179,    89,     0,   180,     0,   179,    91,   180,     0,    15,
     0,    15,    95,    15,     0,    90,    92,    89,     0,   338,
     0,    90,   323,    89,     0,    67,    90,   183,    89,    87,
     0,    67,   183,    87,     0,   184,     0,   183,    91,   184,
     0,     0,   336,   185,    96,   307,     0,    51,   187,    87,
     0,   336,     0,   187,    91,   336,     0,    62,   189,    87,
     0,   336,     0,   189,    91,   336,     0,    69,   191,    87,
     0,   192,     0,   191,    91,   192,     0,    90,   193,    91,
   194,    89,     0,   336,     0,   336,     0,   140,     0,    77,
    87,     0,    77,   196,    87,     0,   197,     0,   196,    91,
   197,     0,   336,     0,    94,   336,    94,     0,    36,   199,
    87,     0,   200,     0,   199,   200,     0,   199,    91,   200,
     0,     0,     0,   203,    94,   201,   205,   202,    94,     0,
   204,     0,   203,    91,   204,     0,   216,     0,   211,     0,
   206,     0,   205,    91,   206,     0,   208,     0,   207,    92,
   208,     0,   338,     0,   336,     0,   337,     0,   336,     0,
   210,     0,   209,    91,   210,     0,   325,     0,   211,     0,
    90,   209,    91,   336,    96,   212,    89,     0,   323,    91,
   323,     0,   323,    91,   323,    91,   323,     0,     0,     0,
   216,    96,   214,   308,   215,    87,     0,   333,     0,   325,
     0,   330,     0,   291,     0,    25,   339,   340,    81,   333,
    87,     0,   221,   339,   340,    87,     0,   221,    90,   222,
    89,   321,    87,     0,   221,    90,   222,    89,    91,   321,
    87,     0,   221,   336,    87,     0,   221,   336,    90,   222,
    89,    87,     0,   221,   336,    91,    90,   222,    89,    87,
     0,    56,     0,    55,    81,     0,   339,   340,     0,   222,
    91,   339,   340,     0,   226,   339,   340,    91,   339,   340,
    91,   339,   340,    87,     0,   226,   115,     0,   226,    80,
    87,     0,     0,    57,    90,   227,   308,    89,     0,    43,
   225,     0,     0,     0,    44,    90,   229,   308,    89,   230,
    80,    87,     0,    43,    87,     0,    48,    87,     0,    45,
    57,    87,     0,   236,   333,    96,   237,    87,     0,     0,
   236,    83,    90,   234,   308,    89,    87,     0,     0,    42,
    90,   235,   308,    89,    87,     0,    38,   339,   340,     0,
    38,   339,   340,    91,     0,    38,   339,     0,   322,    91,
   322,     0,   322,    91,   322,    91,   322,     0,    45,    38,
    87,     0,    46,    87,     0,    35,    87,     0,    78,   242,
    87,     0,    68,   242,    87,     0,     0,     6,     0,   336,
     0,    12,     0,     0,   246,   244,    87,     0,     0,   246,
   261,   245,    87,     0,     0,    84,   247,    90,   257,    89,
     0,   249,    90,   257,    89,    87,     0,   249,    90,   257,
    89,   261,    87,     0,   249,    90,   257,    89,    91,   261,
    87,     0,   249,   277,    87,     0,   249,   277,    91,   261,
    87,     0,    73,     0,    24,   277,    87,     0,    24,   277,
    91,   261,    87,     0,    71,   277,    87,     0,     0,     0,
    71,   277,    91,   252,   261,   253,    87,     0,    82,   277,
    87,     0,     0,     0,    82,   277,    91,   255,   261,   256,
    87,     0,   258,     0,   257,    91,   258,     0,   336,    96,
   276,     0,   276,     0,   276,     0,   336,    96,   276,     0,
   259,    91,   260,     0,   336,    96,   276,     0,   336,     0,
   262,     0,   261,    91,   262,     0,   308,     0,   263,     0,
    90,   261,    91,   333,    96,   237,    89,     0,     0,    66,
   265,    90,   259,    89,    87,     0,     0,    32,   267,    90,
   257,    89,    87,     0,     0,    60,   269,    90,   257,    89,
    87,     0,   271,   276,    87,     0,   271,    90,   257,    89,
    87,     0,    26,     0,   273,   276,    87,     0,   273,    90,
   257,    89,    87,     0,    47,     0,    45,    52,     0,   275,
   276,    87,     0,   275,    90,   257,    89,    87,     0,    76,
     0,   308,     0,    92,     0,   317,     0,    92,     0,     0,
    53,   279,    90,   280,    89,    87,     0,     0,   281,     0,
   282,     0,   281,   282,     0,   283,     0,   284,     0,   285,
     0,    90,   281,    89,     0,    14,     0,    12,     0,    13,
     0,   287,     0,   288,     0,    91,     0,    94,     0,    23,
     0,    93,     0,    97,     0,   286,     0,    98,     0,     6,
     0,    95,     6,     0,    99,     6,     0,     0,     0,   100,
   289,   321,   290,   101,     0,   334,    90,   292,    89,     0,
     0,   293,     0,   294,     0,   293,    91,   294,     0,   333,
     0,     0,   299,   296,    87,     0,     0,   299,    90,    89,
   297,    87,     0,     0,   299,    90,   300,    89,   298,    87,
     0,    30,   336,     0,   301,     0,   300,    91,   301,     0,
   308,     0,    92,   339,   340,     0,    75,    87,     0,    75,
   321,    87,     0,   304,    90,   305,    89,     0,   334,     0,
     0,   306,     0,   308,     0,   306,    91,   308,     0,   308,
     0,   309,     0,   310,     0,   308,    19,   310,     0,   308,
    20,   310,     0,   311,     0,   310,    18,   311,     0,   312,
     0,   311,    17,   312,     0,   313,     0,    21,   313,     0,
   314,     0,   313,    16,   313,     0,   315,     0,    95,   315,
     0,    99,   315,     0,   314,    99,   315,     0,   314,    95,
   315,     0,   316,     0,   315,    94,   316,     0,   315,    92,
   316,     0,   317,     0,   317,    22,   316,     0,   318,     0,
   317,    23,   318,     0,   333,     0,   326,     0,   303,     0,
   329,     0,   319,     0,    90,   308,    89,     0,   320,     0,
    12,     0,    13,     0,    11,     0,     6,     0,     7,     0,
     8,     0,     9,     0,    10,     0,   314,     0,   314,     0,
   314,     0,   314,     0,   335,    90,   327,    89,     0,   335,
    90,   327,    89,     0,   328,     0,   327,    91,   328,     0,
   308,     0,   304,   331,     0,   303,   331,     0,   326,   331,
     0,   334,   331,     0,   325,   331,     0,    90,    93,    89,
     0,    90,   332,    93,    89,     0,    90,    93,   332,    89,
     0,    90,   332,    93,   332,    89,     0,   314,     0,   334,
     0,   335,     0,     3,     0,     4,     0,     3,     0,     4,
     0,   320,     0,    95,   320,     0,    99,   320,     0,    11,
     0,    12,     0,    13,     0,     6,     0,     0,     6,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   256,   257,   260,   261,   265,   292,   293,   301,   302,   305,
   313,   324,   329,   333,   346,   352,   366,   370,   374,   378,
   384,   385,   388,   391,   410,   416,   425,   429,   435,   441,
   445,   448,   449,   450,   451,   452,   453,   454,   455,   456,
   461,   466,   472,   473,   474,   475,   476,   479,   480,   481,
   482,   483,   484,   485,   486,   487,   488,   489,   490,   491,
   492,   493,   494,   495,   498,   503,   509,   511,   512,   524,
   536,   537,   538,   542,   543,   560,   565,   577,   581,   597,
   613,   629,   647,   650,   653,   659,   660,   661,   668,   672,
   683,   701,   707,   711,   714,   718,   724,   729,   741,   758,
   771,   775,   782,   785,   786,   790,   796,   803,   811,   819,
   831,   836,   845,   846,   849,   850,   853,   857,   864,   868,
   872,   878,   882,   883,   887,   901,   909,   927,   931,   939,
   952,   957,   961,   967,   972,   985,   990,   997,  1002,  1015,
  1027,  1028,  1034,  1041,  1047,  1051,  1064,  1070,  1079,  1080,
  1081,  1082,  1085,  1091,  1112,  1115,  1120,  1125,  1130,  1137,
  1143,  1149,  1158,  1167,  1177,  1188,  1205,  1206,  1209,  1216,
  1225,  1226,  1229,  1238,  1250,  1259,  1275,  1278,  1290,  1305,
  1306,  1308,  1312,  1313,  1316,  1317,  1320,  1335,  1359,  1369,
  1375,  1392,  1393,  1400,  1401,  1404,  1405,  1415,  1418,  1422,
  1429,  1432,  1436,  1443,  1450,  1451,  1454,  1457,  1464,  1471,
  1479,  1483,  1486,  1487,  1490,  1494,  1502,  1505,  1506,  1507,
  1510,  1513,  1514,  1517,  1518,  1521,  1525,  1528,  1529,  1532,
  1533,  1536,  1537,  1543,  1544,  1551,  1552,  1555,  1559,  1562,
  1569,  1570,  1575,  1577,  1591,  1599,  1600,  1601,  1602,  1608,
  1616,  1620,  1621,  1625,  1629,  1633,  1639,  1643,  1649,  1650,
  1654,  1667,  1677,  1686,  1686,  1699,  1700,  1700,  1709,  1713,
  1717,  1718,  1729,  1759,  1760,  1768,  1769,  1779,  1780,  1781,
  1788,  1792,  1798,  1799,  1803,  1807,  1811,  1814,  1815,  1816,
  1820,  1824,  1826,  1826,  1828,  1830,  1831,  1839,  1840,  1841,
  1842,  1843,  1845,  1848,  1853,  1861,  1862,  1864,  1865,  1867,
  1872,  1874,  1875,  1882,  1886,  1908,  1912,  1952,  1960,  1965,
  1971,  1975,  1982,  1983,  1986,  1996,  2000,  2013,  2014,  2018,
  2019,  2023,  2024,  2028,  2035,  2037,  2041,  2048,  2050,  2051,
  2055,  2062,  2064,  2072,  2073,  2077,  2083,  2087,  2087,  2094,
  2095,  2099,  2100,  2103,  2104,  2105,  2108,  2109,  2112,  2113,
  2114,  2115,  2118,  2119,  2120,  2121,  2122,  2123,  2130,  2133,
  2134,  2135,  2139,  2140,  2141,  2149,  2163,  2167,  2170,  2174,
  2181,  2185,  2191,  2191,  2197,  2197,  2206,  2208,  2214,  2219,
  2225,  2232,  2240,  2244,  2251,  2279,  2289,  2295,  2298,  2303,
  2311,  2340,  2355,  2357,  2362,  2369,  2371,  2378,  2380,  2387,
  2389,  2395,  2397,  2405,  2407,  2411,  2415,  2420,  2427,  2429,
  2439,  2446,  2448,  2455,  2457,  2464,  2466,  2468,  2470,  2472,
  2481,  2504,  2509,  2514,  2523,  2530,  2534,  2538,  2542,  2546,
  2553,  2567,  2587,  2611,  2637,  2653,  2669,  2673,  2679,  2693,
  2708,  2716,  2725,  2729,  2738,  2752,  2762,  2772,  2784,  2805,
  2806,  2809,  2816,  2825,  2826,  2830,  2831,  2832,  2833,  2834,
  2835,  2841,  2858,  2865
};

static const char * const yytname[] = {   "$","error","$illegal.","tok_identifier",
"tok_array_identifier","tok_label","tok_integer_const","tok_real_const","tok_dp_const",
"tok_complex_const","tok_dcomplex_const","tok_logical_const","tok_string","tok_hollerith",
"tok_edit_descriptor","tok_letter","tok_relop","tok_AND","tok_OR","tok_EQV",
"tok_NEQV","tok_NOT","tok_power","tok_concat","tok_ACCEPT","tok_ASSIGN","tok_BACKSPACE",
"tok_BLOCK","tok_BLOCKDATA","tok_BYTE","tok_CALL","tok_CHARACTER","tok_CLOSE",
"tok_COMMON","tok_COMPLEX","tok_CONTINUE","tok_DATA","tok_DIMENSION","tok_DO",
"tok_DOUBLE","tok_DOUBLECOMPLEX","tok_DOUBLEPRECISION","tok_DOWHILE","tok_ELSE",
"tok_ELSEIF","tok_END","tok_ENDDO","tok_ENDFILE","tok_ENDIF","tok_ENTRY","tok_EQUIVALENCE",
"tok_EXTERNAL","tok_FILE","tok_FORMAT","tok_FUNCTION","tok_GO","tok_GOTO","tok_IF",
"tok_IMPLICIT","tok_INCLUDE","tok_INQUIRE","tok_INTEGER","tok_INTRINSIC","tok_LOGICAL",
"tok_NAMELIST","tok_NONE","tok_OPEN","tok_PARAMETER","tok_PAUSE","tok_POINTER",
"tok_PRECISION","tok_PRINT","tok_PROGRAM","tok_READ","tok_REAL","tok_RETURN",
"tok_REWIND","tok_SAVE","tok_STOP","tok_SUBROUTINE","tok_THEN","tok_TO","tok_TYPE",
"tok_WHILE","tok_WRITE","tok_illegal","tok_empty","EOS","REDUCE","')'","'('",
"','","'*'","':'","'/'","'-'","'='","'.'","'$'","'+'","'<'","'>'","prog_body",
"stmt_list","stmt_list_item","ordinary_stmt","stmt","unlabeled_stmt","subprogram_header",
"end_stmt","unlabeled_end_stmt","include_stmt","specification_stmt","anywhere_stmt",
"specif_stmt","executable_stmt","transfer_stmt","nontransfer_stmt","restricted_stmt",
"restricted_nontransfer_stmt","else_or_endif_stmt","prog_stmt","@1","entry_stmt",
"function_stmt","unlabeled_function_stmt","typed_function_handle","plain_function_handle",
"function_keyword","type_name","subroutine_stmt","unlabeled_subroutine_stmt",
"subroutine_handle","dummy_argument_list","non_empty_arg_list","dummy_argument",
"block_data_stmt","block_data_handle","dimension_stmt","array_declarator_list",
"array_declarator","dim_bound_list","dim_bound_item","equivalence_stmt","@2",
"equivalence_list","equivalence_list_item","equiv_entity","array_equiv_name",
"substring_equiv_name","common_stmt","common_block_list","labeled_common_block",
"common_block_name","common_variable_list","common_list_item","common_entity",
"namelist_stmt","namelist_list","namelist_decl","namelist_name","namelist_var_list",
"namelist_item","type_stmt","arith_type_name","sizeable_type_name","unsizeable_type_name",
"plain_char_type_name","char_type_name","arith_type_decl_list","arith_type_decl_item",
"char_type_decl_list","char_type_decl_item","implicit_handle","implicit_stmt",
"implicit_decl_list","@3","implicit_decl_item","@4","letter_list","letter_list_item",
"len_specification","parameter_stmt","parameter_defn_list","parameter_defn_item",
"@5","external_stmt","external_name_list","intrinsic_stmt","intrinsic_name_list",
"pointer_stmt","pointer_item_list","pointer_item","pointer_name","pointee_name",
"save_stmt","save_list","save_item","data_stmt","data_defn_list","data_defn_item",
"@6","@7","data_defn_assignee_list","data_defn_assignee","data_value_list","data_value",
"data_repeat_factor","data_constant_value","data_dlist","data_dlist_item","data_implied_do_list",
"data_do_loop_bounds","assignment_stmt","@8","@9","lvalue","assign_stmt","unconditional_goto",
"computed_goto","assigned_goto","goto","goto_list","arithmetic_if_stmt","logical_if_stmt",
"block_if_stmt","if_handle","@10","else_if_stmt","@11","@12","else_stmt","end_if_stmt",
"do_stmt","@13","@14","do_handle","do_loop_bounds","enddo_stmt","continue_stmt",
"stop_stmt","pause_stmt","stop_info","write_stmt","@15","@16","write_handle",
"@17","read_stmt","read_handle","accept_stmt","print_stmt","@18","@19","type_output_stmt",
"@20","@21","control_info_list","control_info_item","open_info_list","open_info_item",
"io_list","io_item","io_implied_do_list","open_stmt","@22","close_stmt","@23",
"inquire_stmt","@24","backspace_stmt","backspace_handle","endfile_stmt","endfile_handle",
"rewind_stmt","rewind_handle","unit_id","format_id","format_stmt","@25","format_spec",
"nonempty_format_spec","fmt_spec_item","repeatable_fmt_item","unrepeatable_fmt_item",
"fmt_item_separator","nonstandard_fmt_item","repeat_spec","variable_fmt_item",
"@26","@27","stmt_function_handle","stmt_function_dummy_list","nonempty_stmt_fun_dummy_list",
"stmt_function_dummy_arg","call_stmt","@28","@29","@30","call_handle","subr_arg_list",
"subr_arg","return_stmt","function_reference","fun_or_substr_handle","fun_arg_list",
"nonempty_fun_arg_list","parameter_expr","expr","log_expr","log_disjunct","log_term",
"log_factor","log_primary","arith_expr","term","factor","char_expr","primary",
"literal_const","numeric_const","integer_expr","int_real_dp_expr","int_constant_expr",
"dim_bound_expr","array_element_lvalue","array_element_name","subscript_list",
"subscript","substring_name","substring_lvalue","substring_interval","substr_index_expr",
"variable_name","scalar_name","array_name","symbolic_name","data_constant","nonzero_unsigned_int_const",
"pre_label","label",""
};
#endif

static const short yyr1[] = {     0,
   102,   102,   103,   103,   104,   104,   104,   105,   105,   106,
   106,   107,   107,   107,   107,   107,   108,   108,   108,   108,
   109,   109,   110,   111,   112,   112,   112,   112,   112,   113,
   113,   114,   114,   114,   114,   114,   114,   114,   114,   114,
   115,   115,   116,   116,   116,   116,   116,   117,   117,   117,
   117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
   117,   117,   117,   117,   118,   118,   119,   119,   119,   119,
   120,   120,   120,   122,   121,   123,   123,   124,   125,   125,
   125,   125,   126,   127,   128,   129,   129,   129,   130,   131,
   131,   132,   133,   133,   134,   134,   135,   135,   136,   136,
   137,   137,   138,   139,   139,   140,   141,   141,   142,   142,
   142,   142,   144,   143,   145,   145,   146,   146,   147,   147,
   147,   148,   149,   149,   150,   150,   150,   151,   151,   152,
   153,   153,   153,   154,   154,   155,   155,   156,   156,   157,
   158,   158,   159,   160,   161,   161,   162,   162,   163,   163,
   163,   163,   164,   164,   164,   165,   165,   165,   165,   166,
   166,   166,   166,   166,   167,   168,   169,   169,   170,   170,
   171,   171,   172,   172,   172,   172,   173,   174,   174,   175,
   176,   175,   178,   177,   179,   179,   180,   180,   181,   181,
   181,   182,   182,   183,   183,   185,   184,   186,   187,   187,
   188,   189,   189,   190,   191,   191,   192,   193,   194,   194,
   195,   195,   196,   196,   197,   197,   198,   199,   199,   199,
   201,   202,   200,   203,   203,   204,   204,   205,   205,   206,
   206,   207,   207,   208,   208,   209,   209,   210,   210,   211,
   212,   212,   214,   215,   213,   216,   216,   216,   216,   217,
   218,   219,   219,   220,   220,   220,   221,   221,   222,   222,
   223,   224,   225,   227,   226,   228,   229,   230,   228,   231,
   232,   232,   233,   234,   233,   235,   233,   236,   236,   236,
   237,   237,   238,   238,   239,   240,   241,   242,   242,   242,
   242,   244,   243,   245,   243,   247,   246,   248,   248,   248,
   248,   248,   249,   250,   250,   251,   252,   253,   251,   254,
   255,   256,   254,   257,   257,   258,   258,   259,   259,   259,
   260,   260,   261,   261,   262,   262,   263,   265,   264,   267,
   266,   269,   268,   270,   270,   271,   272,   272,   273,   273,
   274,   274,   275,   276,   276,   277,   277,   279,   278,   280,
   280,   281,   281,   282,   282,   282,   283,   283,   284,   284,
   284,   284,   285,   285,   285,   285,   285,   285,   286,   287,
   287,   287,   289,   290,   288,   291,   292,   292,   293,   293,
   294,   296,   295,   297,   295,   298,   295,   299,   300,   300,
   301,   301,   302,   302,   303,   304,   305,   305,   306,   306,
   307,   308,   309,   309,   309,   310,   310,   311,   311,   312,
   312,   313,   313,   314,   314,   314,   314,   314,   315,   315,
   315,   316,   316,   317,   317,   318,   318,   318,   318,   318,
   318,   319,   319,   319,   319,   320,   320,   320,   320,   320,
   321,   322,   323,   324,   325,   326,   327,   327,   328,   329,
   329,   329,   330,   330,   331,   331,   331,   331,   332,   333,
   333,   334,   335,   336,   336,   337,   337,   337,   337,   337,
   337,   338,   339,   340
};

static const short yyr2[] = {     0,
     1,     0,     1,     2,     1,     1,     1,     1,     1,     2,
     1,     1,     1,     1,     1,     2,     1,     1,     1,     1,
     1,     2,     2,     3,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     0,     4,     3,     6,     1,     3,     6,
     3,     6,     2,     1,     1,     1,     1,     1,     1,     3,
     6,     1,     0,     1,     1,     3,     1,     1,     2,     3,
     2,     1,     3,     1,     3,     4,     1,     3,     1,     3,
     1,     3,     0,     4,     3,     5,     3,     3,     1,     1,
     1,     4,     2,     2,     3,     3,     4,     1,     2,     2,
     3,     2,     1,     1,     2,     1,     2,     1,     1,     3,
     1,     2,     2,     3,     1,     2,     1,     2,     3,     3,
     3,     4,     1,     3,     1,     1,     1,     1,     1,     2,
     1,     2,     1,     1,     1,     3,     1,     3,     1,     1,
     1,     3,     1,     3,     1,     3,     1,     3,     3,     1,
     0,     4,     0,     5,     1,     3,     1,     3,     3,     1,
     3,     5,     3,     1,     3,     0,     4,     3,     1,     3,
     3,     1,     3,     3,     1,     3,     5,     1,     1,     1,
     2,     3,     1,     3,     1,     3,     3,     1,     2,     3,
     0,     0,     6,     1,     3,     1,     1,     1,     3,     1,
     3,     1,     1,     1,     1,     1,     3,     1,     1,     7,
     3,     5,     0,     0,     6,     1,     1,     1,     1,     6,
     4,     6,     7,     3,     6,     7,     1,     2,     2,     4,
    10,     2,     3,     0,     5,     2,     0,     0,     8,     2,
     2,     3,     5,     0,     7,     0,     6,     3,     4,     2,
     3,     5,     3,     2,     2,     3,     3,     0,     1,     1,
     1,     0,     3,     0,     4,     0,     5,     5,     6,     7,
     3,     5,     1,     3,     5,     3,     0,     0,     7,     3,
     0,     0,     7,     1,     3,     3,     1,     1,     3,     3,
     3,     1,     1,     3,     1,     1,     7,     0,     6,     0,
     6,     0,     6,     3,     5,     1,     3,     5,     1,     2,
     3,     5,     1,     1,     1,     1,     1,     0,     6,     0,
     1,     1,     2,     1,     1,     1,     3,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     2,     2,     0,     0,     5,     4,     0,     1,     1,     3,
     1,     0,     3,     0,     5,     0,     6,     2,     1,     3,
     1,     3,     2,     3,     4,     1,     0,     1,     1,     3,
     1,     1,     1,     3,     3,     1,     3,     1,     3,     1,
     2,     1,     3,     1,     2,     2,     3,     3,     1,     3,
     3,     1,     3,     1,     3,     1,     1,     1,     1,     1,
     3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     4,     4,     1,     3,     1,     2,
     2,     2,     2,     2,     3,     4,     4,     5,     1,     1,
     1,     1,     1,     1,     1,     1,     2,     2,     1,     1,
     1,     1,     0,     1
};

static const short yydefact[] = {     0,
     0,   462,   463,     0,     0,   473,   336,     0,   102,   164,
     0,   165,   330,     0,   158,     0,     0,     0,   473,     0,
   163,   161,     0,     0,     0,     0,     0,   339,     0,     0,
   113,     0,   348,    85,     0,   257,     0,   177,     0,   332,
   156,     0,   159,     0,   328,     0,   288,     0,     0,    74,
   303,   157,     0,   343,     0,   288,    92,     0,   296,     7,
     0,     3,     5,     8,    11,    12,     9,    21,     6,    13,
    25,    29,    14,    41,    42,    15,    65,    66,    17,    30,
    18,    78,     0,     0,    84,     0,    19,    89,     0,    20,
     0,    32,    33,    34,    35,    36,    86,   153,   155,    87,
    88,     0,    27,    26,    37,    38,    40,    39,    28,    48,
     0,    49,    43,    50,    44,   473,    45,    67,    68,   473,
    71,    72,    73,    69,     0,    70,    51,    46,    52,    55,
   292,    53,     0,    54,    56,    57,    61,    62,    63,    59,
     0,    60,     0,    58,     0,    31,   249,    64,   382,    47,
   247,   248,   246,   460,   461,    16,    10,    22,   436,   437,
   438,   439,   440,   435,   433,   434,     0,   347,     0,   428,
     0,   346,   424,   430,   432,   427,   429,   426,   460,   461,
     0,   101,   464,   465,   388,     0,   133,     0,   139,     0,
   128,     0,     0,   134,   136,   138,   285,     0,     0,   218,
     0,   224,   227,   226,     0,   104,     0,   280,   162,   160,
   276,   270,   266,     0,   267,     0,   340,     0,    23,   284,
   271,     0,     0,     0,   199,     0,   258,   264,     0,     0,
     0,   202,     0,     0,   141,     0,     0,     0,     0,   194,
   196,   289,   291,     0,   290,     0,     0,   205,     0,     0,
   393,     0,     0,   441,   414,   419,   422,     0,   211,     0,
     0,   213,   215,     0,     0,     0,     4,     0,     0,    83,
     0,    99,     0,   170,     0,   167,   169,     0,     0,   175,
     0,   171,   173,     0,     0,     0,     0,    86,    87,    88,
     0,   180,   243,   473,     0,     0,     0,     0,   262,   473,
     0,     0,     0,   460,   461,     0,     0,     0,   294,   323,
   326,   325,   402,   403,   406,   408,   410,   412,     0,     0,
     0,   345,     0,   344,     0,     0,     0,     0,     0,     0,
     0,   454,   377,   453,     0,     0,   304,     0,   451,   397,
   450,     0,   452,     0,   474,     0,     0,   132,     0,   126,
   129,   130,   125,     0,   135,   137,     0,     0,   236,   239,
   238,     0,   217,     0,   219,     0,   221,   103,     0,   278,
     0,     0,   283,   272,    76,    93,     0,     0,   198,     0,
   350,     0,    24,     0,   201,     0,     0,   140,   142,   143,
   145,   147,     0,     0,   193,     0,     0,   287,     0,   208,
   204,     0,   306,   307,     0,   415,   416,     0,     0,     0,
     0,     0,   394,     0,   212,     0,   286,   310,   311,     0,
    79,    93,    81,    93,    90,    93,   100,   149,     0,   472,
   154,     0,   166,   190,     0,   150,     0,     0,     0,   151,
   179,   183,   178,   181,     0,     0,     0,   254,   473,     0,
     0,   263,     0,   274,     0,   411,     0,   325,   293,     0,
     0,     0,     0,     0,     0,     0,   462,   463,     0,   314,
   317,   344,     0,   301,     0,     0,   334,     0,   337,     0,
   341,   384,   473,     0,   389,   391,   383,     0,   459,     0,
     0,   378,   379,   426,   449,     0,   447,   431,     0,     0,
   398,   399,   412,   425,     0,     0,     0,   131,   127,   111,
     0,   107,   444,   109,     0,   220,   225,     0,   105,   279,
     0,     0,    98,     0,    94,    95,    97,     0,     0,   120,
   121,   119,   114,     0,   200,   370,   359,   360,   358,   365,
     0,   363,   366,   364,     0,   367,   369,     0,   373,     0,
   351,   352,   354,   355,   356,   368,   361,   362,     0,     0,
   203,   144,   146,   148,     0,   318,     0,     0,   195,     0,
     0,   206,     0,    75,   418,   417,   421,   420,   423,   216,
   214,     0,     0,     0,     0,     0,   168,     0,   443,     0,
   176,   172,   174,   152,     0,     0,   244,     0,   473,   259,
     0,   473,   251,   473,     0,     0,   442,     0,     0,   324,
   295,   404,   405,   407,   409,   413,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   386,     0,   455,     0,     0,
   376,     0,   445,     0,   305,   395,     0,   446,     0,     0,
   106,     0,     0,   237,     0,   436,   469,   470,   471,     0,
     0,   222,   228,     0,   230,   466,   235,   234,   232,     0,
   268,     0,     0,   115,     0,     0,   124,     0,   123,     0,
     0,   371,   372,     0,     0,   353,   265,     0,     0,     0,
     0,   192,   197,   401,   210,     0,   209,   308,   312,   297,
     0,     0,     0,   189,   191,   187,     0,   185,   182,     0,
     0,     0,     0,     0,     0,     0,     0,   273,     0,   426,
   298,     0,     0,   315,   316,   302,   335,   338,   342,   385,
   392,     0,   390,   457,   456,     0,   380,   381,   448,   400,
   250,   331,   108,   112,   110,     0,   467,   468,     0,     0,
     0,   277,     0,    77,    96,   118,   117,     0,     0,   357,
   374,   349,   333,   329,   320,   322,   319,   207,     0,     0,
    80,    82,    91,     0,   184,     0,   245,     0,   252,   260,
   255,     0,     0,     0,   281,     0,     0,   299,   387,   458,
     0,     0,   229,   223,   231,   235,     0,   122,   116,     0,
     0,   309,   313,   188,   186,   253,   256,   473,   275,     0,
     0,   300,   240,     0,   269,   375,   321,     0,   282,   327,
   241,     0,     0,   261,   242,     0,     0,     0
};

static const short yydefgoto[] = {   816,
    61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
    71,    72,    73,    74,    75,    76,    77,    78,    79,   250,
    80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
   524,   525,   526,    90,    91,    92,   205,   189,   511,   512,
    93,   223,   378,   528,   529,   530,   531,    94,   190,   191,
   192,   193,   194,   195,    95,   234,   235,   236,   390,   391,
    96,    97,    98,    99,   100,   101,   275,   276,   281,   282,
   102,   103,   291,   596,   292,   595,   697,   698,   433,   104,
   239,   240,   397,   105,   224,   106,   231,   107,   247,   248,
   399,   686,   108,   261,   262,   109,   199,   200,   518,   740,
   201,   202,   652,   653,   654,   655,   358,   359,   203,   781,
   110,   445,   700,   111,   112,   113,   114,   115,   116,   446,
   117,   118,   119,   120,   382,   121,   372,   743,   122,   123,
   124,   605,   371,   125,   606,   126,   127,   128,   129,   244,
   130,   308,   461,   131,   266,   132,   133,   134,   135,   573,
   759,   136,   582,   760,   469,   470,   565,   755,   309,   310,
   311,   137,   237,   138,   186,   139,   230,   140,   141,   142,
   143,   144,   145,   471,   169,   146,   226,   550,   551,   552,
   553,   554,   555,   556,   557,   558,   674,   790,   147,   491,
   492,   493,   148,   330,   624,   722,   149,   484,   485,   150,
   170,   171,   500,   501,   683,   324,   313,   314,   315,   316,
   317,   318,   255,   256,   257,   173,   174,   175,   258,   608,
   590,   514,   151,   176,   496,   497,   177,   152,   332,   490,
   178,   179,   180,   473,   658,   434,   447,   346
};

static const short yypact[] = {  1162,
   -32,-32768,-32768,  1329,   321,-32768,-32768,    25,-32768,-32768,
   232,-32768,-32768,    97,-32768,    10,   137,   232,-32768,    22,
-32768,-32768,    84,    26,    89,   262,    37,-32768,   105,   232,
-32768,   232,-32768,-32768,   117,-32768,   115,-32768,   202,-32768,
-32768,   232,-32768,   129,-32768,   190,   347,   169,   321,-32768,
-32768,-32768,   472,-32768,    71,   347,-32768,   321,-32768,-32768,
  1247,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   232,   232,-32768,   222,-32768,-32768,   232,-32768,
    95,-32768,-32768,-32768,-32768,-32768,   232,   200,-32768,   114,
    75,   432,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   201,-32768,-32768,-32768,-32768,   199,-32768,-32768,-32768,  1406,
-32768,-32768,-32768,-32768,   192,-32768,-32768,-32768,-32768,-32768,
   807,-32768,   525,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   300,-32768,   509,-32768,   617,-32768,-32768,-32768,   233,-32768,
   245,-32768,-32768,   256,   275,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   831,-32768,    32,   245,
   292,   316,-32768,-32768,-32768,   245,-32768,-32768,   320,   322,
   412,-32768,-32768,-32768,-32768,   350,-32768,    66,-32768,   150,
-32768,   232,    82,-32768,   352,   358,-32768,    49,   134,-32768,
   126,-32768,-32768,-32768,   159,-32768,   358,   412,-32768,-32768,
-32768,-32768,-32768,   371,-32768,   372,-32768,   375,-32768,-32768,
-32768,   230,   378,   231,-32768,   404,-32768,-32768,   417,   437,
   267,-32768,   232,   -29,-32768,   232,   465,   232,   270,-32768,
-32768,-32768,-32768,   420,-32768,   232,   281,-32768,   287,   232,
-32768,  1142,  1142,   112,   204,-32768,   353,   474,-32768,   232,
   290,-32768,-32768,   476,   310,   479,-32768,   273,   315,-32768,
   317,-32768,   498,-32768,   337,-32768,   358,   600,    81,   518,
   338,-32768,   304,   232,   344,   531,   522,-32768,   527,-32768,
   346,-32768,-32768,-32768,   -27,   412,   579,   545,-32768,-32768,
   412,   543,   538,-32768,-32768,  1030,   807,   548,   546,-32768,
-32768,   368,-32768,   618,   622,-32768,   624,   112,   732,   351,
   732,-32768,   554,   368,   732,   555,   732,   557,   123,   558,
   866,-32768,   866,-32768,   831,   252,-32768,   807,-32768,   756,
-32768,  1142,-32768,   831,-32768,   565,   732,-32768,   553,-32768,
-32768,   232,-32768,   196,-32768,-32768,   901,   559,-32768,-32768,
-32768,   275,-32768,   137,-32768,   137,-32768,-32768,   232,   560,
   831,   831,-32768,-32768,-32768,   118,   232,   359,-32768,   232,
  1040,   831,-32768,   732,-32768,   232,   561,-32768,-32768,   232,
-32768,   562,   732,   249,-32768,   232,   552,-32768,   563,-32768,
-32768,   169,-32768,-32768,   574,   204,   204,  1142,  1142,  1142,
  1142,  1142,-32768,   568,-32768,   103,-32768,-32768,-32768,   732,
-32768,   118,-32768,   118,-32768,   118,-32768,-32768,   232,-32768,
-32768,   925,-32768,-32768,    81,-32768,   232,    81,   366,-32768,
-32768,-32768,-32768,-32768,   831,   343,   412,-32768,-32768,   575,
   577,-32768,   576,-32768,  1030,   624,   578,   252,-32768,   807,
   581,   831,   831,   831,   831,  1030,   556,   580,   365,-32768,
-32768,   368,   582,-32768,   807,   376,-32768,   398,-32768,   434,
-32768,-32768,-32768,   435,-32768,   368,-32768,   936,   112,   584,
   583,   588,-32768,   450,   368,   454,-32768,-32768,   383,   586,
   589,   368,     9,-32768,   460,   585,   469,-32768,-32768,-32768,
   477,-32768,   112,   590,   205,-32768,-32768,   139,-32768,-32768,
   254,   266,-32768,   592,   594,-32768,-32768,   481,   595,   245,
-32768,   597,-32768,   598,-32768,-32768,-32768,-32768,-32768,-32768,
  1040,-32768,-32768,-32768,   664,-32768,-32768,   665,-32768,   602,
  1040,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   282,   484,
-32768,-32768,-32768,-32768,   488,-32768,   593,   605,-32768,   831,
   232,-32768,   807,-32768,   204,   204,-32768,-32768,-32768,-32768,
-32768,   807,   492,   606,   607,   608,-32768,   609,   112,   610,
-32768,-32768,-32768,-32768,   679,   513,   368,   960,-32768,-32768,
   493,-32768,-32768,-32768,   831,   613,   112,   612,   807,-32768,
-32768,   618,   618,   622,-32768,   685,   257,   732,   772,   399,
   619,   621,   623,   626,   412,-32768,   791,-32768,   615,   971,
-32768,   585,-32768,   831,-32768,-32768,   831,-32768,   627,   628,
-32768,   901,   995,-32768,   625,   630,-32768,-32768,-32768,   248,
   248,   614,-32768,   631,-32768,-32768,   634,-32768,-32768,   632,
-32768,   633,   118,-32768,   232,   232,-32768,   756,-32768,   232,
  1083,-32768,-32768,  1030,   640,-32768,-32768,   641,   643,   232,
   772,-32768,-32768,   368,-32768,   629,   358,   546,   546,-32768,
   644,   646,   659,-32768,-32768,   616,   503,-32768,-32768,   662,
  1030,   663,   412,   668,   511,   412,   296,-32768,  1030,   655,
-32768,   807,   418,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   669,-32768,-32768,-32768,   681,-32768,-32768,-32768,   368,
-32768,-32768,-32768,-32768,-32768,  1030,-32768,-32768,   139,   677,
  1065,-32768,   637,-32768,-32768,-32768,-32768,   516,   520,-32768,
-32768,-32768,-32768,-32768,-32768,   676,-32768,-32768,   671,   686,
-32768,-32768,-32768,   759,-32768,   679,-32768,   699,-32768,-32768,
-32768,   700,   698,   703,   705,  1030,   423,-32768,-32768,-32768,
   702,   714,-32768,-32768,-32768,-32768,   719,-32768,-32768,   706,
   772,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1030,
   720,-32768,-32768,  1030,-32768,-32768,-32768,   412,-32768,-32768,
   717,   734,  1030,-32768,-32768,   823,   825,-32768
};

static const short yypgoto[] = {-32768,
-32768,   765,-32768,-32768,   826,-32768,-32768,   829,-32768,-32768,
-32768,-32768,   709,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   750,  -100,-32768,-32768,-32768,
  -194,-32768,   182,-32768,-32768,-32768,-32768,    15,-32768,   206,
-32768,-32768,-32768,   177,   -69,-32768,-32768,-32768,   657,  -164,
-32768,   661,  -163,-32768,-32768,-32768,   620,-32768,-32768,   466,
-32768,   -97,-32768,-32768,   -96,   -93,-32768,   428,   -80,   421,
-32768,-32768,-32768,-32768,   263,-32768,-32768,    94,   -12,-32768,
   642,   467,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   459,
-32768,-32768,-32768,-32768,   449,-32768,-32768,  -177,-32768,-32768,
-32768,   500,-32768,   143,-32768,   127,-32768,   369,  -179,-32768,
-32768,-32768,-32768,     0,-32768,-32768,-32768,-32768,-32768,  -422,
-32768,-32768,   861,    33,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   111,-32768,-32768,-32768,-32768,   832,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,  -259,   271,-32768,-32768,  -291,  -431,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,  -131,   119,-32768,-32768,-32768,   354,  -508,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   259,-32768,-32768,-32768,-32768,-32768,-32768,   265,-32768,
-32768,-32768,-32768,-32768,-32768,   120,-32768,   151,   429,   431,
  -278,   -52,  -208,  -339,    31,   573,-32768,  -494,  -546,  -628,
  -520,   251,  -178,-32768,  -326,   264,-32768,-32768,  -139,  -447,
    34,    50,    42,    -7,-32768,  -270,    -6,  -205
};


#define	YYLAST		1490


static const short yytable[] = {   181,
   254,   287,   370,   185,   288,   289,   196,   431,   290,   323,
   207,   326,   208,   328,   334,   457,   204,   505,   360,   361,
   285,   365,   222,   656,   225,   351,   601,   456,   610,   355,
   339,   341,   206,   153,   232,   172,   343,   153,   241,   245,
   629,   155,   676,   406,   407,   155,   499,   263,   245,   154,
   153,   702,     3,   154,   156,   209,   214,   388,   155,   448,
   182,   476,   449,   450,   233,   478,   154,   480,   183,   184,
   577,   578,   579,   183,   184,   268,   269,   183,   184,   172,
   775,   271,    37,   273,   183,   184,   430,   507,   172,   277,
   451,   210,   283,   283,   153,   453,   197,   183,   184,   183,
   184,  -459,   155,   408,   187,   183,   184,   409,   295,   296,
   154,   274,   212,   301,   280,   280,   183,   184,   337,   187,
   183,   184,   338,   220,   560,     2,     3,   751,   159,   160,
   161,   162,   163,   164,   165,   166,     2,     3,   198,     2,
     3,   183,   184,   306,   646,   160,   161,   162,   163,   647,
   648,   649,   300,   153,   768,   737,   738,   259,   303,   348,
   583,   155,   676,   172,   260,   284,   305,   249,   353,   154,
   432,   809,   187,   211,   304,   188,   265,   610,   215,   705,
   349,   272,   726,   620,   196,   196,   516,   616,   355,   351,
   188,   221,   183,   184,     2,     3,   260,   227,   204,   575,
   576,   183,   184,   439,   228,   279,   408,   183,   468,   523,
   409,   482,   167,   229,   483,   782,   366,   252,   187,   367,
   363,   253,   233,   198,   364,   387,   198,   584,   392,   585,
   241,   586,   153,   650,   183,   184,   350,   651,   400,   362,
   155,   600,   405,   188,   656,   368,   656,   659,   154,   369,
   312,   320,   414,   159,   160,   161,   162,   163,   246,     2,
     3,   566,   159,   160,   161,   162,   163,   164,   165,   166,
   462,   463,   462,   463,   302,    34,   283,   306,   489,   238,
   489,   688,   509,   811,   462,   463,   336,   503,   294,   188,
   689,   278,   815,   301,   198,   410,   293,   411,   280,   216,
   462,   463,     2,     3,   513,   159,   160,   161,   162,   163,
   164,   165,   166,   217,   462,   463,   375,   379,   218,   376,
   306,   380,   329,     2,     3,   713,   159,   160,   161,   162,
   163,   164,   165,   166,   331,   360,   361,   568,   342,   396,
   498,   748,   660,   711,   196,   333,   307,   712,   219,   183,
   184,   252,   242,   385,   661,   253,   395,   386,   243,   421,
   396,   207,   422,   204,   335,   204,   494,   401,   527,   532,
   677,   402,   535,   403,   412,   342,   415,   404,   561,   589,
   416,   340,   392,   519,   774,   567,   462,   463,   241,   321,
   667,   322,   669,   357,   252,   438,   418,   153,   253,   153,
   419,   423,   607,   425,   424,   155,   426,   155,   263,  -396,
   167,   344,   168,   154,   527,   154,   527,   345,   527,   721,
   777,   277,   591,   428,   436,   593,   458,   429,   437,   283,
   440,   598,   443,   599,   437,   489,   444,   474,   472,   347,
   472,   475,   356,   274,   472,   533,   472,   357,   486,   534,
   298,   280,   594,   617,   495,   618,   437,   312,   373,   502,
    10,   374,    12,   495,   621,    15,   618,   377,   659,   635,
    20,    21,    22,   460,     2,     3,   625,   159,   160,   161,
   162,   163,   164,   165,   166,   716,   622,   715,   618,   460,
   521,   522,    41,   381,    43,   287,   286,   770,   288,   289,
   773,   559,   290,   383,   778,    52,   398,   645,   460,   802,
   657,     2,     3,   460,   159,   160,   161,   162,   163,   164,
   165,   166,   623,   626,   618,   627,   384,     2,     3,   306,
   159,   160,   161,   162,   163,   164,   165,   166,  -381,   639,
  -381,    10,   633,    12,   634,   254,    15,   305,   638,   757,
   634,    20,    21,    22,   393,   304,   362,   640,   251,   618,
   413,   167,   417,   687,   597,   641,   252,   642,   420,   664,
   253,   665,   678,    41,   618,    43,   679,   489,   680,   312,
   690,   704,   618,   599,   427,   685,    52,     2,     3,   513,
   513,   765,   703,   766,   312,   746,   747,   706,   325,   772,
   322,   599,   812,   252,   788,   430,   634,   253,   789,   435,
   665,   442,   612,   613,   319,   503,   168,   441,   279,     2,
     3,   254,   159,   160,   161,   162,   163,   164,   165,   166,
   217,   452,   454,   455,   459,   464,   460,   306,   465,   466,
   477,   479,   710,   481,   487,   506,   508,   570,   254,   515,
   520,  -464,   564,   571,   562,   527,   607,   532,   532,   807,
   574,   580,   532,   603,   602,   728,   604,   611,   609,   672,
   673,   631,   756,   305,   636,  -465,   630,   619,   632,   637,
   662,   304,   643,   589,   663,   666,   668,   670,   681,   684,
   675,   682,   312,   696,   691,   692,   693,   694,   695,   708,
-32768,   312,   709,   724,   739,   717,   327,   718,   322,   719,
   764,   252,   720,   731,   732,   253,   787,   758,   742,   744,
   736,  -472,   741,   607,   707,  -233,   752,   753,   312,   754,
   761,   657,   762,   786,   467,   468,   312,   159,   160,   161,
   162,   163,   164,   165,   166,   763,   486,   607,   767,   769,
   776,   589,   306,   495,   771,   779,   730,   792,     2,     3,
   589,   159,   160,   161,   162,   163,   164,   165,   166,   780,
   784,   791,   793,   794,     2,     3,   306,   159,   160,   161,
   162,   163,   164,   165,   166,   796,   797,   495,   798,   799,
   803,   808,   306,     2,     3,   800,   159,   160,   161,   162,
   163,   164,   165,   166,   804,   805,   806,   813,   810,     2,
     3,   306,   159,   160,   161,   162,   163,   164,   165,   166,
   814,   167,   817,   322,   818,   267,   252,   306,   299,   157,
   253,   312,   158,     2,     3,   270,   159,   160,   161,   162,
   163,   164,   165,   166,   745,   167,   749,   733,   488,   354,
   252,   306,   352,   389,   253,   563,   587,   592,   699,   795,
   572,   167,   569,   322,   581,   517,   252,   785,     2,     3,
   253,   159,   160,   161,   162,   163,   164,   165,   166,   394,
   167,   783,   483,   644,   213,   252,   801,   264,   714,   253,
   727,   723,   614,   735,   671,   615,   307,   729,     0,     0,
     0,   252,     0,     2,     3,   253,   159,   160,   161,   162,
   163,   164,   165,   166,   504,     0,     0,     0,     0,     0,
   167,     0,     0,     0,     0,   252,     0,     2,     3,   253,
   159,   160,   161,   162,   163,   164,   165,   166,     2,     3,
     0,   159,   160,   161,   162,   163,   164,   165,   166,     0,
     0,     0,     0,     0,     0,   167,     0,     0,   488,     0,
   252,     0,     2,     3,   253,   159,   160,   161,   162,   163,
   164,   165,   166,     2,     3,     0,   159,   160,   161,   162,
   163,   164,   165,   166,     0,     0,     0,     0,     0,     0,
   167,     0,   510,     0,     0,   252,     0,     2,     3,   253,
   159,   160,   161,   162,   163,   164,   165,   166,     0,     0,
     0,     0,     0,     0,   167,     0,   588,     0,     0,   252,
     0,     0,     0,   253,   628,   167,     0,     0,     0,     0,
   252,     0,     2,     3,   253,   159,   160,   161,   162,   163,
   164,   165,   166,     0,     0,   536,     0,     0,     0,   167,
   701,   537,   538,   539,   252,     0,     0,     0,   253,   725,
   167,     0,   540,     0,     0,   252,     0,   183,   184,   253,
   159,   160,   161,   162,   163,   647,   648,   649,     0,     0,
     0,     0,     0,     0,   167,     0,   734,     0,   536,   252,
     0,     0,     0,   253,   537,   538,   539,     0,     0,     0,
     0,     0,     0,     0,     0,   540,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
     0,     0,     0,     0,   252,     0,     0,     0,   253,   541,
   542,     0,   543,   544,   545,     0,   546,   547,   548,   549,
     0,     0,     0,     0,     2,     3,     0,   159,   160,   161,
   162,   163,   164,   165,   166,     0,     0,     0,     0,   650,
     0,    -2,     1,   651,     2,     3,     4,     0,     0,     0,
     0,   750,   541,   542,     0,   543,   544,   545,     0,   546,
   547,   548,   549,     0,     0,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
    30,    31,    32,     0,    33,    34,    35,    36,    37,    38,
    39,    40,    41,    42,    43,    44,     0,    45,    46,    47,
    48,   167,    49,    50,    51,    52,    53,    54,    55,    56,
    57,     0,     0,    58,     0,    59,    -1,     1,    60,     2,
     3,     4,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    28,    29,    30,    31,    32,     0,    33,
    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
    44,     0,    45,    46,    47,    48,     0,    49,    50,    51,
    52,    53,    54,    55,    56,    57,     0,     0,    58,     1,
    59,     2,     3,    60,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
     0,    33,    34,    35,    36,    37,    38,     0,    40,    41,
    42,    43,    44,     0,    45,    46,    47,    48,     0,    49,
    50,    51,    52,    53,    54,    55,    56,    57,     2,     3,
    58,     0,    59,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
     6,     7,     0,     0,     0,    11,     0,    13,     0,     0,
    16,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   297,     0,    28,     0,     0,     0,     0,     0,     0,     0,
    35,    36,    37,     0,     0,    40,     0,     0,     0,     0,
     0,    45,     0,    47,     0,     0,    49,     0,    51,     0,
    53,    54,     0,    56,     0,   298,     0,    58,     0,    59
};

static const short yycheck[] = {     6,
    53,   102,   208,    11,   102,   102,    14,   278,   102,   141,
    18,   143,    19,   145,   154,   307,    17,   344,   198,   198,
   101,   199,    30,   518,    32,   190,   449,   306,   460,   193,
   170,   171,    18,     0,    42,     5,   176,     4,    46,    47,
   488,     0,   551,   252,   253,     4,   338,    55,    56,     0,
    17,   598,     4,     4,    87,    34,    24,    87,    17,    87,
    36,   321,    90,    91,    94,   325,    17,   327,     3,     4,
   410,   411,   412,     3,     4,    83,    84,     3,     4,    49,
   709,    89,    57,    91,     3,     4,     6,   347,    58,    97,
   296,    70,   100,   101,    61,   301,    87,     3,     4,     3,
     4,    93,    61,    95,    23,     3,     4,    99,   116,   116,
    61,    97,    87,   120,   100,   101,     3,     4,    87,    23,
     3,     4,    91,    87,   384,     3,     4,   674,     6,     7,
     8,     9,    10,    11,    12,    13,     3,     4,    90,     3,
     4,     3,     4,    21,     6,     7,     8,     9,    10,    11,
    12,    13,   120,   120,   701,   650,   651,    87,   125,    94,
   420,   120,   671,   133,    94,    91,   125,    49,    87,   120,
    90,   800,    23,    90,   125,    94,    58,   609,    90,   602,
   188,    87,   630,   475,   192,   193,   364,   466,   352,   354,
    94,    87,     3,     4,     3,     4,    94,    81,   199,   408,
   409,     3,     4,   284,    90,    92,    95,     3,     4,    92,
    99,    89,    90,    12,    92,   736,    91,    95,    23,    94,
    87,    99,    94,    90,    91,   233,    90,   422,   236,   424,
   238,   426,   199,    95,     3,     4,    87,    99,   246,   198,
   199,   447,   250,    94,   739,    87,   741,   518,   199,    91,
   131,   133,   260,     6,     7,     8,     9,    10,    90,     3,
     4,   393,     6,     7,     8,     9,    10,    11,    12,    13,
    19,    20,    19,    20,    83,    54,   284,    21,   331,    90,
   333,   573,    87,   804,    19,    20,   167,   340,    90,    94,
   582,    92,   813,   300,    90,    92,    96,    94,   284,    38,
    19,    20,     3,     4,   357,     6,     7,     8,     9,    10,
    11,    12,    13,    52,    19,    20,    87,    87,    57,    90,
    21,    91,    90,     3,     4,   617,     6,     7,     8,     9,
    10,    11,    12,    13,    90,   515,   515,    89,    23,    91,
    89,   668,    89,    87,   352,    90,    90,    91,    87,     3,
     4,    95,     6,    87,    89,    99,    87,    91,    12,    87,
    91,   369,    90,   364,    90,   366,   333,    87,   376,   377,
    89,    91,   380,    87,    22,    23,    87,    91,   386,   432,
    91,    90,   390,   369,    89,   393,    19,    20,   396,    90,
   530,    92,   532,    90,    95,    92,    87,   364,    99,   366,
    91,    87,   455,    87,    90,   364,    90,   366,   416,    90,
    90,    90,    92,   364,   422,   366,   424,     6,   426,   625,
   712,   429,   435,    87,    87,   438,   307,    91,    91,   437,
    87,    89,    87,    91,    91,   488,    91,    87,   319,    90,
   321,    91,    91,   429,   325,    87,   327,    90,   329,    91,
    80,   437,    87,    89,   335,    91,    91,   338,    87,   340,
    29,    87,    31,   344,    89,    34,    91,    90,   739,    87,
    39,    40,    41,    91,     3,     4,   483,     6,     7,     8,
     9,    10,    11,    12,    13,    87,    89,   619,    91,    91,
   371,   372,    61,    90,    63,   596,    65,   703,   596,   596,
   706,   382,   596,    87,    87,    74,    87,   515,    91,    87,
   518,     3,     4,    91,     6,     7,     8,     9,    10,    11,
    12,    13,    89,    89,    91,    91,    90,     3,     4,    21,
     6,     7,     8,     9,    10,    11,    12,    13,    89,   506,
    91,    29,    89,    31,    91,   598,    34,   506,    89,   681,
    91,    39,    40,    41,    90,   506,   515,    89,    87,    91,
    87,    90,    87,   571,   445,    89,    95,    91,    90,    89,
    99,    91,    89,    61,    91,    63,    89,   630,    91,   460,
    89,    89,    91,    91,    87,   571,    74,     3,     4,   642,
   643,    89,   599,    91,   475,   665,   666,   604,    90,    89,
    92,    91,   808,    95,    89,     6,    91,    99,    89,    92,
    91,    90,   462,   463,    90,   668,    92,    87,    92,     3,
     4,   674,     6,     7,     8,     9,    10,    11,    12,    13,
    52,    87,    90,    96,    87,    18,    91,    21,    17,    16,
    87,    87,   609,    87,    87,    81,    94,    96,   701,    91,
    91,    96,    91,    91,    94,   663,   709,   665,   666,   791,
    87,    94,   670,    87,    90,   632,    91,    87,    91,     6,
     6,    89,   680,   632,    89,    96,    93,    96,    91,    91,
    89,   632,    93,   736,    91,    91,    90,    90,    96,   570,
    89,    87,   573,    15,    89,    89,    89,    89,    89,    87,
    16,   582,    91,    89,    91,    87,    90,    87,    92,    87,
    95,    95,    87,    87,    87,    99,    80,    89,    87,    87,
    96,    92,    92,   776,   605,    92,    87,    87,   609,    87,
    87,   739,    87,   741,     3,     4,   617,     6,     7,     8,
     9,    10,    11,    12,    13,    87,   627,   800,    87,    87,
    96,   804,    21,   634,    87,    87,   637,    87,     3,     4,
   813,     6,     7,     8,     9,    10,    11,    12,    13,    89,
    94,    96,    87,    15,     3,     4,    21,     6,     7,     8,
     9,    10,    11,    12,    13,    87,    87,   668,    91,    87,
    89,   798,    21,     3,     4,    91,     6,     7,     8,     9,
    10,    11,    12,    13,    91,    87,   101,    91,    89,     3,
     4,    21,     6,     7,     8,     9,    10,    11,    12,    13,
    87,    90,     0,    92,     0,    61,    95,    21,   120,     4,
    99,   712,     4,     3,     4,    86,     6,     7,     8,     9,
    10,    11,    12,    13,   663,    90,   670,   642,    93,   193,
    95,    21,   192,   234,    99,   390,   429,   437,   596,   766,
   402,    90,   396,    92,   416,   366,    95,   741,     3,     4,
    99,     6,     7,     8,     9,    10,    11,    12,    13,   238,
    90,   739,    92,   515,    24,    95,   776,    56,   618,    99,
   632,   627,   464,   643,   541,   465,    90,   634,    -1,    -1,
    -1,    95,    -1,     3,     4,    99,     6,     7,     8,     9,
    10,    11,    12,    13,   342,    -1,    -1,    -1,    -1,    -1,
    90,    -1,    -1,    -1,    -1,    95,    -1,     3,     4,    99,
     6,     7,     8,     9,    10,    11,    12,    13,     3,     4,
    -1,     6,     7,     8,     9,    10,    11,    12,    13,    -1,
    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,
    95,    -1,     3,     4,    99,     6,     7,     8,     9,    10,
    11,    12,    13,     3,     4,    -1,     6,     7,     8,     9,
    10,    11,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
    90,    -1,    92,    -1,    -1,    95,    -1,     3,     4,    99,
     6,     7,     8,     9,    10,    11,    12,    13,    -1,    -1,
    -1,    -1,    -1,    -1,    90,    -1,    92,    -1,    -1,    95,
    -1,    -1,    -1,    99,    89,    90,    -1,    -1,    -1,    -1,
    95,    -1,     3,     4,    99,     6,     7,     8,     9,    10,
    11,    12,    13,    -1,    -1,     6,    -1,    -1,    -1,    90,
    91,    12,    13,    14,    95,    -1,    -1,    -1,    99,    89,
    90,    -1,    23,    -1,    -1,    95,    -1,     3,     4,    99,
     6,     7,     8,     9,    10,    11,    12,    13,    -1,    -1,
    -1,    -1,    -1,    -1,    90,    -1,    92,    -1,     6,    95,
    -1,    -1,    -1,    99,    12,    13,    14,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
    -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    99,    90,
    91,    -1,    93,    94,    95,    -1,    97,    98,    99,   100,
    -1,    -1,    -1,    -1,     3,     4,    -1,     6,     7,     8,
     9,    10,    11,    12,    13,    -1,    -1,    -1,    -1,    95,
    -1,     0,     1,    99,     3,     4,     5,    -1,    -1,    -1,
    -1,    89,    90,    91,    -1,    93,    94,    95,    -1,    97,
    98,    99,   100,    -1,    -1,    24,    25,    26,    27,    28,
    29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
    39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
    49,    50,    51,    -1,    53,    54,    55,    56,    57,    58,
    59,    60,    61,    62,    63,    64,    -1,    66,    67,    68,
    69,    90,    71,    72,    73,    74,    75,    76,    77,    78,
    79,    -1,    -1,    82,    -1,    84,     0,     1,    87,     3,
     4,     5,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
    44,    45,    46,    47,    48,    49,    50,    51,    -1,    53,
    54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
    64,    -1,    66,    67,    68,    69,    -1,    71,    72,    73,
    74,    75,    76,    77,    78,    79,    -1,    -1,    82,     1,
    84,     3,     4,    87,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
    32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
    42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -1,    53,    54,    55,    56,    57,    58,    -1,    60,    61,
    62,    63,    64,    -1,    66,    67,    68,    69,    -1,    71,
    72,    73,    74,    75,    76,    77,    78,    79,     3,     4,
    82,    -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
    25,    26,    -1,    -1,    -1,    30,    -1,    32,    -1,    -1,
    35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    45,    -1,    47,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    55,    56,    57,    -1,    -1,    60,    -1,    -1,    -1,    -1,
    -1,    66,    -1,    68,    -1,    -1,    71,    -1,    73,    -1,
    75,    76,    -1,    78,    -1,    80,    -1,    82,    -1,    84
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/usr/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 5:
#line 266 "fortran.y"
{
				/* Create id token for prog if unnamed. */
			  if(current_module_hash == -1) {
			    implied_id_token(&(yyvsp[0]),unnamed_prog);
			    def_function(
					 type_PROGRAM,	/* type */
					 size_DEFAULT,	/* size */
					 (char *)NULL,	/* size text */
					 &(yyvsp[0]),		/* name */
					 (Token*)NULL);	/* args */
			    current_module_hash =
			      def_curr_module(&(yyvsp[0]));
			    current_module_type = type_PROGRAM;
			  }

					/* Handle END statement */
			  if(curr_stmt_class == tok_END) {
			    if(prev_stmt_class != tok_RETURN)
			      do_RETURN(current_module_hash,&(yyvsp[0]));
			    END_processing(&(yyval));
			    goto_flag = prev_goto = FALSE;
			  }
			  prev_stmt_class = curr_stmt_class;
			  integer_context = FALSE;
			  true_prev_stmt_line_num = yyval.line_num;
			;
    break;}
case 10:
#line 306 "fortran.y"
{
#ifdef CHECK_LABELS
			  def_label(&(yyvsp[-1]));
#endif
			  if(executable_stmt)
			    prev_goto = goto_flag;
			;
    break;}
case 11:
#line 314 "fortran.y"
{
			  if(executable_stmt) {
			    if(prev_goto)
				syntax_error(yyvsp[0].line_num, NO_COL_NUM,
					"No path to this statement");
			    prev_goto = goto_flag;
			  }
			;
    break;}
case 12:
#line 325 "fortran.y"
{
			    exec_stmt_count = 0;
			    executable_stmt = FALSE;
			;
    break;}
case 13:
#line 330 "fortran.y"
{
			    executable_stmt = FALSE;
			;
    break;}
case 14:
#line 334 "fortran.y"
{	/* handle statement functions correctly */
			  if(is_true(STMT_FUNCTION_EXPR, yyvsp[0].TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN) {
			    stmt_sequence_no = SEQ_STMT_FUN;
			    executable_stmt = FALSE;
			  }
			  else {
			    stmt_sequence_no = SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			  }
			;
    break;}
case 15:
#line 347 "fortran.y"
{
			    stmt_sequence_no = SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			;
    break;}
case 16:
#line 353 "fortran.y"
{
			    executable_stmt = TRUE;
			    if(stmt_sequence_no == 0)
			      stmt_sequence_no = SEQ_HEADER;
			    complex_const_allowed = FALSE; /* turn off flags */
			    inside_format=FALSE;
			    integer_context = FALSE;
			    in_assignment_stmt = FALSE;
			    yyval.line_num = prev_stmt_line_num; /* best guess */
			    yyerrok; /* (error message already given) */
			;
    break;}
case 17:
#line 367 "fortran.y"
{
			    current_module_type = type_PROGRAM;
			;
    break;}
case 18:
#line 371 "fortran.y"
{
			    current_module_type = type_SUBROUTINE;
			;
    break;}
case 19:
#line 375 "fortran.y"
{
			    current_module_type = type_SUBROUTINE;
			;
    break;}
case 20:
#line 379 "fortran.y"
{
			    current_module_type = type_BLOCK_DATA;
			;
    break;}
case 24:
#line 392 "fortran.y"
{
#ifdef ALLOW_INCLUDE
			  if(f77_include) {
			      nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num);
			  }
 			  open_include_file(yyvsp[-1].value.string,yyvsp[-2].line_num);
#else
			  syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
				"statement not permitted");
#endif
 			;
    break;}
case 25:
#line 411 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				stmt_sequence_no = SEQ_IMPLICIT;
			     }
			;
    break;}
case 26:
#line 417 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				   stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     else if(stmt_sequence_no > SEQ_SPECIF) {
			       check_stmt_sequence(&(yyvsp[0]),SEQ_SPECIF);
			     }
			;
    break;}
case 27:
#line 426 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[0]),SEQ_IMPLICIT);
			;
    break;}
case 28:
#line 430 "fortran.y"
{
			     if(stmt_sequence_no < SEQ_STMT_FUN) {
				stmt_sequence_no = SEQ_STMT_FUN;
		 	     }
			;
    break;}
case 29:
#line 436 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[0]),SEQ_SPECIF);
			;
    break;}
case 30:
#line 442 "fortran.y"
{
			     goto_flag = prev_goto = FALSE;
			;
    break;}
case 41:
#line 463 "fortran.y"
{
			    goto_flag=TRUE;
			;
    break;}
case 42:
#line 467 "fortran.y"
{
			    goto_flag=FALSE;
			;
    break;}
case 65:
#line 500 "fortran.y"
{
			    goto_flag=FALSE;
			;
    break;}
case 66:
#line 504 "fortran.y"
{
			    prev_goto = goto_flag =FALSE;
			;
    break;}
case 69:
#line 513 "fortran.y"
{	/* Flag DO w/o label or DO WHILE forms here */
			  if(is_true(NONSTD_USAGE_FLAG,yyvsp[0].TOK_flags))
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
#else
			    syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
				    "Nonstandard syntax");
#endif
			;
    break;}
case 70:
#line 525 "fortran.y"
{
#ifdef ALLOW_DO_ENDDO
			    if(f77_do_enddo)
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
#else
			    syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
				    "Nonstandard syntax");
#endif
			;
    break;}
case 74:
#line 542 "fortran.y"
{check_seq_header(&(yyvsp[0]));;
    break;}
case 75:
#line 544 "fortran.y"
{
			     def_function(
					  type_PROGRAM,	/* type */
					  size_DEFAULT,	/* size */
					  (char *)NULL,	/* size text */
					  &(yyvsp[-1]),	/* name */
					  (Token*)NULL);/* args */
			     current_module_hash =
			       def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 76:
#line 561 "fortran.y"
{
			  do_ENTRY(&(yyvsp[-1]),(Token*)NULL
				   ,current_module_hash);
			;
    break;}
case 77:
#line 566 "fortran.y"
{
			  do_ENTRY(&(yyvsp[-4]),&(yyvsp[-2])
				   ,current_module_hash);
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("entry stmt",&(yyvsp[-2]));
#endif
			;
    break;}
case 79:
#line 582 "fortran.y"
{
			     if(f77_function_noparen) {
				nonstandard(yyvsp[-1].line_num,
			     (unsigned)(yyvsp[-1].col_num+strlen(token_name(yyvsp[-1]))));
				msg_tail(": parentheses required");
			     }
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      &(yyvsp[-1]),
				      (Token*)NULL);
			 current_module_hash=
			   def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 80:
#line 599 "fortran.y"
{
			 def_function(
				      current_datatype,
				      current_typesize,
				      current_len_text,
				      &(yyvsp[-4]),
				      &(yyvsp[-2]));
			 current_module_hash=
			   def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&(yyvsp[-2]));
#endif
			;
    break;}
case 81:
#line 614 "fortran.y"
{
			     if(f77_function_noparen) {
				nonstandard(yyvsp[-1].line_num,
			      (unsigned)(yyvsp[-1].col_num+strlen(token_name(yyvsp[-1]))));
				msg_tail(": parentheses required");
			     }
			 def_function(
				      type_UNDECL,
				      size_DEFAULT,
				      (char *)NULL,
				      &(yyvsp[-1]),
				      (Token*)NULL);
			 current_module_hash=
			   def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 82:
#line 631 "fortran.y"
{
			 def_function(
				      type_UNDECL,	/* type */
				      size_DEFAULT,	/* size */
				      (char *)NULL,	/* size text */
				      &(yyvsp[-4]),		/* name */
				      &(yyvsp[-2]));		/* args */
			 current_module_hash=
			   def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&(yyvsp[-2]));
#endif
			;
    break;}
case 85:
#line 654 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 90:
#line 673 "fortran.y"
{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       &(yyvsp[-1]),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 91:
#line 685 "fortran.y"
{
			  def_function(
				       type_SUBROUTINE,
				       size_DEFAULT,
				       (char *)NULL,
				       &(yyvsp[-4]),
				       &(yyvsp[-2]));
			  current_module_hash=
			    def_curr_module(&(yyvsp[-4]));
#ifdef DEBUG_PARSER
			  if(debug_parser)
			    print_exprlist("subroutine stmt",&(yyvsp[-2]));
#endif
			;
    break;}
case 92:
#line 702 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 93:
#line 708 "fortran.y"
{
			    yyval.next_token = (Token*)NULL;
			;
    break;}
case 95:
#line 715 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 96:
#line 719 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 97:
#line 725 "fortran.y"
{
			     def_arg_name(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			;
    break;}
case 98:
#line 730 "fortran.y"
{
			     yyval.TOK_type = type_byte(class_LABEL,type_LABEL);
			     yyval.size = size_DEFAULT;
			     yyval.TOK_flags = 0;
			     yyval.left_token = (Token *)NULL;
			;
    break;}
case 99:
#line 742 "fortran.y"
{
				  /* form name %DATnn */
			  ++block_data_number;
			  (void)sprintf(unnamed_block_data+4,"%02d",
					block_data_number%100);
			  implied_id_token(&(yyval),unnamed_block_data);

			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       &(yyval),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyval));
			;
    break;}
case 100:
#line 759 "fortran.y"
{
			  def_function(
				       type_BLOCK_DATA,
				       size_DEFAULT,
				       (char *)NULL,
				       &(yyvsp[-1]),
				       (Token*)NULL);
			  current_module_hash=
			    def_curr_module(&(yyvsp[-1]));
			;
    break;}
case 101:
#line 772 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 102:
#line 776 "fortran.y"
{
			  check_seq_header(&(yyvsp[0]));
			;
    break;}
case 106:
#line 791 "fortran.y"
{
			     def_array_dim(&(yyvsp[-3]),&(yyvsp[-1]));
			;
    break;}
case 107:
#line 798 "fortran.y"
{
			     yyval.TOK_dims = 1;
			     yyval.TOK_elts = yyvsp[0].TOK_elts;
			     yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 108:
#line 804 "fortran.y"
{
			     yyval.TOK_dims = yyvsp[-2].TOK_dims + 1; /* one more dimension */
			     yyval.TOK_elts = yyvsp[-2].TOK_elts * yyvsp[0].TOK_elts;
			     yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 109:
#line 812 "fortran.y"
{
			      if( datatype_of(yyvsp[0].TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[0].TOK_flags) )
				yyval.TOK_elts = yyvsp[0].value.integer;
			      else
				yyval.TOK_elts = 0;
			;
    break;}
case 110:
#line 820 "fortran.y"
{	/* avoid getting 0 - 0 + 1 = 1 if bounds nonconstant */
			      if( datatype_of(yyvsp[-2].TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[-2].TOK_flags)
				 && datatype_of(yyvsp[0].TOK_type) == type_INTEGER
				 && is_true(EVALUATED_EXPR,yyvsp[0].TOK_flags) )
				yyval.TOK_elts = yyvsp[0].value.integer - yyvsp[-2].value.integer + 1;
			      else
				yyval.TOK_elts = 0;

			      yyval.left_token = add_tree_node(&(yyvsp[-1]),&(yyvsp[-2]),&(yyvsp[0]));
			;
    break;}
case 111:
#line 832 "fortran.y"
{
			     yyval.TOK_elts = 0;
			     yyval.left_token = (Token *)NULL;
			;
    break;}
case 112:
#line 837 "fortran.y"
{
			     yyval.TOK_elts = 0;
			     yyvsp[0].left_token = (Token *)NULL;
			     yyval.left_token = add_tree_node(&(yyvsp[-1]),&(yyvsp[-2]),&(yyvsp[0]));
			;
    break;}
case 113:
#line 845 "fortran.y"
{equivalence_flag = TRUE;;
    break;}
case 114:
#line 846 "fortran.y"
{equivalence_flag = FALSE;;
    break;}
case 117:
#line 854 "fortran.y"
{
			  equivalence(&(yyvsp[-2]), &(yyvsp[0]));
			;
    break;}
case 118:
#line 858 "fortran.y"
{
			  equivalence(&(yyvsp[-2]), &(yyvsp[0]));
			;
    break;}
case 119:
#line 865 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			;
    break;}
case 120:
#line 869 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			;
    break;}
case 121:
#line 873 "fortran.y"
{
			     def_equiv_name(&(yyvsp[0]));
			;
    break;}
case 125:
#line 888 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			     def_com_block(&(yyval), &(yyvsp[-1]));
			     if(is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
			   	syntax_error(
					     yyvsp[-1].line_num,yyvsp[-1].col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&(yyvsp[-1]));
#endif

			;
    break;}
case 126:
#line 902 "fortran.y"
{
			     if(is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(
					     yyvsp[-1].line_num,yyvsp[-1].col_num,
					     "trailing comma");

			;
    break;}
case 127:
#line 910 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			     def_com_block(&(yyval),&(yyvsp[-2]));
			     if(is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(
					     yyvsp[-1].line_num,yyvsp[-1].col_num,
					     "trailing comma");
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&(yyvsp[-2]));
#endif
			;
    break;}
case 128:
#line 928 "fortran.y"
{
			     yyval.TOK_flags = yyvsp[0].TOK_flags;
			;
    break;}
case 129:
#line 932 "fortran.y"
{
			     yyval.TOK_flags = yyvsp[0].TOK_flags;
			     yyval.line_num = yyvsp[0].line_num;
			     yyval.col_num = yyvsp[0].col_num;
			;
    break;}
case 130:
#line 940 "fortran.y"
{
			     def_com_block(&(yyvsp[-1]),&(yyvsp[0]));
			     yyval.TOK_flags = yyvsp[0].TOK_flags;
			     yyval.line_num = yyvsp[0].line_num;
			     yyval.col_num = yyvsp[0].col_num;
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("labeled common",&(yyvsp[0]));
#endif
			;
    break;}
case 131:
#line 953 "fortran.y"
{
			     yyval = yyvsp[-1];
			;
    break;}
case 132:
#line 958 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			;
    break;}
case 133:
#line 962 "fortran.y"
{
			     implied_id_token(&(yyval),blank_com_name);
			;
    break;}
case 134:
#line 968 "fortran.y"
{
			    yyval.TOK_flags = yyvsp[0].TOK_flags;
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 135:
#line 973 "fortran.y"
{
			    if(!is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num-1,
					"missing comma");
			    yyval.TOK_flags = yyvsp[0].TOK_flags;
			    yyval.line_num = yyvsp[0].line_num;
			    yyval.col_num = yyvsp[0].col_num;
			    yyval.next_token = append_token(yyvsp[-1].next_token,&(yyvsp[0]));
			;
    break;}
case 136:
#line 986 "fortran.y"
{			   /* no comma */
			     yyval.TOK_flags = yyvsp[0].TOK_flags;
			     make_false(COMMA_FLAG,yyval.TOK_flags);
			;
    break;}
case 137:
#line 991 "fortran.y"
{			   /* has comma */
			     yyval.TOK_flags = yyvsp[-1].TOK_flags;
			     make_true(COMMA_FLAG,yyval.TOK_flags);
   			;
    break;}
case 138:
#line 998 "fortran.y"
{
			     def_com_variable(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			;
    break;}
case 139:
#line 1003 "fortran.y"
{
			     def_com_variable(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			;
    break;}
case 140:
#line 1016 "fortran.y"
{
			    if(is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(yyvsp[-1].line_num,
				 (unsigned)(yyvsp[-1].col_num+strlen(token_name(yyvsp[-1]))),
					"trailing comma");
			    if(f77_namelist) {
				nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num);
			    }
			;
    break;}
case 142:
#line 1029 "fortran.y"
{
			    yyval = yyvsp[0];
			;
    break;}
case 143:
#line 1035 "fortran.y"
{
			     def_namelist(&(yyvsp[-1]),&(yyvsp[0]));
			     yyval = yyvsp[0];
			;
    break;}
case 144:
#line 1042 "fortran.y"
{
			    yyval = yyvsp[-1];
			;
    break;}
case 145:
#line 1048 "fortran.y"
{
			     yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 146:
#line 1052 "fortran.y"
{
			    if(!is_true(COMMA_FLAG,yyvsp[-1].TOK_flags))
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num-1,
					"missing comma");
			    yyval.TOK_flags = yyvsp[0].TOK_flags;
			    yyval.line_num = yyvsp[0].line_num;
			    yyval.col_num = yyvsp[0].col_num;
			    yyval.next_token = append_token(yyvsp[-1].next_token,&(yyvsp[0]));
			;
    break;}
case 147:
#line 1065 "fortran.y"
{			   /* no comma */
			     def_namelist_item(&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[0]),&(yyval));
			     make_false(COMMA_FLAG,yyval.TOK_flags);
			;
    break;}
case 148:
#line 1071 "fortran.y"
{			   /* has comma */
			     def_namelist_item(&(yyvsp[-1]));
			     primary_id_expr(&(yyvsp[-1]),&(yyval));
			     make_true(COMMA_FLAG,yyval.TOK_flags);
			;
    break;}
case 153:
#line 1086 "fortran.y"
{
			  current_typesize = size_DEFAULT;
			  current_len_text = NULL;
			;
    break;}
case 154:
#line 1092 "fortran.y"
{
			    current_typesize = yyvsp[0].value.integer;
			    current_len_text = NULL;
#if 0 /* defunct feature */
			    if(local_wordsize > 0) {
			      /*  recognize REAL*2w as DOUBLE PRECISION */
			      if(current_datatype == type_REAL
				 && yyvsp[0].value.integer == type_size[type_DP])
				current_datatype = type_DP;
			      /*  recognize COMPLEX*4w as DOUBLE COMPLEX */
			      if(current_datatype == type_COMPLEX
				 && yyvsp[0].value.integer==type_size[type_DCOMPLEX])
				current_datatype = type_DCOMPLEX;
			    }
#endif
			     if(f77_typesize) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
			     }
			;
    break;}
case 156:
#line 1116 "fortran.y"
{
			     current_datatype = type_INTEGER;
			     integer_context = TRUE;
			;
    break;}
case 157:
#line 1121 "fortran.y"
{
			     current_datatype = type_REAL;
			     integer_context = TRUE;
			;
    break;}
case 158:
#line 1126 "fortran.y"
{
			     current_datatype = type_COMPLEX;
			     integer_context = TRUE;
			;
    break;}
case 159:
#line 1131 "fortran.y"
{
			     current_datatype = type_LOGICAL;
			     integer_context = TRUE;
			;
    break;}
case 160:
#line 1138 "fortran.y"
{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			;
    break;}
case 161:
#line 1144 "fortran.y"
{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			;
    break;}
case 162:
#line 1150 "fortran.y"
{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     if(f77_double_complex) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
			     }
			;
    break;}
case 163:
#line 1159 "fortran.y"
{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			     current_len_text = NULL;
			     if(f77_double_complex) {
				nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
			     }
			;
    break;}
case 164:
#line 1168 "fortran.y"
{
			     current_datatype = type_INTEGER;
			     current_typesize = 1;
			     current_len_text = NULL;
			     if(f77_byte)
			       nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
			;
    break;}
case 165:
#line 1178 "fortran.y"
{
			     current_datatype = type_STRING;
			     current_typesize = 1;
			     current_len_text = NULL;
			     current_size_is_adjustable = 0;
			     current_size_is_expression = 0;
			     integer_context = TRUE;
			;
    break;}
case 166:
#line 1189 "fortran.y"
{
			     current_typesize = yyvsp[0].value.integer;
			     current_size_is_adjustable = yyvsp[0].size_is_adjustable;
			     current_size_is_expression = yyvsp[0].size_is_expression;
				/* Save length spec text if expression */
			     if(current_size_is_expression) {
			       if(yyvsp[0].left_token == NULL)
				 current_len_text = new_tree_text(&(yyvsp[0]));
			       else
				 current_len_text = new_tree_text(yyvsp[0].left_token);
			     }
			     else
			       current_len_text = NULL;
			;
    break;}
case 169:
#line 1210 "fortran.y"
{
			     declare_type(&(yyvsp[0]),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			;
    break;}
case 170:
#line 1217 "fortran.y"
{
			     declare_type(&(yyvsp[0]),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			;
    break;}
case 173:
#line 1230 "fortran.y"
{
			     yyvsp[0].size_is_adjustable = current_size_is_adjustable;
			     yyvsp[0].size_is_expression = current_size_is_expression;
			     declare_type(&(yyvsp[0]),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			;
    break;}
case 174:
#line 1239 "fortran.y"
{
			     yyvsp[-2].size_is_adjustable = yyvsp[0].size_is_adjustable;
			     yyvsp[-2].size_is_expression = yyvsp[0].size_is_expression;
			     declare_type(&(yyvsp[-2]),
					  current_datatype,
					  yyvsp[0].value.integer,
					  new_tree_text(
					     yyvsp[0].left_token == NULL?
					     &(yyvsp[0]): yyvsp[0].left_token )
					  );
			;
    break;}
case 175:
#line 1251 "fortran.y"
{
			     yyvsp[0].size_is_adjustable = current_size_is_adjustable;
			     yyvsp[0].size_is_expression = current_size_is_expression;
			     declare_type(&(yyvsp[0]),
					  current_datatype,
					  current_typesize,
					  current_len_text);
			;
    break;}
case 176:
#line 1260 "fortran.y"
{
			     yyvsp[-2].size_is_adjustable = yyvsp[0].size_is_adjustable;
			     yyvsp[-2].size_is_expression = yyvsp[0].size_is_expression;
			     declare_type(&(yyvsp[-2]),
					  current_datatype,
					  yyvsp[0].value.integer,
					  new_tree_text(
					     yyvsp[0].left_token == NULL?
					     &(yyvsp[0]): yyvsp[0].left_token )
					  );
			;
    break;}
case 177:
#line 1275 "fortran.y"
{implicit_flag=TRUE;;
    break;}
case 178:
#line 1279 "fortran.y"
{
			    implicit_flag=FALSE;
			    if(implicit_none) {
				syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
				     "conflicts with IMPLICIT NONE");
			    }
			    else {
				implicit_type_given = TRUE;
			    }
			;
    break;}
case 179:
#line 1291 "fortran.y"
{
			    implicit_flag=FALSE;
				if(implicit_type_given) {
				    syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
					 "conflicts with IMPLICIT statement");
				}
				else {
				    if(f77_implicit_none)
				      nonstandard(yyvsp[-1].line_num,yyvsp[-1].col_num);
				    implicit_none = TRUE;
				}
			;
    break;}
case 181:
#line 1306 "fortran.y"
{initial_flag = TRUE;;
    break;}
case 183:
#line 1312 "fortran.y"
{implicit_letter_flag = TRUE;;
    break;}
case 184:
#line 1313 "fortran.y"
{implicit_letter_flag = FALSE;;
    break;}
case 187:
#line 1321 "fortran.y"
{
			  int c1 = (int)yyvsp[0].subclass;

			  if( (f77_dollarsigns && c1=='$')
			   || (f77_underscores && c1=='_') ) {
			    nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c1);
			;
    break;}
case 188:
#line 1336 "fortran.y"
{
			  int c1 = (int)yyvsp[-2].subclass,
			      c2 = (int)yyvsp[0].subclass;

			  if( (f77_dollarsigns && (c1 == '$' || c2 == '$'))
			   || (f77_underscores && (c1 == '_' || c2 == '_')))
			  {
			    if(!isalpha(c1))
			      nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num);
			    else
			      nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
			    msg_tail(": nonalphabetic character");
			  }

			   set_implicit_type(current_datatype,
					     current_typesize,
					     current_len_text,
					     c1,c2);
			;
    break;}
case 189:
#line 1360 "fortran.y"
{
			     yyvsp[-1].left_token = (Token *)NULL;
			     yyval.value.integer = size_ADJUSTABLE;
			     yyval.size_is_adjustable = 1;
			     yyval.size_is_expression = 0;
				/* Store as a parenthesized expr tree */
			     yyval.left_token = add_tree_node(&(yyvsp[-2]),&(yyvsp[-1]),
							  (Token*)NULL);
			;
    break;}
case 190:
#line 1370 "fortran.y"
{
			     yyval.value.integer = yyvsp[0].value.integer;
			     yyval.size_is_adjustable = 0;
			     yyval.size_is_expression = 0;
			;
    break;}
case 191:
#line 1376 "fortran.y"
{
			     yyval = yyvsp[-1];
			     yyval.size_is_adjustable = 0;
			     yyval.size_is_expression = 1;
			     if( yyval.value.integer <= 0 ){
			       warning(yyvsp[-1].line_num,yyvsp[-1].col_num,
					"invalid length specification");
			       msg_tail(": substituting 1");
			       yyval.value.integer = 1;
			     }
			     yyval.left_token = add_tree_node(&(yyvsp[-2]),&(yyvsp[-1]),
							  (Token*)NULL);
			;
    break;}
case 193:
#line 1394 "fortran.y"
{
			    if(f77_param_noparen)
				nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num);
			;
    break;}
case 196:
#line 1404 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 197:
#line 1406 "fortran.y"
{
			     def_parameter(&(yyvsp[-3]),&(yyvsp[0]));
			     primary_id_expr(&(yyvsp[-3]),&(yyvsp[-3]));
			     assignment_stmt_type(&(yyvsp[-3]),&(yyvsp[-1]),&(yyvsp[0]));
			     complex_const_allowed = FALSE;
			;
    break;}
case 199:
#line 1419 "fortran.y"
{
			     def_ext_name(&(yyvsp[0]));
			;
    break;}
case 200:
#line 1423 "fortran.y"
{
			     def_ext_name(&(yyvsp[0]));
			;
    break;}
case 202:
#line 1433 "fortran.y"
{
			     def_intrins_name(&(yyvsp[0]));
			;
    break;}
case 203:
#line 1437 "fortran.y"
{
			     def_intrins_name(&(yyvsp[0]));
			;
    break;}
case 204:
#line 1444 "fortran.y"
{
		  if(f77_cray_pointers)
		    nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num);
		;
    break;}
case 208:
#line 1458 "fortran.y"
{
			     declare_type(&(yyvsp[0]),type_INTEGER,local_wordsize,
					  NULL );
			;
    break;}
case 209:
#line 1465 "fortran.y"
{
				/* Suppress set/used warnings since
				   often is accessed only via pointer */
		             use_lvalue(&(yyvsp[0]));
		             use_variable(&(yyvsp[0]));
		        ;
    break;}
case 210:
#line 1472 "fortran.y"
{
		             use_lvalue(&(yyvsp[0]));
		             use_variable(&(yyvsp[0]));
		        ;
    break;}
case 211:
#line 1480 "fortran.y"
{
			  global_save = TRUE;
			;
    break;}
case 215:
#line 1491 "fortran.y"
{
			     save_variable(&(yyvsp[0]));
			;
    break;}
case 216:
#line 1495 "fortran.y"
{
/***			     def_com_block(&($2),(Token*)NULL);***/
			     save_com_block(&(yyvsp[-1]));
			;
    break;}
case 221:
#line 1511 "fortran.y"
{complex_const_allowed=TRUE;;
    break;}
case 222:
#line 1513 "fortran.y"
{complex_const_allowed=FALSE;;
    break;}
case 226:
#line 1522 "fortran.y"
{
			     use_lvalue(&(yyvsp[0]));
			;
    break;}
case 233:
#line 1538 "fortran.y"
{
			     use_parameter(&(yyvsp[0]));
			;
    break;}
case 235:
#line 1545 "fortran.y"
{
			     use_parameter(&(yyvsp[0]));
			;
    break;}
case 238:
#line 1556 "fortran.y"
{
			     use_lvalue(&(yyvsp[0]));
			;
    break;}
case 240:
#line 1564 "fortran.y"
{
			    use_implied_do_index(&(yyvsp[-3]));
			;
    break;}
case 243:
#line 1575 "fortran.y"
{complex_const_allowed = TRUE;
				    in_assignment_stmt = TRUE;;
    break;}
case 244:
#line 1577 "fortran.y"
{
			  if( ! (is_true(LVALUE_EXPR,yyvsp[-3].TOK_flags)
			       || is_true(STMT_FUNCTION_EXPR,yyvsp[-3].TOK_flags) )) {
			    syntax_error(yyvsp[-3].line_num,yyvsp[-3].col_num,
					 "left side is not assignable");
			  }
			  else {
			    assignment_stmt_type(&(yyvsp[-3]),&(yyvsp[-2]),
					&(yyvsp[0]));
			  }
			  complex_const_allowed = FALSE;
			  in_assignment_stmt = FALSE;
			;
    break;}
case 245:
#line 1591 "fortran.y"
{
				/* Clear u-b-s flags spuriously set */
			  if(is_true(STMT_FUNCTION_EXPR, yyvsp[-5].TOK_flags)
				     && stmt_sequence_no <= SEQ_STMT_FUN)
			     stmt_function_stmt(&(yyvsp[-5]));
		        ;
    break;}
case 250:
#line 1609 "fortran.y"
{
			    do_ASSIGN(&(yyvsp[-1]));
			;
    break;}
case 254:
#line 1626 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-1]));
			;
    break;}
case 255:
#line 1630 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-4]));
			;
    break;}
case 256:
#line 1634 "fortran.y"
{
			     do_assigned_GOTO(&(yyvsp[-5]));
			;
    break;}
case 257:
#line 1640 "fortran.y"
{
			    integer_context=TRUE;
			;
    break;}
case 258:
#line 1644 "fortran.y"
{
			    integer_context=TRUE;
			;
    break;}
case 261:
#line 1656 "fortran.y"
{
			  int t=datatype_of(yyvsp[-9].class);
			  if(t != type_INTEGER && t != type_REAL
			     && t != type_DP && t != type_ERROR ) {
			    syntax_error(yyvsp[-9].line_num,yyvsp[-9].col_num,
		  "integer, real, or double precision expression required");
			  }
			;
    break;}
case 262:
#line 1668 "fortran.y"
{
			  int t=datatype_of(yyvsp[-1].TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error(yyvsp[-1].line_num,yyvsp[-1].col_num,
					  "logical expression required");
			;
    break;}
case 263:
#line 1678 "fortran.y"
{
			  int t=datatype_of(yyvsp[-2].TOK_type);
			  if(t != type_LOGICAL && t != type_ERROR)
			     syntax_error(yyvsp[-2].line_num,yyvsp[-2].col_num,
					  "logical expression required");
			;
    break;}
case 264:
#line 1686 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 265:
#line 1687 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;	/* for is_keyword */
			    yyval = yyvsp[-1]; /* Inherit expr for type checking above */
			;
    break;}
case 267:
#line 1700 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 268:
#line 1701 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;
			;
    break;}
case 273:
#line 1731 "fortran.y"
{
			  if( ! is_true(LVALUE_EXPR,yyvsp[-3].TOK_flags) ) {
			    syntax_error(yyvsp[-3].line_num,yyvsp[-3].col_num,
					 "index is not assignable");
			  }
			  else {
			     use_lvalue(&(yyvsp[-3]));
			     use_variable(&(yyvsp[-3]));
			  }

				/* Check for non-integer DO index or bounds */
			     if(datatype_of(yyvsp[-3].TOK_type) == type_INTEGER
				&& datatype_of(yyvsp[-1].TOK_type) != type_INTEGER)
			       warning(yyvsp[-2].line_num,yyvsp[-2].col_num,
				  "type mismatch between DO index and bounds");

			     else if(datatype_of(yyvsp[-3].TOK_type) != type_INTEGER)
			       if(datatype_of(yyvsp[-1].TOK_type) != type_INTEGER) {
				 if(port_real_do)
				   nonportable(yyvsp[-1].line_num,yyvsp[-1].col_num,
					       "non-integer DO loop bounds");
			       }
			       else {
				 if(trunc_real_do_index)
				   warning(yyvsp[-3].line_num,yyvsp[-3].col_num,
					   "DO index is not integer");
			       }
			;
    break;}
case 274:
#line 1760 "fortran.y"
{complex_const_allowed=TRUE;;
    break;}
case 275:
#line 1761 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-2].TOK_flags)){
				use_variable(&(yyvsp[-2]));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,yyval.TOK_flags);
			;
    break;}
case 276:
#line 1769 "fortran.y"
{complex_const_allowed=TRUE;;
    break;}
case 277:
#line 1770 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[-2].TOK_flags)){
				use_variable(&(yyvsp[-2]));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,yyval.TOK_flags);
			;
    break;}
case 280:
#line 1782 "fortran.y"
{
			    make_true(NONSTD_USAGE_FLAG,yyval.TOK_flags);
			    integer_context=FALSE;
			;
    break;}
case 281:
#line 1789 "fortran.y"
{
			    yyval.TOK_type=do_bounds_type(&(yyvsp[-2]),&(yyvsp[0]),&(yyvsp[0]));
			;
    break;}
case 282:
#line 1793 "fortran.y"
{
			    yyval.TOK_type=do_bounds_type(&(yyvsp[-4]),&(yyvsp[-2]),&(yyvsp[0]));
			;
    break;}
case 290:
#line 1817 "fortran.y"
{
			     use_variable(&(yyvsp[0]));
			;
    break;}
case 292:
#line 1825 "fortran.y"
{complex_const_allowed = FALSE;;
    break;}
case 294:
#line 1827 "fortran.y"
{complex_const_allowed = FALSE;;
    break;}
case 296:
#line 1830 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 297:
#line 1832 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 303:
#line 1845 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 304:
#line 1849 "fortran.y"
{
			    if(f77_accept_type)
				nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num);
			;
    break;}
case 305:
#line 1854 "fortran.y"
{
			    if(f77_accept_type)
				nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num);
			;
    break;}
case 307:
#line 1863 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 308:
#line 1864 "fortran.y"
{complex_const_allowed = FALSE;;
    break;}
case 310:
#line 1868 "fortran.y"
{
			    if(f77_accept_type)
				nonstandard(yyvsp[-2].line_num,yyvsp[-2].col_num);
			;
    break;}
case 311:
#line 1873 "fortran.y"
{complex_const_allowed = TRUE;;
    break;}
case 312:
#line 1874 "fortran.y"
{complex_const_allowed = FALSE;;
    break;}
case 313:
#line 1875 "fortran.y"
{
			    if(f77_accept_type)
				nonstandard(yyvsp[-6].line_num,yyvsp[-6].col_num);
			;
    break;}
case 314:
#line 1883 "fortran.y"
{
			    ++control_item_count;
			;
    break;}
case 315:
#line 1887 "fortran.y"
{
			    ++control_item_count;
			    if(! io_warning_given) {
			      if( io_internal_file ) {
				if( (curr_stmt_class == tok_WRITE ||
				     curr_stmt_class == tok_READ) &&
				    io_list_directed ) {
				  if(f77_internal_list_io) {
				    nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
	    msg_tail(": internal file cannot be used with list-directed I/O");
				  }
				  io_warning_given = TRUE;
				}
			      }
			    }
			;
    break;}
case 316:
#line 1909 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			;
    break;}
case 317:
#line 1913 "fortran.y"
{
			    if( yyvsp[0].class == '*' ) {
			      if(control_item_count == 1) /* format id */
				{
				  io_list_directed = TRUE;
				}
			    }
			    else if( is_true(ID_EXPR,yyvsp[0].TOK_flags)){

					/* Handle special cases */
				if(control_item_count == 0 &&
				 datatype_of(yyvsp[0].TOK_type) == type_STRING) {
					/* unit id=char variable is
					   an internal file.  I/O goes in
					   and out of the variable. */
				  io_internal_file = TRUE;
				  if(curr_stmt_class == tok_WRITE) {
				    use_lvalue(&(yyvsp[0]));
				  }
				}

					/* format id=namelist means
					   I/O with variables of namelist. */
				else if( control_item_count == 1 &&
				 datatype_of(yyvsp[0].TOK_type) == type_NAMELIST) {
				    ref_namelist(&(yyvsp[0]),curr_stmt_class);
				}

					/* Handle use of variable */
				use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 318:
#line 1953 "fortran.y"
{
			    if( yyvsp[0].class != '*'
			       && is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }
			    ++control_item_count;
			;
    break;}
case 319:
#line 1961 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			    ++control_item_count;
			;
    break;}
case 320:
#line 1966 "fortran.y"
{
			    ++control_item_count;
			;
    break;}
case 321:
#line 1972 "fortran.y"
{
			    use_io_keyword(&(yyvsp[-2]),&(yyvsp[0]),curr_stmt_class);
			;
    break;}
case 322:
#line 1976 "fortran.y"
{
			    use_special_open_keywd(&(yyvsp[0]));
			;
    break;}
case 325:
#line 1987 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				if( curr_stmt_class == tok_READ ||
				    curr_stmt_class == tok_ACCEPT )
				    use_lvalue(&(yyvsp[0]));
				else
				    use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 327:
#line 2001 "fortran.y"
{
			  if( ! is_true(LVALUE_EXPR,yyvsp[-3].TOK_flags) ) {
			    syntax_error(yyvsp[-3].line_num,yyvsp[-3].col_num,
					 "index is not assignable");
			  }
			  else {
			     use_implied_do_index(&(yyvsp[-3]));
			  }
			;
    break;}
case 328:
#line 2013 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 330:
#line 2018 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 332:
#line 2023 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 334:
#line 2029 "fortran.y"
{
			    if( yyvsp[-1].class != '*'
			       && is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			;
    break;}
case 336:
#line 2037 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 337:
#line 2042 "fortran.y"
{
			    if( yyvsp[-1].class != '*'
			       && is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			;
    break;}
case 339:
#line 2050 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 340:
#line 2051 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 341:
#line 2056 "fortran.y"
{
			    if( yyvsp[-1].class != '*'
			       && is_true(ID_EXPR,yyvsp[-1].TOK_flags)){
				use_variable(&(yyvsp[-1]));
			    }
			;
    break;}
case 343:
#line 2064 "fortran.y"
{init_io_ctrl_list();;
    break;}
case 346:
#line 2078 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				 use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 348:
#line 2087 "fortran.y"
{inside_format=TRUE;;
    break;}
case 349:
#line 2088 "fortran.y"
{
			  inside_format=FALSE;
			;
    break;}
case 368:
#line 2124 "fortran.y"
{
			  if(f77_format_dollarsigns)
			     nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
			;
    break;}
case 373:
#line 2139 "fortran.y"
{inside_format=FALSE;;
    break;}
case 374:
#line 2140 "fortran.y"
{inside_format=TRUE;;
    break;}
case 375:
#line 2141 "fortran.y"
{
			  if(f77_variable_format)
			     nonstandard(yyvsp[-4].line_num,yyvsp[-4].col_num);
			;
    break;}
case 376:
#line 2150 "fortran.y"
{
			  check_stmt_sequence(&(yyvsp[-3]),SEQ_STMT_FUN);

				def_stmt_function(&(yyvsp[-3]),&(yyvsp[-1]));
					/* make token info */
				primary_id_expr(&(yyvsp[-3]),&(yyval));
#ifdef DEBUG_PARSER
				if(debug_parser)
				  print_exprlist("stmt function",&(yyvsp[-1]));
#endif
			;
    break;}
case 377:
#line 2164 "fortran.y"
{
			    yyval.next_token = (Token*)NULL;
			;
    break;}
case 379:
#line 2171 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 380:
#line 2176 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 382:
#line 2186 "fortran.y"
{
			     call_subr(&(yyvsp[0]),(Token*)NULL);
			     complex_const_allowed = FALSE;
			;
    break;}
case 384:
#line 2192 "fortran.y"
{
			     call_subr(&(yyvsp[-2]),(Token*)NULL);
			     complex_const_allowed = FALSE;
			;
    break;}
case 386:
#line 2198 "fortran.y"
{
			     call_subr(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("call stmt",&(yyvsp[-1]));
#endif
			     complex_const_allowed = FALSE;
			;
    break;}
case 388:
#line 2209 "fortran.y"
{
			     complex_const_allowed = TRUE;
			     yyval = yyvsp[0];
			;
    break;}
case 389:
#line 2215 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			    yyval.left_token = (Token *)NULL;
			;
    break;}
case 390:
#line 2220 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 391:
#line 2226 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				 use_actual_arg(&(yyvsp[0]));
				 use_variable(&(yyvsp[0]));
			    }
			;
    break;}
case 392:
#line 2233 "fortran.y"
{
			  yyval = yyvsp[0];
			  yyval.left_token = (Token *)NULL;
			;
    break;}
case 393:
#line 2241 "fortran.y"
{
			     do_RETURN(current_module_hash,&(yyvsp[-1]));
			;
    break;}
case 394:
#line 2245 "fortran.y"
{
			     do_RETURN(current_module_hash,&(yyvsp[-2]));
			;
    break;}
case 395:
#line 2252 "fortran.y"
{
				   /* restore context */
				if(!is_true(COMPLEX_FLAG,yyvsp[-3].TOK_flags))
				  complex_const_allowed=FALSE;
				if(is_true(IN_ASSIGN,yyvsp[-3].TOK_flags))
				  in_assignment_stmt = TRUE;

				  /* Change empty arg list to no arg list */
				if(yyvsp[-1].next_token == NULL)
				  call_func(&(yyvsp[-3]),(Token *)NULL);
				else
				  call_func(&(yyvsp[-3]),&(yyvsp[-1]));
							/* make token info */
				func_ref_expr(&(yyvsp[-3]),&(yyvsp[-1]),&(yyval));
				/* Substitute empty token for null arglist */
				yyval.left_token = add_tree_node(
						   &(yyvsp[-2]),&(yyvsp[-3]),
						   (yyvsp[-1].next_token == NULL?
						    empty_token(&(yyvsp[-1])) :
						    yyvsp[-1].next_token) );
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("function",&(yyvsp[-1]));
#endif
			;
    break;}
case 396:
#line 2280 "fortran.y"
{
			  if(complex_const_allowed)/* save context */
			    make_true(COMPLEX_FLAG,yyval.TOK_flags);
			  complex_const_allowed=TRUE;
			  if(in_assignment_stmt)
			    make_true(IN_ASSIGN,yyval.TOK_flags);
			  in_assignment_stmt = FALSE;
			;
    break;}
case 397:
#line 2290 "fortran.y"
{
				yyval.class = 0;
				yyval.next_token = (Token *)NULL;
				yyval.left_token = (Token *)NULL;
			;
    break;}
case 399:
#line 2299 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			    yyval.left_token = (Token *)NULL;
			;
    break;}
case 400:
#line 2304 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 401:
#line 2312 "fortran.y"
{
			  int t=datatype_of(yyvsp[0].TOK_type);
			  if( t != type_ERROR){
			    if( ! is_const_type(t) ) {
			      syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
		      "arithmetic, char, or logical expression expected");
			    }
			    else {
			      if( !is_true(PARAMETER_EXPR,yyvsp[0].TOK_flags) ) {
				syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
					   "constant expression expected");
			      }
			    /* Here we allow, with some warnings, expr
			       containing intrins func or **REAL in
			       PARAMETER defn. */
			      else if( !is_true(CONST_EXPR,yyvsp[0].TOK_flags) ) {
				if(f77_param_intrinsic) {
				  nonstandard(yyvsp[0].line_num,yyvsp[0].col_num);
				  msg_tail(
			 "intrinsic function or **REAL in PARAMETER defn");
				}
			      }
			    }
			  }
			;
    break;}
case 402:
#line 2341 "fortran.y"
{
				/* Fix it up in case it is used in expr list */
			  yyval.next_token = (Token *) NULL;
#ifdef DEBUG_PARSER
			    if(debug_parser) {
				(void)fprintf(list_fd,
					"\nexpr: class=0x%x subclass=0x%x",
					yyvsp[0].class,
					yyvsp[0].subclass);
			    }
#endif
			;
    break;}
case 404:
#line 2358 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 405:
#line 2363 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 407:
#line 2372 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 409:
#line 2381 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 411:
#line 2390 "fortran.y"
{
			    do_unexpr(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			;
    break;}
case 413:
#line 2398 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 415:
#line 2408 "fortran.y"
{
			    do_unexpr(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			;
    break;}
case 416:
#line 2412 "fortran.y"
{
			    do_unexpr(&(yyvsp[-1]),&(yyvsp[0]),&(yyval));
			;
    break;}
case 417:
#line 2416 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 418:
#line 2421 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 420:
#line 2430 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			    if(div_check &&
			       !is_true(CONST_EXPR,yyvsp[0].TOK_flags)){
				warning(yyvsp[-1].line_num,yyvsp[-1].col_num,
					"Possible division by zero");
			    }
			;
    break;}
case 421:
#line 2440 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 423:
#line 2449 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 425:
#line 2458 "fortran.y"
{
			    do_binexpr(&(yyvsp[-2]),&(yyvsp[-1]),&(yyvsp[0])
					 ,&(yyval));
			;
    break;}
case 430:
#line 2473 "fortran.y"
{
			    yyval.TOK_flags = 0;
			    yyval.left_token = (Token *)NULL;
			    make_true(CONST_EXPR,yyval.TOK_flags);
			    make_true(PARAMETER_EXPR,yyval.TOK_flags);
			    make_true(LIT_CONST,yyval.TOK_flags);
			    make_true(EVALUATED_EXPR,yyval.TOK_flags);
			;
    break;}
case 431:
#line 2482 "fortran.y"
{
			    yyval = yyvsp[-1];
				/* (identifier) becomes a non-identifier */
			    if(is_true(LVALUE_EXPR,yyvsp[-1].TOK_flags)) {
				if(pretty_parens) {
				  ugly_code(yyvsp[-1].line_num,yyvsp[-1].col_num,
					  "Extraneous parentheses");
				}
				use_variable(&(yyvsp[-1]));
				make_false(LVALUE_EXPR,yyval.TOK_flags);
				make_false(ARRAY_ID_EXPR,yyval.TOK_flags);
				make_false(ARRAY_ELEMENT_EXPR,yyval.TOK_flags);
				make_false(ID_EXPR,yyval.TOK_flags);
			    }
				/* (expr) becomes tree node with root = '(' */
			    yyval.left_token = add_tree_node(&(yyvsp[-2]),&(yyvsp[-1]),
							  (Token*)NULL);
			;
    break;}
case 432:
#line 2505 "fortran.y"
{
			    /* (class is set in numeric_const productions) */
			    yyval.size = size_DEFAULT;
			;
    break;}
case 433:
#line 2510 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_STRING);
			    /* (size is set in get_string) */
			;
    break;}
case 434:
#line 2515 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_HOLLERITH);
			    /* (size is set in get_hollerith) */
			    if(port_hollerith) {
				warning(yyvsp[0].line_num,yyvsp[0].col_num,
				"hollerith constant may not be portable");
			    }
			;
    break;}
case 435:
#line 2524 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_LOGICAL);
			    yyval.size = size_DEFAULT;
			;
    break;}
case 436:
#line 2531 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_INTEGER);
			;
    break;}
case 437:
#line 2535 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_REAL);
			;
    break;}
case 438:
#line 2539 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_DP);
			;
    break;}
case 439:
#line 2543 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_COMPLEX);
			;
    break;}
case 440:
#line 2547 "fortran.y"
{
			    yyval.TOK_type = type_byte(class_VAR,type_DCOMPLEX);
			;
    break;}
case 441:
#line 2554 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }
			    if(datatype_of(yyvsp[0].TOK_type) != type_INTEGER) {
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num,
					"expression must be integer type");
			    }
			;
    break;}
case 442:
#line 2568 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }
			    {
				int t=datatype_of(yyvsp[0].TOK_type);
				    if(t != type_INTEGER && t != type_REAL
					&& t != type_DP ) {
					syntax_error(
					  yyvsp[0].line_num,yyvsp[0].col_num,
		"expression must be integer, real, or double precision type");
			    	    }
			    }
			;
    break;}
case 443:
#line 2588 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }
			    if( ! is_true(CONST_EXPR,yyvsp[0].TOK_flags) ) {
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num,
					"constant expression expected");
			    }
			    else {
			      if(datatype_of(yyvsp[0].TOK_type) != type_INTEGER){
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num,
					"integer expression expected");
			      }
			      else {
				yyval.value.integer = int_expr_value(&(yyvsp[0]));
			      }
			    }
			;
    break;}
case 444:
#line 2612 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				use_variable(&(yyvsp[0]));
			    }

			    if( datatype_of(yyvsp[0].TOK_type) != type_INTEGER ){
				syntax_error(
					yyvsp[0].line_num,yyvsp[0].col_num,
					"integer dimension expected");
				yyval.value.integer = 0;
			    }
			    else {
			      if( is_true(EVALUATED_EXPR,yyvsp[0].TOK_flags) )
				yyval.value.integer =
				  int_expr_value(&(yyvsp[0]));
			      else		/* must be dummy */
				yyval.value.integer = 0;
			    }
			;
    break;}
case 445:
#line 2638 "fortran.y"
{
				ref_array(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array lvalue",&(yyvsp[-1]));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,yyval.TOK_flags);
				make_true(ARRAY_ELEMENT_EXPR,yyval.TOK_flags);
				yyval.left_token = add_tree_node(
						   &(yyvsp[-2]),&(yyvsp[-3]),yyvsp[-1].next_token);
				yyval.next_token = (Token *) NULL;
			;
    break;}
case 446:
#line 2654 "fortran.y"
{
				ref_array(&(yyvsp[-3]),&(yyvsp[-1]));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array",&(yyvsp[-1]));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,yyval.TOK_flags);
				make_true(ARRAY_ELEMENT_EXPR,yyval.TOK_flags);
				yyval.left_token = add_tree_node(
						   &(yyvsp[-2]),&(yyvsp[-3]),yyvsp[-1].next_token);
				yyval.next_token = (Token *) NULL;
			;
    break;}
case 447:
#line 2670 "fortran.y"
{
			    yyval.next_token = append_token((Token*)NULL,&(yyvsp[0]));
			;
    break;}
case 448:
#line 2674 "fortran.y"
{
			    yyval.next_token = append_token(yyvsp[-2].next_token,&(yyvsp[0]));
			;
    break;}
case 449:
#line 2680 "fortran.y"
{
			    if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
				 use_variable(&(yyvsp[0]));
			    }
				/* check subscript exprs for integer type */
			    if(datatype_of(yyvsp[0].TOK_type) != type_INTEGER)
			      if(trunc_real_subscript)
			         warning(yyvsp[0].line_num,yyvsp[0].col_num,
					 "subscript is not integer");
			;
    break;}
case 450:
#line 2694 "fortran.y"
{
				   /* restore status of complex flag */
			    if(!is_true(COMPLEX_FLAG,yyvsp[-1].TOK_flags))
				  complex_const_allowed=FALSE;
				/* set flag to keep more than just id for
				   arg list text */
			    if(is_true(ID_EXPR,yyvsp[-1].TOK_flags))
			       make_true(ARRAY_ELEMENT_EXPR,yyval.TOK_flags);
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.left_token = add_tree_node(
					       &save_token,&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.next_token = (Token *) NULL;
			;
    break;}
case 451:
#line 2709 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.left_token = add_tree_node(
					       &save_token,&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.next_token = (Token *) NULL;
			;
    break;}
case 452:
#line 2717 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.left_token = add_tree_node(
					       &save_token,&(yyvsp[-1]),&(yyvsp[0]));
			    yyval.next_token = (Token *) NULL;
			;
    break;}
case 453:
#line 2726 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			;
    break;}
case 454:
#line 2730 "fortran.y"
{
			    yyval.size=substring_size(&(yyvsp[-1]),&(yyvsp[0]));
			;
    break;}
case 455:
#line 2739 "fortran.y"
{
			    yyval.TOK_start=1;
			    yyval.TOK_end=0; /* 0 means LEN */

			    save_token = yyvsp[-2]; /* Save the paren for tree node */
			    yyval.left_token =
			      add_tree_node(&(yyvsp[-1]),
				     empty_token(&(yyvsp[-2])),empty_token(&(yyvsp[0])));
				/* Nullify next_token so it looks like
				   a tokenlist */
			    yyval.next_token = (Token *)NULL;
			;
    break;}
case 456:
#line 2753 "fortran.y"
{
			    yyval.TOK_start=yyvsp[-2].value.integer;
			    yyval.TOK_end=0; /* 0 means LEN */

			    save_token = yyvsp[-3]; /* Save the paren for tree node */
			    yyval.left_token =
			      add_tree_node(&(yyvsp[-1]),&(yyvsp[-2]),empty_token(&(yyvsp[0])));
			    yyval.next_token = (Token *)NULL;
			;
    break;}
case 457:
#line 2763 "fortran.y"
{
			    yyval.TOK_start=1;
			    yyval.TOK_end=yyvsp[-1].value.integer;

			    save_token = yyvsp[-3]; /* Save the paren for tree node */
			    yyval.left_token =
			      add_tree_node(&(yyvsp[-2]),empty_token(&(yyvsp[-3])),&(yyvsp[-1]));
			    yyval.next_token = (Token *)NULL;
			;
    break;}
case 458:
#line 2773 "fortran.y"
{
			    yyval.TOK_start=yyvsp[-3].value.integer;
			    yyval.TOK_end=yyvsp[-1].value.integer;

			    save_token = yyvsp[-4]; /* Save the paren for tree node */
			    yyval.left_token =
			      add_tree_node(&(yyvsp[-2]),&(yyvsp[-3]),&(yyvsp[-1]));
			    yyval.next_token = (Token *)NULL;
			;
    break;}
case 459:
#line 2785 "fortran.y"
{
			  if(is_true(ID_EXPR,yyvsp[0].TOK_flags)){
			    use_variable(&(yyvsp[0]));
			  }
				/* check validity and replace nonconst
				   value by size_UNKNOWN. */
			  if(is_true(CONST_EXPR,yyvsp[0].TOK_flags)) {
			    if( (yyval.value.integer=int_expr_value(&(yyvsp[0]))) < 1) {
			      syntax_error(yyvsp[0].line_num,yyvsp[0].col_num,
					   "invalid substring index");
			    }
			  }
			  else  /* (no longer need ID hash index) */
			    yyval.value.integer=size_UNKNOWN;
			;
    break;}
case 462:
#line 2810 "fortran.y"
{
			    ref_variable(&(yyvsp[0]));
			    primary_id_expr(&(yyvsp[0]),&(yyval));
			;
    break;}
case 463:
#line 2817 "fortran.y"
{
			    ref_variable(&(yyvsp[0]));
			    primary_id_expr(&(yyvsp[0]),&(yyval));
			;
    break;}
case 472:
#line 2843 "fortran.y"
{
			  if(yyvsp[0].value.integer == 0) {
			    warning(yyvsp[0].line_num,yyvsp[0].col_num,
				    "nonzero integer expected");
			    msg_tail(": substituting 1");
			    yyval.value.integer = 1;
			  }
			  yyval.left_token = (Token *)NULL;
			;
    break;}
case 473:
#line 2859 "fortran.y"
{
			    integer_context=TRUE;
			;
    break;}
case 474:
#line 2866 "fortran.y"
{
				integer_context=FALSE;
				yyval.TOK_type = type_byte(class_LABEL,type_LABEL);
				yyval.size = size_DEFAULT;
				yyval.TOK_flags = 0;
			;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 2876 "fortran.y"


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
