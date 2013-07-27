/*
	Copyright (c) 1998 Canon Research Centre Europe (CRE).

	You may redistribute and modify this module, provided you duplicate
	all of the original copyright notices and associated disclaimers.
	If you have modified the module in any way, you must document the
	changes, and include a reference to the original distribution.
 
	THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
	WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES
	OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE. 
*/

/* Extra code on top of standard Gdbm interface */
/* For use with GHC/Green Card */

#include "gdbm.h"
#include <sys/stat.h>
#include <stdio.h>

/* Synonyms for complex constants */
#define Gdbm_reader  GDBM_READER
#define Gdbm_writer  GDBM_WRITER|GDBM_FAST
#define Gdbm_wrcreat GDBM_WRCREAT|GDBM_FAST
#define Gdbm_newdb   GDBM_NEWDB|GDBM_FAST

#define Gdbm_replace GDBM_REPLACE
#define Gdbm_insert  GDBM_INSERT

/* Standard opening permissions. This will vary between Unix and PC */
#define Permissions S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH
