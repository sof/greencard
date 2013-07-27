/*
  For GHC, we override the normal 'error' function
  prefix of "\nFail: ".

  Turn off default trace msg gumpf too.
  */
#include <stdio.h>

#if __GLASGOW_HASKELL__ >= 303
void ErrorHdrHook (long fd)
{}

void IOErrorHdrHook (long fd)
{}

#else
void ErrorHdrHook (FILE *where)
{
    fflush( stdout );			/* Flush out any pending output */
}

void IOErrorHdrHook (FILE *where)
{
    fflush( stdout );			/* Flush out any pending output */
}

void PreTraceHook (FILE *where)
{
    return;
}

void PostTraceHook (FILE *where)
{
    fprintf(stderr, "\n");
    return;
}

#endif
