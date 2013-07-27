
"Hello World" in GreenCard
--------------------------
         Antony Courtney,
         Yale University
         New Haven, CT 06511
         <antony@apocalyse.org>

This directory contains sources and Makefiles to build a very simple
GreenCard example.  The example is intended to illustrate how
GreenCard is used in conjunction with different Haskell
implementations on different platforms.  I currently provide Makefiles
in this directory for hugs98 and ghc on Linux and Windows.

There are only two (tiny!) source files:

   World.gc -- GreenCard source file.  This defines a function "foo"
               implemented in C but callable from Haskell.  The
               functions defined in this source file will be
               accessible from Haskell as a module named "World".

   Main.hs -- A Haskell file that implements a simple top-level "main"
              IO action that invokes World.foo.

Build Instructions:
-------------------

Platform-specific Notes:
------------------------

Windows users:  You will need a reasonably up-to-date version of the
"net" release of Cygwin (which includes GNU make) in order to use the
Makefile provided.  Cygwin is available from:

	http://sources.redhat.com/cygwin/

Glasgow Haskell Compiler (ghc) (5.02 or greater):
-------------------------------------------------

For GHC, GreenCard generates a World.hs file from World.gc which
contains a mixture of Haskell and in-line C code.  This World.hs file
is compiled using ghc with the -fvia-c option, and the resulting
World.o file is placed in a static library (libworld.a).  Main.hs is
then compiled and linked against this static library.

To build and test, do the following:

   1.  Edit Makefile.ghc-<platform>, where <platform> is either
       "linux" or "win32" as appropriate.  Change the definition of the 
        site-specific GCDIR variable to the path of your greencard
        source distribution.

   2.  From bash:
       $ make -f Makefile.ghc-linux

   3.  Try it!
       $./main

If all went well, you should see the following output:

$ ./main
entering foo()...:
called with arguments: ("A string from Haskell", 2.500000)
foo() returning...
foo returned the value 7.5
$

Hugs 98
-------

Under hugs, GreenCard processes the World.gc file to produce two
files:
        World.c -- The C code that implements World.gc, plus
                   additional marshalling code to be compiled into a
                   DLL or shared library.

	World.hs -- Haskell code with hugs primitives to dynamically
                    load the DLL or shared library that contains the C
                    code.

Windows users:  The Makefile provided uses the Microsoft Visual C++
tools to build the DLL for hugs.  You will need to have these tools
installed, and have cl.exe and link.exe accesible via your PATH
environment variable.  (It should be possible to build a DLL using
just the GNU C compiler provided with Cygwin, but I haven't tried this
yet.)

To build and test, do the following:

   1.  Edit Makefile.hugs-<platform>, where <platform> is either
       "linux" or "win32" as appropriate.  Change the definition of the 
        site-specific GCDIR variable to the path of your greencard
        source distribution.

   2.  From bash:
       $ hugs Main

   3.  From the hugs prompt, type "main":
       Main> main

If all goes well, you should see the following:
Type :? for help
Main> main
entering foo()...:
called with arguments: ("A string from Haskell", 2.500000)
foo() returning...
foo returned the value 7.5

Main>


Please let me know if you run in to any problems building this example
for the various platforms, or if you would like to contribute
Makefile(s) or build instructions for another Haskell implementation
or operating system.

Happy Hacking!

	       
	-Antony Courtney
	<antony@apocalypse.org>

