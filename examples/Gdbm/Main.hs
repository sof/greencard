{-
	Copyright (c) 1998 Canon Research Centre Europe (CRE).

	You may redistribute and modify this module, provided you duplicate
	all of the original copyright notices and associated disclaimers.
	If you have modified the module in any way, you must document the
	changes, and include a reference to the original distribution.
 
	THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
	WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES
	OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE. 
-}

module Main where

import Gdbm
import IO

-- Two versions: monadic and non-monadic

main = do f <- openDB "comlex.db" gdbm_reader
          d <- fetchDB f d0
	  closeDB f
	  putStr ("Result is "++(upkDatum d)++"\n")
	  return ()
	where d0 = mkDatum "copy"

{-
main =
  let handle = gdbm_open "comlex.db" gdbm_reader
      isok   = gdbm_valid_handle handle
      key1   = gdbm_firstkey handle
      key2   = gdbm_nextkey handle key1
  in  if (isok)
      then putStr ("Key 1 was "++upkDatum key1++"\n"++
                   "Key 2 was "++upkDatum key2++"\n")
      else putStr "Bad handle\n"
-}
