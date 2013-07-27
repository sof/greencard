--
-- StdDIS for FFI
--
-- (c) Thomas Nordin and Alastair Reid, 1997-2003
--

module Foreign.GreenCard
        ( module Foreign
        , module Foreign.C
	, unsafePerformIO

        , MbString
        , marshall_bool_,      unmarshall_bool_
        , marshall_string_,    unmarshall_string_
        , marshall_stringLen_, unmarshall_stringLen_
	   -- re-exporting base Prelude types
	   -- (useful when generating source that
	   --  import StdDIS qualified.)
	, Float
	, Double
	, Char
        ) where

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)






marshall_bool_ :: Bool -> IO Int
marshall_bool_ True  = return 1
marshall_bool_ False = return 0

unmarshall_bool_ :: Int -> IO Bool
unmarshall_bool_ 0 = return False
unmarshall_bool_ _ = return True

-- Ignore "IO" part of result type

----------------------------------------------------------------
-- Strings
----------------------------------------------------------------


type MbString      = Maybe String

marshall_string_ :: [Char] -> IO CString
marshall_string_ = newCString

marshall_stringLen_ :: [Char] -> IO CStringLen
marshall_stringLen_ = newCStringLen

unmarshall_string_ :: CString -> IO String
unmarshall_string_ = peekCString

unmarshall_stringLen_ :: CString -> Int -> IO String
unmarshall_stringLen_ ptr l = peekCStringLen (ptr, l)


----------------------------------------------------------------
-- Stable pointers
----------------------------------------------------------------

--
-- Use "stable" to create a stable pointer
--
-- Use "stablePtr" to manipulate (previously constructed) stable pointers
-- in Haskell.
--


----------------------------------------------------------------
-- Foreign pointers
----------------------------------------------------------------

--
-- Use "foreignP" to create a stable pointer
--
-- Use "foreignPtr" to manipulate (previously constructed) foreign pointers
-- in Haskell.
--


----------------------------------------------------------------
-- End of StdDIS
----------------------------------------------------------------
