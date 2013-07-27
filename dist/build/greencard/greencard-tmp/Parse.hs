{-# OPTIONS -fglasgow-exts -cpp #-}
module Parse
       (
        gcParse		-- :: [Token] -> [Decl]
       ) where

import Lex
import LexM
import GCToken
import Name( Name )
import Type( Type(..), typeApply )
import DIS ( DIS(..), apply )
import Decl
{-
BEGIN_GHC_ONLY
import GHC.Exts
END_GHC_ONLY
-}
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.17

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = GHC.Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: ([Decl]) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ([Decl])
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Decl) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Decl)
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ([Name]) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ([Name])
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ([(Name, Name)]) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ([(Name, Name)])
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ((Name, Name)) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ((Name, Name))
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([Name]) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([Name])
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([Name]) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([Name])
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([Name]) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([Name])
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Name) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Name)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Decl) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Decl)
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Sig) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Sig)
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ((Maybe Type, Type)) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ((Maybe Type, Type))
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Type) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Type)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Type) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Type)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ([Type]) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ([Type])
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Type) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Type)
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Type) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Type)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([Type]) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([Type])
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (String) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (String)
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Maybe Call) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Maybe Call)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Maybe String) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Maybe String)
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Maybe CCode) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Maybe CCode)
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Maybe Fail) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Maybe Fail)
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([(String, String)]) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([(String, String)])
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Maybe Result) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Maybe Result)
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Maybe String) -> (HappyAbsSyn )
happyIn29 x = unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Maybe String)
happyOut29 x = unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (DIS) -> (HappyAbsSyn )
happyIn30 x = unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (DIS)
happyOut30 x = unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (DIS) -> (HappyAbsSyn )
happyIn31 x = unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (DIS)
happyOut31 x = unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ([DIS]) -> (HappyAbsSyn )
happyIn32 x = unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ([DIS])
happyOut32 x = unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (DIS) -> (HappyAbsSyn )
happyIn33 x = unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (DIS)
happyOut33 x = unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([DIS]) -> (HappyAbsSyn )
happyIn34 x = unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ([DIS])
happyOut34 x = unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ([(String, DIS)]) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> ([(String, DIS)])
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ((String, DIS)) -> (HappyAbsSyn )
happyIn36 x = unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ((String, DIS))
happyOut36 x = unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x01\x00\x00\x00\xe3\x00\x01\x00\x00\x00\xed\x00\xe2\x00\x2b\x00\xff\xff\x00\x00\xe1\x00\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x00\xdf\x00\x25\x00\xdd\x00\xdc\x00\x00\x00\x1f\x00\x22\x00\xff\xff\xff\xff\xa6\x00\xf6\xff\x00\x00\xf6\xff\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\xe6\x00\x00\x00\x00\x00\xdb\x00\xd8\x00\xd6\x00\x6c\x00\xda\x00\x00\x00\x22\x00\xff\xff\xd7\x00\x00\x00\x28\x00\x00\x00\x1f\x00\xff\xff\xff\xff\xff\xff\xd5\x00\xd4\x00\x00\x00\x09\x00\xd9\x00\x00\x00\xd3\x00\xce\x00\x28\x00\x47\x00\x00\x00\xff\xff\xd1\x00\xcf\x00\xcc\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x22\x00\x00\x00\xde\x00\x00\x00\xa5\x00\x2f\x00\x00\x00\xd2\x00\x02\x00\x00\x00\xbc\x00\xbb\x00\xb5\x00\xf6\xff\xff\xff\xf6\xff\xf6\xff\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\x00\xc5\x00\xc3\x00\x00\x00\xff\xff\xcd\x00\xca\x00\x2c\x00\x00\x00\x00\x00\x09\x00\xa5\x00\x00\x00\x00\x00\xae\x00\x09\x00\x00\x00\xc2\x00\xc1\x00\x00\x00\x75\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\xd0\x00\xbd\x00\x00\x00\x00\x00\xf6\xff\xf6\xff\x88\x00\xff\xff\x00\x00\x09\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x4f\x00\x00\x00\x00\x00\x4d\x00\x00\x00\xbf\x00\x95\x00\x85\x00\xc9\x00\x00\x00\x00\x00\xc8\x00\xc7\x00\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x96\x00\x80\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x79\x00\xad\x00\x99\x00\x7e\x00\xaa\x00\x00\x00\xa7\x00\x00\x00\x00\x00\x8d\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x00\x9a\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x54\x00\x91\x00\x78\x00\xb9\x00\x00\x00\x72\x00\x00\x00\x97\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x00\x00\x71\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x00\x00\x00\x65\x00\x00\x00\x51\x00\x00\x00\x49\x00\x00\x00\x00\x00\x00\x00\x8b\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x00\x11\x00\x9e\x00\xf7\xff\x93\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x00\x27\x00\x00\x00\x00\x00\x34\x00\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x94\x00\x00\x00\x00\x00\x00\x00\x00\x00\x64\x00\xab\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x9b\x00\x6f\x00\x00\x00\xfd\xff\x00\x00\x1a\x00\x00\x00\x00\x00\xe7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfe\xff\x00\x00\x00\x00\xfe\xff\xfc\xff\xcc\xff\xbf\xff\x00\x00\x00\x00\xf5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\xfb\xff\xf7\xff\xe5\xff\xe4\xff\xf3\xff\xf4\xff\xf2\xff\xeb\xff\xd8\xff\x00\x00\xdf\xff\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc8\xff\xad\xff\xfd\xff\xad\xff\xcb\xff\xaf\xff\x00\x00\xb0\xff\xb1\xff\xae\xff\xc5\xff\xc6\xff\xc7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\x00\x00\xd3\xff\xda\xff\xd8\xff\xd5\xff\x00\x00\x00\x00\x00\x00\xe9\xff\x00\x00\xf2\xff\xf1\xff\x00\x00\x00\x00\xe8\xff\xe7\xff\x00\x00\xd8\xff\x00\x00\xd7\xff\x00\x00\x00\x00\xf0\xff\xee\xff\xde\xff\xd1\xff\xdd\xff\x00\x00\xd9\xff\x00\x00\xbe\xff\xc1\xff\xc4\xff\x00\x00\x00\x00\xb6\xff\x00\x00\x00\x00\xab\xff\x00\x00\x00\x00\x00\x00\xad\xff\x00\x00\xad\xff\x00\x00\xb5\xff\xac\xff\xb4\xff\xb9\xff\xbc\xff\x00\x00\x00\x00\xa7\xff\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xaa\xff\xb2\xff\x00\x00\x00\x00\xcd\xff\xce\xff\xca\xff\x00\x00\xe2\xff\xe0\xff\xd0\xff\xcf\xff\x00\x00\x00\x00\xf9\xff\xd4\xff\xd6\xff\xdb\xff\x00\x00\x00\x00\xea\xff\xfa\xff\xe6\xff\x00\x00\xef\xff\xed\xff\xec\xff\x00\x00\xc0\xff\xe3\xff\xc9\xff\xc3\xff\xa9\xff\xa8\xff\xbd\xff\xad\xff\xad\xff\x00\x00\x00\x00\xba\xff\x00\x00\xa5\xff\xa6\xff\x00\x00\xb8\xff\xb7\xff\xc2\xff\xe1\xff\xf8\xff\xb3\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x1a\x00\x01\x00\x0d\x00\x1d\x00\x08\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x08\x00\x0d\x00\x0e\x00\x0f\x00\x1b\x00\x11\x00\x1d\x00\x1f\x00\x0d\x00\x21\x00\x0f\x00\x08\x00\x11\x00\x25\x00\x1f\x00\x20\x00\x1f\x00\x1d\x00\x21\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x24\x00\x1d\x00\x25\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x0d\x00\x0e\x00\x25\x00\x0d\x00\x1f\x00\x20\x00\x0d\x00\x14\x00\x1a\x00\x0d\x00\x14\x00\x1d\x00\x0d\x00\x12\x00\x0e\x00\x17\x00\x17\x00\x0e\x00\x1f\x00\x14\x00\x21\x00\x1f\x00\x16\x00\x21\x00\x1f\x00\x16\x00\x21\x00\x1f\x00\x14\x00\x21\x00\x1f\x00\x08\x00\x21\x00\x00\x00\x01\x00\x00\x00\x01\x00\x0e\x00\x0f\x00\x10\x00\x08\x00\x0e\x00\x09\x00\x0a\x00\x09\x00\x0a\x00\x08\x00\x12\x00\x08\x00\x16\x00\x0c\x00\x0d\x00\x0c\x00\x0d\x00\x10\x00\x11\x00\x10\x00\x11\x00\x08\x00\x03\x00\x04\x00\x18\x00\x0c\x00\x0d\x00\x08\x00\x08\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x02\x00\x10\x00\x08\x00\x0c\x00\x0d\x00\x08\x00\x0e\x00\x10\x00\x0e\x00\x0f\x00\x10\x00\x08\x00\x08\x00\x08\x00\x16\x00\x0c\x00\x0d\x00\x0c\x00\x0d\x00\x10\x00\x08\x00\x10\x00\x1b\x00\x1c\x00\x1d\x00\x08\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x15\x00\x1f\x00\x10\x00\x21\x00\x03\x00\x04\x00\x08\x00\x25\x00\x05\x00\x08\x00\x03\x00\x04\x00\x02\x00\x1a\x00\x08\x00\x08\x00\x1d\x00\x1e\x00\x1a\x00\x1e\x00\x1a\x00\x1d\x00\x1e\x00\x1d\x00\x1e\x00\x1a\x00\x1a\x00\x19\x00\x1d\x00\x1d\x00\x1a\x00\x07\x00\x08\x00\x1d\x00\x08\x00\x1b\x00\x1c\x00\x1d\x00\x1b\x00\x1c\x00\x1d\x00\x1b\x00\x1c\x00\x1d\x00\x06\x00\x07\x00\x08\x00\x1b\x00\x1c\x00\x1d\x00\x1b\x00\x1c\x00\x1d\x00\x16\x00\x17\x00\x25\x00\x26\x00\x26\x00\x27\x00\x08\x00\x08\x00\x08\x00\x08\x00\x13\x00\x16\x00\x04\x00\x15\x00\x28\x00\x16\x00\x13\x00\x16\x00\x25\x00\x19\x00\x12\x00\x10\x00\x23\x00\x23\x00\x0e\x00\x03\x00\x14\x00\x18\x00\x18\x00\x16\x00\x15\x00\x0e\x00\x0e\x00\x16\x00\x04\x00\x15\x00\x0d\x00\x18\x00\x17\x00\x02\x00\xff\xff\x14\x00\xff\xff\x1b\x00\xff\xff\x21\x00\x1a\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\x21\x00\x21\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x9f\x00\x07\x00\x26\x00\x56\x00\x66\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x87\x00\x59\x00\x5a\x00\x5b\x00\x64\x00\x5c\x00\x24\x00\x27\x00\x59\x00\x28\x00\x5b\x00\x66\x00\x5c\x00\x29\x00\x98\x00\x68\x00\x12\x00\x5d\x00\x13\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x10\x00\x5d\x00\x62\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x1c\x00\x32\x00\x62\x00\x1c\x00\x67\x00\x68\x00\x39\x00\x1d\x00\x97\x00\x39\x00\x1d\x00\x56\x00\x1c\x00\x8d\x00\x91\x00\x9c\x00\x3a\x00\x70\x00\x12\x00\x1d\x00\x13\x00\x12\x00\x71\x00\x13\x00\x12\x00\x71\x00\x13\x00\x12\x00\x8b\x00\x13\x00\x12\x00\x34\x00\x13\x00\x21\x00\x03\x00\x02\x00\x03\x00\x7f\x00\x36\x00\x37\x00\x93\x00\x7f\x00\x04\x00\x05\x00\x04\x00\x05\x00\x17\x00\x71\x00\x17\x00\x4f\x00\x78\x00\x19\x00\x45\x00\x19\x00\x1a\x00\x79\x00\x1a\x00\x30\x00\x17\x00\x85\x00\x49\x00\x74\x00\x2f\x00\x19\x00\x4a\x00\x17\x00\x1a\x00\x30\x00\x76\x00\x77\x00\x19\x00\x17\x00\x3e\x00\x1a\x00\x34\x00\x9d\x00\x19\x00\x7d\x00\x4e\x00\x1a\x00\x46\x00\x36\x00\x37\x00\x17\x00\x43\x00\x17\x00\x4f\x00\x4b\x00\x19\x00\x2e\x00\x19\x00\x1a\x00\x34\x00\x1a\x00\x22\x00\x9a\x00\x24\x00\x17\x00\x35\x00\x36\x00\x37\x00\x18\x00\x19\x00\x29\x00\x12\x00\x1a\x00\x13\x00\x86\x00\x49\x00\x44\x00\x89\x00\x3a\x00\x4a\x00\x48\x00\x49\x00\x3c\x00\x8e\x00\x2c\x00\x4a\x00\x56\x00\x8f\x00\x6d\x00\x9a\x00\x55\x00\x56\x00\x57\x00\x56\x00\x57\x00\x8a\x00\x63\x00\x1d\x00\x56\x00\x56\x00\x83\x00\x84\x00\x42\x00\x56\x00\x2d\x00\x22\x00\x9b\x00\x24\x00\x22\x00\x65\x00\x24\x00\x22\x00\x69\x00\x24\x00\x40\x00\x41\x00\x42\x00\x22\x00\x62\x00\x24\x00\x22\x00\x23\x00\x24\x00\x52\x00\x53\x00\x73\x00\x74\x00\x2b\x00\x2c\x00\x10\x00\x13\x00\x14\x00\x16\x00\x1f\x00\x71\x00\x55\x00\x9f\x00\x8d\x00\x4f\x00\x96\x00\x95\x00\x6b\x00\x8a\x00\x92\x00\x93\x00\x6c\x00\x6d\x00\x6f\x00\x76\x00\x81\x00\x97\x00\x7b\x00\x7c\x00\x7d\x00\x83\x00\x4d\x00\x82\x00\x55\x00\x50\x00\x3c\x00\x40\x00\x48\x00\x21\x00\x00\x00\x34\x00\x00\x00\x51\x00\x00\x00\x3e\x00\x33\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x3e\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 90) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90)
	]

happy_n_terms = 43 :: Int
happy_n_nonterms = 33 :: Int

happyReduce_1 = happySpecReduce_0  0# happyReduction_1
happyReduction_1  =  happyIn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  0# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_haskell  happy_var_1) -> 
	happyIn5
		 (Haskell happy_var_1
	)}

happyReduce_5 = happyReduce 5# 1# happyReduction_5
happyReduction_5 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (T_disname  happy_var_2) -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	case happyOut30 happy_x_5 of { happy_var_5 -> 
	happyIn5
		 (DisDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_6 = happyReduce 5# 1# happyReduction_6
happyReduction_6 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn5
		 (Constant happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_7 = happyReduce 7# 1# happyReduction_7
happyReduction_7 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	case happyOut7 happy_x_6 of { happy_var_6 -> 
	happyIn5
		 (Enum happy_var_2 (TypeVar happy_var_4 Nothing) happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_8 = happySpecReduce_2  1# happyReduction_8
happyReduction_8 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (Prefix happy_var_2
	)}

happyReduce_9 = happySpecReduce_1  1# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_c happy_var_1) -> 
	happyIn5
		 (C happy_var_1
	)}

happyReduce_10 = happySpecReduce_1  1# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_hinclude happy_var_1) -> 
	happyIn5
		 (Include happy_var_1
	)}

happyReduce_11 = happySpecReduce_2  1# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (DllName happy_var_2
	)}

happyReduce_12 = happySpecReduce_2  1# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (CConv happy_var_2
	)}

happyReduce_13 = happySpecReduce_0  2# happyReduction_13
happyReduction_13  =  happyIn6
		 ([]
	)

happyReduce_14 = happySpecReduce_2  2# happyReduction_14
happyReduction_14 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_disname  happy_var_1) -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_15 = happySpecReduce_1  3# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ([happy_var_1]
	)}

happyReduce_16 = happySpecReduce_3  3# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_17 = happySpecReduce_1  4# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ((happy_var_1, happy_var_1)
	)}

happyReduce_18 = happySpecReduce_3  4# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 ((happy_var_1, happy_var_3)
	)}}

happyReduce_19 = happySpecReduce_3  4# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (T_cexp     happy_var_3) -> 
	happyIn8
		 ((happy_var_1, happy_var_3)
	)}}

happyReduce_20 = happySpecReduce_0  5# happyReduction_20
happyReduction_20  =  happyIn9
		 ([]
	)

happyReduce_21 = happySpecReduce_3  5# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (happy_var_2
	)}

happyReduce_22 = happySpecReduce_0  6# happyReduction_22
happyReduction_22  =  happyIn10
		 ([]
	)

happyReduce_23 = happySpecReduce_1  6# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  7# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ([happy_var_1]
	)}

happyReduce_25 = happySpecReduce_3  7# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (happy_var_1:happy_var_3
	)}}

happyReduce_26 = happySpecReduce_1  8# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_name     happy_var_1) -> 
	happyIn12
		 (happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  8# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_disname  happy_var_1) -> 
	happyIn12
		 (happy_var_1
	)}

happyReduce_28 = happyReduce 6# 9# happyReduction_28
happyReduction_28 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	case happyOut28 happy_x_5 of { happy_var_5 -> 
	case happyOut24 happy_x_6 of { happy_var_6 -> 
	happyIn13
		 (ProcSpec happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_29 = happyMonadReduce 5# 10# happyReduction_29
happyReduction_29 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut15 happy_x_5 of { happy_var_5 -> 
	( let (mb_ctxt, t) = happy_var_5 in getSrcLoc >>= \ sl -> return (setSrcLocName happy_var_3 sl, happy_var_3, happy_var_2, mb_ctxt, t))}}}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_30 = happySpecReduce_3  11# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 ((Just happy_var_1, happy_var_3)
	)}}

happyReduce_31 = happySpecReduce_1  11# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((Nothing, happy_var_1)
	)}

happyReduce_32 = happySpecReduce_1  12# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_33 = happySpecReduce_3  12# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (Arrow happy_var_1 happy_var_3
	)}}

happyReduce_34 = happySpecReduce_3  13# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (happy_var_2
	)}

happyReduce_35 = happySpecReduce_1  13# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (happy_var_1
	)}

happyReduce_36 = happyReduce 4# 13# happyReduction_36
happyReduction_36 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 (typeApply (TypeVar happy_var_3 (Just happy_var_1)) happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_37 = happySpecReduce_2  13# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (typeApply (TypeVar happy_var_1 Nothing) happy_var_2
	)}}

happyReduce_38 = happySpecReduce_3  13# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (TypeList happy_var_2
	)}

happyReduce_39 = happySpecReduce_0  14# happyReduction_39
happyReduction_39  =  happyIn18
		 ([]
	)

happyReduce_40 = happySpecReduce_2  14# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_41 = happySpecReduce_3  15# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (happy_var_2
	)}

happyReduce_42 = happySpecReduce_1  15# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  15# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (TypeVar happy_var_3 (Just happy_var_1)
	)}}

happyReduce_44 = happySpecReduce_1  15# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (TypeVar happy_var_1 Nothing
	)}

happyReduce_45 = happySpecReduce_2  16# happyReduction_45
happyReduction_45 happy_x_2
	happy_x_1
	 =  happyIn20
		 (TypeTuple []
	)

happyReduce_46 = happySpecReduce_3  16# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (TypeTuple happy_var_2
	)}

happyReduce_47 = happySpecReduce_3  17# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 ([happy_var_1, happy_var_3]
	)}}

happyReduce_48 = happySpecReduce_3  17# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_49 = happySpecReduce_1  18# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_ccode    happy_var_1) -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  18# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_cexp     happy_var_1) -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_51 = happySpecReduce_0  19# happyReduction_51
happyReduction_51  =  happyIn23
		 (Nothing
	)

happyReduce_52 = happySpecReduce_2  19# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (Just happy_var_2
	)}

happyReduce_53 = happySpecReduce_0  20# happyReduction_53
happyReduction_53  =  happyIn24
		 (Nothing
	)

happyReduce_54 = happySpecReduce_1  20# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_end      happy_var_1) -> 
	happyIn24
		 (Just happy_var_1
	)}

happyReduce_55 = happySpecReduce_0  21# happyReduction_55
happyReduction_55  =  happyIn25
		 (Nothing
	)

happyReduce_56 = happySpecReduce_1  21# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_safecode happy_var_1) -> 
	happyIn25
		 (Just (True,happy_var_1)
	)}

happyReduce_57 = happySpecReduce_1  21# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_ccode    happy_var_1) -> 
	happyIn25
		 (Just (False,happy_var_1)
	)}

happyReduce_58 = happySpecReduce_0  22# happyReduction_58
happyReduction_58  =  happyIn26
		 (Nothing
	)

happyReduce_59 = happySpecReduce_1  22# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (Just happy_var_1
	)}

happyReduce_60 = happySpecReduce_3  23# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 ([(happy_var_2, happy_var_3)]
	)}}

happyReduce_61 = happyReduce 4# 23# happyReduction_61
happyReduction_61 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	case happyOut27 happy_x_4 of { happy_var_4 -> 
	happyIn27
		 ((happy_var_2, happy_var_3) : happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_62 = happySpecReduce_0  24# happyReduction_62
happyReduction_62  =  happyIn28
		 (Nothing
	)

happyReduce_63 = happySpecReduce_2  24# happyReduction_63
happyReduction_63 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (Just happy_var_2
	)}

happyReduce_64 = happySpecReduce_0  25# happyReduction_64
happyReduction_64  =  happyIn29
		 (Nothing
	)

happyReduce_65 = happySpecReduce_3  25# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (Just happy_var_2
	)}

happyReduce_66 = happySpecReduce_3  26# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (happy_var_2
	)}

happyReduce_67 = happySpecReduce_2  26# happyReduction_67
happyReduction_67 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_disname  happy_var_1) -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (apply (Var happy_var_1) happy_var_2
	)}}

happyReduce_68 = happySpecReduce_2  26# happyReduction_68
happyReduction_68 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_name     happy_var_1) -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (apply (Constructor happy_var_1) happy_var_2
	)}}

happyReduce_69 = happySpecReduce_3  26# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_reccon   happy_var_1) -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (let (fs,ds) = unzip happy_var_2
                                                in Apply (Record happy_var_1 fs) ds
	)}}

happyReduce_70 = happySpecReduce_2  26# happyReduction_70
happyReduction_70 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_kind     happy_var_1) -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (apply (Kind happy_var_1) [happy_var_2]
	)}}

happyReduce_71 = happyReduce 4# 26# happyReduction_71
happyReduction_71 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (T_user     happy_var_2) -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	happyIn30
		 (let (marshall,unmarshall,kind) = happy_var_2 
						  in apply (UserDIS False kind marshall unmarshall) happy_var_4
	) `HappyStk` happyRest}}

happyReduce_72 = happyReduce 4# 26# happyReduction_72
happyReduction_72 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (T_user     happy_var_2) -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	happyIn30
		 (let (marshall,unmarshall,kind) = happy_var_2 
						  in apply (UserDIS True kind marshall unmarshall) happy_var_4
	) `HappyStk` happyRest}}

happyReduce_73 = happySpecReduce_1  26# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_74 = happySpecReduce_1  26# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_cexp     happy_var_1) -> 
	happyIn30
		 (CCode happy_var_1
	)}

happyReduce_75 = happySpecReduce_2  26# happyReduction_75
happyReduction_75 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_cexp     happy_var_1) -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (apply (CCode happy_var_1) [happy_var_2]
	)}}

happyReduce_76 = happyReduce 5# 26# happyReduction_76
happyReduction_76 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (T_cexp     happy_var_2) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut30 happy_x_5 of { happy_var_5 -> 
	happyIn30
		 (apply (Declare happy_var_2 (Var happy_var_3)) [happy_var_5]
	) `HappyStk` happyRest}}}

happyReduce_77 = happySpecReduce_3  27# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (happy_var_2
	)}

happyReduce_78 = happySpecReduce_1  27# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_disname  happy_var_1) -> 
	happyIn31
		 (Var happy_var_1
	)}

happyReduce_79 = happySpecReduce_1  27# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_name     happy_var_1) -> 
	happyIn31
		 (Constructor happy_var_1
	)}

happyReduce_80 = happySpecReduce_1  27# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_81 = happySpecReduce_1  27# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T_cexp     happy_var_1) -> 
	happyIn31
		 (CCode happy_var_1
	)}

happyReduce_82 = happySpecReduce_0  28# happyReduction_82
happyReduction_82  =  happyIn32
		 ([]
	)

happyReduce_83 = happySpecReduce_2  28# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_84 = happySpecReduce_2  29# happyReduction_84
happyReduction_84 happy_x_2
	happy_x_1
	 =  happyIn33
		 (Tuple
	)

happyReduce_85 = happySpecReduce_3  29# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (apply Tuple happy_var_2
	)}

happyReduce_86 = happySpecReduce_3  30# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 ([happy_var_1, happy_var_3]
	)}}

happyReduce_87 = happySpecReduce_3  30# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_88 = happySpecReduce_1  31# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 ([happy_var_1]
	)}

happyReduce_89 = happySpecReduce_3  31# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_90 = happySpecReduce_3  32# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 ((happy_var_1, happy_var_3)
	)}}

happyNewToken action sts stk
	= topLex(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	T_eof -> happyDoAction 42# tk action sts stk;
	T_fun -> cont 1#;
	T_call -> cont 2#;
	T_result -> cont 3#;
	T_fail -> cont 4#;
	T_const -> cont 5#;
	T_enum -> cont 6#;
	T_hinclude happy_dollar_dollar -> cont 7#;
	T_dis -> cont 8#;
	T_dllname -> cont 9#;
	T_callconv -> cont 10#;
	T_prefix -> cont 11#;
	T_c happy_dollar_dollar -> cont 12#;
	T_oparen -> cont 13#;
	T_cparen -> cont 14#;
	T_odangle -> cont 15#;
	T_cangle -> cont 16#;
	T_oangle -> cont 17#;
	T_cdangle -> cont 18#;
	T_ccurly -> cont 19#;
	T_osquare -> cont 20#;
	T_csquare -> cont 21#;
	T_comma -> cont 22#;
	T_dot -> cont 23#;
	T_equal -> cont 24#;
	T_darrow -> cont 25#;
	T_arrow -> cont 26#;
	T_dcolon -> cont 27#;
	T_dquote -> cont 28#;
	T_declare -> cont 29#;
	T_in -> cont 30#;
	T_name     happy_dollar_dollar -> cont 31#;
	T_reccon   happy_dollar_dollar -> cont 32#;
	T_disname  happy_dollar_dollar -> cont 33#;
	T_kind     happy_dollar_dollar -> cont 34#;
	T_user     happy_dollar_dollar -> cont 35#;
	T_haskell  happy_dollar_dollar -> cont 36#;
	T_cexp     happy_dollar_dollar -> cont 37#;
	T_ccode    happy_dollar_dollar -> cont 38#;
	T_safecode happy_dollar_dollar -> cont 39#;
	T_end      happy_dollar_dollar -> cont 40#;
	T_unknown  happy_dollar_dollar -> cont 41#;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => LexM a -> (a -> LexM b) -> LexM b
happyThen = (thenLexM)
happyReturn :: () => a -> LexM a
happyReturn = (returnLexM)
happyThen1 = happyThen
happyReturn1 :: () => a -> LexM a
happyReturn1 = happyReturn
happyError' :: () => Token -> LexM a
happyError' tk = (\token -> happyError) tk

gcParse = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


happyError :: LexM a
happyError = do
 l   <- getSrcLoc
 str <- getStream
 error (show l ++ ": Parse error: " ++ take 100 str)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList





{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
