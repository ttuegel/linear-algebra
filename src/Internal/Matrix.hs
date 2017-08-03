module Internal.Matrix where

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

import Data.Dim
import Internal.Int
import Internal.Mut


data UpLo = Upper | Lower

uploToI :: UpLo -> I
uploToI Upper = 121
uploToI Lower = 122

data Diag = NonUnit | Unit

instance Enum Diag where
  toEnum 131 = NonUnit
  toEnum 132 = Unit
  toEnum x = error ("CblasDiag: illegal value `" ++ show x ++ "'")

diagToI :: Diag -> I
diagToI NonUnit = 131
diagToI Unit = 132

data Trans = NoTrans | Trans | ConjTrans

transToI :: Trans -> I
transToI NoTrans = 111
transToI Trans = 112
transToI ConjTrans = 113

data GE (m :: Dim) (n :: Dim) a
  = GE !Trans
    {-# UNPACK #-} !I  -- rows
    {-# UNPACK #-} !I  -- columns
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

unsafeWithGE
  :: GE m n a
  -> (Trans -> I -> I -> Ptr a -> I -> IO b)
  -> IO b
unsafeWithGE (GE tr nr nc fp ld) cont =
  withForeignPtr fp $ \p -> cont tr nr nc p ld

withGE
  :: Mut (GE m n) s a
  -> (Trans -> I -> I -> Ptr a -> I -> IO b)
  -> IO b
withGE (Mut (GE tr nr nc fp ld)) cont =
  withForeignPtr fp $ \p -> cont tr nr nc p ld

data GB (m :: Dim) (n :: Dim) a
  = GB !Trans
    {-# UNPACK #-} !I  -- rows
    {-# UNPACK #-} !I  -- columns
    {-# UNPACK #-} !I  -- sub-diagonals
    {-# UNPACK #-} !I  -- super-diagonals
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

unsafeWithGB
  :: GB m n a
  -> (Trans -> I -> I -> I -> I -> Ptr a -> I -> IO b)
  -> IO b
unsafeWithGB (GB tr nr nc kl ku fp ld) cont =
  withForeignPtr fp $ \p -> cont tr nr nc kl ku p ld

data HE (n :: Dim) a
  = HE !UpLo
    {-# UNPACK #-} !I  -- dimension
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

unsafeWithHE :: HE n a -> (UpLo -> I -> Ptr a -> I -> IO b) -> IO b
unsafeWithHE (HE up nn fp ld) cont =
  withForeignPtr fp $ \p -> cont up nn p ld

withHE
  :: Mut (HE n) s a
  -> (UpLo -> I -> Ptr a -> I -> IO b)
  -> IO b
withHE (Mut (HE up nn fp ld)) cont =
  withForeignPtr fp $ \p -> cont up nn p ld

data HB (n :: Dim) a
  = HB !UpLo
    {-# UNPACK #-} !I  -- dimension
    {-# UNPACK #-} !I  -- sub- or super-diagonals
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

unsafeWithHB :: HB n a -> (UpLo -> I -> I -> Ptr a -> I -> IO b) -> IO b
unsafeWithHB (HB up nn kk fp ld) cont =
  withForeignPtr fp $ \p -> cont up nn kk p ld

data HP (n :: Dim) a
  = HP !UpLo
    {-# UNPACK #-} !I  -- dimension
    {-# UNPACK #-} !(ForeignPtr a)

unsafeWithHP :: HP n a -> (UpLo -> I -> Ptr a -> IO b) -> IO b
unsafeWithHP (HP up nn fp) cont =
  withForeignPtr fp $ \p -> cont up nn p

withHP
  :: Mut (HP n) s a
  -> (UpLo -> I -> Ptr a -> IO b)
  -> IO b
withHP (Mut (HP up nn fp)) cont =
  withForeignPtr fp $ \ p -> cont up nn p

data TR (n :: Dim) a
  = TR !UpLo !Trans !Diag
    {-# UNPACK #-} !I  -- dimension
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

unsafeWithTR
  :: TR n a
  -> (UpLo -> Trans -> Diag -> I -> Ptr a -> I -> IO b)
  -> IO b
unsafeWithTR (TR up tr di nn fp ld) cont =
  withForeignPtr fp $ \p -> cont up tr di nn p ld

data TB (n :: Dim) a
  = TB !UpLo !Trans !Diag
    {-# UNPACK #-} !I  -- dimension
    {-# UNPACK #-} !I  -- off-diagonals
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

unsafeWithTB
  :: TB n a
  -> (UpLo -> Trans -> Diag -> I -> I -> Ptr a -> I -> IO b)
  -> IO b
unsafeWithTB (TB up tr di nn kk fp ld) cont =
  withForeignPtr fp $ \p -> cont up tr di nn kk p ld

data TP (n :: Dim) a
  = TP !UpLo !Trans !Diag
    {-# UNPACK #-} !I  -- dimension
    {-# UNPACK #-} !(ForeignPtr a)

unsafeWithTP
  :: TP n a
  -> (UpLo -> Trans -> Diag -> I -> Ptr a -> IO b)
  -> IO b
unsafeWithTP (TP up tr di nn fp) cont =
  withForeignPtr fp $ \p -> cont up tr di nn p
