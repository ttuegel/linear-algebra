module Internal.Matrix where

import Foreign.ForeignPtr (ForeignPtr)

import Internal.Int


data UpLo = Upper | Lower

uploToI :: UpLo -> I
uploToI Upper = 121
uploToI Lower = 122

data Diag = NonUnit | Unit

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
    {-# UNPACK #-} !(N m)  -- rows
    {-# UNPACK #-} !(N n)  -- columns
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

data GB (m :: Dim) (n :: Dim) a
  = GB !Trans
    {-# UNPACK #-} !(N m)  -- rows
    {-# UNPACK #-} !(N n)  -- columns
    {-# UNPACK #-} !I  -- sub-diagonals
    {-# UNPACK #-} !I  -- super-diagonals
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

data HE (n :: Dim) a
  = HE !UpLo
    {-# UNPACK #-} !(N n)  -- dimension
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

data HB (n :: Dim) a
  = HB !UpLo
    {-# UNPACK #-} !(N n)  -- dimension
    {-# UNPACK #-} !I  -- sub- or super-diagonals
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

data HP (n :: Dim) a
  = HP !UpLo
    {-# UNPACK #-} !(N n)  -- dimension
    {-# UNPACK #-} !(ForeignPtr a)

data TR (n :: Dim) a
  = TR !UpLo !Trans !Diag
    {-# UNPACK #-} !(N n)  -- dimension
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

data TB (n :: Dim) a
  = TB !UpLo !Trans !Diag
    {-# UNPACK #-} !(N n)  -- dimension
    {-# UNPACK #-} !I  -- off-diagonals
    {-# UNPACK #-} !(ForeignPtr a)
    {-# UNPACK #-} !I  -- leading dimension

data TP (n :: Dim) a
  = TP !UpLo !Trans !Diag
    {-# UNPACK #-} !(N n)  -- dimension
    {-# UNPACK #-} !(ForeignPtr a)
