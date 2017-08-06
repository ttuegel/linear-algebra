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

data GE :: (Nat, Nat) -> * -> * where
  GE ::
    { getrans :: !Trans
    , gedim   :: {-# UNPACK #-} !(N m)
    , gecodim :: {-# UNPACK #-} !(N n)
    , geptr   :: {-# UNPACK #-} !(ForeignPtr a)
    , gelead  :: {-# UNPACK #-} !I
    }
    -> GE '(m, n) a

data GB :: (Nat, Nat) -> * -> * where
  GB ::
    { gbtrans     :: !Trans
    , gbdim       :: {-# UNPACK #-} !(N m)
    , gbcodim     :: {-# UNPACK #-} !(N n)
    , gbsubdiag   :: {-# UNPACK #-} !I
    , gbsuperdiag :: {-# UNPACK #-} !I
    , gbptr       :: {-# UNPACK #-} !I
    , gblead      :: {-# UNPACK #-} !I
    }
    -> GB '(m, n) a

data HE :: (Nat, Nat) -> * -> * where
  HE ::
    { heuplo :: !UpLo
    , hedim  :: {-# UNPACK #-} !(N n)
    , heptr  :: {-# UNPACK #-} !(ForeignPtr a)
    , helead :: {-# UNPACK #-} !I
    }
    -> HE '(n, n) a

data HB :: (Nat, Nat) -> * -> * where
  HB ::
    { hbuplo    :: !UpLo
    , hbdim     :: {-# UNPACK #-} !(N n)
    , hboffdiag :: {-# UNPACK #-} !I
    , hbptr     :: {-# UNPACK #-} !(ForeignPtr a)
    , hblead    :: {-# UNPACK #-} !I
    }
    -> HB '(n, n) a

data HP :: (Nat, Nat) -> * -> * where
  HP ::
    { hpuplo :: !UpLo
    , hpdim  :: {-# UNPACK #-} !(N n)
    , hpptr  :: {-# UNPACK #-} !(ForeignPtr a)
    }
    -> HP '(n, n) a

data TR :: (Nat, Nat) -> * -> * where
  TR ::
    { truplo  :: !UpLo
    , trtrans :: !Trans
    , trdiag  :: !Diag
    , trdim   :: {-# UNPACK #-} !(N n)  -- dimension
    , trptr   :: {-# UNPACK #-} !(ForeignPtr a)
    , trlead  :: {-# UNPACK #-} !I  -- leading dimension
    }
    -> TR '(n, n) a

data TB :: (Nat, Nat) -> * -> * where
  TB ::
    { tbuplo    :: !UpLo
    , tbtrans   :: !Trans
    , tbdiag    :: !Diag
    , tbdim     :: {-# UNPACK #-} !(N n)  -- dimension
    , tboffdiag :: {-# UNPACK #-} !I  -- off-diagonals
    , tbptr     :: {-# UNPACK #-} !(ForeignPtr a)
    , tblead    :: {-# UNPACK #-} !I  -- leading dimension
    }
    -> TB '(n, n) a

data TP :: (Nat, Nat) -> * -> * where
  TP ::
    { tpuplo  :: !UpLo
    , tptrans :: !Trans
    , tpdiag  :: !Diag
    , tpdim   :: {-# UNPACK #-} !(N n)  -- dimension
    , tpptr   :: {-# UNPACK #-} !(ForeignPtr a)
    }
    -> TP '(n, n) a
