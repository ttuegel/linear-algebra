{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.TH where

import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Data.Complex
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Dual(..), Endo(..))
import Data.Semigroup (Semigroup(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (with)
import Language.Haskell.TH

import qualified Data.Map as Map

import Internal.Int (I, Dim, N(..))
import Internal.Matrix
import Internal.Mut
import Internal.Vector (V(..))
import Internal.Writer


getsUpLo :: Map Name (Q Exp)
getsUpLo =
  Map.fromList
  [ (''GE, nonTriangular ''GE)
  , (''GB, nonTriangular ''GB)
  , (''HE, [| \case HE up _ _ _ -> up |])
  , (''HB, [| \case HB up _ _ _ _ -> up |])
  , (''HP, [| \case HP up _ _ -> up |])
  , (''TR, [| \case TR up _ _ _ _ _ -> up |])
  , (''TB, [| \case TB up _ _ _ _ _ _ -> up |])
  , (''TP, [| \case TP up _ _ _ _ -> up |])
  ]
  where
    nonTriangular ty =
      fail ("`" ++ show ty ++ "' is not a triangular or Hermitian matrix type")

getsTrans :: Map Name (Q Exp)
getsTrans =
  Map.fromList
  [ (''GE, [| \case GE tr _ _ _ _ -> tr |])
  , (''GB, [| \case GB tr _ _ _ _ _ _ -> tr |])
  , (''HE, hermitian ''HE)
  , (''HB, hermitian ''HB)
  , (''HP, hermitian ''HP)
  , (''TR, [| \case TR _ tr _ _ _ _ -> tr |])
  , (''TB, [| \case TB _ tr _ _ _ _ _ -> tr |])
  , (''TP, [| \case TP _ tr _ _ _ -> tr |])
  ]
  where
    hermitian ty =
      fail ("`" ++ show ty ++ "' is a Hermitian matrix type")

getsDiag :: Map Name (Q Exp)
getsDiag =
  Map.fromList
  [ (''GE, nonTriangular ''GE)
  , (''GB, nonTriangular ''GB)
  , (''HE, nonTriangular ''HE)
  , (''HB, nonTriangular ''HB)
  , (''HP, nonTriangular ''HP)
  , (''TR, [| \case TR _ _ d _ _ _ -> d |])
  , (''TB, [| \case TB _ _ d _ _ _ _ -> d |])
  , (''TP, [| \case TP _ _ d _ _ -> d |])
  ]
  where
    nonTriangular ty =
      fail ("`" ++ show ty ++ "' is not a triangular matrix type")

getsDim :: Map Name (Q Exp)
getsDim =
  Map.fromList
  [ (''GE, nonSquare ''GE)
  , (''GB, nonSquare ''GB)
  , (''HE, [| \case HE _ (N n) _ _ -> n |])
  , (''HB, [| \case HB _ (N n) _ _ _ -> n |])
  , (''HP, [| \case HP _ (N n) _ -> n |])
  , (''TR, [| \case TR _ _ _ (N n) _ _ -> n |])
  , (''TB, [| \case TB _ _ _ (N n) _ _ _ -> n |])
  , (''TP, [| \case TP _ _ _ (N n) _ -> n |])
  , (''V, [| \case V (N n) _ _ -> n |])
  ]
  where
    nonSquare ty =
      fail ("`" ++ show ty ++ "' is not a square matrix type")

getsRows :: Map Name (Q Exp)
getsRows =
  Map.fromList
  [ (''GE, [| \case GE _ (N n) _ _ _ -> n |])
  , (''GB, [| \case GB _ (N n) _ _ _ _ _ -> n |])
  , (''HE, square ''HE)
  , (''HB, square ''HB)
  , (''HP, square ''HP)
  , (''TR, square ''TR)
  , (''TB, square ''TB)
  , (''TP, square ''TP)
  ]
  where
    square ty =
      fail ("`" ++ show ty ++ "' is a square matrix type")

getsCols :: Map Name (Q Exp)
getsCols =
  Map.fromList
  [ (''GE, [| \case GE _ _ (N n) _ _ -> n |])
  , (''GB, [| \case GB _ _ (N n) _ _ _ _ -> n |])
  , (''HE, square ''HE)
  , (''HB, square ''HB)
  , (''HP, square ''HP)
  , (''TR, square ''TR)
  , (''TB, square ''TB)
  , (''TP, square ''TP)
  ]
  where
    square ty =
      fail ("`" ++ show ty ++ "' is a square matrix type")

getsBand :: Map Name (Q Exp)
getsBand =
  Map.fromList
  [ (''GE, nonSquare ''GE)
  , (''GB, nonSquare ''GB)
  , (''HE, nonBanded ''HE)
  , (''HB, [| \case HB _ _ b _ _ -> b |])
  , (''HP, nonBanded ''HP)
  , (''TR, nonBanded ''TR)
  , (''TB, [| \case TB _ _ _ _ b _ _ -> b |])
  , (''TP, nonBanded ''TP)
  ]
  where
    nonSquare ty =
      fail ("`" ++ show ty ++ "' is not a square matrix type")
    nonBanded ty =
      fail ("`" ++ show ty ++ "' is not a banded matrix type")

getsLower :: Map Name (Q Exp)
getsLower =
  Map.fromList
  [ (''GE, nonBanded ''GE)
  , (''GB, [| \case GB _ _ _ l _ _ _ -> l |])
  , (''HE, nonBanded ''HE)
  , (''HB, square ''HB)
  , (''HP, nonBanded ''HP)
  , (''TR, nonBanded ''TR)
  , (''TB, square ''TB)
  , (''TP, nonBanded ''TP)
  ]
  where
    square ty =
      fail ("`" ++ show ty ++ "' is a square matrix type")
    nonBanded ty =
      fail ("`" ++ show ty ++ "' is not a banded matrix type")

getsUpper :: Map Name (Q Exp)
getsUpper =
  Map.fromList
  [ (''GE, nonBanded ''GE)
  , (''GB, [| \case GB _ _ _ _ u _ _ -> u |])
  , (''HE, nonBanded ''HE)
  , (''HB, square ''HB)
  , (''HP, nonBanded ''HP)
  , (''TR, nonBanded ''TR)
  , (''TB, square ''TB)
  , (''TP, nonBanded ''TP)
  ]
  where
    square ty =
      fail ("`" ++ show ty ++ "' is a square matrix type")
    nonBanded ty =
      fail ("`" ++ show ty ++ "' is not a banded matrix type")

getsPtr :: Map Name (Q Exp)
getsPtr =
  Map.fromList
  [ (''GE, [| \case GE _ _ _ p l -> \cont -> cont p l |])
  , (''GB, [| \case GB _ _ _ _ _ p l -> \cont -> cont p l |])
  , (''HE, [| \case HE _ _ p l -> \cont -> cont p l |])
  , (''HB, [| \case HB _ _ _ p l -> \cont -> cont p l |])
  , (''HP, [| \case HP _ _ p -> \cont -> cont p |])
  , (''TR, [| \case TR _ _ _ _ p l -> \cont -> cont p l |])
  , (''TB, [| \case TB _ _ _ _ _ p l -> \cont -> cont p l |])
  , (''TP, [| \case TP _ _ _ _ p -> \cont -> cont p |])
  , (''V, [| \case V _ p l -> \cont -> cont p l |])
  ]

isPacked :: Map Name Bool
isPacked =
  Map.fromList
  [ (''GE, False)
  , (''GB, False)
  , (''HE, False)
  , (''HB, False)
  , (''HP, True)
  , (''TR, False)
  , (''TB, False)
  , (''TP, True)
  ]

newtype C a = C { getC :: a }

newtype Hs a = Hs { getHs :: a }

newtype HsExp = HsExp { getHsExp :: Exp }

class Build a where
  data Acc a
  type M a
  type D a
  type Nm a
  build :: Monoid (Acc a) => Nm a -> (M a -> WriterT (Acc a) Q Type) -> Q a
  bind :: String -> WriterT (Acc a) Q (Nm a)
  order :: WriterT (Acc a) Q ()
  uplo :: Name -> Nm a -> WriterT (Acc a) Q ()
  trans :: Name -> Nm a -> WriterT (Acc a) Q ()
  diag :: Name -> Nm a -> WriterT (Acc a) Q ()
  dim :: Name -> Nm a -> WriterT (Acc a) Q (D a)
  dimMut :: Name -> Nm a -> WriterT (Acc a) Q (D a)
  rows :: Name -> Nm a -> WriterT (Acc a) Q (D a)
  cols :: Name -> Nm a -> WriterT (Acc a) Q (D a)
  band :: Name -> Nm a -> WriterT (Acc a) Q ()
  lower :: Name -> Nm a -> WriterT (Acc a) Q ()
  upper :: Name -> Nm a -> WriterT (Acc a) Q ()
  vec :: Nm a -> D a -> Q Type -> WriterT (Acc a) Q ()
  mutVec :: Nm a -> M a -> D a -> Q Type -> WriterT (Acc a) Q ()
  scalar :: Nm a -> Q Type -> WriterT (Acc a) Q ()
  mat :: Name -> Nm a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
  mutMat :: Name -> Nm a -> M a -> D a -> D a -> Q Type -> WriterT (Acc a) Q ()
  sqMat :: Name -> Nm a -> D a -> Q Type -> WriterT (Acc a) Q ()
  mutSqMat :: Name -> Nm a -> M a -> D a -> Q Type -> WriterT (Acc a) Q ()

tellC :: (Q Type -> Q Type) -> WriterT (Acc (C Type)) Q ()
tellC = tell . AccC . Endo

instance Build (C Type) where
  newtype Acc (C Type) = AccC { getType :: Endo (Q Type) }
    deriving (Monoid, Semigroup)
  type M (C Type) = ()
  type D (C Type) = ()
  type Nm (C Type) = ()

  build _ go = do
    (a, finish) <- runWriterT (go ())
    let r = calling a [t| IO $(pure a) |] [t| Ptr $(pure a) -> IO () |]
    C <$> (appEndo . getType) finish r

  bind _ = pure ()

  order = tellC $ \r -> [t| I -> $r |]

  uplo _ _ = tellC $ \r -> [t| I -> $r |]

  trans _ _ = tellC $ \r -> [t| I -> $r |]

  diag _ _ = tellC $ \r -> [t| I -> $r |]

  dim _ _ = tellC $ \r -> [t| I -> $r |]

  dimMut = dim

  rows = dim

  cols = dim

  band _ _ = tellC $ \r -> [t| I -> $r |]

  lower = band

  upper = band

  vec _ _ a = tellC $ \r -> [t| Ptr $a -> I -> $r |]
  mutVec e _ = vec e

  scalar _ _a = lift _a >>= \_a -> calling _a byValue byRef
    where
      byValue = tellC $ \r -> [t| $_a -> $r |]
      byRef = tellC $ \r -> [t| Ptr $_a -> $r |]

  mat nTy _ _ _ a = do
    let notMatrix = fail ("`" ++ show nTy ++ "' is not a matrix type")
    packed <- maybe notMatrix pure (Map.lookup nTy isPacked)
    if packed
      then tellC $ \r -> [t| Ptr $a -> $r |]
      else tellC $ \r -> [t| Ptr $a -> I -> $r |]

  mutMat nTy e _ = mat nTy e

  sqMat nTy _ _ a = do
    let notMatrix = fail ("`" ++ show nTy ++ "' is not a matrix type")
    packed <- maybe notMatrix pure (Map.lookup nTy isPacked)
    if packed
      then tellC $ \r -> [t| Ptr $a -> $r |]
      else tellC $ \r -> [t| Ptr $a -> I -> $r |]

  mutSqMat nTy e _ = sqMat nTy e

tellHsType :: (Q Type -> Q Type) -> WriterT (Acc (Hs Type)) Q ()
tellHsType = tell . AccHsType . Endo

instance Build (Hs Type) where
  newtype Acc (Hs Type) = AccHsType { getHsType :: Endo (Q Type) }
    deriving (Monoid, Semigroup)
  type M (Hs Type) = Type
  type D (Hs Type) = Type
  type Nm (Hs Type) = ()

  build _ go = do
    m <- newName "m"
    tv <- varT m
    (a, finish) <- runWriterT (go tv)
    let
      tvars = [ KindedTV m (AppT (AppT ArrowT StarT) StarT) ]
      ctx = (:[]) <$> [t| PrimMonad $(pure tv) |]
      r = [t| $(pure tv) $(pure a) |]
    (<$>) Hs $ forallT tvars ctx $ (appEndo . getHsType) finish r

  bind _ = pure ()

  order = pure ()

  uplo _ _ = pure ()

  trans _ _ = pure ()

  diag _ _ = pure ()

  dim _ _ = do
    n <- lift $ newName "n"
    dimk <- lift [t| Dim |]
    let
      tvars = [ KindedTV n dimk ]
      ctx = pure []
    tellHsType (forallT tvars ctx)
    lift (varT n)

  dimMut = dim

  rows = dim
  cols = dim

  band _ _ = pure ()
  lower = band
  upper = band

  vec _ n a = tellHsType $ \r -> [t| V $(pure n) $a -> $r |]

  mutVec _ m n a =
    tellHsType $ \r ->
      [t| Mut (V $(pure n)) (PrimState $(pure m)) $a -> $r |]

  scalar _ a = tellHsType $ \r -> [t| $a -> $r |]

  mat n _ j k a =
    tellHsType $ \r -> [t| $(conT n) $(pure j) $(pure k) $a -> $r |]

  mutMat n _ m j k a =
    tellHsType $ \r ->
      [t| Mut ($(conT n) $(pure j) $(pure k)) (PrimState $(pure m)) $a -> $r |]

  sqMat nTy _ n a = tellHsType $ \r -> [t| $(conT nTy) $(pure n) $a -> $r |]

  mutSqMat nTy _ m n a = tellHsType $ \r ->
    [t| Mut ($(conT nTy) $(pure n)) (PrimState $(pure m)) $a -> $r |]

tellHsCall :: (Q Exp -> Q Exp) -> WriterT (Acc Exp) Q ()
tellHsCall = tell . (\f -> mempty { getCall = f }) . Dual . Endo

tellHsBody :: (Q Exp -> Q Exp) -> WriterT (Acc Exp) Q ()
tellHsBody = tell . (\f -> mempty { getBody = f }) . Endo

tellHsBind :: (Q Exp -> Q Exp) -> WriterT (Acc Exp) Q ()
tellHsBind = tell . (\f -> mempty { getBind = f }) . Endo

dimWith :: Map Name (Q Exp) -> Name -> Name -> WriterT (Acc Exp) Q ()
dimWith gets nTy nExp =
  tellHsCall $ \call ->
    let
      notMatrix =
        fail ("`" ++ show nExp ++ " :: " ++ show nTy ++ "' is not matrix-typed")
      get = fromMaybe notMatrix (Map.lookup nTy gets)
      expr = varE nExp
    in
      [| $call ($get $expr) |]

getPtrOf :: Name -> Name -> WriterT (Acc Exp) Q ()
getPtrOf nTy nExp = do
  let
    notMatrix :: Q a
    notMatrix = fail ("`" ++ show nTy ++ "' has no pointer")
    get = fromMaybe notMatrix (Map.lookup nTy getsPtr)
    expr = varE nExp
  packed <- pure $ fromMaybe False (Map.lookup nTy isPacked)
  if packed
    then do
      p <- lift $ newName "p"
      tellHsCall $ \call -> [| $call $(varE p) |]
      tellHsBody $ \r ->
        [| $get $expr $ \fp ->
            withForeignPtr fp $(lam1E (varP p) r) |]
    else do
      p <- lift $ newName "p"
      i <- lift $ newName "i"
      tellHsCall $ \call -> [| $call $(varE p) $(varE i) |]
      tellHsBody $ \r ->
        [| $get $expr $ \fp $(varP i) ->
            withForeignPtr fp $(lam1E (varP p) r) |]

instance Build Exp where
  data Acc Exp
    = AccExp
    { getCall :: Dual (Endo (Q Exp))
    , getBody :: Endo (Q Exp)
    , getBind :: Endo (Q Exp)
    }
  type M Exp = ()
  type D Exp = ()
  type Nm Exp = Name

  build cnam go = do
    (a, finish) <- runWriterT (go ())
    let wrap = calling a id (\_body -> [| alloca $ \z -> $_body z >> peek z |])
        call = wrap ((appEndo . getDual . getCall) finish (varE cnam))
        body = (appEndo . getBody) finish call
    (appEndo . getBind) finish [| unsafePrimToPrim $body |]

  bind _nm = do
    _nm <- lift (newName _nm)
    tell mempty { getBind = Endo $ lam1E (varP _nm) }
    pure _nm

  order = tellHsCall $ \call -> [| $call 102 |]

  uplo nTy nExp = tellHsCall $ \call ->
    let
      notMatrix =
        fail ("`" ++ show nExp ++ " :: " ++ show nTy ++ "' is not matrix-typed")
      get = fromMaybe notMatrix (Map.lookup nTy getsUpLo)
      expr = varE nExp
    in
      [| $call (uploToI ($get $expr)) |]

  trans nTy nExp = tellHsCall $ \call ->
    let
      notMatrix =
        fail ("`" ++ show nExp ++ " :: " ++ show nTy ++ "' is not matrix-typed")
      get = fromMaybe notMatrix (Map.lookup nTy getsTrans)
      expr = varE nExp
    in
      [| $call (transToI ($get $expr)) |]

  diag nTy nExp = tellHsCall $ \call ->
    let
      notMatrix =
        fail ("`" ++ show nExp ++ " :: " ++ show nTy ++ "' is not matrix-typed")
      get = fromMaybe notMatrix (Map.lookup nTy getsDiag)
      expr = varE nExp
    in
      [| $call (diagToI ($get $expr)) |]

  dim = dimWith getsDim

  dimMut nTy nExp = do
    v <- lift (newName "v")
    tellHsBody $ \r -> [| case $(varE nExp) of Mut $(varP v) -> $r |]
    dim nTy v

  rows = dimWith getsRows

  cols = dimWith getsCols

  band = dimWith getsBand
  lower = dimWith getsLower
  upper = dimWith getsUpper

  vec v _ _ = getPtrOf ''V v

  mutVec v _ n a = do
    m <- lift (newName "m")
    tellHsBody $ \r -> [| case $(varE v) of Mut $(varP m) -> $r |]
    vec m n a

  scalar s a = do
    z <- lift $ newName "z"
    tellHsCall $ \call -> [| $call $(varE z) |]
    tellHsBody $ \r -> [| $(withE =<< a) $(varE s) $(lam1E (varP z) r) |]

  mat nTy nExp _ _ _ = getPtrOf nTy nExp

  mutMat nTy nExp _ j k a = do
    m <- lift (newName "m")
    tellHsBody $ \r -> [| case $(varE nExp) of Mut $(varP m) -> $r |]
    mat nTy m j k a

  sqMat nTy nExp n a = mat nTy nExp n n a

  mutSqMat nTy nExp m n a = mutMat nTy nExp m n n a

instance Semigroup (Acc Exp) where
  (<>) a b =
    AccExp
    { getCall = getCall a <> getCall b
    , getBody = getBody a <> getBody b
    , getBind = getBind a <> getBind b
    }

instance Monoid (Acc Exp) where
  mempty = AccExp { getCall = mempty, getBody = mempty, getBind = mempty }
  mappend = (<>)

dimGE :: Build a => Nm a -> WriterT (Acc a) Q (D a, D a)
dimGE m = do
  trans ''GE m
  (,) <$> rows ''GE m <*> cols ''GE m

dimGB :: Build a => Nm a -> WriterT (Acc a) Q (D a, D a)
dimGB m = do
  trans ''GB m
  n <- (,) <$> rows ''GB m <*> cols ''GB m
  lower ''GB m
  upper ''GB m
  pure n

dimHE :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimHE v = do
  uplo ''HE v
  dim ''HE v

dimHB :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimHB v = do
  uplo ''HB v
  n <- dim ''HB v
  band ''HB v
  pure n

dimHP :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimHP v = do
  uplo ''HP v
  dim ''HP v

dimTR :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimTR v = do
  uplo ''TR v
  trans ''TR v
  diag ''TR v
  dim ''TR v

dimTB :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimTB v = do
  uplo ''TB v
  trans ''TB v
  diag ''TB v
  n <- dim ''TB v
  band ''TB v
  pure n

dimTP :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimTP v = do
  uplo ''TP v
  trans ''TP v
  diag ''TP v
  dim ''TP v

dimVec :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimVec = dim ''V

dimMutVec :: Build a => Nm a -> WriterT (Acc a) Q (D a)
dimMutVec = dimMut ''V

withE :: Type -> Q Exp
withE a = calling a [| flip ($) |] [| with |]

allocaE :: Type -> Q Exp
allocaE a = calling a [| flip ($) () |] [| alloca |]

callE :: Type -> Q Exp
callE a = calling a [| \r _ -> r |] [| \f z -> f z >> peek z |]

calling :: Type -> a -> a -> a
calling t byVal byRef = do
  case t of
    AppT (ConT complex) _
      | complex == ''Complex -> byRef
    _ -> byVal

cblas_import :: String -> Q (C Type) -> Q (Name, Dec)
cblas_import fnam ctyp = do
  let
    cnam = "cblas_" ++ fnam
    hnam = mkName cnam
  (,) hnam <$> forImpD cCall unsafe cnam hnam (getC <$> ctyp)

cblas :: (forall a. Build a => M a -> WriterT (Acc a) Q Type)
      -> String
      -> Q [Dec]
cblas sig fnam = do
  (cnam, cdec) <- cblas_import fnam (build () sig)
  let hnam = mkName fnam
  body <- build cnam sig
  hsig <- sigD hnam (getHs <$> build () sig)
  hdec <- valD (varP hnam) (normalB $ pure body) []
  pure [cdec, hsig, hdec]

dot :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
dot t _ = do
  x <- bind "x"
  y <- bind "y"
  n <- dim ''V x
  vec x n t
  vec y n t
  lift t

asum :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
asum r t _ = do
  x <- bind "x"
  n <- dim ''V x
  vec x n t
  lift r

unitT :: WriterT s Q Type
unitT = lift (tupleT 0)

swap :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
swap t m = do
  x <- bind "x"
  y <- bind "y"
  n <- dimMutVec x
  mutVec x m n t
  mutVec y m n t
  unitT

copy :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
copy t m = do
  x <- bind "x"
  y <- bind "y"
  n <- dimVec x
  vec x n t
  mutVec y m n t
  unitT

axpy :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
axpy t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  n <- dimVec x
  scalar alpha t
  vec x n t
  mutVec y m n t
  unitT

scal :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
scal a t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  n <- dimMutVec x
  scalar alpha a
  mutVec x m n t
  unitT

gemv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
gemv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  (r, c) <- dimGE a
  scalar alpha t
  mat ''GE a r c t
  vec x c t
  scalar beta t
  mutVec y m r t
  unitT

ger :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
ger t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  a <- bind "a"
  order
  j <- dimVec x
  k <- dimVec y
  scalar alpha t
  vec x j t
  vec y k t
  mutMat ''GE a m j k t
  unitT

gbmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
gbmv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  (r, c) <- dimGB a
  scalar alpha t
  mat ''GB a r c t
  vec x c t
  scalar beta t
  mutVec y m r t
  unitT

hemv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
hemv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  n <- dimHE a
  scalar alpha t
  sqMat ''HE a n t
  vec x n t
  scalar beta t
  mutVec y m n t
  unitT

her :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
her s t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  a <- bind "a"
  order
  n <- dimVec x
  scalar alpha s
  vec x n t
  mutSqMat ''HE a m n t
  unitT

her2 :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
her2 t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  a <- bind "a"
  order
  n <- dimVec x
  scalar alpha t
  vec x n t
  vec y n t
  mutSqMat ''HE a m n t
  unitT

hbmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
hbmv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  n <- dimHB a
  scalar alpha t
  sqMat ''HB a n t
  vec x n t
  scalar beta t
  mutVec y m n t
  unitT

hpmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
hpmv t m = do
  alpha <- bind "alpha"
  a <- bind "a"
  x <- bind "x"
  beta <- bind "beta"
  y <- bind "y"
  order
  n <- dimHP a
  scalar alpha t
  sqMat ''HP a n t
  vec x n t
  scalar beta t
  mutVec y m n t
  unitT

hpr :: Build a => Q Type -> Q Type -> M a -> WriterT (Acc a) Q Type
hpr s t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  a <- bind "a"
  order
  n <- dimVec x
  scalar alpha s
  vec x n t
  mutSqMat ''HP a m n t
  unitT

hpr2 :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
hpr2 t m = do
  alpha <- bind "alpha"
  x <- bind "x"
  y <- bind "y"
  a <- bind "a"
  order
  n <- dimVec x
  scalar alpha t
  vec x n t
  vec y n t
  mutSqMat ''HP a m n t
  unitT

trmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
trmv t m = do
  a <- bind "a"
  x <- bind "x"
  order
  n <- dimTR a
  sqMat ''TR a n t
  mutVec x m n t
  unitT

tpmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
tpmv t m = do
  a <- bind "a"
  x <- bind "x"
  order
  n <- dimTP a
  sqMat ''TP a n t
  mutVec x m n t
  unitT

tbmv :: Build a => Q Type -> M a -> WriterT (Acc a) Q Type
tbmv t m = do
  a <- bind "a"
  x <- bind "x"
  order
  n <- dimTB a
  sqMat ''TB a n t
  mutVec x m n t
  unitT

cblas_dot :: Q Type -> String -> Q [Dec]
cblas_dot t = cblas (dot t)

cblas_asum :: Q Type -> Q Type -> String -> Q [Dec]
cblas_asum r t = cblas (asum r t)

cblas_swap :: Q Type -> String -> Q [Dec]
cblas_swap t = cblas (swap t)

cblas_copy :: Q Type -> String -> Q [Dec]
cblas_copy t = cblas (copy t)

cblas_axpy :: Q Type -> String -> Q [Dec]
cblas_axpy t = cblas (axpy t)

cblas_scal :: Q Type -> Q Type -> String -> Q [Dec]
cblas_scal s t = cblas (scal s t)

cblas_gemv :: Q Type -> String -> Q [Dec]
cblas_gemv t = cblas (gemv t)

cblas_ger :: Q Type -> String -> Q [Dec]
cblas_ger t = cblas (ger t)

cblas_gbmv :: Q Type -> String -> Q [Dec]
cblas_gbmv t = cblas (gbmv t)

cblas_hemv :: Q Type -> String -> Q [Dec]
cblas_hemv t = cblas (hemv t)

cblas_her :: Q Type -> Q Type -> String -> Q [Dec]
cblas_her s t = cblas (her s t)

cblas_her2 :: Q Type -> String -> Q [Dec]
cblas_her2 t = cblas (her2 t)

cblas_hbmv :: Q Type -> String -> Q [Dec]
cblas_hbmv t = cblas (hbmv t)

cblas_hpmv :: Q Type -> String -> Q [Dec]
cblas_hpmv t = cblas (hpmv t)

cblas_hpr :: Q Type -> Q Type -> String -> Q [Dec]
cblas_hpr s t = cblas (hpr s t)

cblas_hpr2 :: Q Type -> String -> Q [Dec]
cblas_hpr2 t = cblas (hpr2 t)

cblas_trmv :: Q Type -> String -> Q [Dec]
cblas_trmv t = cblas (trmv t)

cblas_tpmv :: Q Type -> String -> Q [Dec]
cblas_tpmv t = cblas (tpmv t)

cblas_tbmv :: Q Type -> String -> Q [Dec]
cblas_tbmv t = cblas (tbmv t)
