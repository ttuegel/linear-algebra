{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.C.Inline.Context.Blas where

import Data.Complex
import Foreign.C.Types (CInt)
import Language.C.Inline.Context
       ( AntiQuoter(..), Context(..), SomeAntiQuoter(..) )
import Language.C.Inline.HaskellIdentifier
import Language.Haskell.TH (Q, Exp)

import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

import Internal.Int
import Internal.Matrix

blasCtx :: Context
blasCtx =
  mempty
  { ctxTypesTable =
      Map.fromList
      [ (C.TypeName "blasint", [t| I |])
      , (C.Double, [t| Double |])
      , (C.TypeName "openblas_complex_double", [t| Complex Double |])
      , (C.TypeName "CBLAS_UPLO", [t| CInt |])
      , (C.TypeName "CBLAS_DIAG", [t| CInt |])
      , (C.TypeName "CBLAS_TRANSPOSE", [t| CInt |])
      ]
  , ctxAntiQuoters =
      Map.fromList
      [ ("uplo", SomeAntiQuoter aqUpLo)
      , ("diag", SomeAntiQuoter aqDiag)
      , ("trans", SomeAntiQuoter aqTrans)
      ]
  }

aqUpLo :: AntiQuoter HaskellIdentifier
aqUpLo =
  AntiQuoter
  { aqParser = do
      hsId <- C.parseIdentifier
      let
        cId = mangleHaskellIdentifier hsId
        cTy = C.TypeSpecifier mempty (C.TypeName "CBLAS_UPLO")
      pure (cId, cTy, hsId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.TypeSpecifier _ (C.TypeName "CBLAS_UPLO") -> do
          hsExp <-
            [| \cont ->
                 case $(getHsVariable "blasCtx.uplo" cId) of
                   Upper -> cont [C.pure| int { CblasUpper } |]
                   Lower -> cont [C.pure| int { CblasLower } |]
             |]
          hsTy <- [t| CInt |]
          return (hsTy, hsExp)
        _ -> fail "blasCtx.uplo: got type different from `CBLAS_UPLO'"
  }

aqDiag :: AntiQuoter HaskellIdentifier
aqDiag =
  AntiQuoter
  { aqParser = do
      hsId <- C.parseIdentifier
      let
        cId = mangleHaskellIdentifier hsId
        cTy = C.TypeSpecifier mempty (C.TypeName "CBLAS_DIAG")
      pure (cId, cTy, hsId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.TypeSpecifier _ (C.TypeName "CBLAS_DIAG") -> do
          hsExp <-
            [| \cont ->
                 case $(getHsVariable "blasCtx.diag" cId) of
                   Unit -> cont [C.pure| int { CblasUnit } |]
                   NonUnit -> cont [C.pure| int { CblasNonUnit } |]
             |]
          hsTy <- [t| CInt |]
          return (hsTy, hsExp)
        _ -> fail "blasCtx.diag: got type different from `CBLAS_DIAG'"
  }

aqTrans :: AntiQuoter HaskellIdentifier
aqTrans =
  AntiQuoter
  { aqParser = do
      hsId <- C.parseIdentifier
      let
        cId = mangleHaskellIdentifier hsId
        cTy = C.TypeSpecifier mempty (C.TypeName "CBLAS_TRANSPOSE")
      pure (cId, cTy, hsId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.TypeSpecifier _ (C.TypeName "CBLAS_TRANSPOSE") -> do
          hsExp <-
            [| \cont ->
                 case $(getHsVariable "blasCtx.trans" cId) of
                   NoTrans -> cont [C.pure| int { CblasNoTrans } |]
                   Trans -> cont [C.pure| int { CblasTrans } |]
                   ConjTrans -> cont [C.pure| int { CblasConjTrans } |]
                   ConjNoTrans -> cont [C.pure| int { CblasConjNoTrans } |]
             |]
          hsTy <- [t| CInt |]
          return (hsTy, hsExp)
        _ -> fail "blasCtx.diag: got type different from `CBLAS_DIAG'"
  }

getHsVariable :: String -> HaskellIdentifier -> Q Exp
getHsVariable ctx hsId = do
  mName <- TH.lookupValueName (unHaskellIdentifier hsId)
  case mName of
    Nothing ->
      fail (ctx ++ ": not in scope: `" ++ unHaskellIdentifier hsId ++ "'")
    Just name -> TH.varE name
