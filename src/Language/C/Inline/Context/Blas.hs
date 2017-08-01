{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.C.Inline.Context.Blas where

import Data.Complex
import Foreign.Ptr (Ptr)
import Language.C.Inline.Context
       ( AntiQuoter(..), Context(..), SomeAntiQuoter(..) )
import Language.C.Inline.HaskellIdentifier
import Language.Haskell.TH (Q, Exp)

import qualified Data.Map as Map
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
        cTy = C.Ptr [] (C.TypeSpecifier mempty (C.Char Nothing))
      pure (cId, cTy, hsId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.Ptr _ (C.TypeSpecifier _ (C.Char _)) -> do
          hsExp <-
            [| \cont ->
                 case $(getHsVariable "blasCtx.uplo" cId) of
                   Upper -> with 'U' cont
                   Lower -> with 'L' cont
             |]
          hsTy <- [t| Ptr Char |]
          return (hsTy, hsExp)
        _ -> fail "blasCtx.uplo: got type different from `char *'"
  }

aqDiag :: AntiQuoter HaskellIdentifier
aqDiag =
  AntiQuoter
  { aqParser = do
      hsId <- C.parseIdentifier
      let
        cId = mangleHaskellIdentifier hsId
        cTy = C.Ptr [] (C.TypeSpecifier mempty (C.Char Nothing))
      pure (cId, cTy, hsId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.Ptr _ (C.TypeSpecifier _ (C.Char _)) -> do
          hsExp <-
            [| \cont ->
                 case $(getHsVariable "blasCtx.diag" cId) of
                   Unit -> with 'U' cont
                   NonUnit -> with 'N' cont
             |]
          hsTy <- [t| Ptr Char |]
          return (hsTy, hsExp)
        _ -> fail "blasCtx.diag: got type different from `char *'"
  }

aqTrans :: AntiQuoter HaskellIdentifier
aqTrans =
  AntiQuoter
  { aqParser = do
      hsId <- C.parseIdentifier
      let
        cId = mangleHaskellIdentifier hsId
        cTy = C.Ptr [] (C.TypeSpecifier mempty (C.Char Nothing))
      pure (cId, cTy, hsId)
  , aqMarshaller = \_ _ cTy cId -> do
      case cTy of
        C.Ptr _ (C.TypeSpecifier _ (C.Char _)) -> do
          hsExp <-
            [| \cont ->
                 case $(getHsVariable "blasCtx.trans" cId) of
                   NoTrans -> with 'N' cont
                   Trans -> with 'T' cont
                   ConjTrans -> with 'C' cont
             |]
          hsTy <- [t| Ptr Char |]
          return (hsTy, hsExp)
        _ -> fail "blasCtx.trans: got type different from `char *'"
  }

getHsVariable :: String -> HaskellIdentifier -> Q Exp
getHsVariable ctx hsId = do
  mName <- TH.lookupValueName (unHaskellIdentifier hsId)
  case mName of
    Nothing ->
      fail (ctx ++ ": not in scope: `" ++ unHaskellIdentifier hsId ++ "'")
    Just name -> TH.varE name
