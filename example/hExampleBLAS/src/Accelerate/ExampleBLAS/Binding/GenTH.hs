{-# LANGUAGE TemplateHaskell #-}

module Accelerate.ExampleBLAS.Binding.GenTH where

import           Control.Monad
import           Data.Char
import           Data.List.Split
import           Foreign.FAI
import           Foreign.Ptr
import           Foreign.Storable
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

mkNewFuncName :: Name -> Name
mkNewFuncName nm =
  let str = reverse . takeWhile (/= '.') . reverse $ show nm
      d:c:names =  splitOn ['_'] str
      name = foldl (\e n -> e ++ headUpper n) "" names
  in mkName $ map toLower c ++ headUpper d ++ name

headUpper :: String -> String
headUpper (h:str) = toUpper h : str
headUpper []      = []

l1p :: Type
    -> Q (Int, Type)
l1p = return . flip step 0
  where step (AppT (AppT ArrowT (AppT (ConT tp1) (ConT tp2))) t) i =
          if tp1 == ''Ptr && tp2 == ''Float then step t (i + 1) else (i, t)
        step t i = (i, t)

l2p :: Type
    -> Q (Int, Type)
l2p = return . flip step 0
  where step (AppT (AppT ArrowT (ConT tp1)) t) i =
          if tp1 == ''Int then step t (i + 1) else (i, t)
        step t i = (i, t)

mkContextP :: Name -> Type -> Type
mkContextP pn = AppT (AppT ArrowT (AppT (ConT ''Context) (VarT pn)))

mkFunSig :: Int -> Int -> Name -> Name -> Type
mkFunSig p1 p2 shN pN = mk1p shN pN (p1 - 1) . mkAccT pN . mkBufferTSP (ConT ''Int) pN $ ConT ''Float

mkSel :: Bool -> [Bool] -> [Name] -> Exp
mkSel sg is bs =
  let bK   = if sg then 'True else 'False
      bufs = if null is then bs else sel is bs
  in  step bK bufs
  where step k = AppE (AppE (VarE 'selShapeN ) (ConE k)). ListE . map mkBufLen
        sel          _     [] = []
        sel  (True:is) (b:bs) = b : sel is bs
        sel (False:is) (_:bs) = sel is bs

mkBufLen :: Name -> Exp
mkBufLen = AppE (VarE 'bufSize) . VarE

selShapeN :: Monad m => Bool -> [Int] -> m (Int, Int)
selShapeN is (t:ts) =
  let bufSize = step t ts
      newSize = if is then bufSize else 1
  in return (newSize, bufSize)
  where step t []      = t
        step t (t':ts) = if t == t' then step t ts else error "Not same shape"

mkIOT :: Name -> Type -> Type
mkIOT p t = AppT (ConT ''IO) $ AppT (AppT (ConT ''(,)) t) (AppT (ConT ''Context) (VarT p))

mkAccT :: Name -> Type -> Type
mkAccT p = AppT (AppT (ConT ''Accelerate) (VarT p))

mkBufferTSP :: Type -> Name -> Type -> Type
mkBufferTSP sh p = AppT (AppT (AppT (ConT ''Buffer) sh) (VarT p))

mk1p :: Name -> Name -> Int -> Type ->Type
mk1p _ _ 0 t = t
mk1p s p i t = mk1p s p (i-1) (AppT (AppT ArrowT (mkBufferTSP (VarT s) p (ConT ''Float)) ) t)

mk2p :: Int -> Type -> Type
mk2p 0 t = t
mk2p i t = mk2p (i-1) (AppT (AppT ArrowT (ConT ''Int)) t)

mkWith :: Name -> Name -> Name-> Exp -> Exp
mkWith fn vn pn e = AppE (AppE (VarE fn) (VarE vn)) (LamE [VarP pn] e)

mkWithSeq :: Name -> Exp -> [(Name, Name)] -> Exp
mkWithSeq fn = foldl (\e (vn, pn) -> mkWith fn vn pn e)

appFunc :: Name -> Name -> [Name] -> Exp
appFunc shn fn = flip AppE (AppE (VarE 'shLen) (VarE shn)) . foldl (\e p -> AppE e (AppE (VarE 'castPtr) (VarE p))) (VarE fn)

mkReturn :: Name -> Name -> Exp
mkReturn cpn bfn = AppE (VarE 'return) (TupE [VarE bfn, VarE cpn])

mkNewBuffer :: Name -> Name -> Exp
mkNewBuffer pn = flip AppE (VarE pn) . AppE (VarE 'newBufferIO) . VarE

mkBindingFAI :: Name -> Bool -> [Bool] -> Q [Dec]
mkBindingFAI bindingName' sg is = do
  VarI bindingName typ _
                  <- reify bindingName'
  (floatN, typ')  <- l1p typ
  (intN  , _)     <- l2p typ'
  shName   <- newName "sh"
  platName <- newName "p"
  pconName <- newName "c"
  pncnName <- newName "c"
  aName    <- newName "a"
  bufSh    <- newName "sh"
  newSh    <- newName "sh"
  allBufName @ (outBufName : bufNames)
    <-  replicateM floatN $ newName "buf"
  allPtrName @ (outPtrName : ptrNames)
    <-  replicateM floatN $ newName "ptr"
  let funcName    = mkNewFuncName bindingName
      bindingType = mkFunSig floatN intN shName platName
      bindingKind = ForallT [ PlainTV shName, PlainTV platName, PlainTV aName]
                            [ AppT (ConT ''Shape) (VarT shName)
                            , AppT (ConT ''FAI) (VarT platName)
                            , AppT (AppT EqualityT (VarT aName)) (AppT (AppT (ConT ''Pf) (VarT platName)) (ConT ''Float))
                            , AppT (ConT ''Storable) (VarT aName) ]
                            bindingType
      bindingSigD = SigD funcName bindingKind
      bindingBody = NormalB $ AppE (ConE 'Accelerate) $ LamE [VarP pconName] $ DoE
                    [ BindS (TupP [VarP newSh, VarP bufSh]) (mkSel sg is bufNames)
                    , BindS (TupP [VarP outBufName, VarP pncnName]) (mkNewBuffer pconName newSh)
                    , NoBindS (mkWithSeq (mkName "withBuffer") (appFunc bufSh bindingName allPtrName) (zip allBufName allPtrName))
                    , NoBindS (mkReturn pncnName outBufName)
                    ]
      bindingPat  = map VarP bufNames
      bindingFun  = FunD funcName [Clause bindingPat bindingBody []]
  return [bindingSigD, bindingFun]
