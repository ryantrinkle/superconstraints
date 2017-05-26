{-# LANGUAGE TypeFamilies, ConstraintKinds, Rank2Types, TypeOperators, ScopedTypeVariables, GADTs, TemplateHaskell, LambdaCase #-}
module Data.Constraint.Super where

import Data.Proxy (Proxy)
import Data.Constraint (Constraint, Dict (Dict), (:-) (Sub))
import Control.Monad (filterM)
import Control.Applicative ((<$>))
import Data.Data (Data, gmapM)
import Type.Eq ((:~:) (Eq), dynamicEq)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (StateT, evalStateT, get, put, lift)
import Language.Haskell.Meta.Parse (parseType)

import Language.Haskell.TH.Syntax hiding (lift)

class HasSuper c where
  type Super c :: Constraint
  instantiate :: HasSuper c :- c
  super :: Proxy c -> Dict (Super c)

normalizeInstanceHead :: Type -> Q Type
normalizeInstanceHead t = do
  let f :: forall d. Data d => d -> StateT (Integer, Map String Name) Q d
      f x = case dynamicEq :: Maybe (d :~: Name) of
        Just Eq ->
          let b = nameBase x
          in lift (qLookupName True b) >>= \case
            Just n' -> return n'
            Nothing -> do
              (nextId, m) <- get
              case Map.lookup b m of
                Just n' -> return n'
                Nothing -> do
                  let n' = mkName $ "a" ++ show nextId
                  put (succ nextId, Map.insert b n' m)
                  return n'
        _ -> case dynamicEq :: Maybe (d :~: Type) of
          Just Eq
            | ConT n' <- x
            , nameBase n' == "()"
            -> return $ TupleT 0
          _ -> gmapM f x
  evalStateT (gmapM f t) (1, Map.empty)

getClassName :: Type -> Maybe Name
getClassName = \case
  ConT n -> Just n
  AppT t _ -> getClassName t
  _ -> Nothing

-- | Note: this is not compatible with PolyKinds
makeSuper :: String -> Q [Dec]
makeSuper tStr = case parseType tStr of
  Left s -> fail $ "Error: Could not parse instance head: " ++ s
  Right t -> do
    t' <- normalizeInstanceHead t
    case getClassName t' of
      Nothing -> fail $ "Error: Instance head must be of the form \"ClassName (t1) (t2) (t3)...\""
      Just className -> do
        qReify className >>= \case
          ClassI _ instances -> do
            filterM (\(InstanceD _ _ t2 _) -> (==t') <$> normalizeInstanceHead t2) instances >>= \x -> case x of
              [InstanceD _ cxt t2 _] -> return
                [InstanceD Nothing cxt (AppT (ConT ''HasSuper) t2)
                  [ TySynInstD ''Super $ TySynEqn [t2] $ foldl AppT (TupleT $ length cxt) cxt
                  , FunD 'super [Clause [WildP] (NormalB (ConE 'Dict)) []]
                  , FunD 'instantiate [Clause [] (NormalB (AppE (ConE 'Sub) (ConE 'Dict))) []]
                  ]
                ]
              [] -> fail "Error: No matching instances found"
              _ -> fail "Error: Found multiple instances"
          _ -> fail $ "Error: " ++ show className ++ " is not a typeclass"
