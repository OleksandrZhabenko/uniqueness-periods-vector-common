-- |
-- Module      :  Languages.UniquenessPeriods.Vector.Data
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Is a generalization of the DobutokO.Poetry.Data module
-- functionality from the @dobutokO-poetry-general@ package.
--

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Languages.UniquenessPeriods.Vector.Data where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Languages.UniquenessPeriods.Vector.Auxiliary (lastFrom3)

type UniquenessG1 a b = ([b],V.Vector b,[a])

-- | The list in the 'PA' variant represent the prepending @[a]@ and the postpending one respectively. 'K' constuctor actually means no prepending and
-- postpending (usually of the text). Are used basically to control the behaviour of the functions.
data PreApp a = K | PA [a] [a] deriving Eq

class UGG1 a b where
  get1m :: a -> [b]
  get2m :: a -> [b]
  getm :: Bool -> a -> [b]
  getm True = get1m
  getm _ = get2m
  preapp :: a -> [[b]] -> [[b]]
  setm :: [b] -> [b] -> a

instance Eq a => UGG1 (PreApp a) a where
  get1m K = []
  get1m (PA xs _) = xs
  get2m K = []
  get2m (PA _ ys) = ys
  preapp K xss = xss
  preapp (PA xs ys) yss = xs:yss ++ [ys]
  setm [] [] = K
  setm xs ys = PA xs ys

type Preapp a = PreApp a

isPA :: PreApp a -> Bool
isPA K = False
isPA _ = True

isK :: PreApp a -> Bool
isK K = True
isK _ = False

data UniquenessG2 a b = UL2 ([a],b) deriving Eq

instance (Show a, Show b) => Show (UniquenessG2 (UniquenessG1 a b) (V.Vector (UniquenessG1 a b))) where
  show (UL2 (ws,_)) = show ws

type UniqG2 a b = UniquenessG2 (UniquenessG1 a b) (V.Vector (UniquenessG1 a b))

get22 :: UniqG2 a b -> ([UniquenessG1 a b], V.Vector (UniquenessG1 a b))
get22 (UL2 (ws, x)) = (ws, x)

-- | Is used to avoid significant code duplication.
data FuncRep a b c = U1 (a -> c) | D2 (a -> b) (b -> c)

getAC :: FuncRep a b c -> (a -> c)
getAC (U1 f) = f
getAC (D2 g1 g2) = g2 . g1

isU1 :: FuncRep a b c -> Bool
isU1 (U1 _) = True
isU1 _ = False

isD2 :: FuncRep a b c -> Bool
isD2 (D2 _ _) = True
isD2 _ = False

