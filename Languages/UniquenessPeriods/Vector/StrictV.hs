-- |
-- Module      :  Languages.UniquenessPeriods.Vector.StrictV
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization of the DobutokO.Poetry.StrictV module functionality from
-- the @dobutokO-poetry-general@ package.
-- Can be used to get possible permutations of no more than 7 sublists
-- in the list separated with the elements of the \"whitespace symbols\"
-- list.

{-# LANGUAGE CPP, BangPatterns #-}

module Languages.UniquenessPeriods.Vector.StrictV (
  uniquenessVariants2GN
  , uniquenessVariants2GNP
  , sublistsG
  , preAppend
) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
/* code that applies only to GHC 8.4.* and higher versions */
import Data.Semigroup ((<>))
import Prelude hiding ((<>))
#endif
#endif
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import qualified Data.Vector as V
import qualified Data.List as L (permutations)
import Languages.UniquenessPeriods.Vector.Data

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

-- | Given a [a] consisting of no more than 7 sublists interspersed with the elements of the first argument,
-- it returns a 'V.Vector' of possible combinations without repeating of the sublists in different order and for each of them appends also
-- the information about generalized \"uniqueness periods\" (see @uniqueness-periods-vector@ package) to it and finds out
-- some different metrics -- named \"properties\".
--
-- Afterwards, depending on these norms some (usually phonetic for the words) properties of the sublists can be specified that
-- allow to use them for special cases of repetitions (usually, for the words -- poetically or to create a varied melody with them).
uniquenessVariants2GN ::
  (Eq a, Ord b) => [a]
  -> V.Vector ([b] -> b)
  -> FuncRep [a] (V.Vector c) [b] -- ^ Since version 0.5.0.0 it includes the previous variant with data constructor 'D2', but additionally allows to use just single argument with data constructor 'U1'
  -> [a]
  -> V.Vector ([b],V.Vector b, [a])
uniquenessVariants2GN whspss vN frep !xs = uniquenessVariants2GNP [] [] whspss vN frep xs
{-# INLINE uniquenessVariants2GN #-}

-- | Generalized variant of 'uniquenessVariants2GN' with prepending and appending @[a]@ (given as the first and the second argument).
uniquenessVariants2GNP ::
  (Eq a, Ord b) => [a]
  -> [a]
  -> [a]
  -> V.Vector ([b] -> b)
  -> FuncRep [a] (V.Vector c) [b] -- ^ Since version 0.5.0.0 it includes the previous variant with data constructor 'D2', but additionally allows to use just single argument with data constructor 'U1'
  -> [a]
  -> V.Vector ([b],V.Vector b, [a])
uniquenessVariants2GNP !ts !us whspss vN frep !xs
  | null . sublistsG whspss $ xs = V.empty
  | not . null $ whspss =
     let !hd = head whspss
         !ns = take 8 . sublistsG whspss $ xs
         !uss = [hd:us] in
           V.fromList . map ((\vs -> let !rs = getAC frep $ vs in (rs, (V.map (\f -> f rs) vN), vs)) . mconcat . preAppend ts uss) .
              L.permutations . map (hd:) $ ns
  | otherwise = error "Languages.UniquenessPeriods.Vector.StrictV.uniquenessVariants2GNP: undefined for the empty third argument. "

-- | Inspired by: https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#words
sublistsG :: Eq a => [a] -> [a] -> [[a]]
sublistsG whspss xs =
 case dropWhile (`elem` whspss) xs of
   [] -> []
   s' -> w : sublistsG whspss s''
     where (w, s'') = break (`elem` whspss) s'

-- | Prepends and appends the given two first arguments to the third one.
preAppend :: [a] -> [[a]] -> [[a]] -> [[a]]
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
preAppend ts !uss tss = ts:tss <> uss
#endif
#endif
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__<804
preAppend ts !uss tss = ts:tss ++ uss
#endif
#endif
{-# INLINE preAppend #-}
