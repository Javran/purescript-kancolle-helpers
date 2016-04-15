module UtilTests where

import Test.QuickCheck
import Test.QuickCheck.Gen

import Prelude
import Data.Array
import KanColle.Util
import DamageProto as DProto

prop_HeapSortCorrectness :: Array Int -> Result
prop_HeapSortCorrectness xs = sort xs === heapSortSafe compare xs

prop_SortByThenTake :: (Array Int -> Int) -> Array Int -> Result
prop_SortByThenTake sample xs = sortByThenTake compare s xs === sortByThenTakeQuick compare s xs
  where
    s = sample xs

qcTestUtils = do
    quickCheck prop_HeapSortCorrectness
    quickCheck (prop_SortByThenTake (\xs -> length xs / 2))
    quickCheck (prop_SortByThenTake (\xs -> length xs / 3))
    quickCheck (prop_SortByThenTake (\xs -> length xs / 5))
    quickCheck DProto.prop_DamageResultAgree1
    quickCheck DProto.prop_DamageResultAgree2
