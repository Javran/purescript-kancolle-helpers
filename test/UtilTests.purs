module UtilTests where

import Test.QuickCheck

import Prelude
import Data.Array
import KanColle.Util

prop_HeapSortCorrectness :: Array Int -> Result
prop_HeapSortCorrectness xs = sort xs === heapSortSafe xs

qcTestUtils = do
    quickCheck prop_HeapSortCorrectness
