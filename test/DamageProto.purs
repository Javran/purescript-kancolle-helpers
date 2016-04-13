module DamageProto where

import Prelude
import Data.Array
import Data.Foldable
import Data.Monoid.Additive
import Data.Monoid.Multiplicative
import Control.Monad.Eff
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
import Test.QuickCheck
import Benchotron.Core
import Benchotron.UI.Console
import Data.List as L

type Ship = 
  { hp :: Int
  , sunk :: Boolean
  }
  
type Damage1 = Array Int
type Damage2 = Ship -> Ship
type Damage3 = L.List Int

mkDmg1 :: Int -> Damage1
mkDmg1 = singleton

mkDmg3 :: Int -> Damage3
mkDmg3 = L.singleton

mergeDmg1 :: Damage1 -> Int -> Damage1
mergeDmg1 = snoc

mergeDmg3 :: Damage3 -> Int -> Damage3
mergeDmg3 d v = L.(:) v d
  
applyDamage1 :: Ship -> Damage1 -> Ship
applyDamage1 s ds = foldl update s ds
  where
    update s v = if s.sunk
      then s 
      else
        let newHp = s.hp - v
        in s { hp = newHp, sunk = newHp <= 0  }
        
applyDamage3 :: Ship -> Damage3 -> Ship
applyDamage3 s ds = foldl update s ds'
  where
    ds' = L.reverse ds
    update s v = if s.sunk
      then s 
      else
        let newHp = s.hp - v
        in s { hp = newHp, sunk = newHp <= 0  }

mkDmg2 :: Int -> Damage2
mkDmg2 v s = if s.sunk
    then s 
    else
      let newHp = s.hp - v
      in s { hp = newHp, sunk = newHp <= 0  }

mergeDmg2 :: Damage2 -> Int -> Damage2
mergeDmg2 d v = d >>> mkDmg2 v

applyDamage2 :: Ship -> Damage2 -> Ship
applyDamage2 s f = f s

experiment1 :: Array Int -> Ship
experiment1 xs = applyDamage1 initShip (foldl mergeDmg1 (mkDmg1 0) xs)
  where
    initShip = { hp: 1000, sunk: false }

experiment2 :: Array Int -> Ship
experiment2 xs = applyDamage2 initShip (foldl mergeDmg2 (mkDmg2 0) xs)
  where
    initShip = { hp: 1000, sunk: false }

experiment3 :: Array Int -> Ship
experiment3 xs = applyDamage3 initShip (foldl mergeDmg3 (mkDmg3 0) xs)
  where
    initShip = { hp: 1000, sunk: false }

prop_DamageResultAgree1 xs = experiment1 xs `shipEq` experiment2 xs
  where
    shipEq s1 s2 = s1.hp == s2.hp && s1.sunk == s2.sunk
    
prop_DamageResultAgree2 xs = experiment1 xs `shipEq` experiment3 xs
  where
    shipEq s1 s2 = s1.hp == s2.hp && s1.sunk == s2.sunk

benchSum :: Benchmark
benchSum = mkBenchmark
  { slug: "sum"
  , title: "DA - Prototype"
  , sizes: [10,20,100,1000]
  , sizeInterpretation: "array size"
  , inputsPerSize: 100
  , gen: \n -> vectorOf n (chooseInt 0 140)
  , functions: [ benchFn "Array impl" experiment1
               , benchFn "List impl" experiment3
               , benchFn "CPS impl" experiment2
               ]
  }

mainBench = runSuite [benchSum]
