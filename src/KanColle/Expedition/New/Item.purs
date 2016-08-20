module KanColle.Expedition.New.Item
  ( Item(..)
  , itemFromInt
  ) where

data Item
  = Bucket
  | Flamethrower
  | DevMat
  | FCoinSmall
  | FCoinMedium
  | FCoinLarge
  | Other Int

itemFromInt :: Int -> Item
itemFromInt 1 = Bucket
itemFromInt 2 = Flamethrower
itemFromInt 3 = DevMat
itemFromInt 10 = FCoinSmall
itemFromInt 11 = FCoinMedium
itemFromInt 12 = FCoinLarge
itemFromInt i = Other i
