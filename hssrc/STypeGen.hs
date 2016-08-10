module GenSType where

-- to deal with not having a deriving mechanism in PureScript
-- we workaround it by generating some source code.

import Data.List
import Text.Printf

sourceContents :: [String]
sourceContents =
    [ "-- Generated from STypeGen.hs"
    , "module KanColle.Generated.SType where", ""
    , "import Prelude", ""] ++
    dataDef ++ [""] ++
    derivDecls ++ [""] ++
    showDefs ++ [""] ++
    readDefs ++ [""] ++
    fromIntDefs ++ [""] ++
    toIntDefs
  where
    dataDef = ["data SType = " ++ intercalate " | "  alts ++ " | Unknown String"]
    derivDecls = ["derive instance eqSType :: Eq SType"]
    showDefs = "showSType :: SType -> String"
             : map (\x -> printf "showSType %s = \"%s\"" x x) alts
            ++ ["showSType (Unknown s) = \"<Unknown:\" <> s <> \">\""]
    readDefs = "readSType :: String -> SType"
             : map (\x -> printf "readSType \"%s\" = %s" x x) alts
            ++ ["readSType s = Unknown s"]
    fromIntDefs = "fromInt :: Int -> SType"
                : zipWith (\n st -> "fromInt " ++ show n ++ " = " ++ st)
                          [1 :: Int ..]
                          alts
               ++ ["fromInt v = Unknown (\"num \" <> show v)"]
    toIntDefs = "toInt :: SType -> Int"
              : zipWith (\st n -> "toInt " ++ st ++ " = " ++ show n)
                          alts
                          [1 :: Int ..]
               ++ ["toInt (Unknown _) = -1"]
    alts = words "DDE DD  CL  CLT \
                 \CA  CAV CVL FBB \
                 \BB  BBV CV  XBB \
                 \SS  SSV AP  AV  \
                 \LHA CVB AR  AS  \
                 \CT  AO"

main :: IO ()
main = mapM_ putStrLn sourceContents
