module GenSType where

-- to deal with not having a deriving mechanism in PureScript
-- we workaround it by generating some source code.

import Data.List
import Text.Printf

sourceContents :: [String]
sourceContents =
    [ "-- Generated from STypeGen.hs"
    , "module KanColle.Generated.SType where", ""] ++
    dataDef ++ [""] ++
    showDefs ++ [""] ++
    readDefs
  where
    dataDef = ["data SType = " ++ intercalate " | "  alts]
    showDefs = "showSType :: SType -> String"
             : map (\x -> printf "showSType %s = \"%s\"" x x) alts
    readDefs = "readSType :: String -> SType"
             : map (\x -> printf "readSType \"%s\" = %s" x x) alts
    alts = words "DDE DD  CL  CLT \
                 \CA  CAV CVL FBB \
                 \BB  BBV CV  XBB \
                 \SS  SSV AP  AV  \
                 \LHA CVB AR  AS  \
                 \CT "

main :: IO ()
main = mapM_ putStrLn sourceContents
