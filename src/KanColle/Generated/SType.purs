-- Generated from STypeGen.hs
module KanColle.Generated.SType where

import Prelude

data SType = DDE | DD | CL | CLT | CA | CAV | CVL | FBB | BB | BBV | CV | XBB | SS | SSV | AP | AV | LHA | CVB | AR | AS | CT | AO | Unknown String

showSType :: SType -> String
showSType DDE = "DDE"
showSType DD = "DD"
showSType CL = "CL"
showSType CLT = "CLT"
showSType CA = "CA"
showSType CAV = "CAV"
showSType CVL = "CVL"
showSType FBB = "FBB"
showSType BB = "BB"
showSType BBV = "BBV"
showSType CV = "CV"
showSType XBB = "XBB"
showSType SS = "SS"
showSType SSV = "SSV"
showSType AP = "AP"
showSType AV = "AV"
showSType LHA = "LHA"
showSType CVB = "CVB"
showSType AR = "AR"
showSType AS = "AS"
showSType CT = "CT"
showSType AO = "AO"
showSType (Unknown s) = "<Unknown:" <> s <> ">"

readSType :: String -> SType
readSType "DDE" = DDE
readSType "DD" = DD
readSType "CL" = CL
readSType "CLT" = CLT
readSType "CA" = CA
readSType "CAV" = CAV
readSType "CVL" = CVL
readSType "FBB" = FBB
readSType "BB" = BB
readSType "BBV" = BBV
readSType "CV" = CV
readSType "XBB" = XBB
readSType "SS" = SS
readSType "SSV" = SSV
readSType "AP" = AP
readSType "AV" = AV
readSType "LHA" = LHA
readSType "CVB" = CVB
readSType "AR" = AR
readSType "AS" = AS
readSType "CT" = CT
readSType "AO" = AO
readSType s = Unknown s

fromInt :: Int -> SType
fromInt 1 = DDE
fromInt 2 = DD
fromInt 3 = CL
fromInt 4 = CLT
fromInt 5 = CA
fromInt 6 = CAV
fromInt 7 = CVL
fromInt 8 = FBB
fromInt 9 = BB
fromInt 10 = BBV
fromInt 11 = CV
fromInt 12 = XBB
fromInt 13 = SS
fromInt 14 = SSV
fromInt 15 = AP
fromInt 16 = AV
fromInt 17 = LHA
fromInt 18 = CVB
fromInt 19 = AR
fromInt 20 = AS
fromInt 21 = CT
fromInt 22 = AO
fromInt v = Unknown ("num " ++ show v)
