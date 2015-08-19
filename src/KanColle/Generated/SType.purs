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
