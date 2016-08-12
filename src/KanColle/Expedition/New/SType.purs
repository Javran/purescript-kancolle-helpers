module KanColle.Expedition.New.SType
  ( SType(..)
  , matchSType
  ) where

import Prelude
import KanColle.SType as K

-- ship type specialized for expeditions
data SType
  = DD
  | CL
  | CVLLike
  | SSLike
  | CA
  | BBV
  | AS
  | CT
  | AV
  
derive instance eqSType :: Eq SType
derive instance ordSType :: Ord SType

matchSType :: SType -> K.SType -> Boolean
matchSType s ks = case s of
    DD -> ks == K.DD
    CL -> ks == K.CL
    CVLLike ->
        ks == K.CV
     || ks == K.CVL
     || ks == K.AV
     || ks == K.CVB
    SSLike ->
        ks == K.SS
     || ks == K.SSV
    CA -> ks == K.CA
    BBV -> ks == K.BBV
    AS -> ks == K.AS
    CT -> ks == K.CT
    AV -> ks == K.AV
