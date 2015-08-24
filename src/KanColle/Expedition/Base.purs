module KanColle.Expedition.Base where

-- basic expedition information
-- this module is intended to be imported by other expedition modules
-- so that we can solve the circular module dependency issue

import Prelude
import Data.Array

allExpeditionIds :: Array Int
allExpeditionIds = (1..32) <> (35..40)
