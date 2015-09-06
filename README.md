# purescript-kancolle-helpers

[![Build Status](https://travis-ci.org/Javran/purescript-kancolle-helpers.svg?branch=master)](https://travis-ci.org/Javran/purescript-kancolle-helpers)

This library contains tools for analyzing data related to KanColle.

Currently implemented features are:

* Expedition Requirement Checker

    Check a fleet against expedition requirements,
    and explain requirements not yet met.

* Damage Analysis

    Calculate damage took for battles / night battles

## Modules

* `KanColle.Expedition`

    Features related to expeditions

* `KanColle.DamageAnalysis`

    Damage calculation

* `KanColle.Generated`

    Generated codes

* `KanColle.KCAPI`

    Type definitions for KanColle API

## JavaScript interfaces

* `KanColle.DamageAnalysis`

    * `analyzeRawBattleJS(data)`

        Analyze regular battle data

    * `analyzeRawNightBattleJS(data)`

        Analyze regular night battle data (for combined fleet)

    * `analyzeRawCarrierTaskForceBattleJS(data)`

        Analyze battle with Combined Fleet, specialized for Carrier Task Force

    * `analyzeRawSurfaceTaskForceBattleJS(data)`

        Analyze battle with Combined Fleet, specialized for Surface Task Force

    * `analyzeRawNightBattleCombinedJS(data)`

        Analyze night battle data (for combined fleet)
