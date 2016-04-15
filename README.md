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

* `KanColle.DamageAnalysis.FFI`

    * `analyzeBattleJS(dc6,data)`

        Analyze regular battle data

    * `analyzeNightBattleJS(dc6,data)`

        Analyze regular night battle data

    * `analyzeCTFBattleJS(dc12,data)`

        Analyze battle with Combined Fleet, specialized for Carrier Task Force

    * `analyzeSTFBattleJS(dc12,data)`

        Analyze battle with Combined Fleet, specialized for Surface Task Force

    * `analyzeTECFBattleJS(dc12,data)`

        Analyze battle with Combined Fleet, specialized for Transport Escort

    * `analyzeCombinedNightBattleJS(dc6,data)`

        Analyze night battle data (for combined fleet)

    `dc6` is an array of 6-elements (similarly `dc12` is an array of 12-elements for the same purpose).

    Each element takes value `0`,`1` or `2`:
    - `0`: no DameCon on this ship
    - `1`: this fleet will be using Repair Team when sinking
    - `2`: this fleet will be using Repair Goddess when sinking

* `KanColle.RepairTime`

    * `dockingInSecJS(stype, level, current HP, max HP)`

        Calculate docking time for ships

    * `facilityInSecJS(stype, level, current HP, max HP)`

        Calculate docking time if Akashi is used for repairing
