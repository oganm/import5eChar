#' @export
spellDC = function(char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    8 + char$proficiencyBonus + char$abilityMods[char$castingStatCode+1] + char$miscSpellDCBonus
}

#' @export
spellAttack = function(char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    char$proficiencyBonus  + char$abilityMods[char$castingStatCode+1] + char$miscSpellAttackBonus
}


#' @export
AC =  function(char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    statAdd = 0
    if(char$unarmoredDefense != ''){
        statAdd = char$abilityMods[char$unarmoredDefense]
    }
    char$armorBonus +
        char$shieldBonus +
        char$miscArmorBonus +
        min(stat2mod(char$abilityScores['Dex']),char$maxDex) +
        statAdd
}


#' @export
initBonus = function(char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    statAdd = 0
    if(char$statToInit != ''){
        statAdd = char$abilityMods[char$statToInit]
    }

    unname(char$abilityMods['Dex'] + char$initMiscMod +
               char$profToInit['profToInit']*char$proficiencyBonus +
               char$profToInit['doubleProfToInit']*char$proficiencyBonus +
               char$profToInit['halfProfToInit']*floor(char$proficiencyBonus/2) +
               char$profToInit['halfProfToInitRoundUp']*(char$proficiencyBonus %% 2) +
               statAdd)
}



#' @export
skillBonus = function(char=  getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    char$skillProf*char$proficiencyBonus +
        char$skillDoubleProf*char$proficiencyBonus +
        (char$skillHalfProf*!char$skillProf*char$proficiencyBonus/2) +
        char$abilityMods[char$skillAttributes] +
        char$skillMiscMod
}

#' @export
saveBonus = function(char=  getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    statAdd = 0
    if(char$statToSave != ''){
        statAdd = char$abilityMods[char$statToSave]
    }

    char$abilityMods + char$proficiencyBonus*char$abilityProf + char$miscSaveBonus + statAdd
}
