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
        min(max(stat2mod(char$abilityScores['Dex']),0),char$maxDex) +
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
        (char$skillHalfProf*(!char$skillProf)*char$proficiencyBonus/2) +
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


#' @export
casterLevel =  function(char=  getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    classInfo = char$classInfo

    fullLevel = classInfo[classInfo[,'Caster Type'] == 0,'Level'] %>% as.numeric() %>% sum

    halfLevel = classInfo[classInfo[,'Caster Type'] == 1 & classInfo[,'ArtificerFlag'] == 0,'Level'] %>% as.numeric() %>% sum

    halfLevelArtificer = classInfo[classInfo[,'Caster Type'] == 1 & classInfo[,'ArtificerFlag'] == 1,'Level'] %>% as.numeric() %>% sum

    thirdLevel = classInfo[classInfo[,'Caster Type'] == 2,'Level'] %>% as.numeric() %>% sum


    if(nrow(classInfo)==1){
        totalLevel = fullLevel + ceiling(halfLevel*1/2)*(halfLevel>1) + ceiling(thirdLevel*1/3)*(thirdLevel>2) + ceiling(halfLevelArtificer*1/2)
    } else{
        totalLevel = fullLevel + floor(1/2*halfLevel) + floor(1/3*thirdLevel) + ceiling(1/2*halfLevelArtificer)
    }

    warlockLevels = classInfo[classInfo[,'Caster Type'] == 3,'Level'] %>% as.numeric() %>% sum

    return(c(casterLevel = totalLevel, pactMagic = warlockLevels))

}

#' @export
spellSlots = function(char=  getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    casterScale =
"Level	1st	2nd	3rd	4th	5th	6th	7th	8th	9th
1	2	0	0	0	0	0	0	0	0
2	3	0	0	0	0	0	0	0	0
3	4	2	0	0	0	0	0	0	0
4	4	3	0	0	0	0	0	0	0
5	4	3	2	0	0	0	0	0	0
6	4	3	3	0	0	0	0	0	0
7	4	3	3	1	0	0	0	0	0
8	4	3	3	2	0	0	0	0	0
9	4	3	3	3	1	0	0	0	0
10	4	3	3	3	2	0	0	0	0
11	4	3	3	3	2	1	0	0	0
12	4	3	3	3	2	1	0	0	0
13	4	3	3	3	2	1	1	0	0
14	4	3	3	3	2	1	1	0	0
15	4	3	3	3	2	1	1	1	0
16	4	3	3	3	2	1	1	1	0
17	4	3	3	3	2	1	1	1	1
18	4	3	3	3	3	1	1	1	1
19	4	3	3	3	3	2	1	1	1
20	4	3	3	3	3	2	2	1	1"

    warlockScale =
"Level	1st	2nd	3rd	4th	5th	6th	7th	8th	9th
1	1	0	0	0	0	0	0	0	0
2	2	0	0	0	0	0	0	0	0
3	0	2	0	0	0	0	0	0	0
4	0	2	0	0	0	0	0	0	0
5	0	0	2	0	0	0	0	0	0
6	0	0	2	0	0	0	0	0	0
7	0	0	0	2	0	0	0	0	0
8	0	0	0	2	0	0	0	0	0
9	0	0	0	0	2	0	0	0	0
10	0	0	0	0	2	0	0	0	0
11	0	0	0	0	3	0	0	0	0
12	0	0	0	0	3	0	0	0	0
13	0	0	0	0	3	0	0	0	0
14	0	0	0	0	3	0	0	0	0
15	0	0	0	0	3	0	0	0	0
16	0	0	0	0	3	0	0	0	0
17	0	0	0	0	4	0	0	0	0
18	0	0	0	0	4	0	0	0	0
19	0	0	0	0	4	0	0	0	0
20	0	0	0	0	4	0	0	0	0"

    casterScale = read.table(textConnection(casterScale),sep = '\t',header = TRUE,stringsAsFactors = FALSE) %>%
        dplyr::select(-Level)
    warlockScale = read.table(textConnection(warlockScale),sep = '\t',header = TRUE,stringsAsFactors = FALSE) %>%
        dplyr::select(-Level)

    cl = casterLevel(char)

    levels = c(0,0,0,0,0,0,0,0,0)
    if(cl['casterLevel']>0){
        levels = levels + unname(unlist(casterScale[cl['casterLevel'],]))
    }
    if(cl['pactMagic']>0){
        levels = levels + unname(unlist(warlockScale[cl['pactMagic'],]))
    }

    return(levels)
}
