
#' Attack
#'
#' @param adv integer. 0: Normal, -1 Disadvantage, +1 Advantage
#' @param sharpShoot logical. are we sharpshooting
#' @param attackStat character. which stat to use for attack. 3 letter
#' @param damageDice character. attack dice
#' @param proficient logical. is the attacker proficient in the shot
#' @param modToHit integer. hit modifier
#' @param modToDamage integer. damage modifier
#' @param useAmmo logical. should the shot expand ammo
#' @param ammo character. what is the name of the ammo. Before using, you need to set your ammo count manually by character$ammoName = 30 (character$bolt = 30)
#' @param vocal logical. Should dice rolls and crit notifications be printed.
#' @param char A list outputted from importCharacter or processCharacter, or a character naming such a list
#'
#' @return Named vector. Attack roll and damage. If a 1 is rolled damage is always 0
#' @export
#'
attack = function(adv = 0,
                  sharpShoot = F,
                  attackStat = c('Str','Dex','Con','Wis','Int','Chr'),
                  damageDice = '1d6',
                  proficient = TRUE,
                  modToHit = 0,
                  modToDamage = 0,
                  useAmmo = FALSE,
                  ammo = 'arrow',
                  vocal = TRUE,
                  char = getOption('defaultCharacter')){
    # if char isn't provided see what the defaultCharacter option is
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    attackStat = match.arg(attackStat)

    if(useAmmo){
        char[[ammo]] <<- char[[ammo]] - 1
        if(vocal){
            print(paste('Ammo left: ',char[[ammo]]))
        }
        if(char[[ammo]] <= 0){
            warning('No Ammo')
        }
    }

    if(adv == 0){
        attackRoll = diceSyntax::r(r1d20,vocal=vocal)
    } else if(adv == 1){
        attackRoll = diceSyntax::r(r2d20d1,vocal = vocal)
    } else if (adv == -1){
        attackRoll = diceSyntax::r(r2d20dh1, vocal = vocal)
    }
    finalRoll = unname(attackRoll + char$abilityMods[attackStat]+ char$proficiencyBonus+ modToHit)
    if(sharpShoot){
        finalRoll = finalRoll - 5
    }

    if(attackRoll == 1){
        if(vocal){
            print('CRIT MISS')
        }
        damageRoll = 0
    } else if(attackRoll == 20){
        if(vocal){
            print('CRIT HIT')
        }
        diceCount = stringr::str_extract(damageDice,pattern = '[0-9]*?(?=d)')
        diceCount = as.integer(diceCount)*2
        damageDice %<>% stringr::str_replace(pattern = '[0-9]*?(?=d)',
                                    as.character(diceCount))
        damageRoll = unname(diceSyntax::r(damageDice,vocal = vocal) + char$abilityMods[attackStat] +modToDamage)
    } else{
        damageRoll = unname(diceSyntax::r(damageDice, vocal = vocal) + char$abilityMods[attackStat] + modToDamage)
    }
    if(sharpShoot & attackRoll != 1){
        damageRoll = damageRoll+10
    }
    return(c('Attack'= finalRoll, 'Damage' = damageRoll,
             crit = (attackRoll == 20)-(attackRoll == 1))
    )
}


#' Title
#'
#' @param skill
#'
#' @export
#'
skillCheck = function(skill,
                      char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    skill = as.character(substitute(skill))
    skillNames = names(char$skillProf)
    skill = skillNames[grepl(pattern = tolower(skill), x = tolower(skillNames))]

    bonus = char$skillProf[skill]*char$proficiencyBonus +
        char$skillDoubleProf[skill]*char$proficiencyBonus +
        (char$skillHalfProf[skill]*!char$skillProf[skill]*char$proficiencyBonus/2) +
        char$abilityMods[char$skillAttributes[skill]]

    if(char$skillHalfProfRoundUp[skill]){
        bonus %<>% ceiling()
    } else{
        bonus %>% floor()
    }

    diceSyntax::r(r1d20) + bonus
}

#' Title
#'
#' @export
#'
quickCheck = function(char =  getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    rounding = function(x,ceiling){
        for(i in 1:length(x)){
            if(ceiling[i]){
                x[i] %<>% ceiling
            } else{
                x[i] %<>% floor
            }
        }
        return(x)
    }

    roll = diceSyntax::r(r1d20)
    bonus = char$skillProf*char$proficiencyBonus +
        char$skillDoubleProf*char$proficiencyBonus +
        (char$skillHalfProf*!char$skillProf*char$proficiencyBonus/2)

    bonus %<>% rounding(char$skillHalfProfRoundUp)

    roll + bonus
}

#' @export
abilityCheck = function(ability,char=  getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    roll = diceSyntax::r(r1d20)
    bonus = char$abilityMods[ability]

    roll + bonus

}

#' Title
#'
#' @param stat
#'
#' @export
#'
save = function(stat, char =  getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }


    unname(diceSyntax::r(r1d20) + char$abilityMods[stat] + char$proficiencyBonus*char$abilityProf[stat])
}


#' Title
#'
#' @export
#'
init = function(char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    diceSyntax::r(r1d20) + unname(char$abilityMods['Dex'] + char$initMiscMod)
}
