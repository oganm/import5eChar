
#' Attack
#'
#' @param adv integer. 0: Normal, -1 Disadvantage, +1 Advantage
#' @param sharpShoot logical. are we sharpshooting
#' @param attackStat character. which stat to use for attack. 3 letter. Added to hit and damage rolls
#' @param damageDice character vector. attack dice. If more than one dice type is used (as in sneak attack) add them as a vector (\code{damageDice = c('1d8','1d6')})
#' @param proficient logical. is the attacker proficient in the weapon. If so proficiency bonus will be added to hit rolls
#' @param modToHit integer. hit modifier
#' @param modToDamage integer. damage modifier
#' @param useAmmo logical. should the shot expand ammo. This uses \code{<<-} to set the ammo variable so not exactly function friendly
#' @param ammo character. what is the name of the ammo. Before using, you need to set your ammo count manually by character$ammoName = 30 (character$bolt = 30)
#' @param vocal logical. Should dice rolls and crit notifications be printed.
#' @param critRange vector of integers. Which rolls to count as a critical hit
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
                  critRange = 20,
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
    finalRoll = unname(attackRoll + char$abilityMods[attackStat] + proficient*char$proficiencyBonus + modToHit)
    if(sharpShoot){
        finalRoll = finalRoll - 5
    }

    if(attackRoll == 1){
        if(vocal){
            print('CRIT MISS')
        }
        damageRoll = 0
    } else if(attackRoll %in%  critRange){
        if(vocal){
            print('CRIT HIT')
        }
        damageDice = c(damageDice,damageDice)
        damageRolls = damageDice %>% sapply(function(x){
            diceSyntax::r(x,vocal = vocal)
        })
        damageRoll = unname(sum(damageRolls) + char$abilityMods[attackStat] +modToDamage)
    } else{
        damageRolls = damageDice %>% sapply(function(x){
            diceSyntax::r(x,vocal = vocal)
        })
        damageRoll = unname(sum(damageRolls) + char$abilityMods[attackStat] +modToDamage)
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
        (char$skillHalfProf*!char$skillProf*char$proficiencyBonus/2) +
        char$abilityMods[char$skillAttributes]


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


#' @export
quickSave = function(char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    diceSyntax::r(r1d20)  + char$abilityMods + char$proficiencyBonus*char$abilityProf
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
