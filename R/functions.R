#' @export
shinySheet = function(char =getOption('defaultCharacter'),...,autoClose = FALSE,
                      spellSource = 'https://www.dndbeyond.com/spells/'){

    if(autoClose){
        options(AutoClosCharSheet = TRUE)
    }
    if(is.character(char)){
        if(exists(char,envir = parent.frame())){
            char = char %>% parse(text = .) %>% eval(envir = parent.frame())
        }
    }
    spellSource = spellSource
    shinyDir = system.file('app',package = 'import5eChar')
    shiny::runApp(shinyDir,...)

    if(autoClose){
        options(AutoCloseCharSheet = NULL)
    }
}

#' @export
shinySheetAC =  function(char =getOption('defaultCharacter'),...,
                         spellSource = 'https://www.dndbeyond.com/spells/'){
    shinySheet(char = char, ..., autoClose = TRUE,spellSource = spellSource)
}

#' @export
weaponAttack = function(weapon,
                        adv = 0,
                       sharpShoot = F,
                       modToHit = 0,
                       modToDamage = 0,
                       ammo = NULL,
                       vocal = TRUE,
                       critRange = 20,
                       char = getOption('defaultCharacter')){

    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    bonus = weaponBonus(weapon,char = char)

    out = attack(adv = adv,
           sharpShoot = sharpShoot,
           attackStat = weapon$attackStat,
           damageDice = weapon$dice,
           proficient = weapon$proficient,
           modToHit = bonus['weaponTypeAttackBonus'] + modToHit ,
           modToDamage = bonus['weaponTypeDamageBonus'] + modToDamage,
           ammo = ammo,
           vocal = vocal,
           char = char)

    print(paste(weapon$damageType,'damage'))
    return(out)

}

#' Calculate bonuses that affect a weapon attack
#' @export
weaponBonus =function(weapon,
                      char = getOption('defaultCharacter')){

    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    weaponTypeAttackBonus =
        (weapon$type=='ranged')*char$weaponAttackMods['allRanged'] +
        (weapon$type=='ranged' & weapon$hands == 1)*char$weaponAttackMods['oneHandRanged'] +
        (weapon$type=='ranged' & weapon$hands == 2)*char$weaponAttackMods['twoHandRanged'] +
        (weapon$type=='melee')*char$weaponAttackMods['allMelee'] +
        (weapon$type=='melee' & weapon$hands == 1)*char$weaponAttackMods['oneHandMelee'] +
        (weapon$type=='melee' & weapon$hands == 2)*char$weaponAttackMods['twoHandMelee']

    weaponTypeDamageBonus =
        (weapon$type=='ranged')*char$weaponDamageMods['allRanged'] +
        (weapon$type=='ranged' & weapon$hands == 1)*char$weaponDamageMods['oneHandRanged'] +
        (weapon$type=='ranged' & weapon$hands == 2)*char$weaponDamageMods['twoHandRanged'] +
        (weapon$type=='melee')*char$weaponDamageMods['allMelee'] +
        (weapon$type=='melee' & weapon$hands == 1)*char$weaponDamageMods['oneHandMelee'] +
        (weapon$type=='melee' & weapon$hands == 2)*char$weaponDamageMods['twoHandMelee']

    c('weaponTypeAttackBonus' = unname(weaponTypeAttackBonus) + weapon$miscAttackBonus + weapon$magicAttackBonus,
      'weaponTypeDamageBonus' = unname(weaponTypeDamageBonus) + weapon$miscDamageBonus + weapon$magicDamageBonus)

}

#' Attack
#'
#' @param adv integer. 0: Normal, -1 Disadvantage, +1 Advantage
#' @param sharpShoot logical. are we sharpshooting
#' @param attackStat character. which stat to use for attack. 3 letter. Added to hit and damage rolls
#' @param damageDice character vector. attack dice. If more than one dice type is used (as in sneak attack) add them as a vector (\code{damageDice = c('1d8','1d6')})
#' @param proficient logical. is the attacker proficient in the weapon. If so proficiency bonus will be added to hit rolls
#' @param modToHit integer. hit modifier
#' @param modToDamage integer. damage modifier
#' @param ammo character. If provided ammo will be used. Before using, you need to set your ammo count manually by character$ammoName = 30 (character$bolt = 30)
#' @param vocal logical. Should dice rolls and crit notifications be printed.
#' @param critRange vector of integers. Which rolls to count as a critical hit
#' @param char A list outputted from importCharacter or processCharacter, or a character naming such a list
#'
#' @return Named vector. Attack roll and damage. If a 1 is rolled damage is always 0
#' @export
#'
attack = function(adv = 0,
                  sharpShoot = F,
                  attackStat = c('Str','Dex','Con','Wis','Int','Cha'),
                  damageDice = '1d6',
                  proficient = TRUE,
                  modToHit = 0,
                  modToDamage = 0,
                  ammo = NULL,
                  vocal = TRUE,
                  critRange = 20,
                  char = getOption('defaultCharacter')){
    # if char isn't provided see what the defaultCharacter option is
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    attackStat = match.arg(attackStat)

    if(!is.null(ammo)){
        char[[ammo]] <<- char[[ammo]] - 1
        char[[ammo]] = char[[ammo]] -1
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

    skillNames = names(char$skillProf)
    skill = skillNames[grepl(pattern = tolower(skill), x = tolower(skillNames))]

    bonus = skillBonus(char = char)[skill]

    if(char$skillHalfProfRoundUp[skill]){
        bonus %<>% ceiling()
    } else{
        bonus %>% floor()
    }

    diceSyntax::r(r1d20) + unname(bonus)
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
    bonus = skillBonus(char = char)


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

    roll + unname(bonus)

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


    unname(diceSyntax::r(r1d20) + saveBonus(char = char)[stat])
}


#' @export
quickSave = function(char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    diceSyntax::r(r1d20)  + saveBonus(char = char)
}


#' Title
#'
#' @export
#'
init = function(char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    diceSyntax::r(r1d20) + initBonus(char)
}

