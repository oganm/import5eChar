#' Import character from google drive
#'
#' @description Provided a regex or fileID this will import the character from your google drive so it can be used in utility functions. Will ask for
#' googledrive authentication on first load. If you are using Rstudio server, you can authenticate using another machine and move the generated
#' .httr-oauth file to your working directory.
#'
#' @param regex A regular expression that matches the file name. If there are multiple matches, the most recently edited file will the chosen
#' @param fileID A googledrive file ID.
#' @param file path to file
#' @param output if provided, the file will be saved here. if not, a temporary file will be used
#' @param overwrite If TRUE the new file will overwrite the old one. Not important if output isn't provided as the file will be deleted eventually
#' @param ... Variables to pass to googledrive::drive_find (if regex is given) or googledrive::drive_get (if fileID is given).
#' @return The output is a nested list extracted from the original XML file, edited to make certain fields more computer readable.
#'
#' @export
#'
importCharacter = function(regex=NULL, fileID = NULL,file = NULL, output=NULL,overwrite=TRUE,...){
    if(is.null(regex) & is.null(fileID) & is.null(file)){
        error('Either regex, fileID or file should be provided')
    }
    if(is.null(file)){
    if(!is.null(regex) & !is.null(fileID)){
        error('Either regex OR fileID should be provided, not both of them at once.')
    } else if(is.null(fileID)){
        character = googledrive::drive_find(pattern = regex,verbose=FALSE,...)[1,]
    } else if(is.null(regex)){
        character = googledrive::drive_get(fileID,verbose=FALSE,...)
    }

    download_link <- character$drive_resource[[1]]$webContentLink
    if(is.null(output)){
        output = tempfile()
    }
    res = httr::GET(download_link,
                    httr::write_disk(output,overwrite = TRUE),
                    googledrive:::drive_token())
    } else{
        output = file
    }

    char = paste(readLines(output,encoding = 'UTF-8'),collapse = '\n')

    return(processCharacter(char))
}


processCharacter = function(char){

    char %<>% XML::xmlParse() %>%  (XML::xmlToList)

    char$proficiencyBonus %<>% as.integer


    # ability score -------------
    abilityScores = char$abilityScores %>% stringr::str_extract_all('[0-9]+') %>% {.[[1]][1:6]} %>% as.integer()
    names(abilityScores) = c('Str','Dex','Con','Int','Wis','Chr')
    abilityMods = stat2mod(abilityScores)
    proficiency = char$abilityScores %>% stringr::str_extract_all('(true)|(false)') %>% {.[[1]][1:6]}
    proficiency %<>% ogbox::replaceElement(c('true'=TRUE,'false'=FALSE)) %$% newVector %>% as.logical()
    names(proficiency) = c('Str','Dex','Con','Int','Wis','Chr')

    char$initMiscMod %<>% as.integer()
    char$abilityScores = abilityScores
    char$abilityProf = proficiency
    char$maxHealth %<>% as.integer()
    char$abilityMods = abilityMods

    # process skills ---------
    skillNames = c('Athletics',
                   'Acrobatics',
                   'Sleight of Hand',
                   'Stealth',
                   'Arcana',
                   'History',
                   'Investigation',
                   'Nature',
                   'Religion',
                   'Animal Handling',
                   'Insight',
                   'Medicine',
                   'Perception',
                   'Survival',
                   'Deception',
                   'Intimidation',
                   'Performance',
                   'Persuasion')

    skillAttributes = c('Str',
                         rep('Dex',3),
                         rep('Int',5),
                         rep('Wis',5),
                         rep('Chr',4))
    names(skillAttributes) = skillNames
    char$skillAttributes = skillAttributes
    # char$skillNames = skillNames

    skillData = char$skillInfo %>% strsplit('⊠|(â\u008a.)|(\u{22a0})') %>% {.[[1]]} %>% trimws()

    skillProf = skillData[1:18] %>% logicConvert
    names(skillProf) = skillNames
    skillMiscMod = skillData[20:37] %>% as.integer()
    names(skillMiscMod)= skillNames
    skillDoubleProf = skillData[39:56]  %>% logicConvert
    names(skillDoubleProf) = skillNames

    # true for everything, ignore if also proficient
    skillHalfProf = skillData[58:75] %>%  logicConvert
    names(skillHalfProf) = skillNames

    skillHalfProfRoundUp = skillData[77:94] %>%  logicConvert
    names(skillHalfProfRoundUp) = skillNames

    char$skillProf = skillProf
    char$skillMiscMod = skillMiscMod
    char$skillDoubleProf = skillDoubleProf
    char$skillHalfProf = skillHalfProf
    char$skillHalfProfRoundUp = skillHalfProfRoundUp



    weapons = char$weaponList %>% strsplit('⊠|(â\u008a.)|(\u{22a0})')  %>% .[[1]]
    weapons = weapons[-1]
    suppressWarnings(
    {splitPoints = weapons %>% ogbox::replaceElement(c('true'=1,
                                                          'false' = 0)) %$%
        newVector  %>% as.integer %>% is.na %>% which()
    })

    splitPoints = splitPoints[-(which(diff(splitPoints)==1)+1)]

    weapons %<>% splitAt(splitPoints)
    weapons %<>% lapply(function(x){
        name = x[1]
        range = x[2]
        dice = sapply(seq(length(12:length(x))/2),function(i){
            out = paste0(x[12+(i-1)*2],'d',x[13+(i-1)*2])
        })
        proficient = x[9] %>%
            logicConvert
        miscInfo = x[[3]] %>% strsplit('') %>% .[[1]]
        hands = miscInfo[2] %>% as.integer()
        type = miscInfo[1] %>%
            ogbox::replaceElement(dictionary = c('2'='ranged','1' = 'melee')) %$% newVector

        damageType = miscInfo[4] %>%
            ogbox::replaceElement(
                dictionary = c('0'='Bludgeoning',
                               '1' = 'Piercing',
                               '2' = 'Slashing',
                               '3' = 'Acid',
                               '4' = 'Cold',
                               '5' = 'Fire',
                               '6' = 'Force',
                               '7' = 'Lightning',
                               '8' = 'Necrotic',
                               '9' = 'Poison',
                               '10' = 'Psychic',
                               '11' = 'Radiant',
                               '12' = 'Thunder')) %$%
            newVector

        miscDamageBonus = x[8] %>% as.integer()
        magicDamageBonus = x[7] %>% as.integer()

        miscAttackBonus = x[5] %>% as.integer
        magicAttackBonus = x[6] %>% as.integer

        attackStat = x[4] %>%
            ogbox::replaceElement(dictionary = c('0'='Str',
                                                 '1' = 'Dex',
                                                 '2' = 'Con',
                                                 '3' = 'Int',
                                                 '4' = 'Wis',
                                                 '5' = 'Chr')) %$% newVector

        return(list(name = name,
                 range = range,
                 dice=  dice,
                 hands = hands,
                 attackStat =attackStat,
                 proficient = proficient,
                 type = type,
                 damageType = damageType,
                 miscDamageBonus = miscDamageBonus,
                 magicDamageBonus = magicDamageBonus,
                 miscAttackBonus = miscAttackBonus,
                 magicAttackBonus = magicAttackBonus))
    })

    names(weapons) = weapons %>% purrr::map_chr('name')

    char$weapons = weapons

    classData = char$classData %>% strsplit('⊟|(\u{229f})') %>% {.[[1]]}

    whereStart = classData %>% grep(pattern = '^[0-9]',x = .) %>% {.[[1]]}

    weaponAttackMods = classData[whereStart] %>% strsplit('⊠|\u{22a0}') %>% {.[[1]]} %>% as.integer()

    oneHandMeleeAttack = weaponAttackMods[5]
    twoHandMeleeAttack = weaponAttackMods[8]
    allMeleeAttack =  weaponAttackMods[2]

    oneHandRangedAttack = weaponAttackMods[6]
    twoHandRangedAttack = weaponAttackMods[9]
    allRangedAttack = weaponAttackMods[3]

    char$weaponAttackMods = c('oneHandMelee' = oneHandMeleeAttack,
                              'twoHandMelee' = twoHandMeleeAttack,
                              'allMelee' = allMeleeAttack,
                              'oneHandRanged' = oneHandRangedAttack,
                              'twoHandRanged' = twoHandRangedAttack,
                              'allRanged' = allRangedAttack)

    weaponDamageMods = classData[whereStart + 1] %>% strsplit('⊠|\u{22a0}') %>% {.[[1]]} %>% as.integer()
    oneHandMeleeDamage = weaponDamageMods[5]
    twoHandMeleeDamage = weaponDamageMods[8]
    allMeleeDamage =  weaponDamageMods[2]

    oneHandRangedDamage = weaponDamageMods[6]
    twoHandRangedDamage = weaponDamageMods[9]
    allRangedDamage = weaponDamageMods[3]

    char$weaponDamageMods = c('oneHandMelee' = oneHandMeleeDamage,
                              'twoHandMelee' = twoHandMeleeDamage,
                              'allMelee' = allMeleeDamage,
                              'oneHandRanged' = oneHandRangedDamage,
                              'twoHandRanged' = twoHandRangedDamage,
                              'allRanged' = allRangedDamage)

    return(char)
}
