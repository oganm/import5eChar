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

    char %<>% stringr::str_replace('&','and') %>%  XML::xmlParse() %>%  (XML::xmlToList)

    # simple statistics -----
    char$proficiencyBonus %<>% as.integer
    char$armorBonus %<>% as.integer
    char$shieldBonus %<>% as.integer
    char$miscArmorBonus %<>% as.integer
    char$maxDex %<>% as.integer
    char$miscSpellDCBonus %<>% as.integer
    char$miscSpellAttackBonus %<>% as.integer
    char$castingStatCode %<>% as.integer
    char$baseSpeed %<>% as.integer
    char$speedMiscMod %<>% as.integer
    char$currentHealth %<>% as.integer
    char$currentTempHP %<>% as.integer

    char$unarmoredDefense %<>%  ogbox::replaceElement(dictionary = c('0' = '',
                                                                     '1'='Str',
                                                                     '2' = 'Dex',
                                                                     '3' = 'Con',
                                                                     '4' = 'Int',
                                                                     '5' = 'Wis',
                                                                     '6' = 'Cha')) %$% newVector


    # ability score -------------
    abilityScores = char$abilityScores %>% stringr::str_extract_all('[0-9]+') %>% {.[[1]][1:6]} %>% as.integer()
    names(abilityScores) = c('Str','Dex','Con','Int','Wis','Cha')

    miscSaveBonus = char$abilityScores %>% stringr::str_extract_all('[0-9]+') %>% {.[[1]][7:12]} %>% as.integer()
    names(miscSaveBonus) = names(abilityScores)
    statToSave =  char$abilityScores %>% stringr::str_extract_all('[0-9]+') %>% {.[[1]][13]} %>%
        ogbox::replaceElement(dictionary = c('0' = '',
                                      '1'='Str',
                                      '2' = 'Dex',
                                      '3' = 'Con',
                                      '4' = 'Int',
                                      '5' = 'Wis',
                                      '6' = 'Cha')) %$% newVector

    abilityMods = stat2mod(abilityScores)
    proficiency = char$abilityScores %>% stringr::str_extract_all('(true)|(false)') %>% {.[[1]][1:6]}
    proficiency %<>% ogbox::replaceElement(c('true'=TRUE,'false'=FALSE)) %$% newVector %>% as.logical()
    names(proficiency) = c('Str','Dex','Con','Int','Wis','Cha')

    char$initMiscMod %<>% as.integer()
    char$abilityScores = abilityScores
    char$abilityProf = proficiency
    char$miscSaveBonus = miscSaveBonus
    char$statToSave = statToSave
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
                        rep('Cha',4))
    names(skillAttributes) = skillNames
    char$skillAttributes = skillAttributes
    # char$skillNames = skillNames

    skillData = char$skillInfo %>% strsplit('⊠|(â\u008a.)|(\u{22a0})') %>% {.[[1]]} %>% trimws()
    skillProf = skillData[1:18] %>% logicConvert

    profToInit = (skillData[19] %>% logicConvert())
    doubleProfToInit = (skillData[57] %>% logicConvert()) & (skillData[19] %>% logicConvert())
    halfProfToInit = (skillData[76] %>% logicConvert())
    halfProfToInitRoundUp = (skillData[76] %>% logicConvert()) & (skillData[95] %>% logicConvert())

    char$profToInit = c(profToInit = profToInit,
                        doubleProfToInit = doubleProfToInit,
                        halfProfToInit = halfProfToInit,
                        halfProfToInitRoundUp = halfProfToInitRoundUp)

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


    # weapon lists ---------------

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

        # attack and damage bonuses --------------

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
                                                 '5' = 'Cha')) %$% newVector

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

    char$statToInit = classData[whereStart+3] %>% ogbox::replaceElement(dictionary = c('0' = '',
                                                                                       '1'='Str',
                                                                                       '2' = 'Dex',
                                                                                       '3' = 'Con',
                                                                                       '4' = 'Int',
                                                                                       '5' = 'Wis',
                                                                                       '6' = 'Cha')) %$% newVector

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

    # class resources ---------------
    resources = classData[3] %>% strsplit('⊠|\u{22a0}') %>% {.[[1]]} %>% sapply(function(x){
        resData = x %>%strsplit('⊡|\u{22A1}') %>% {.[[1]]}
        c('name' = resData[1],
          'shortName' = resData[2],
          'remainingUse' = resData[4] %>% as.integer(),
          'maxUse' = resData[3] %>% as.integer(),
          'dice' = resData[5] %>% as.integer(),
          'ResourceDisplay' = {
              if(resData[5]>0){
                  out = (paste0(resData[3],'d',resData[5]))
              } else if(resData[3]>0){
                  out = (resData[3])
              } else{
                  out = ('')
              }
              out
          },
          'Reset' = {
              if(resData[10]==1){
                  out = 'static'
              }else if(resData[9] == 303){
                  out = 'never'
              }else{
                  out=
                      switch(resData[6] %>% as.integer,
                             "short rest",
                             'long rest')
              }
              out
          },
          'RecoverPerShortRest' = resData[7],
          'RecoverPerLongRest' = resData[8])
    }) %>% t

    resources %<>%as.data.frame(stringsAsFactors = FALSE) %>% dplyr::mutate(remainingUse = as.integer(remainingUse),
                                                  maxUse = as.integer(maxUse),
                                                  dice = as.integer(dice),
                                                  RecoverPerShortRest = as.integer(RecoverPerShortRest),
                                                  RecoverPerLongRest  = as.integer(RecoverPerLongRest))

    rownames(resources) = NULL

    char$resources = resources


    # feats -------
    char$feats = classData[4] %>% strsplit('⊠|\u{22a0}') %>% {.[[1]]}


    # character notes --------------
    notes = char$noteList %>% strsplit('⊠|\u{22a0}') %>% {.[[1]]}

    char$Features = notes[1]
    char$ArmorProficiencies = notes[2]
    char$WeaponProficiencies = notes[3]
    char$ToolProficiencies =notes[4]
    char$LanguagesKnown = notes[5]
    char$Equipment = notes[6]
    char$notes = notes[7]
    char$Class = notes[8]
    char$Race = notes[9]
    char$Background = notes[10]
    char$Alignment= notes[11]
    char$personality = list()
    char$personality$traits = notes[12]
    char$personality$ideals = notes[13]
    char$personality$bonds = notes[14]
    char$personality$flaws = notes[15]

    # 12-15 unkown
    char$Name = notes[16]
    char$ClassField = notes[17]

    char$currency = list(CP = notes[18],
                         SP = notes[19],
                         EP = notes[20],
                         GP = notes[21],
                         PP = notes[22])

    hitDice= char$hitDiceList %>% strsplit('⊠|\u{22a0}') %>% {.[[1]][-1]}
    diceCount = length(hitDice)/3

    char$hitDice = 1:diceCount %>% sapply(function(i){
        paste0(hitDice[(i-1)*3+1],'d',hitDice[(i-1)*3+2])
    })
    char$hitDiceRemain = 1:diceCount %>% sapply(function(i){
        paste0(hitDice[(i-1)*3+3],'d',hitDice[(i-1)*3+2])
    })

    # spells --------
    spellData = char$spellList  %>% strsplit('⊠|\u{22a0}') %>% {.[[1]]}
    slots=  spellData[2] %>% strsplit('⊡|\u{22A1}') %>% {.[[1]]} %>% as.integer()
    names(slots) = c('Cantrip',1:9)
    char$spellSlots = slots

    spells = spellData[9] %>%strsplit('⊡|\u{22A1}') %>% {.[[1]]} %>% sapply(function(x){
        spell = x %>% strsplit('⊟|(\u{229f})') %>% {.[[1]]}
        level = spell[1] %>% as.integer()
        name = spell[2]
        prepared = spell[8] %>% logicConvert()
        return(c('level' = level,
                 'name' = name,
                 'prepared' = prepared))
    }) %>% t



    rownames(spells)= NULL

    spells %<>% as.data.frame(stringsAsFactors=FALSE) %>% dplyr::mutate(level = level %>%as.character%>% as.integer, prepared = prepared %>% as.logical)

    if(all(is.na(spells[1,]))){
        spells = NULL
    }

    char$spells = spells


    return(char)
}
