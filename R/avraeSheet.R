#' @export
avraeSheet = function(char = getOption('defaultCharacter')){

    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    oganSheet = "1a9CUrPOdPPV5qbh3uUBT4cqq1Ue5Eo70fWLUp6pSZAY"
    sheet = googlesheets::gs_key(x = oganSheet)
    sheetToFill = sheet %>% googlesheets::gs_copy(to=char$Name)

    file = googledrive::drive_get(id = sheetToFill$sheet_key,verbose=FALSE)

    share = googledrive::drive_share(googledrive::as_id(file$id),
                                     role='reader', type = 'anyone')


    sheetToFill %>% googlesheets::gs_edit_cells(input = char$abilityScores['Str'], anchor = 'C15')
    sheetToFill %>% googlesheets::gs_edit_cells(input = char$abilityScores['Dex'], anchor = 'C20')
    sheetToFill %>% googlesheets::gs_edit_cells(input = char$abilityScores['Con'], anchor = 'C25')
    sheetToFill %>% googlesheets::gs_edit_cells(input = char$abilityScores['Int'], anchor = 'C30')
    sheetToFill %>% googlesheets::gs_edit_cells(input = char$abilityScores['Wis'], anchor = 'C35')
    sheetToFill %>% googlesheets::gs_edit_cells(input = char$abilityScores['Cha'], anchor = 'C40')



    topInfo = matrix(c(char$ClassField,
                       rep('',5),
                       char$Background,
                       rep('',4),
                       '',
                       'CLASS',rep('',5), 'BACKGROUND',rep('',4),'PLAYER NAME',
                       char$Race,
                       rep('',5),
                       char$Alignment,
                       rep('',5)),nrow = 3,byrow = TRUE)



    sheetToFill %>% googlesheets::gs_edit_cells(input = topInfo, byrow= TRUE, anchor = 'T5')

    sheetToFill %>% googlesheets::gs_edit_cells(input = char$classInfo[,'Level'] %>% as.integer %>% sum,
                                                anchor = 'AL6')

    sheetToFill %>% googlesheets::gs_edit_cells(input = AC(char),
                                                anchor = 'R12')

    sheetToFill %>% googlesheets::gs_edit_cells(input = char$baseSpeed + char$speedMiscMod,
                                                anchor = 'Z12')


    sheetToFill %>% googlesheets::gs_edit_cells(input = char$personality$traits,
                                                anchor = 'AE12')

    sheetToFill %>% googlesheets::gs_edit_cells(input = char$personality$ideals,
                                                anchor = 'AE16')

    sheetToFill %>% googlesheets::gs_edit_cells(input = char$personality$bonds,
                                                anchor = 'AE20')

    sheetToFill %>% googlesheets::gs_edit_cells(input = char$personality$flaws,
                                                anchor = 'AE24')

    sheetToFill %>% googlesheets::gs_edit_cells(input = char$maxHealth,
                                                anchor = 'U16')

    sheetToFill %>% googlesheets::gs_edit_cells(input = char$currentHealth,
                                                anchor = 'R17')

    sheetToFill %>% googlesheets::gs_edit_cells(input = char$currentTempHP,
                                                anchor = 'R21')


    proficiencies = c(char$ArmorProficiencies %>% gsub('\n',', ', .) %>%
                          paste('Proficient in Armor:',.),
                      char$WeaponProficiencies %>%  gsub('\n',', ', .) %>%
                          paste('Proficient in Weapons:',.),
                      'Proficient in Vehicles:',
                      char$ToolProficiencies %>%  gsub('\n',', ', .) %>%
                          paste('Proficient in Tools:',.))

    sheetToFill %>% googlesheets::gs_edit_cells(input =proficiencies,
                                                anchor = 'C49')

    # saves ----
    sheetToFill %>%
        googlesheets::gs_edit_cells(input = char$abilityProf %>% as.integer(),anchor = 'H17' )



    # skills ------

    skillNames = names(char$skillProf) %>% sort

    skillProfs = seq_along(skillNames) %>% sapply(function(i){
        inp = 0

        if(char$skillDoubleProf[skillNames[i]]){
            inp = 'e'
        } else if(char$skillProf[skillNames[i]]){
            inp = '1'
        } else if(char$skillHalfProf[skillNames[i]] | char$skillHalfProfRoundUp[skillNames[i]]){
            inp = 'h'
        }
        return(inp)
    })

    sheetToFill %>%
        googlesheets::gs_edit_cells(input = skillProfs,anchor = 'H25' )

    # weapons -----

    if(length(char$weapons)>0){
        weaponTable = char$weapons %>% sapply(function(x){
            c(x$name,
              weaponBonus(x,char=char)['weaponTypeAttackBonus'] +
                  x$proficient*char$proficiencyBonus +
                  char$abilityMods[x$attackStat],
              paste0(paste(x$dice,collapse=' + '),
                     '+',
                     weaponBonus(x,char=char)['weaponTypeDamageBonus'] +
                         char$abilityMods[x$attackStat]),
              x$damageType,
              x$range
            )
        }) %>% t
        colnames(weaponTable) = c('Name','Attack','Damage','Type','Range')

        weaponTable[,'Damage'] %<>% gsub('\\+\\-','-',x=.)

        weaponsToWrite = data.frame('Name' = paste0(weaponTable[,'Name'],' (',weaponTable[,'Range'],')'),
                                    Attack = weaponTable[,'Attack'],
                                    'Damage/Type' = paste0(weaponTable[,'Damage'],' [',weaponTable[,'Type'],']'),
                                    check.names = FALSE,stringsAsFactors = FALSE)

        weaponsToStructuredBox = cbind(weaponsToWrite$Name,'','','','','','=IMAGE("http://i.imgur.com/qlVxTyw.png",3)',
                               weaponsToWrite$Attack, '','','=IMAGE("http://i.imgur.com/qlVxTyw.png",3)',
                               weaponsToWrite$`Damage/Type`)


        sheetToFill %>%
            googlesheets::gs_edit_cells(input = weaponsToStructuredBox[1:min(nrow(weaponsToWrite),5),],
                                        anchor = 'R32',col_names = FALSE )


        if(nrow(weaponsToWrite)>5){
            otherWeapons = weaponsToWrite[6:nrow(weaponsToWrite),]
            otherWeapons %<>% apply(1,paste,collapse=' ')

            sheetToFill %>%
                googlesheets::gs_edit_cells(input = otherWeapons[1:min(length(otherWeapons),6)],
                                        anchor = 'R37',col_names = FALSE )
        }
    }


    langsAndTools = c(char$LanguagesKnown %>% stringr::str_split('\n') %>% {.[[1]]},
      char$ToolProficiencies %>% stringr::str_split('\n') %>% {.[[1]]})[1:12] %>%
        ogbox::trimNAs()


    sheetToFill %>%
        googlesheets::gs_edit_cells(input = langsAndTools,
                                    anchor = 'R45' )


    features = char$Features %>% stringr::str_split('\n') %>% {.[[1]]}

    featuresCol1 = features[1:12] %>% ogbox::trimNAs()
    featuresCol2 = features[13:24] %>% ogbox::trimNAs()

    features = cbind(featuresCol1,
                     '','','','','','','=IMAGE("http://i.imgur.com/YThwbjs.png",3)',
                     featuresCol2)


    sheetToFill %>%
        googlesheets::gs_edit_cells(input = features,
                                    anchor = 'Z45',col_names = FALSE )


    currency = c(char$currency$CP,
                 rep('',2),
                 char$currency$SP,
                 rep('',2),
                 char$currency$EP,
                 rep('',2),
                 char$currency$GP,
                 rep('',2),
                 char$currency$PP)

    sheetToFill %>%
        googlesheets::gs_edit_cells(input = currency,
                                    anchor = 'D60' )


    # spellcasting ------
    DC = spellDC(char = char)
    spellAttack  =  spellAttack(char = char)

    spellStuff = c(names(spellAttack),
                   rep('',3),
                   '=IMAGE("http://i.imgur.com/vGypnz4.png",2)',
                   '',
                   '=IMAGE("http://i.imgur.com/bMB1ljY.png",2)',
                   DC,
                   rep('',3),
                   '=IMAGE("http://i.imgur.com/vGypnz4.png",2)',
                   '',
                   '=IMAGE("http://i.imgur.com/bMB1ljY.png",2)',
                   spellAttack)

    sheetToFill %>%
        googlesheets::gs_edit_cells(input = spellStuff,
                                    anchor = 'U91',byrow = TRUE )


    slots = spellSlots(char)
    maxSlotBoxes = c('AK101','E107','AK113','E119','AK124','E129','AK134','E138','AK142')
    slotBoxes = c('AG101','I107','AG113','I119','AG124','I129','AG134','I138','AG142')

    spellCols = list('0' = c('N','X','AH'),
                     '1' = c('D','N','X'),
                     '2' = c('N','X','AH'),
                     '3' = c('D','N','X'),
                     '4' = c('N','X','AH'),
                     '5' = c('D','N','X'),
                     '6' = c('N','X','AH'),
                     '7' = c('D','N','X'),
                     '8' = c('N','X','AH'),
                     '9' = c('D','N','X'))

    spellRows = c('0' = 96,
                  '1' = 100,
                  '2' = 106,
                  '3' = 112,
                  '4' = 118,
                  '5' = 123,
                  '6' = 128,
                  '7' = 133,
                  '8' = 137,
                  '9' = 141)

    preparedBoxes = c(N = 'M',
                      X = 'W',
                      AH = 'AG',
                      D = 'C')

    rowCount = c('0'=3,
                 '1' = 5,
                 '2'= 5,
                 '3' = 5,
                 '4' = 4,
                 '5' = 4,
                 '6' = 4,
                 '7' = 3,
                 '8' = 3,
                 '9' = 3)

    for(i in 0:9){
        if(i>0 && slots[i]>0){
            sheetToFill %>%
                googlesheets::gs_edit_cells(input = slots[i],
                                            anchor = maxSlotBoxes[i])
            sheetToFill %>%
                googlesheets::gs_edit_cells(input = char$spellSlots[i+1],
                                            anchor = slotBoxes[i])
        }

        spellsToWrite = char$spells %>% dplyr::filter(level == i)
        if(nrow(spellsToWrite)>0){
            level = i %>% as.character
            seq_along(spellCols[[level]]) %>% sapply(function(j){
                start = rowCount[level]*j-(rowCount[level]-1)
                end = min(rowCount[level]*j,nrow(spellsToWrite))
                if(start>end){
                    return(NULL)
                }
                sheetToFill %>%
                    googlesheets::gs_edit_cells(input = spellsToWrite[start:end,]$name,
                                                anchor = paste0(spellCols[[level]][j], spellRows[level]))
                if(i >0){
                    sheetToFill %>%
                        googlesheets::gs_edit_cells(input = spellsToWrite[start:end,]$prepared %>% as.integer(),
                                                    anchor = paste0(preparedBoxes[spellCols[[level]][j]], spellRows[level]))
                }
            })
        }

    }

    return(file)

}
