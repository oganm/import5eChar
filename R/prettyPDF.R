# animation::pdftk(sourcePDF,operation = 'generate_fdf',output = fdfFile)
# fields = staplr::get_fields(input_filepath = system.file('character.pdf',package='import5eChar'))
# saveRDS(fields,'inst/char.rds')

fdfGet = function(field,fdf){
    stringr::str_extract(string = fdf,
                pattern =  paste0('/V\\s.*\n/T\\s\\(',field,'\\)'))
}

#' @export
prettyPDF = function(file,char = getOption('defaultCharacter')){
    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    # fix all character fields
    charFields = char %>% purrr::map_lgl(is.character)
    noDims = char %>% purrr::map(dim) %>% purrr::map_lgl(is.null)

    fixChars = which(charFields&noDims) %>% names

    disallowedCharRegex = "[^\u0001-\u007F\u0080-\u00FF\u0100-\u017F\u0180-\u024F\u1E00-\u1EFF\u0370-\u03FF\u1F00-\u1FFF\u0400-\u04FF\u0500-\u052F\u0530-\u058F\u0590-\u05FF\u0600-\u06FF]"

    for (x in fixChars){
        char[[x]] %<>% stringr::str_replace_all('•|\u{2022}','-') %>%
            stringr::str_replace_all('(½)|(\u{00BD})','1/2') %>%
            stringr::str_replace_all('(¾)|(\u{00BE})','3/4') %>%
            stringr::str_replace_all('(¼)|(\u{00BC})','1/4') %>%
            stringr::str_replace_all('(¼)|(\u{00BC})','1/4') %>%
            stringr::str_replace_all('(‘)|(\u{2018})',"'") %>%
            stringr::str_replace_all('(’)|(\u{2019})',"'") %>%
            stringr::str_replace_all('(—)|(\u{FE58})',"-") %>%
            stringr::str_replace_all('(“)|(\u{201C})','"') %>%
            stringr::str_replace_all('(”)|(\u{201D})','"') %>%
            stringr::str_replace_all('(→)|(\u{201D})','->') %>%
            # gsub("[^\x01-\x7F]", "", .)
            gsub(disallowedCharRegex,'',.)


    }
    # cat(char$Features)

    fixCharsPersonality = names(char$personality)
    for (x in fixCharsPersonality){
        char$personality[[x]] %<>% stringr::str_replace_all('•|\u{2022}','-') %>%
            stringr::str_replace_all('(½)|(\u{00BD})','1/2') %>%
            stringr::str_replace_all('(¾)|(\u{00BE})','3/4') %>%
            stringr::str_replace_all('(¼)|(\u{00BC})','1/4') %>%
            stringr::str_replace_all('(¼)|(\u{00BC})','1/4') %>%
            stringr::str_replace_all('(‘)|(\u{2018})',"'") %>%
            stringr::str_replace_all('(’)|(\u{2018})',"'") %>%
            stringr::str_replace_all('(—)|(\u{FE58})',"-") %>%
            stringr::str_replace_all('(“)|(\u{201C})','"') %>%
            stringr::str_replace_all('(”)|(\u{201D})','"') %>%
            stringr::str_replace_all('(→)|(\u{201D})','->') %>%
            # gsub("[^\x01-\x7F]", "", .)
            gsub(disallowedCharRegex,'',.)

    }


    sourcePDF = system.file('character.pdf',package='import5eChar')

    fields = readRDS(system.file('char.rds',package='import5eChar'))

    inChar = c('Str','Dex','Con','Int','Wis','Cha')
    inPDF =  c('str','dex','con','int','wis','cha')

    for(i in 1:length(char$abilityScores)){
        # ability scores

        fields[[inPDF[i]]]$value = char$abilityMods[inChar[i]]


        # ability mods

        fields[[paste0(inPDF[i],'-mod')]]$value = char$abilityScores[inChar[i]]

    }

    # saves
    saves = saveBonus(char = char)
    for(i in 1:length(char$abilityScores)){


        fields[[names(saves)[i] %>% tolower %>% paste0(.,'-save')]]$value = saves[i]



        fields[[names(saves)[i] %>% tolower %>% paste0(.,'-save')]]$value = saves[i]

        fields[[names(saves)[i] %>% tolower %>% paste0(.,'-save-check')]]$value =
            char$abilityProf[i] %>% {if(.){'Yes'}else{"Off"}}

    }


    # AC


    fields[['ac']]$value =  AC(char = char)


    # init


    fields[['initiative']]$value =  initBonus(char = char)


    # speed


    fields[['speed']]$value =  char$baseSpeed + char$speedMiscMod


    # HP


    fields[['hp-max']]$value = char$maxHealth



    fields[['hp-current']]$value =  char$currentHealth


    if(char$currentTempHP>0){

        fields[['hp-temp']]$value =  char$currentTempHP

    }

    # hit dice


    fields[['hd-total']]$value = char$hitDice %>% paste(collapse=', ')


    # traits


    fields[['personality-traits']]$value = char$personality$traits


    # ideals

    fields[['ideals']]$value = char$personality$ideals

    # bonds

    fields[['bonds']]$value = char$personality$bonds


    # flaws


    fields[['flaws']]$value = char$personality$flaws


    # features


    fields[['features-and-traits']]$value =
        char$Features


    fields[['features-and-traits-2']]$value =
        char$notes

    # class level

    fields[['class-level']]$value = char$ClassField


    # background
    fields[['background']]$value = char$Background


    # alignment
    fields[['alignment']]$value = char$Alignment


    # race
    fields[['race']]$value = char$Race

    # name
    fields[['character-name']]$value = char$Name

    fields[['character-name-2']]$value = char$Name

    # spells
    DC = spellDC(char = char)
    spellAttack  =  spellAttack(char = char)

    fields[['spell-save-dc-1']]$value = DC


    fields[['spell-attack-bonus-1']]$value = spellAttack


    fields[['spellcasting-ability-1']]$value =  names(spellAttack)

    slots = spellSlots(char)

    if(!is.null(char$spells)){
        for(i in 1:9){
            fields[[glue::glue('spell-slots-{i}-1')]]$value =  slots[i]
        }

        limits = c('0' = 8,
                   '1' = 12,
                   '2' = 13,
                   '3' = 13,
                   '4' = 13,
                   '5' = 9,
                   '6' = 9,
                   '7' = 9,
                   '8' = 7,
                   '9' = 7)
        for(i in 0:9){
            spells  = char$spells %>% dplyr::filter(level ==i)
            for(t in seq_len(min(nrow(spells), limits[i %>% as.character()]))){
                fields[[glue::glue('spells-{i}-{t}-1')]]$value =  spells$name[t]
            }
        }


    }

    # equipment
    fields[['equipment']]$value = char$Equipment


    # gold
    for(x in names(char$currency)){
        fields[[tolower(x)]]$value =char$currency[[x]]

    }

    # language and proficiency
    profAndLang = paste0('Armor Prof.\n',char$ArmorProficiencies,'\n',
                         'Weapon Prof.\n',char$WeaponProficiencies,'\n',
                         'Tool Prof.\n',char$ToolProficiencies,'\n',
                         'Languages\n',char$LanguagesKnown)

    # weapons
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

        for(i in 1:min(3,nrow(weaponTable))){

            fields[[paste0('weapon-name-',i)]]$value = paste0(weaponTable[i,1],' (',weaponTable[i,5],')')


            fields[[paste0('weapon-attack-bonus-',i)]]$value = weaponTable[i,2]


            fields[[paste0('weapon-damage-',i)]]$value = paste0(weaponTable[i,3],'/',weaponTable[i,4])

        }

        moreWeapons = ''
        for(i in seq_len(max(nrow(weaponTable)-3,0))){
            index= 3+i
            wepDat = paste0(weaponTable[index,1],' (',weaponTable[index,5],')    ',
                            weaponTable[index,2],'    ',
                            weaponTable[index,3],'/',weaponTable[index,4])
            moreWeapons = paste0(moreWeapons,
                                 wepDat,
                                 '\n')
        }

        fields[['attacks-and-spellcasting']]$value = moreWeapons

    }


    # skills
    skillBonuses = skillBonus(char = char)
    pdfNames = skillBonuses %>% names %>% tolower %>% stringr::str_replace_all(' ','-')
    pdfCheckbox= paste0(pdfNames,'-check')
    for(i in 1:length(skillBonuses)){

        fields[[pdfNames[i]]]$value = skillBonuses[i]

        fields[[pdfCheckbox[i]]]$value = char$skillProf[i] %>% {if(.){'Yes'}else{"Off"}}

    }

    # passive perception

    fields[['passive']]$value = 10 + skillBonuses['Perception']


    # prof bonus
    fields[['prof-bonus']]$value = char$proficiencyBonus


    fields[['other-profs']]$value = profAndLang

    staplr::set_fields(sourcePDF,output_filepath = file,fields = fields)

}
