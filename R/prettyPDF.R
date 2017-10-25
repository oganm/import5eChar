# animation::pdftk(sourcePDF,operation = 'generate_fdf',output = fdfFile)



fdfEdit = function(x, field,fdf){
    if(x == TRUE & is.logical(x)){
        x = '/Yes'
    } else if (x == FALSE & is.logical(x)){
        x = '/Off'
    } else {
        x %<>% gsub(x = ., pattern = '\\(',replacement = '\\\\(') %>%
            gsub(x = ., pattern = '\\)',replacement = '\\\\)')
        x = paste0('(',x,')')
    }

    fdf = stringr::str_replace(string  = fdf,pattern = paste0('/V\\s.*\n/T\\s\\(',field,'\\)'),
                               replacement = paste0('/V ',x,'\n/T \\(',field,'\\)'))
    return(fdf)
}



fdfGet = function(field,fdf){
    str_extract(string = fdf,
                pattern =  paste0('/V\\s.*\n/T\\s\\(',field,'\\)'))
}

#' @export
prettyPDF = function(file,char = getOption('defaultCharacter')){
    if(Sys.info()['sysname'] =='Windows'){
        path = system2('where','pdftk',stdout = TRUE)
    } else {
        path = system2('which','pdftk',stdout = TRUE)
    }
    animation::ani.options(pdftk = path)


    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }

    fdfFile = system.file('char.fdf',package = 'import5eChar')
    fdf = readLines(fdfFile) %>% paste(collapse = '\n')

    sourcePDF = system.file('character.pdf',package='import5eChar')


    inChar = c('Str','Dex','Con','Int','Wis','Cha')
    inPDF =  c('str','dex','con','int','wis','cha')

    for(i in 1:length(char$abilityScores)){
        # ability scores
        fdf = fdfEdit(
            x = char$abilityScores[inChar[i]],
            field = inPDF[i],
            fdf = fdf)

        # ability mods
        fdf = fdfEdit(
            x = char$abilityMods[inChar[i]],
            field = paste0(inPDF[i],'-mod'),fdf = fdf)
    }

    # saves
    saves = saveBonus(char = char)
    for(i in 1:length(char$abilityScores)){
        fdf = fdfEdit(
            x = saves[i],
            field = names(saves)[i] %>% tolower %>% paste0(.,'-save'),
            fdf = fdf)

        fdf = fdfEdit(
            x = char$abilityProf[i],
            field = names(char$abilityProf)[i] %>% tolower %>% paste0(.,'-save-check'),
            fdf = fdf)
    }


    # AC
    fdf = fdfEdit(
        x = AC(char = char),
        field = 'ac',
        fdf = fdf)

    # init
    fdf = fdfEdit(
        x = initBonus(char = char),
        field = 'initiative',
        fdf = fdf)

    # speed
    fdf = fdfEdit(
        x = char$baseSpeed + char$speedMiscMod,
        field = 'speed',
        fdf = fdf)

    # HP
    fdf = fdfEdit(
        x = char$maxHealth,
        field = 'hp-max',
        fdf = fdf)

    # hit dice
    fdf = fdfEdit(
        x = char$hitDice %>% paste(collapse=', '),
        field = 'hd-total',
        fdf = fdf)

    # traits
    fdf = fdfEdit(
        x = char$personality$traits,
        field = 'personality-traits',
        fdf = fdf)

    # ideals
    fdf = fdfEdit(
        x = char$personality$ideals,
        field = 'ideals',
        fdf = fdf)
    # bonds
    fdf = fdfEdit(
        x = char$personality$bonds,
        field = 'bonds',
        fdf = fdf)

    # flaws
    fdf = fdfEdit(
        x = char$personality$flaws,
        field = 'flaws',
        fdf = fdf)

    # features
    fdf = fdfEdit(
        x = char$Features %>% stringr::str_replace_all('•|\u{2022}','-'),
        field = 'features-and-traits',
        fdf = fdf)

    # class level
    fdf = fdfEdit(
        x = char$ClassField,
        field = 'class-level',
        fdf = fdf)

    # background
    fdf = fdfEdit(
        x = char$Background,
        field = 'background',
        fdf = fdf)

    # alignment
    fdf = fdfEdit(
        x = char$Alignment,
        field = 'alignment',
        fdf = fdf)

    # race
    fdf = fdfEdit(
        x = char$Race,
        field = 'race',
        fdf = fdf)

    # name
    fdf = fdfEdit(
        x = char$Name,
        field = 'character-name',
        fdf = fdf)
    fdf = fdfEdit(
        x = char$Name,
        field = 'character-name-2',
        fdf = fdf)

    # spells
    DC = spellDC(char = char)
    spellAttack  =  spellAttack(char = char)
    fdf = fdfEdit(
        x = DC,
        field = 'spell-save-dc-1',
        fdf = fdf)

    fdf = fdfEdit(
        x = spellAttack,
        field = 'spell-attack-bonus-1',
        fdf = fdf)

    fdf = fdfEdit(
        x = names(spellAttack),
        field = 'spellcasting-ability-1',
        fdf = fdf)

    if(!is.null(char$spells)){
        for(i in 1:9){
            fdf = fdfEdit(
                x = char$spellSlots[i+1],
                field = glue('spell-slots-{i}-1'),
                fdf = fdf)
        }


        for(i in 0:9){
            spells  = char$spells %>% filter(level ==i)
            for(t in seq_len(nrow(spells))){
                fdf = fdfEdit(
                    x = spells$name[t],
                    field = glue('spells-{i}-{t}-1'),
                    fdf = fdf)
            }
        }


    }




    # equipment
    fdf = fdfEdit(
        x = char$Equipment,
        field = 'equipment',
        fdf = fdf)

    # gold
    for(x in names(char$currency)){
        fdf = fdfEdit(
            x = char$currency[[x]],
            field =tolower(x),
            fdf = fdf)
    }

    # language and proficiency
    profAndLang = paste0('Armor Prof.\n',char$ArmorProficiencies,'\n',
                         'Weapon Prof.\n',char$WeaponProficiencies,'\n',
                         'Tool Prof.\n',char$ToolProficiencies,'\n',
                         'Languages\n',char$LanguagesKnown)

    # weapons
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
        fdf = fdfEdit(
            x = paste0(weaponTable[i,1],' (',weaponTable[i,5],')'),
            field =paste0('weapon-name-',i),
            fdf = fdf)
        fdf = fdfEdit(
            x = weaponTable[i,2],
            field =paste0('weapon-attack-bonus-',i),
            fdf = fdf)

        fdf = fdfEdit(
            x = paste0(weaponTable[i,3],'/',weaponTable[i,4]),
            field =paste0('weapon-damage-',i),
            fdf = fdf)
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
    fdf = fdfEdit(
        x = moreWeapons,
        field ='attacks-and-spellcasting',
        fdf = fdf)


    # skills
    skillBonuses = skillBonus(char = char)
    pdfNames = skillBonuses %>% names %>% tolower %>% stringr::str_replace_all(' ','-')
    pdfCheckbox= paste0(pdfNames,'-check')
    for(i in 1:length(skillBonuses)){
        fdf = fdfEdit(
            x = skillBonuses[i],
            field =pdfNames[i],
            fdf = fdf)
        fdf = fdfEdit(
            x = char$skillProf[i],
            field =pdfCheckbox[i],
            fdf = fdf)
    }

    # passive perception
    fdf = fdfEdit(
        x = 10 + skillBonuses['Perception'],
        field = 'passive',
        fdf = fdf)


    # prof bonus
    fdf = fdfEdit(
        x = char$proficiencyBonus,
        field = 'prof-bonus',
        fdf = fdf)


    fdf = fdfEdit(
        x = profAndLang,
        field ='other-profs',
        fdf = fdf)

    newFDF = tempfile()
    write(fdf,newFDF)
    animation::pdftk(sourcePDF,
                     operation = paste0('fill_form ',newFDF),
                     output = file)



}
