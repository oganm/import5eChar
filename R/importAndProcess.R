#' Import character from google drive
#'
#' @description Provided a regex or fileID this will import the character from your google drive so it can be used in utility functions. Will ask for
#' googledrive authentication on first load. If you are using Rstudio server, you can authenticate using another machine and move the generated
#' .httr-oauth file to your working directory.
#'
#' @param regex A regular expression that matches the file name. If there are multiple matches, the most recently edited file will the chosen
#' @param fileID A googledrive file ID.
#' @param output if provided, the file will be saved here. if not, a temporary file will be used
#' @param overwrite If TRUE the new file will overwrite the old one. Not important if output isn't provided as the file will be deleted eventually
#' @param ... Variables to pass to googledrive::drive_find (if regex is given) or googledrive::drive_get (if fileID is given).
#' @return The output is a nested list extracted from the original XML file, edited to make certain fields more computer readable.
#'
#' @export
#'
importCharacter = function(regex=NULL, fileID = NULL, output=NULL,overwrite=TRUE,...){
    if(is.null(regex) & is.null(fileID)){
        error('Either regex or fileID should be provided')
    }
    else if(!is.null(regex) & !is.null(fileID)){
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

    char = paste(readLines(output),collapse = '\n')

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

    skillData = char$skillInfo %>% strsplit('⊠|(â\u008a.)') %>% {.[[1]]} %>% trimws()

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



    weapons = char$weaponList %>% strsplit('⊠|(â\u008a.)')  %>% .[[1]]
    weapons = weapons[-1]
    numOfWeapons = (length(weapons))/13

    splitPoints = 1:numOfWeapons*13+1

    weapons %<>% splitAt(splitPoints)
    weapons %<>% lapply(function(x){
        name = x[1]
        range = x[2]
        dice = paste0(x[12],'d',x[13])
        return(c(name = name,
                 range = range,
                 dice=  dice))
    })

    names(weapons) = weapons %>% purrr::map_chr('name')

    char$weapons = weapons

    return(char)
}
