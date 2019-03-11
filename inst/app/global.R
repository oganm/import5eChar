library(shinyWidgets)
library(shiny)
library(shinyjs)
library(diceSyntax)
library(glue)
library(import5eChar)
library(DT)
library(magrittr)
library(purrr)
library(dplyr)
library(shinyBS)
library(jsonlite)
library(shinythemes)
library(stringr)

spellData = tryCatch(fromJSON(readLines('https://raw.githubusercontent.com/oganm/wizaRd/master/data-raw/spelldice.json') %>% paste(collapse = '\n')),
                     error = function(e){
                         spellData = list()
                     })

spellDetails = tryCatch(fromJSON(readLines('https://raw.githubusercontent.com/oganm/wizaRd/master/data-raw/spellDetails.json') %>% paste(collapse = '\n')),
                        error = function(e){
                            spellDetails = list()
                        })
# spellData = wizaRd::spells
source('modules/healthModule.R')
source('modules/spellsModule.R')
source('modules/resourceModule.R')
source('modules/choicesModule.R')
source('modules/characterDescriptionModule.R')
source('modules/attributesModule.R')
source('modules/weaponsModule.R')
source('modules/skillsModule.R')
source('modules/diceRollerModule.R')
source('modules/packageVersionModule.R')

consoleLength  = 15


if(!exists('char')){
    files = list.files()
    timFile = files[grepl('Tim',files)]
    if(length(timFile)==1){
        char = import5eChar::importCharacter(file = timFile)
    } else{
        characterFile <- system.file("JimTheCommoner_Rogue1", package = "import5eChar")
        #characterFile = 'chars/_137.82.157.147_c51e88ade00d9508710bbbc476b6061b'
        # characterFile = 'inst/app/chars/_173.244.48.74_3a531df0147f56052e4d58b7cb15ebfa'
        char = import5eChar::importCharacter(file = characterFile)
    }
}


if(Sys.info()['nodename'] == 'oganm'){
    options(ImTheWebClient= TRUE)
}

iconCredits = c('Lorc',
                'Delapouite',
                'Carl Olsen',
                'Skoll')
if(!exists('.sheetApp.spellSource')){
    .sheetApp.spellSource = 'https://www.dndbeyond.com/spells/'
    # .sheetApp.spellSource = 'https://thebombzen.com/grimoire/spells/'
}
