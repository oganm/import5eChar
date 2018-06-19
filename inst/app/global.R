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

spellData = tryCatch(fromJSON(readLines('https://pastebin.com/raw/2LqVRve5') %>% paste(collapse = '\n')),
                     error = function(e){
                         spellData = list()
                     })
# spellData = wizaRd::spells
source('modules.R')
source('modules/healthModule.R')
source('modules/spellsModule.R')
source('modules/resourceModule.R')
source('modules/choicesModule.R')

consoleLength  = 15


if(!exists('char')){
    characterFile <- system.file("JimTheCommoner_Rogue1", package = "import5eChar")
    #characterFile = 'chars/_137.82.157.147_c51e88ade00d9508710bbbc476b6061b'
    # characterFile = 'inst/app/chars/_173.244.48.74_3a531df0147f56052e4d58b7cb15ebfa'
    char = import5eChar::importCharacter(file = characterFile)

}


if(Sys.info()['nodename'] == 'oganmDO'){
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
