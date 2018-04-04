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
source('modules.R')

consoleLength  = 15


if(!exists('char')){
    characterFile <- system.file("JimTheCommoner_Rogue1", package = "import5eChar")
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

