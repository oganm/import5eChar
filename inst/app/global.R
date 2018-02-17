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
source('modules.R')

consoleLength  = 15


if(!exists('char')){
    characterFile <- system.file("JimTheCommoner_Fighter1", package = "import5eChar")
    char = import5eChar::importCharacter(file = characterFile)

}


iconCredits = c('Lorc',
                'Delapouite',
                'Carl Olsen',
                'Skoll')

spellSource = 'https://www.dndbeyond.com/spells/'
# spellSource = 'https://thebombzen.com/grimoire/spells/'
