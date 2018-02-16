

library(shiny)




shinyUI(
    fluidPage(
        useShinyjs(),
        includeCSS('www/style.css'),
        theme = shinythemes::shinytheme('cosmo'),
        characterDescriptionUI(id = 'character'),
        fluidRow(
            column(3,attributesUI(id = 'attributes'),
                   skillsUI(id = 'skills')),
            column(4,healthUI(id = 'health'),
                   hr(),
                   tabsetPanel(id = 'tabs',
                               tabPanel('Weapons',
                                        weaponsUI(id = 'weapons')),
                               tabPanel('Spells'))),
            column(5,
                   wellPanel(verbatimTextOutput('console',placeholder = TRUE)),
                   resourcesUI('resources')))
        )
    )
