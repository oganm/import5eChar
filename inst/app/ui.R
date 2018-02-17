

shinyUI(
    fluidPage(
        useShinyjs(),
        includeCSS('www/style.css'),
        theme = shinythemes::shinytheme('cosmo'),
        characterDescriptionUI(id = 'character'),
        fluidRow(
            column(3,attributesUI(id = 'attributes'),
                   skillsUI(id = 'skills')),
            column(5,healthUI(id = 'health'),
                   hr(),
                   tabsetPanel(id = 'tabs',
                               tabPanel('Weapons',
                                        weaponsUI(id = 'weapons')),
                               tabPanel('Spells',
                                        spellsUI(id = 'spells')))),
            column(3,
                   wellPanel(verbatimTextOutput('console',placeholder = TRUE)),
                   resourcesUI('resources'))),
        fluidRow(
            wellPanel(p('Icon credits:',
                        paste0(paste(iconCredits,collapse =', '),'.')))
            )
        )
    )
