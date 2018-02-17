

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
                                        spellsUI(id = 'spells'))),
                   diceRollerUI(id = 'diceRoller')),
            column(3,
                   wellPanel(verbatimTextOutput('console',placeholder = TRUE)),
                   resourcesUI('resources'))),
        fluidRow(
            hr(),
            fluidRow(column(2),
                     column(2,
                            p('Developed by',a(href = 'https://github.com/oganm', target="_blank",'Ogan Mancarci'),
                              style = 'text-align:center'),
                            p('Source code', a(href = 'https://github.com/oganm/import5eChar/tree/master/inst/app', target="_blank",'here'),
                              style = 'text-align:center')),
                     column(3,
                            p(a(href = 'https://play.google.com/store/apps/details?id=com.wgkammerer.testgui.basiccharactersheet.app&hl=en', target="_blank",'Fifth Edition Character Sheet'),
                              'is created by Walter Kammerer'),style = 'text-align:center'),
                     column(3,
                            p('Icon credits:',
                              paste(iconCredits,collapse =', '),
                              'from',a(href = 'http://game-icons.net/', target="_blank",'game-icons.net'),
                              style = 'text-align:center'),
                            p('Released under', a(href = 'https://creativecommons.org/licenses/by/3.0/', target="_blank",'CC BY 3.0'),
                              style = 'text-align:center')),
                     column(2))

            )
        )
    )
