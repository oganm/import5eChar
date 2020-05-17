

shinyUI(
    fluidPage(
        tags$head(
            tags$link(
                rel = "icon",
                type = "image/x-icon",
                href = "http://localhost:1984/default.ico")
        ),
        useShinyjs(),
        setSliderColor('#8B1A1A',1),
        chooseSliderSkin(skin = 'Square'),
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
                   bsCollapse(open = 'Pet',
                       bsCollapsePanel('Pet',
                                       petUI(id = 'pet'))
                   )
                   ),
            column(3,
                   consoleUI("console"),
                   resourcesUI('resources'),
                   br(),
                   choicesUI('choices'),
                   fluidRow(diceRollerUI(id = 'diceRoller'),
                            br(),
                            diceRollerUI(id = 'diceRoller2', label = ''),
                            br(),
                            diceRollerUI(id = 'diceRoller3', label = ''),
                            br(),
                            diceRollerUI(id = 'diceRoller4', label = '')),
                   fluidRow(
                                span(
                            actionButton('help',label = 'Help!',style = 'margin-top: 28px',
                                         onclick =
                                                        "window.open('https://github.com/oganm/import5eChar/blob/master/interactiveSheetDocumentation.md', '_blank')"),
                                       actionButton('meh','Donate',
                                                    icon = icon('gift'),
                                                    onclick =
                                                        "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=NBC57LQVGMAJG', '_blank')",
                                                    style = 'margin-top: 28px;'),
                                       bsTooltip('meh',title = readLines('http://oganm.com/donation.txt')),
                                       style = 'float:right'))
                   )
            ),
        fluidRow(
            hr(),
            fluidRow(column(1),
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
                     column(1),
                     column(1,packageVersionUI('pVersion'))
                     )

            )
        )
    )
