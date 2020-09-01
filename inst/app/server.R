


shinyServer(function(input, output,session) {
    # close the R session when Chrome closes
    session$onSessionEnded(function() {
        print('main session ended')
        if(!is.null(getOption('ImThePortableClient')) |
           !is.null(getOption('AutoCloseCharSheet'))){
            stopApp()
            q("no")
        }

    })


    charInitial = do.call(reactiveValues,char) # used to detect when character changes

    char = do.call(reactiveValues,char)

    callModule(packageVersion,
               'pVersion')

    consoleOut = reactiveVal(rep('\n',consoleLength) %>% paste(collapse = ''))


    rolls = reactiveVal(value = '', label = 'rollLog')

    characterDescription = callModule(characterDescription,'character',
                                      char = char,
                                      charInitial = charInitial)

    healthModule = callModule(health,'health',
                              char = char,
                              charInitial = charInitial)

    attributeModule = callModule(attributes,'attributes', char = char)

    weaponModule = callModule(weapons,'weapons',char = char)

    skillModule = callModule(skills,'skills',char = char)

    resourceModule = callModule(resources,'resources',char = char)

    spellsModule = callModule(spells,'spells',char=char)

    diceModule = callModule(diceRoller,'diceRoller')
    diceModule2 = callModule(diceRoller,'diceRoller2')
    diceModule3 = callModule(diceRoller,'diceRoller3')
    diceModule4 = callModule(diceRoller,'diceRoller4')

    choicesModule = callModule(choices,'choices', char = char)

    petModule = callModule(pet,'pet', char = char)

    callModule(roll20,'roll20',
               attributeModule,
               weaponModule,
               skillModule,
               resourceModule,
               diceModule,
               diceModule2,
               diceModule3,
               diceModule4,
               characterDescription,
               healthModule,
               spellsModule,
               petModule)

    callModule(console,'console',
               consoleLength = consoleLength,
               attributeModule,
               weaponModule,
               skillModule,
               resourceModule,
               diceModule,
               diceModule2,
               diceModule3,
               diceModule4,
               characterDescription,
               healthModule,
               spellsModule,
               petModule)



    observe({
        query = parseQueryString(session$clientData$url_search)
        if(!is.null(query$valid) && query$valid==TRUE){
            showModal(
                modalDialog(title = "We are moving!",
                            p("This app is moving. You can now access it from ",
                              a(href="https://oganm.com/shiny/interactiveSheet",target= '_blank', 'oganm.com/shiny/interactiveSheet'),'.'),
                            p("This link will continue to work but you will get this annoying message every time."),
                            easyClose = FALSE)
            )
        }
    }
    )

})
