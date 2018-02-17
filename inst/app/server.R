


shinyServer(function(input, output,session) {
    # close the R session when Chrome closes
    session$onSessionEnded(function() {
        if(!is.null(getOption('ImThePortableClient')) |
           !is.null(getOption('AutoClosCharSheet'))){
            stopApp()
            q("no")
        }

    })


    charInitial = do.call(reactiveValues,char) # used to detect when character changes

    char = do.call(reactiveValues,char)


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

    output$console = renderText({
        out = ''

        if(out == ''){
            out = attributeModule()
        }
        if(out ==''){
            out = weaponModule()
        }
        if(out == ''){
            out = skillModule()
        }
        if(out == ''){
            out = resourceModule()
        }
        if(out == ''){
            out = diceModule()
        }


        isolate({

            # console memory
            if(out != ''){
                out %<>% strsplit('\n') %>% {.[[1]]} # kinda not necesarry
                oldConsole = consoleOut() %>% strsplit(split = '\n') %>% {.[[1]]}
                newConsole = c(oldConsole[(length(out)+1):consoleLength],out)
                console = newConsole %>% paste(collapse='\n')
                consoleOut(console)
            }

            return(consoleOut())
        })

    })

})
