

library(shiny)

shinyServer(function(input, output,session) {
    char = do.call(reactiveValues,char)
    consoleOut = reactiveVal(rep('\n',consoleLength) %>% paste(collapse = ''))


    rolls = reactiveVal(value = '', label = 'rollLog')

    characterDescription = callModule(characterDescription,'character',
                                      char = char)

    healthModule = callModule(health,'health',
                              char = char)

    attributeModule = callModule(attributes,'attributes', char = char)

    weaponModule = callModule(weapons,'weapons',char = char)


    output$console = renderText({
        print(input$`attributes-save_button`)

        out = ''

        if(!is.null(input$`attributes-save_button`)){
            out  = glue('{input$`attributes-save_button`} save:\n',
                        capture.output(save(input$`attributes-save_button`,
                                            char = char)) %>%
                            gsub('(\\[1\\] )|"','',.) %>%
                            paste(collapse = '\n'))
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  'attributes-save_button')
            # consoleOut(out)
        }

        if(!is.null(input$`attributes-check_button`)){
            out = glue('{input$`attributes-check_button`} check:\n',
                       capture.output(abilityCheck(input$`attributes-check_button`,
                                                   char = char)) %>%
                           gsub('(\\[1\\] )|"','',.) %>%
                           paste(collapse = '\n'))
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  'attributes-check_button')

            # consoleOut(out)
        }

        if(!is.null(input$`weapons-weaponButton`)){
            w = char$weapons
            out = capture.output(weaponAttack(w[[input$`weapons-weaponButton`]])) %>%
                gsub('(\\[1\\] )|"','',.) %>%
                paste(collapse = '\n')

            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  'weapons-weaponButton')
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
