

library(shiny)

shinyServer(function(input, output,session) {
    char = do.call(reactiveValues,char2)
    consoleOut = reactiveVal(rep('\n',consoleLength) %>% paste(collapse = ''))


    rolls = reactiveVal(value = '', label = 'rollLog')

    characterDescription = callModule(characterDescription,'character',
                                      char = char)

    healthModule = callModule(health,'health',
                              char = char)

    attributeModule = callModule(attributes,'attributes', char = char)


    output$console = renderText({
        print(input$`attributes-save_button`)

        out = ''

        if(!is.null(input$`attributes-save_button`)){
            out  = glue('{input$`attributes-save_button`} save:\n',
                           '{save(stat = input$`attributes-save_button`,char = char)}')
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  'attributes-save_button')
            # consoleOut(out)
        }

        if(!is.null(input$`attributes-check_button`)){
           out = glue('{input$`attributes-check_button`} check:\n',
                      '{abilityCheck(input$`attributes-check_button`, char = char)}')
           session$sendCustomMessage(type = 'resetInputValue',
                                     message =  'attributes-check_button')

           # consoleOut(out)
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
