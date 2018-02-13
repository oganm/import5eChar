

library(shiny)

shinyServer(function(input, output,session) {
    char = do.call(reactiveValues,char2)
    consoleOut = reactiveVal('')


    rolls = reactiveVal(value = '', label = 'rollLog')

    characterDescription = callModule(characterDescription,'character',
                                      char = char)

    healthModule = callModule(health,'health',
                              char = char)

    attributeModule = callModule(attributes,'attributes', char = char)


    output$console = renderText({
        print(input$`attributes-save_button`)

        if(!is.null(input$`attributes-save_button`)){
            out  = glue('{input$`attributes-save_button`} save:\n',
                           '{save(stat = input$`attributes-save_button`,char = char)}')
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  'attributes-save_button')
            consoleOut(out)
        }

        if(!is.null(input$`attributes-check_button`)){
           out = glue('{input$`attributes-check_button`} save:\n',
                      '{abilityCheck(input$`attributes-check_button`, char = char)}')
           session$sendCustomMessage(type = 'resetInputValue',
                                     message =  'attributes-check_button')

           consoleOut(out)
        }

        return(consoleOut())

    })

})
