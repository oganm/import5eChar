

library(shiny)

shinyServer(function(input, output) {
    char = do.call(reactiveValues,char)


    rolls = reactiveVal(value = '', label = 'rollLog')

    characterDescription = callModule(characterDescription,'character',
                                      char = char)

    healthModule = callModule(health,'health',
                              char = char)

    attributeModule = callModule(attributes,'attributes', char = char)

    observe({
        observe({
            print('server')
            print(input$`attributes-save_button`)
        })
    })

})
