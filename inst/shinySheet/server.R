

library(shiny)

shinyServer(function(input, output) {


    healthModule = callModule(health,'health')

})
