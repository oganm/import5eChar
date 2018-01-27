

library(shiny)

shinyServer(function(input, output) {
    char = reactiveValues(maxHealth = char$maxHealth,
                          currentHealth = char$currentHealth,
                          currentTempHP = char$currentTempHP,
                          hitDice = char$hitDice,
                          hitDiceRemain = char$hitDiceRemain,

                          Name =char$Name,
                          ClassField = char$ClassField,
                          Background = char$Background,
                          Race = char$Race,
                          Alignment = char$Alignment)

    characterDescription = callModule(characterDescription,'character',
                                      char = char)

    healthModule = callModule(health,'health',
                              char = char)

    attributeModule = callModule(attributes,'attributes', char = char)

})
