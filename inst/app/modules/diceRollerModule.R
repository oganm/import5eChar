
diceRollerUI = function(id,label = 'Roll dice'){
    ns = NS(id)
    tagList(
        tags$script("Shiny.addCustomMessageHandler('inputZero', function(variableName){
                    Shiny.onInputChange(variableName, 0);
});"),
        div(textInput(ns('diceText'),placeholder = 'eg. 4d6k3',label = label),style = 'display: inline-block;width: 30%'),
        actionButton(ns('diceRoll'),
                     label = div(img(src = 'icons/dice-twenty-faces-twenty.png',
                                     height = 20,
                                     width = 20),'Roll!'), style = "display: inline-block")

    )
}

diceRoller = function(input,output,session){
    out = reactive({
        if(input$diceRoll>0){
            isolate({
                out = tryCatch(capture.output(roll(input$diceText)) %>% paste(collapse='\n'),
                               error = function(e){''})

                session$sendCustomMessage(type = 'inputZero',
                                          message =  session$ns('diceRoll'))
            })
        } else {
            out = ''
        }
        return(out)
    })

    return(out)
}
