attributesUI = function(id){
    ns = NS(id)
    tagList(
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
});"),
        dataTableOutput(ns('attributesTable'))
    )
}

attributes = function(input, output, session, char){
    output$attributesTable = renderDataTable({
        saveButtons = sapply(1:length(char$abilityScores),function(i){
            actionButton(label = saveBonus(char)[i],
                         inputId=names(char$abilityScores)[i],
                         icon =switch(char$abilityProf[i] %>% as.character(),
                                      'TRUE' = icon('check-square'),
                                      'FALSE' = icon('square')),
                         onclick = glue('Shiny.onInputChange("',session$ns('save_button'),'",  this.id)'),
                         class = 'attributeButton')%>%
                as.character
        })

        checkButtons=  sapply(1:length(char$abilityScores),function(i){
            actionButton(label = char$abilityMods[i],
                         inputId=names(char$abilityScores)[i],
                         onclick = glue('Shiny.onInputChange("',session$ns('check_button'),'",  this.id)'),
                         class = 'attributeButton')%>%
                as.character
        })


        table = data.frame(Score = char$abilityScores,
                           Mod =char$abilityMods,
                           Saves =saveButtons,
                           Checks = checkButtons)

        table = datatable(table,escape = FALSE,selection = 'none',
                          options = list(bFilter = 0,
                                         bLengthChange = 0,
                                         paging = 0,
                                         ordering = 0,
                                         bInfo = 0))
        return(table)
    })



    out = reactive({
        out = ''
        if(!is.null(input$save_button)){
            out  = glue('{input$save_button} save:\n',
                        capture.output(save(input$save_button,
                                            char = char)) %>%
                            gsub('(\\[1\\] )|"','',.) %>%
                            paste(collapse = '\n'))
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('save_button'))
            # consoleOut(out)
        }

        if(!is.null(input$check_button)){
            out = glue('{input$check_button} check:\n',
                       capture.output(abilityCheck(input$check_button,
                                                   char = char)) %>%
                           gsub('(\\[1\\] )|"','',.) %>%
                           paste(collapse = '\n'))
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('check_button'))

            # consoleOut(out)
        }

        return(out)
    })


    return(out)

    # observe({
    #     print('module')
    #     print(input$save_button)
    #     session$sendCustomMessage(type = 'resetInputValue',
    #                               message =  session$ns('save_button'))
    # })
}
