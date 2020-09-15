
skillsUI = function(id){
    ns = NS(id)
    tagList(
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
});"),
        dataTableOutput(ns('skillsTable'))
    )
}

skills = function(input, output,session,char){
    output$skillsTable = renderDataTable({


        skillBonus = skillBonus(char = char)
        profMark = char$skillProf
        statSep = char$skillAttributes %>% duplicated() %>% not %>% which %>% {.[-1] -1}
        skillAttributes = unique(char$skillAttributes)

        prof = char$proficiencyBonus
        attributeRows = data.frame(skillName = skillAttributes %>% map(strong) %>% map_chr(as.character),
                                   profMark='',
                                   skillBonus = '',
                                   index = c(0,statSep+.1),
                                   buttons = skillAttributes %>% map_chr(function(x){
                                       base = char$abilityMods[x]

                                       tagList(
                                           actionButton(label = base+prof, inputId = x,
                                                        onclick = glue('Shiny.onInputChange("',session$ns('proficientSkill'),'",  this.id)'),
                                                        class = 'skillButton')
                                       ) %>% as.character
                                   }),
                                   stringsAsFactors = FALSE)
        charAts = data.frame(skillName = names(skillBonus),
                             profMark,
                             skillBonus,
                             index = 1:length(skillBonus),stringsAsFactors = FALSE)


        charAts$buttons = charAts$skillName %>% sapply(function(x){
            actionButton(label = charAts$skillBonus[charAts$skillName == x],
                         icon = switch(charAts$profMark[charAts$skillName == x] %>% as.character(),
                                       'TRUE' = icon('check-square'),
                                       'FALSE' = icon('square')),
                         inputId=x,
                         onclick = glue('Shiny.onInputChange("',session$ns('skillButton'),'",  this.id)'),
                         class = 'skillButton')%>%
                as.character
        })

        charAts %<>% rbind(attributeRows) %>% arrange(index) %>% select(skillName,buttons)


        charAts1 = charAts[1:12,]
        charAts2 = rbind(charAts[13:nrow(charAts),],data.frame(skillName = '',
                                                               buttons = ''))

        charAts = cbind(charAts1,charAts2)

        table = datatable(charAts,escape = FALSE,selection = 'none',rownames = FALSE,
                          colnames= rep('',4),
                          options = list(bFilter = 0,
                                         bLengthChange = 0,
                                         paging = 0,
                                         ordering = 0,
                                         bInfo = 0,
                                         pageLength = nrow(charAts)))
        return(table)

    })


    out = reactive({
        out = ''

        if(!is.null(input$skillButton)){

            out = glue(
                input$skillButton,' check:\n',
                capture.output(skillCheck(input$skillButton,char = char)) %>%
                    gsub('(\\[1\\] )|"','',.) %>%
                    paste(collapse = '\n'))

            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('skillButton'))
        }



        if(!is.null(input$proficientSkill)){
            out = glue(
                'Proficient ',input$proficientSkill,' check:\n',
                unname(roll('1d20')+ char$proficiencyBonus+char$abilityMods[input$proficientSkill])
            )
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('proficientSkill'))
        }

        return(out)
    })


    return(out)

}
