characterDescriptionUI = function(id){
    ns = NS(id)
    tagList(htmlOutput(ns('description')))
}

characterDescription = function(input,output,session,char){
    output$description = renderUI({
        tagList(
            fluidRow(
                column(4,
                       wellPanel(h2(char$Name))),
                column(8,
                       wellPanel(
                           fluidRow(
                               column(6, p(char$ClassField, class ='narrowElement field'),
                                      hr(class = 'narrowElement'),
                                      p('Class & Level', class = 'narrowElement minorText')),
                               column(6,p(char$Background, class ='narrowElement field'),
                                      hr(class = 'narrowElement'),
                                      p('Background', class = 'narrowElement minorText'))
                           ),
                           fluidRow(
                               column(6,p(char$Race, class ='narrowElement field'),
                                      hr(class = 'narrowElement'),
                                      p('Race',class='narrowElement minorText')),
                               column(6,p(char$Alignment, class ='narrowElement field'),
                                      hr(class = 'narrowElement'),
                                      p('Alignment',class='narrowElement minorText'))
                           )
                       ))
            )
        )
    })
}

healthUI = function(id){
    ns = NS(id)
    tagList(
        fixedRow(column(11,
                        progressBar(id = ns("healthBar"), value = 1,
                                    total = 1)),
                 column(1,
                        dropdownButton(
                            numericInput(inputId = ns('maxHealth'),
                                         label = 'Max health',
                                         value = -1),
                            numericInput(inputId = ns('currentHealth'),
                                         label = 'Current health',
                                         value = -1),
                            numericInput(inputId = ns('tempHealth'),
                                         label = 'Temp health',
                                         value = -1),
                            circle = TRUE,
                            icon = icon("bars"),
                            size=  'xs',
                            width = '30px',
                            right = TRUE))),
        div(
            numericInput(inputId = ns('increment'),
                         label = '', value = 1,step =1, min = 1,
                         width = '70px'),style="display:inline-block"),
        actionButton(ns('minusHealth'),'-', class = 'modButton'),
        actionButton(ns('plusHealth'),'+',class = 'modButton'),
        actionButton(ns('plusTempHealth'),'+ 0',
                     class = 'modButton',style = 'background-color:#6959CD')

    )
}

health = function(input, output, session,
                  char){
    # init = reactiveVal(FALSE)

    observe({
        if(input$maxHealth == -1){
            # init(TRUE)
            updateNumericInput(session,inputId = 'maxHealth',value = char$maxHealth,min= 1)
            updateNumericInput(session,inputId = 'currentHealth',value = char$currentHealth,min= 0, max = char$maxHealth)
            updateNumericInput(session,inputId = 'tempHealth',value = char$currentTempHP,min= 0)

        }
    })

    observe({
        if(input$maxHealth != -1){
            char$maxHealth = input$maxHealth
            char$currentHealth = input$currentHealth
            char$currentTempHP = input$tempHealth

            isolate({
                if(char$currentHealth > char$maxHealth){
                    char$currentHealth = char$maxHealth
                }
            })
        }
    })


    observeEvent(input$minusHealth,{

        healthDamage = min(0,char$currentTempHP - input$increment)

        char$currentTempHP = max(0,char$currentTempHP - input$increment)

        char$currentHealth =   max(0,char$currentHealth + healthDamage)


    })

    observeEvent(input$plusHealth,{
        char$currentHealth = min(char$maxHealth,char$currentHealth + input$increment)

    })

    observeEvent(input$plusTempHealth,{
        char$currentTempHP = char$currentTempHP + input$increment

    })

    observe({
        if(char$currentTempHP>0){
            status = 'primary'
        } else{
            status = 'danger'
        }

        updateActionButton(session,
                           'plusTempHealth',
                           label = glue::glue('+ {char$currentTempHP}'))

        updateNumericInput(session,inputId = 'currentHealth',value = char$currentHealth,min= 0, max = char$maxHealth)
        updateNumericInput(session,inputId = 'tempHealth',value = char$currentTempHP,min= 0)

        updateProgressBar(session,
                          id = session$ns('healthBar'),
                          value =  char$currentHealth + char$currentTempHP,
                          total = char$maxHealth + char$currentTempHP,
                          status= status)
    })

}

attributesUI = function(id){
    ns = NS(id)
    tagList(

        dataTableOutput(ns('attributesTable'))
        # attributeUI(ns('Str')),
        # attributeUI(ns('Dex')),
        # attributeUI(ns('Con')),
        # attributeUI(ns('Int')),
        # attributeUI(ns('Wis')),
        # attributeUI(ns('Cha'))
    )
}

attributes = function(input, output, session, char){
    output$attributesTable = renderDataTable({
        shinyInput <- function(FUN, len, id, ...) {
            inputs <- character(len)
            for (i in seq_len(len)) {
                inputs[i] <- as.character(FUN(paste0(id, i), ...))
            }
            inputs
        }

        saveButtons = sapply(1:length(char$abilityScores),function(i){
            actionButton(label = saveBonus(char)[i],
                         inputId=session$ns(paste0('save', names(char$abilityScores)[i])),
                         icon =switch(char$abilityProf[i] %>% as.character(),
                                      'TRUE' = icon('check-square'),
                                      'FALSE' = icon('square')),
                         onclick = glue('Shiny.onInputChange("',session$ns('save_button'),'",  this.id)'))%>%
                as.character
        })


        table = data.frame(Score = char$abilityScores,
                           Mod =char$abilityMods,
                           Saves =saveButtons)

        table = datatable(table,escape = FALSE,selection = 'none',
                          options = list(bFilter = 0,
                                         bLengthChange = 0,
                                         paging = 0,
                                         ordering = 0))
        return(table)
    })


    observe({
        print('module')
        print(input$save_button)
    })
    # strModule = callModule(attribute,'Str',char = char, attribute ='Str')
    # dexModule = callModule(attribute,'Dex',char = char, attribute ='Dex')
    # conModule = callModule(attribute,'Con',char = char, attribute ='Con')
    # intModule = callModule(attribute,'Int',char = char, attribute ='Int')
    # wisModule = callModule(attribute,'Wis',char = char, attribute ='Wis')
    # chaModule = callModule(attribute,'Cha',char = char, attribute ='Cha')
}



attributeUI = function(id){
    ns = NS(id)
    tagList(
        htmlOutput(ns('name'))
    )
}

attribute = function(input,output,session,char,attribute){

    output$name = renderUI({
        return(tagList(
            strong(attribute)
        ))
    })
}
