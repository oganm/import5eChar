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
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
                    });"),
        dataTableOutput(ns('attributesTable'))
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


    # observe({
    #     print('module')
    #     print(input$save_button)
    #     session$sendCustomMessage(type = 'resetInputValue',
    #                               message =  session$ns('save_button'))
    # })
}

weaponsUI = function(id){
    ns = NS(id)
    tagList(
        # not strictly necesary as the function is already defined. before. keeping
        # it to ensure it'll work on its own
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
                    });"),
        fluidRow(column(6,radioGroupButtons(inputId = ns('advantage'),
                                            choices = c('DisAdv','Norm','Adv'),
                                            selected = 'Norm',
                                            status = "primary")),
                 column(6,switchInput(ns('sharpshoot'),label = 'SharpS/GreatWM'))),
        dataTableOutput(ns('weaponsTable'))
    )
}

weapons =function(input, output,session,char){
    output$weaponsTable = renderDataTable({
        weaponTable = char$weapons %>% sapply(function(x){

            c(actionButton(label = x$name,
                           inputId=x$name,
                           onclick = glue('Shiny.onInputChange("',session$ns('weaponButton'),'",  this.id)'),
                           class = 'weaponButton')%>%
                  as.character,
              weaponBonus(x,char=char)['weaponTypeAttackBonus'] +
                  x$proficient*char$proficiencyBonus +
                  char$abilityMods[x$attackStat],
              paste0(paste(x$dice,collapse=' + '),
                     '+',
                     weaponBonus(x,char=char)['weaponTypeDamageBonus'] +
                         char$abilityMods[x$attackStat]),
              x$damageType,
              x$range
            )
        }) %>% t %>% as.data.frame()
        names(weaponTable) = c('name','Atk Bonus','Damage','Type','Range')


        table = datatable(weaponTable,escape = FALSE,selection = 'none',rownames = FALSE,
                          options = list(bFilter = 0,
                                         bLengthChange = 0,
                                         paging = 0,
                                         ordering = 0,
                                         bInfo = 0))
        return(table)
    })


    weaponOut = reactive({
        list(advantage = input$advantage,
             sharpshoot = input$sharpshoot)
    })

    return(weaponOut)
}
