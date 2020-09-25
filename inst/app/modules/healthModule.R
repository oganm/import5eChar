
healthUI = function(id){
    ns = NS(id)
    tagList(
        fixedRow(column(11,
                        htmlOutput(ns('HPSliderColorController')),
                        div(class = ns('HPsliderDiv'),
                            sliderInput(inputId = ns('healthSlider'), value = 1, label = 'HP',min = 0,max = 1,step = 1, width = '100%')
                        )#,
                        # progressBar(id = ns("healthBar"), value = 1,
                        #             total = 1)
        ),
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
                   right = FALSE,
                   inputId = ns('healthDropdown')))),
        div(
            numericInput(inputId = ns('increment'),
                         label = '', value = 1,step =1, min = 1,
                         width = '70px'),style="display:inline-block"),
        actionButton(ns('minusHealth'),'-', class = 'modButton'),
        actionButton(ns('plusHealth'),'+',class = 'modButton'),
        actionButton(ns('plusTempHealth'),'+ 0',
                     class = 'modButton',style = 'background-color:#6959CD'),
        htmlOutput(ns('hitDiceSection'),inline = TRUE),
        bsTooltip(ns('minusHealth'),'Damage Health',placement = 'bottom'),
        bsTooltip(ns('plusHealth'),'Heal',placement = 'bottom'),
        bsTooltip(ns('plusTempHealth'),'Add temp. HP',placement = 'bottom'),
        bsTooltip(ns('increment'),'HP to change',placement = 'bottom')

    )
}

health = function(input, output, session,
                  char,charInitial){
    init = reactiveVal(FALSE)
    out = reactiveVal('') # used for hit dice


    # creating hit dice buttons
    output$hitDiceSection = renderUI({
        if(is.null(char$hitDice)){
            return(NULL)
        }
        tagList(
            HTML('<strong>&emsp;&emsp;&emsp;&emsp;HD: </strong>'),
            lapply(char$hitDiceRemain,function(x){
                actionButton(session$ns(paste0(x)),x,class = 'modButton',
                             onclick = glue('Shiny.onInputChange("',session$ns('hitDice'),'",  this.id)'))
            }) %>% tagList,
            HTML('<strong>&emsp;&emsp;&emsp;&emsp;Recover: </strong>'),
            lapply(char$hitDice,function(x){
                actionButton(session$ns(paste0(x)),x,class = 'modButton',
                             onclick = glue('Shiny.onInputChange("',session$ns('recoverHD'),'",  this.id)'))
            }) %>% tagList
        )
    })

    # hit dice recovery
    observe({
        input$recoverHD
        isolate({
            if(!is.null(input$recoverHD)){
                maxDice = input$recoverHD %>% gsub('^.*-','',.) %>% gsub('d[0-9]+','',.) %>% as.integer()
                whichDice = input$recoverHD %>% gsub('^.*-[0-9]+','',.)

                remainingDice = char$hitDiceRemain[grepl(whichDice,char$hitDiceRemain)] %>%
                    gsub('^.*-','',.) %>% gsub('d[0-9]+','',.) %>% as.integer()

                if(remainingDice<maxDice){
                    char$hitDiceRemain[grepl(whichDice,char$hitDiceRemain)] = paste0(remainingDice+1,whichDice)
                }
            }

            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('recoverHD'))
        })

    })

    # spend hit dice
    observe({
        out('')
        input$hitDice
        isolate({
            if(!is.null(input$hitDice)){

                diceAtHand = input$hitDice %>% gsub('^.*-','',.) %>% gsub('d[0-9]+','',.) %>% as.integer()
                whichDice = input$hitDice %>% gsub('^.*-[0-9]+','',.)

                if(diceAtHand>0){
                    char$hitDiceRemain[char$hitDiceRemain %in% paste0(diceAtHand,whichDice)] = paste0(diceAtHand-1,whichDice)

                    HDroll = paste0(1,whichDice) %>% roll

                    char$currentHealth = min(char$maxHealth,char$currentHealth + HDroll + char$abilityMods['Con'])
                    updateSliderInput(session,
                                      inputId = 'healthSlider',
                                      value = char$currentHealth + char$currentTempHP,
                                      max = char$maxHealth + char$currentTempHP)

                    out(paste0('Hit Dice Roll:\n',HDroll,' + ', char$abilityMods['Con']))
                } else {
                    out('No more hit dice. Need sleep...')
                }



                session$sendCustomMessage(type = 'resetInputValue',
                                          message =  session$ns('hitDice'))

            }
        })
        # return(out)
    })

    observeEvent(input$hitDice,{
        print(input$hitDice)
        session$sendCustomMessage(type = 'resetInputValue',
                                  message =  session$ns('hitDice'))
    })

    observe({
        if(input$maxHealth == -1){
            print('set initial input boxes')
            # init(TRUE)
            updateNumericInput(session,inputId = 'maxHealth',value = char$maxHealth,min= 1)
            updateNumericInput(session,inputId = 'currentHealth',value = char$currentHealth,min= 0, max = char$maxHealth)
            updateNumericInput(session,inputId = 'tempHealth',value = char$currentTempHP,min= 0)
            updateSliderInput(session,
                              inputId = 'healthSlider',
                              value = char$currentHealth + char$currentTempHP,
                              max = char$maxHealth + char$currentTempHP)

        }
    })

    observe({
        input$currentHealth
        input$tempHealth
        input$maxHealth

        isolate({
            noNull = !is.null(input$currentHealth) | !is.null(input$tempHealth) | !is.null(input$maxHealth)

            if(noNull &&
               input$maxHealth != -1 &
               (input$currentHealth != char$currentHealth |
                input$tempHealth != char$currentTempHP |
                input$maxHealth != char$maxHealth)){

                print('input box update')
                if(noNull){
                    char$currentHealth = input$currentHealth
                    char$currentTempHP = input$tempHealth
                    char$maxHealth = input$maxHealth
                }

                if(char$currentHealth > char$maxHealth){
                    char$currentHealth = char$maxHealth
                }
                updateSliderInput(session,
                                  inputId = 'healthSlider',
                                  value = char$currentHealth + char$currentTempHP,
                                  max = char$maxHealth + char$currentTempHP)

            }
        })
    })


    observeEvent(input$minusHealth,{
        print('minus health button')

        healthDamage = min(0,char$currentTempHP - input$increment)

        char$currentTempHP = max(0,char$currentTempHP - input$increment)

        char$currentHealth =   max(0,char$currentHealth + healthDamage)
        updateSliderInput(session,
                          inputId = 'healthSlider',
                          value = char$currentHealth + char$currentTempHP,
                          max = char$maxHealth + char$currentTempHP)


    })

    observeEvent(input$plusHealth,{
        print('plus health button')

        char$currentHealth = min(char$maxHealth,char$currentHealth + input$increment)
        updateSliderInput(session,
                          inputId = 'healthSlider',
                          value = char$currentHealth + char$currentTempHP,
                          max = char$maxHealth + char$currentTempHP)

    })

    observeEvent(input$plusTempHealth,{
        print('plus temp health button')
        char$currentTempHP = char$currentTempHP + input$increment
        updateSliderInput(session,
                          inputId = 'healthSlider',
                          value = char$currentHealth + char$currentTempHP,
                          max = char$maxHealth + char$currentTempHP)

    })

    observeEvent(input$healthSlider,{
        print('slider input')
        if(!init()){
            init(TRUE)
        } else{
            totalHP = char$currentHealth + char$currentTempHP
            deltaHP =  input$healthSlider - totalHP
            if(deltaHP > 0){
                char$currentHealth = min(char$maxHealth,char$currentHealth + deltaHP)
            } else {
                healthDamage = min(0,char$currentTempHP + deltaHP)
                preTempHP =  char$currentTempHP
                char$currentTempHP = max(0,char$currentTempHP + deltaHP)
                char$currentHealth =   max(0,char$currentHealth + healthDamage)

                if(preTempHP != char$currentTempHP){
                    print('slider input temp hp change')
                    updateSliderInput(session,
                                      inputId = 'healthSlider',
                                      value = char$currentHealth + char$currentTempHP,
                                      max = char$maxHealth + char$currentTempHP)
                }

            }
        }
    })

    # update the dropdown fields only when clicking the dropdown button
    observeEvent(input$healthDropdown,{
        updateNumericInput(session,inputId = 'currentHealth',value = char$currentHealth,min= 0, max = char$maxHealth)
        updateNumericInput(session,inputId = 'tempHealth',value = char$currentTempHP,min= 0)
        updateNumericInput(session,inputId = 'maxHealth',value = char$maxHealth,min= 1)
    })

    observe({
        updateActionButton(session,
                           'plusTempHealth',
                           label = glue::glue('+ {char$currentTempHP}'))
        print('old update and update boxes')

    },priority = -999)



    output$HPSliderColorController = renderUI({
        controllerID = session$ns('HPsliderDiv')
        if(char$currentTempHP>0){
            color = '#6959CD'
        } else{
            color = '#8B1A1A'
        }

        tagList(tags$head(
            tags$style(HTML(glue("
.{controllerID} .irs-bar{{
    border-top-color: {color};
    border-bottom-color: {color};
    }}

.{controllerID} .irs-bar-edge {{
    border-color: {color};
    }}

.{controllerID} .irs-single, .{controllerID} .irs-bar-edge, .{controllerID} .irs-bar {{
    background: {color};
    }}")))
        ))
    })

    observe({
        charInitial$maxHealth
        isolate({
            updateSliderInput(session,
                              inputId = 'healthSlider',
                              value = char$currentHealth + char$currentTempHP,
                              max = char$maxHealth + char$currentTempHP)
        })
    },priority = 9999)

    return(out)

}
