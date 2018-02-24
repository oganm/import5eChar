characterDescriptionUI = function(id){
    ns = NS(id)
    tagList(htmlOutput(ns('description')))
}

characterDescription = function(input,output,session,char,charInitial){
    output$description = renderUI({
        descriptiveElement = function(field,label){
            tagList(
                p(field,class = 'narrowElement field'),
                hr(class = 'narrowElement'),
                p(label, class = 'narrowElement minorText')
                )
        }

        tagList(
            fluidRow(
                column(5,
                       wellPanel(fluidRow(
                           column(5,h2(char$Name)),
                           column(4,
                                  {
                                      if(is.null(getOption('ImThePortableClient')) & is.null(getOption('ImTheWebClient'))){
                                          tagList(
                                              div(textInput(session$ns('driveInput'),label = 'G Drive Import',width = '150px') ,style= 'display: inline-block'),
                                              actionButton(session$ns('driveSubmit'),label = '',icon = icon('check'),class = 'modButton',style = 'display: inline-block'),
                                              bsTooltip(session$ns('driveSubmit'),'Search in Google Drive',placement = 'bottom')
                                          )
                                      } else{
                                          NULL
                                      }
                                  }
                                  ),
                           column(3,
                                  fileInput(session$ns('charInput'),label = 'Local import'),
                                  bsTooltip(session$ns('charInput'),'Load local file',placement = 'bottom'))
                       )
                       )
                ),
                column(7,
                       wellPanel(
                           fluidRow(
                               column(2, descriptiveElement(AC(char),'AC')),
                               column(2, descriptiveElement(initBonus(char),'Initiative')),
                               column(4, descriptiveElement(char$ClassField,'Class & Level')),
                               column(4, descriptiveElement(char$Background,'Background'))

                           ),
                           fluidRow(
                               column(2, descriptiveElement(char$proficiencyBonus,'Proficiency')),
                               column(2, descriptiveElement(char$baseSpeed, 'Speed')),
                               column(4, descriptiveElement(char$Race, 'Race')),
                               column(4,descriptiveElement(char$Alignment, 'Alignment'))
                           )
                       ))
            )
        )
    })

    observe({
        input$charInput
        if(!is.null(input$charInput)){
            character = importCharacter(file = input$charInput$datapath)
            isolate({
                for(x in names(reactiveValuesToList(char))){
                    char[[x]] = character[[x]]
                }
                for(x in names(reactiveValuesToList(charInitial))){
                    charInitial[[x]] = character[[x]]
                }

            })

        }
    })

    observe({
        input$driveSubmit
        isolate({
        if(!is.null(input$driveSubmit) && !is.null(input$driveInput) && input$driveInput != ''){
            withProgress({
                googledrive::drive_auth(cache=TRUE)
                character = importCharacter(regex = input$driveInput)
                for(x in names(reactiveValuesToList(char))){
                    char[[x]] = character[[x]]
                }
                for(x in names(reactiveValuesToList(charInitial))){
                    charInitial[[x]] = character[[x]]
                }
            },value  =0.5 ,message = 'Reading from Google Drive')
        }
        })
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
                     class = 'modButton',style = 'background-color:#6959CD'),
        bsTooltip(ns('minusHealth'),'Damage Health',placement = 'bottom'),
        bsTooltip(ns('plusHealth'),'Heal',placement = 'bottom'),
        bsTooltip(ns('plusTempHealth'),'Add temp. HP',placement = 'bottom'),
        bsTooltip(ns('increment'),'HP to change',placement = 'bottom')

    )
}

health = function(input, output, session,
                  char,charInitial){
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
            char$currentHealth = input$currentHealth
            char$currentTempHP = input$tempHealth
            isolate({
                char$maxHealth = input$maxHealth

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

    observe({
        charInitial$maxHealth
        isolate({
            updateNumericInput(session,inputId = 'maxHealth',value = charInitial$maxHealth,min= 1)
        })
    },priority = 9999)

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
    imagePick = function(weapon){
        if(grepl('crossbow', tolower(weapon$name))){
            icon = 'icons/crossbow.png'
        } else if(grepl('bow', tolower(weapon$name))){
            icon = 'icons/high-shot.png'
        } else if(grepl('net', tolower(weapon$name))){
            icon = 'icons/fishing-net.png'
        } else if(grepl('blowgun|dart',tolower(weapon$name))){
            icon = 'icons/dart.png'
        } else if(grepl('whip',tolower(weapon$name))){
            icon = 'icons/whip.png'
        } else if(grepl('hammer|pick|maul', tolower(weapon$name))){
            icon = 'icons/claw-hammer.png'
        } else if(grepl('trident', tolower(weapon$name))){
            icon = 'icons/trident.png'
        } else if(grepl('sword|rapier|scimitar|dagger', tolower(weapon$name))){
            icon = 'icons/pointy-sword.png'
        } else if(grepl('pike|spear|lance|glaive', tolower(weapon$name))){
            icon = 'icons/spear-hook.png'
        } else if(grepl('halberd', tolower(weapon$name))){
            icon = 'icons/sharp-halberd.png'
        } else if(grepl('axe',tolower(weapon$name))){
            icon = 'icons/battered-axe.png'
        } else if(grepl('flail',tolower(weapon$name))){
            icon = 'icons/flail.png'
        } else if(grepl('sling',tolower(weapon$name))){
            icon = 'icons/sling.png'
        } else if(grepl('sickle',tolower(weapon$name))){
            icon = 'icons/sickle.png'
        } else if(grepl('quarterstaff',tolower(weapon$name))){
            icon = 'icons/bo.png'
        } else if(grepl('mace|morningstar',tolower(weapon$name))){
            icon = 'icons/flanged-mace.png'
        } else if(grepl('javelin',tolower(weapon$name))){
            icon = 'icons/thrown-spear.png'
        } else if(grepl('club',tolower(weapon$name))){
            icon = 'icons/wood-club.png'
        } else if(grepl('unarmed',tolower(weapon$name))){
            icon = 'icons/fist.png'
        } else if(weapon$type =='ranged'){
            icon = 'icons/high-shot.png'
        } else if(weapon$damageType =='Bludgeoning'){
            icon = 'icons/flanged-mace.png'
        } else{
            icon = 'icons/pointy-sword.png'
        }

        return(img(src = icon,width = 20,height = 20))
    }
    output$weaponsTable = renderDataTable({
        weaponTable = char$weapons %>% sapply(function(x){



            c(actionButton(label = div(imagePick(x),x$name),
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




    out = reactive({
        out = ''

        if(!is.null(input$weaponButton)){
            advantage = switch(input$advantage,
                               Norm = 0,
                               DisAdv = -1,
                               Adv = 1)

            w = char$weapons
            out = glue(input$weaponButton,':\n',
                       capture.output(weaponAttack(w[[input$weaponButton]],
                                                   sharpShoot =input$sharpshoot,
                                                   adv = advantage,
                                                   char = char)) %>%
                           gsub('(\\[1\\] )|"','',.) %>%
                           paste(collapse = '\n'))

            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('weaponButton'))
        }

        return(out)
    })

    return(out)
}


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

        attributeRows = data.frame(skillName = skillAttributes %>% map(strong) %>% map_chr(as.character),
                                   profMark='',
                                   skillBonus = '',
                                   index = c(0,statSep+.1),
                                   buttons = '',stringsAsFactors = FALSE)
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
                                         bInfo = 0))
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
        return(out)
    })
    return(out)

}

resourcesUI = function(id){
    ns = NS(id)
    tagList(
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
                    });"),
        dataTableOutput(ns('resourcesTable'))
    )
}

resources = function(input,output,session,char){
    output$resourcesTable = renderDataTable({
        # remove first resource if its the default one created
        char$resources %<>% filter(!(name == 'Resource' &
                                         shortName == 'Resource' &
                                         remainingUse ==0 &
                                         maxUse == 0 &
                                         dice == 0))

        if(nrow(char$resources) == 0){
            return(NULL)
        }


        buttons = char$resources$shortName %>% sapply(function(x){
            actionButton(label = x,
                         inputId= x,
                         onclick = glue('Shiny.onInputChange("',session$ns('resourceButton'),'",  this.id)'),
                         class = 'resourceButton') %>% as.character
        })

        displays = 1:nrow(char$resources) %>% sapply(function(i){
            if(char$resources$dice[i]>0){
                out = paste0(char$resources$remainingUse[i],
                              'd',
                              char$resources$dice[i])
            } else if(char$resources$remainingUse[i]>=0){
                out = char$resources$remainingUse[i]
            } else{
                out = ('')
            }
        })

        refill = 1:nrow(char$resources) %>% sapply(function(i){
            resource = char$resources[i,]
            if(resource$Reset != 'static' |
               resource$RecoverPerLongRest >0 |
               resource$RecoverPerShortRest > 0){
                actionButton(inputId = resource$name, # ids must be unique per element shortname is used so using name now
                             label = '+',
                             onclick =  glue('Shiny.onInputChange("',session$ns('recoverResource'),'",  this.id)'),
                             class = 'resourceButton') %>% as.character
            } else{
                ''
            }
        })

        resourceTable = data.frame(name = buttons,
                                   display = displays,
                                   refill = refill,
                                   stringsAsFactors = FALSE)

        table = datatable(resourceTable,escape = FALSE,selection = 'none',rownames = FALSE,
                          colnames= rep('',2),
                          options = list(bFilter = 0,
                                         bLengthChange = 0,
                                         paging = 0,
                                         ordering = 0,
                                         bInfo = 0))

    })

    observe({

    })

    out = reactive({
        out = ''

        if(!is.null(input$resourceButton)){
            isolate({
                resourceToUse = char$resources %>% filter(shortName == input$resourceButton)
                resourceIndex = char$resources$shortName %in% input$resourceButton

                # is it a consumable resource
                if(resourceToUse$Reset != 'static' | resourceToUse$RecoverPerLongRest >0 | resourceToUse$RecoverPerShortRest > 0){
                    if(resourceToUse$remainingUse <=0){
                        out = paste0('Unable to use ',resourceToUse$name,'. No uses left')
                    } else{
                        char$resources$remainingUse[resourceIndex] =
                            char$resources$remainingUse[resourceIndex]-1
                        out = paste('Used',resourceToUse$name)
                        if(resourceToUse$dice>0){
                            out = paste0(out,'\n',roll(glue('1d{resourceToUse$dice}')))
                        }
                    }
                } else{
                    # non consumable resources
                    out = paste('Used',resourceToUse$name)
                    if(resourceToUse$dice > 0){
                        out = paste0(out,'\n',
                                     capture.output(roll(glue('{resourceToUse$remainingUse}d{resourceToUse$dice}'))) %>%
                                         paste(collapse = '\n'))
                    }
                }
            })

            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('resourceButton'))
        }


        if(!is.null(input$recoverResource)){
            isolate({
                resourceIndex = char$resources$name %in% input$recoverResource
                if(char$resources$remainingUse[resourceIndex] <
                   char$resources$maxUse[resourceIndex]){

                    out = paste('Recovered',char$resources$name[resourceIndex])

                    char$resources$remainingUse[resourceIndex] =
                        char$resources$remainingUse[resourceIndex] + 1
                } else{
                    out = paste(char$resources$name[resourceIndex], 'already at max')
                }

                session$sendCustomMessage(type = 'resetInputValue',
                                          message =  session$ns('recoverResource'))

            })
        }


        return(out)
    })

    return(out)
}


spellsUI = function(id){
    ns = NS(id)
    tagList(
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
                    });"),
        dataTableOutput(ns('spellTable')))
}

spells = function(input,output,session,char){

    output$spellTable = renderDataTable({
        if(!is.null(char$spells)){
            groups = char$spells$level %>% duplicated %>% not %>% which
            groups = groups -1
            groups %<>% c(nrow(char$spells))

            availableLevels = unique(char$spells$level)
            # maxLevel = (char$spellSlots>0) %>% which %>% max %>% {.-1} # max level with spell slots


            nameButtons = char$spells$name %>% sapply(function(x){
                a(href = paste0(spellSource,
                                x %>% tolower() %>% gsub(' |/','-',.) %>% gsub("'",'',.)),
                  target= '_blank',x
                ) %>% as.character()
            })

            prepared = char$spells$name %>% sapply(function(x){
                checkboxInput(inputId = paste0(x,'-prep'),
                              label = '',
                              value = char$spells %>% filter(name == x) %$% prepared) %>%
                    as.character
            })


            table  = data.frame(nameButtons, prepared,
                                level = char$spells$level  + .1,
                                stringsAsFactors = FALSE)


            slotInfo = 1:10 %>% sapply(function(x){
                if(names(char$spellSlots[x]) == 'Cantrip'){
                    text = 'Cantrip'
                } else{
                    text = paste0('Level ',names(char$spellSlots[x]), ' (',char$spellSlots[x],')')
                }

                strong(text) %>% as.character()
            })

            slotButtons = 0:9 %>% sapply(function(x){
                if(x>0){
                    tagList(
                               actionButton(inputId = paste0(x,'_cast'),
                                            label = '-',
                                            class = 'modButton',
                                            onclick =  glue('Shiny.onInputChange("',session$ns('spellCast'),'",  this.id)')),
                               actionButton(inputId = paste0(x,'_recover'),
                                            label = '+',
                                            class = 'modButton',
                                            onclick =  glue('Shiny.onInputChange("',session$ns('spellRecover'),'",  this.id)')
                    )) %>% as.character
                } else{
                    ''
                }

            })

            levelTable = data.frame(nameButtons = slotInfo,
                                    prepared = slotButtons,
                                    level = 0:9,
                                    stringsAsFactors = FALSE)

            finalTable = rbind(table,levelTable) %>% arrange(level) %>% select(-level)

            dt = datatable(finalTable,escape = FALSE,selection = 'none',rownames = FALSE,
                      colnames= rep('',2),
                      options = list(bFilter = 0,
                                     bLengthChange = 0,
                                     paging = 1,
                                     ordering = 0,
                                     bInfo = 0))

            return(dt)

        }
    })

    observe({
        if(!is.null(input$spellCast)){
            isolate({
                level = input$spellCast %>% strsplit('_') %>% {.[[1]][1]}
                char$spellSlots[level] = char$spellSlots[level] - 1

                session$sendCustomMessage(type = 'resetInputValue',
                                          message =  session$ns('spellCast'))
            })
        }

        if(!is.null(input$spellRecover)){
            isolate({
                level = input$spellRecover %>% strsplit('_') %>% {.[[1]][1]}
                char$spellSlots[level] = char$spellSlots[level] + 1

                session$sendCustomMessage(type = 'resetInputValue',
                                          message =  session$ns('spellRecover'))
            })
        }
    })


}


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
