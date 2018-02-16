characterDescriptionUI = function(id){
    ns = NS(id)
    tagList(htmlOutput(ns('description')))
}

characterDescription = function(input,output,session,char){
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
                column(4,
                       wellPanel(h2(char$Name))),
                column(8,
                       wellPanel(
                           fluidRow(
                               column(4, descriptiveElement(char$ClassField,'Class & Level')),
                               column(4, descriptiveElement(char$Background,'Background')),
                               column(2, descriptiveElement(AC(char),'AC')),
                               column(2, descriptiveElement(initBonus(char),'Initiative'))
                           ),
                           fluidRow(
                               column(4, descriptiveElement(char$Race, 'Race')),
                               column(4,descriptiveElement(char$Alignment, 'Alignment')),
                               column(2, descriptiveElement(char$proficiencyBonus,'Proficiency')),
                               column(2, descriptiveElement(char$baseSpeed, 'Speed'))
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


    weaponOut = reactive({
        list(advantage = input$advantage,
             sharpshoot = input$sharpshoot)
    })

    return(weaponOut)
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
        buttons = char$resources$shortName %>% sapply(function(x){
            actionButton(label = x,
                         inputId= x,
                         onclick = glue('Shiny.onInputChange("',session$ns('resourceButton'),'",  this.id)'),
                         class = 'resourceButton') %>% as.character
        })

        displays = 1:nrow(char$resources) %>% sapply(function(i){
            if(char$resources$dice[i]>0){
                out = (paste0(char$resources$remainingUse[i],
                              'd',
                              char$resources$dice[i]))
            } else if(char$resources$remainingUse[i]>=0){
                out = char$resources$remainingUse[i]
            } else{
                out = ('')
            }
        })

        resourceTable = data.frame(name = buttons,
                                   display = displays,
                                   stringsAsFactors = FALSE)

        table = datatable(resourceTable,escape = FALSE,selection = 'none',rownames = FALSE,
                          colnames= rep('',2),
                          options = list(bFilter = 0,
                                         bLengthChange = 0,
                                         paging = 0,
                                         ordering = 0,
                                         bInfo = 0))

    })
}
