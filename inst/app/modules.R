characterDescriptionUI = function(id){
    ns = NS(id)
    tagList(htmlOutput(ns('description')))
}

characterDescription = function(input,output,session,char,charInitial){

    inputUserid <- function(inputId, value='') {
        #   print(paste(inputId, "=", value))
        tagList(
            singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
            singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
            tags$body(onload="setvalues()"),
            tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
        )
    }

    inputIp <- function(inputId, value='', ns){
        tagList(
            singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
            singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
            tags$body(onload="setvalues()"),
            tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
        )
    }
    saveCharacter = function(characterFile, consent, fingerprint = ''){
        randomName = tools::md5sum(characterFile)
        if(consent){
            dir.create('chars',showWarnings = FALSE)
            file.copy(characterFile, file.path('chars',paste0(fingerprint,'_',randomName)))
        } else {
            dir.create('naysayer',showWarnings = FALSE)
            file.create(file.path('naysayer',paste0(fingerprint,'_',randomName)))
        }
    }

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
                {
                    if(!is.null(getOption('ImTheWebClient'))){
                        tagList(inputIp(session$ns("ipid")),
                                inputUserid(session$ns("fingerprint")))
                    } else  {
                        NULL
                    }
                },

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
                                  div(id=session$ns('consentDiv') , checkboxInput(inputId = session$ns('consent'),label = 'Can I keep a copy?', value = TRUE), style = 'font-size:70%'),
                                  bsTooltip(session$ns('charInput'),'Load local file',placement = 'bottom'),
                                  bsTooltip(session$ns('consentDiv'),
                                            title = "If the box is checked I save a copy of the uploaded character sheet. I use these saved sheets as test cases when improving the application. I also plan to use them for some statistical analyses examining character building choices. The characters remain your intellectual property. If you\\'d rather I didn\\'t save your character, uncheck this box. I won\\'t be mad. Only dissapointed"))
                       )
                       )
                ),
                column(7,
                       wellPanel(
                           fluidRow(
                               column(2, descriptiveElement(AC(char),'AC')),
                               column(2,
                                      actionButton(session$ns('init'), label = initBonus(char), class = 'skillButton'),
                                      hr(class = 'narrowElement'),
                                      p('Initiative', class = 'narrowElement minorText')
                                      ),
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
        if(is.null(getOption('ImTheWebClient'))){
            hide('consentDiv')

        }
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
                if(!is.null(getOption('ImTheWebClient'))){
                    saveCharacter(input$charInput$datapath, input$consent, paste0(input$fingerprint,'_',input$ipid))
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

    out = reactive({
        out = ''
        if(!is.null(input$init) && input$init > 0){
            out = paste0('Initiative:\n',
                         capture.output(init(char)) %>%
                             gsub('(\\[1\\] )|"','',.) %>%
                             paste(collapse = '\n'))
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('init'))

        }
        return(out)
    })

    return(out)

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
                                         bInfo = 0,
                                         pageLength = nrow(weaponTable)))
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
        return(out)
    })
    return(out)

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

packageVersionUI = function(id){
    ns = NS(id)
    tagList(
        textOutput(outputId = ns('version'))
    )
}

packageVersion = function(input,output,session){
    output$version = renderText({
        version = installed.packages() %>% as.data.frame %>% filter(Package == 'import5eChar') %$% Version %>% as.character()
        paste('Version:',version)
    })

}
