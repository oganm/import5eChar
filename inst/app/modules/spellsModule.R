spellsUI = function(id){
    ns = NS(id)
    tagList(
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
});"),
        htmlOutput(ns('casterInfo')),
        dataTableOutput(ns('spellTable')))
    }

spells = function(input,output,session,char){

    # out =  reactiveVal('')


    descriptiveElement = function(field,label){
        tagList(
            p(field,class = 'narrowElement field'),
            hr(class = 'narrowElement'),
            p(label, class = 'narrowElement minorText')
        )
    }

    output$casterInfo = renderUI({
        wellPanel(
            fluidRow(
                column(4,
                       descriptiveElement(spellDC(char),'Spell DC')),
                column(4,
                       actionButton(session$ns('spellAttack'), label = spellAttack(char), class = 'skillButton'),
                       hr(class = 'narrowElement'),
                       p('Spell Attack', class = 'narrowElement minorText')
                ),
                column(3,
                       descriptiveElement(names(spellDC(char)),'Casting Stat')),
                column(1,
                       dropdownButton(
                           strong('Spell Source'),
                           shinysky::textInput.typeahead(id = session$ns('spellSource'),
                                               placeholder = 'dndbeyond',
                                               local = data.frame(name = c("https://www.dndbeyond.com/spells/",
                                                                           "https://thebombzen.com/grimoire/spells/")),
                                               valueKey = 'name',
                                               tokens =  c('dndbeyond','grimoire'),
                                               template =  HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")),
                           circle = TRUE,
                           icon = icon("bars"),
                           size=  'xs',
                           width = '30px',
                           right = TRUE,
                           inputId = session$ns('spellDropdown')))
            )
        )

    })


    output$spellTable = renderDataTable({

        if(!is.null(char$spells)){
            groups = char$spells$level %>% duplicated %>% not %>% which
            groups = groups -1
            groups %<>% c(nrow(char$spells))



            availableLevels = unique(char$spells$level)
            # maxLevel = (char$spellSlots>0) %>% which %>% max %>% {.-1} # max level with spell slots

            nameButtons = char$spells$name %>% sapply(function(x){
                namelink = x %>% tolower() %>% gsub(' |/','-',.) %>% gsub("'|’",'',.)

                if(is.null(input$spellSource) || input$spellSource == ''){
                    spSource = .sheetApp.spellSource
                } else{
                    spSource = input$spellSource
                }


                # prepare the link
                spelllink = a(href = paste0(spSource,
                                       namelink),
                         target= '_blank',x,
                         id = paste0('name_button_',namelink)
                )

                # prepare additional information

                spellIndex = which(tolower(names(spellDetails)) %in% tolower(x))
                if(length(spellIndex==1)){
                    spellInfo = spellDetails[[spellIndex]]
                } else{
                    spellInfo = list()
                }



                if(length(spellInfo)>0){
                    tooltipData =
                        spellInfo[c('school','components','castingTime','duration','range','aoe','attackSave','damageEffect')] %>%
                        unlist %>%
                        na.omit() %>% {.[.!='']}

                    tooltipText = seq_along(tooltipData) %>% sapply(function(i){
                        paste0(names(tooltipData)[i],': ',tooltipData[[i]])
                    }) %>% paste(collapse = '</br>')

                    div(spelllink,
                        bsTooltip(id = paste0('name_button_',namelink),tooltipText)) %>%
                        as.character -> out
                    return(out)
                }else{
                    return(as.character(spelllink))
                }

            })

            prepared = char$spells$name %>% sapply(function(x){
                checkboxInput(inputId = paste0(x,'-prep'),
                              label = '',
                              value = char$spells %>% filter(name == x) %$% prepared) %>%
                    as.character
            })

            diceButtons = char$spells$name %>% sapply(function(x){
                spellIndex = which(tolower(names(spellData)) %in% tolower(x))

                if(length(spellIndex==1)){
                    spellDice = spellData[[spellIndex]]
                    spellDice %<>%
                        gsub(pattern = 'SPELL\\+PROF', replacement = spellAttack(char),.) %>%
                        gsub(pattern = 'SPELL',replacement = char$abilityMods[char$castingStatCode + 1],.)
                    lapply(spellDice, function(y){
                        actionButton(inputId = paste0(y,'_roll'),
                                     label= y,
                                     class= 'modButton',
                                     onclick =  glue('Shiny.onInputChange("',session$ns('spellDice'),'",  this.id)'))
                    }) %>% tagList %>% as.character
                } else{
                    return('')
                }

            })


            table  = data.frame(nameButtons, prepared,
                                diceButtons,
                                level = char$spells$level  + .1,
                                stringsAsFactors = FALSE)

            slots = spellSlots(char)
            maxLevel = max(which(slots>0),0)

            slotInfo = 1:(1+maxLevel) %>% sapply(function(x){
                if(names(char$spellSlots[x]) == 'Cantrip'){
                    text = 'Cantrip'
                } else{
                    text = paste0('Level ',names(char$spellSlots[x]))
                }

                strong(text) %>% as.character()
            })


            slotMarks = 0:maxLevel %>% sapply(function(x){
                if(x>0 && slots[[x]]>0){
                    radioGroupButtons(
                        status = 'primary',
                        size = 'sm',
                        inputId = session$ns(paste0("slotMark",x)),
                        label = '',
                        choices = c(0,seq_len(slots[x])) %>% lapply(function(y){
                            div(id = paste0(x,'-',y), width= "100%",onclick = glue('Shiny.onInputChange("',session$ns('spellSlots'),'",  this.id)'),
                              y)
                        }) %>% sapply(as.character),
                        selected = char$spellSlots[as.character(x)]
                    ) %>% as.character()
                    #
                    #
                    # checkboxGroupInput('slotMark','',choices = rep('',slots[x]),selected = char$spellSlots[as.character(x)]>=seq_len(slots[x]), inline = TRUE) %>%
                    #     as.character()
                } else {
                    ''
                }
            })

            levelTable = data.frame(nameButtons = slotInfo,
                                    prepared = slotMarks,
                                    diceButtons = '',
                                    level = 0:maxLevel,
                                    stringsAsFactors = FALSE)

            # browser()

            finalTable = rbind(table,levelTable) %>% arrange(level) %>% select(-level)

            dt = datatable(finalTable,escape = FALSE,selection = 'none',rownames = FALSE,width = '100%',
                           colnames= rep('',3),
                           options = list(bFilter = 0,
                                          bLengthChange = 0,
                                          paging = 0,
                                          ordering = 0,
                                          bInfo = 0,
                                          pageLength = nrow(finalTable)))

            return(dt)

        }
    })


    # observe({
    #     print(input$spellSlots)
    # })



    out = reactive({
        if(!is.null(input$spellAttack)  && input$spellAttack > 0){

            # out('heyey')
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('spellAttack'))
            spAt = paste('1d20 +',spellAttack(char))
            return(paste0(
                'Spell Attack:\n',
                capture.output(roll(spAt)) %>% gsub('(\\[1\\] )|"','',.) %>%
                                   paste(collapse = '\n'))
            )

        } else if(!is.null(input$spellDice)){
            dice = input$spellDice %>%  gsub('_roll','',.)
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('spellDice'))
            return(paste0('Rolling ',dice,':\n',
                       capture.output(roll(dice)) %>%
                           gsub('(\\[1\\] )|"','',.) %>%
                           paste(collapse = '\n')))
        } else {
            # out('')
            return('')
        }


    })


    return(out)


}
