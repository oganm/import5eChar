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
                column(4,
                       descriptiveElement(names(spellDC(char)),'Casting Stat'))
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
                a(href = paste0(.sheetApp.spellSource,
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

            diceButtons = char$spells$name %>% sapply(function(x){
                spellIndex = which(tolower(names(spellData)) %in% tolower(x))

                if(length(spellIndex==1)){
                    spellDice = spellData[[which(tolower(names(spellData)) %in% tolower(x))]]
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
                        choices = seq_len(slots[x]),
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
