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
                                    diceButtons = '',
                                    level = 0:9,
                                    stringsAsFactors = FALSE)

            # browser()

            finalTable = rbind(table,levelTable) %>% arrange(level) %>% select(-level)

            dt = datatable(finalTable,escape = FALSE,selection = 'none',rownames = FALSE,
                           colnames= rep('',3),
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
