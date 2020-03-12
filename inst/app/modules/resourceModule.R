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
                                         dice == 0)) %>%
            {.$shortName = {.$shortName[.$shortName==''] = .$name[.$shortName==''];.$shortName};.} %>%
            filter(shortName != '')

        if(nrow(char$resources) == 0){
            return(NULL)
        }


        buttons = 1:nrow(char$resources) %>% sapply(function(i){
            resource =  char$resources[i,]
            x = resource$shortName

            if(resource$dice == 0 & resource$Reset != 'static'){
                out = tags$b(x) %>% as.character
            } else{

                out = actionButton(label = x,
                                   inputId= x,
                                   onclick = glue('Shiny.onInputChange("',session$ns('resourceButton'),'",  this.id)'),
                                   class = 'resourceButton') %>% as.character
            }

            return(out)
        })

        displays = 1:nrow(char$resources) %>% sapply(function(i){
            resource = char$resources[i,]

            if(resource$dice == 0 & resource$Reset != 'static'){
                out = glue::glue('<div style="width:300%"><input type="text" id="resourceslider{i}" name="slider" data-min="0" data-max="{char$resources$maxUse[i]}" value="{char$resources$remainingUse[i]}" /></div>')

                # sliderInput(inputId = paste0(''), value = char$resources$remainingUse[i],
                #             label = '',
                #             min = 0,
                #             max = char$resources$maxUse[i],step = 1, width = '100%') %>%
                #     as.character -> out

            } else if(char$resources$dice[i]>0){
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
            if(resource$dice!=0 & (resource$Reset != 'static' |
               resource$RecoverPerLongRest >0 |
               resource$RecoverPerShortRest > 0)){
                actionButton(inputId = resource$name, # ids must be unique per element shortname is used so using name now. actually not really as these actually set different variables. not sure why I thought this was the case
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

        js = c(
            "function(settings){",
            "  $('[id^=resourceslider]').ionRangeSlider({",
            "    grid: true,",
            "  });",
            "}"
        )

        table = datatable(resourceTable,escape = FALSE,selection = 'none',rownames = FALSE,
                          colnames= rep('',2),
                          options = list(bFilter = 0,
                                         bLengthChange = 0,
                                         paging = 0,
                                         ordering = 0,
                                         bInfo = 0,
                                         pageLength = nrow(resourceTable),
                                         initComplete = JS(js)))

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
