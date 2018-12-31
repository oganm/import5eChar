choicesUI = function(id){
    ns = NS(id)
    tagList(
        htmlOutput(ns('choices'))
    )
}


choices = function(input,output,session,char){

    output$choices = renderUI({
        tabs = seq_along(char$classChoices) %>% lapply(function(i){
            tabPanel(names(char$classChoices)[i] %>% abbreviate(minlength = 3),
                     tagList(
                         strong(names(char$classChoices)[i]),
                         char$classChoices[[i]] %>%
                             lapply(tags$li) %>%
                             do.call(tags$ul,.)
                     )
           )
        })

        feats = char$feats[char$feats!='']

        if(length(feats)>0){
            tabs = c(tabs,
                     tabPanel('Feats',feats %>%
                                  lapply(tags$li) %>%
                                  do.call(tags$ul,.)) %>%
                         list)
        }
        if(char$Features!=''){
            tabs = c(tabs,
                     tabPanel('Features',
                              char$Features  %>%
                                  gsub('\n','<br/>',.,fixed = TRUE) %>%
                                  HTML ) %>%
                         list)
        }

        if(char$notes != ''){
            tabs = c(tabs,
                     tabPanel('Notes',
                              char$notes  %>%
                                  gsub('\n','<br/>',.,fixed = TRUE) %>%
                                  HTML ) %>%
                         list)
        }



        do.call(tabsetPanel,c(id = session$ns('choicesTab'),tabs))
    })

}
