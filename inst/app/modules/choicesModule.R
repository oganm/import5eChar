choicesUI = function(id){
    ns = NS(id)
    tagList(
        htmlOutput(ns('choices'))
    )
}


choices = function(input,output,session,char){

    output$choices = renderUI({
        tabs = seq_along(char$classChoices) %>% lapply(function(i){
            tabPanel(names(char$classChoices)[i] %>% abbreviate(),
                     tagList(
                         strong(names(char$classChoices)[i]),
                         char$classChoices[[i]] %>%
                             lapply(tags$li) %>%
                             do.call(tags$ul,.)
                     )
           )
        })

        do.call(tabsetPanel,c(id = session$ns('choicesTab'),tabs))
    })

}
