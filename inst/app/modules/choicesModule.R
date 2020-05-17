choicesUI = function(id){
    ns = NS(id)
    tagList(
        htmlOutput(ns('choices'))
    )
}


choices = function(input,output,session,char){

    output$choices = renderUI({

        getRelevantFeature = function(x,char){
            extraText = stringr::str_extract(char$Features,
                                             glue('(?<={x}):.*?(?=\\n)'))

            if(!is.na(extraText)){
                x = tagList(strong(x), extraText)
            }
            return(x)
        }

        tabs = seq_along(char$classChoices) %>% lapply(function(i){

            bsCollapsePanel(names(char$classChoices)[i] %>% abbreviate(minlength = 3),
                     tagList(
                         strong(names(char$classChoices)[i]),
                         char$classChoices[[i]] %>%
                             lapply(function(x){
                                 x = getRelevantFeature(x, char)
                                 tags$li(x)
                             }) %>%
                             do.call(tags$ul,.)
                     )
           )
        })

        feats = char$feats[char$feats!='']

        if(length(feats)>0){
            tabs = c(tabs,
                     bsCollapsePanel('Feats',feats %>%
                                  lapply(function(x){
                                      x = getRelevantFeature(x, char)
                                      tags$li(x)
                                  }) %>%
                                  do.call(tags$ul,.)) %>%
                         list)
        }
        if(char$Features!=''){
            tabs = c(tabs,
                     bsCollapsePanel('Features',
                              char$Features  %>%
                                  gsub('\n','<br/>',.,fixed = TRUE) %>%
                                  HTML ) %>%
                         list)
        }

        if(char$notes != ''){
            tabs = c(tabs,
                     bsCollapsePanel('Notes',
                              char$notes  %>%
                                  gsub('\n','<br/>',.,fixed = TRUE) %>%
                                  HTML ) %>%
                         list)
        }



        do.call(bsCollapse,c(id = session$ns('choicesTab'),multiple = TRUE,tabs))
    })

}
