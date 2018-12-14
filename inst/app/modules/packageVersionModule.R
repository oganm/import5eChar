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
