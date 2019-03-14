consoleUI = function(id){
    ns = NS(id)
    tagList(
        verbatimTextOutput(ns('console'), placeholder = TRUE),
        tags$head(tags$style(glue("#<ns('console')>{overflow-y:scroll;max-height: 300px}",.open = '<',.close = '>')))
    )

}


console = function(input, output, session, consoleLength,  ...){

    consoleOut = reactiveVal(rep('\n',consoleLength) %>% paste(collapse = ''))

    textUpdate = reactiveVal(0)


    output$console = renderText({
        requestParams = list(...)
        out = ""
        for(i in seq_along(requestParams)){
            if(requestParams[[i]]()!=''){
                out  = requestParams[[i]]()
                break
            }
        }

        isolate({
            # console memory
            if(out != ''){
                out %<>% strsplit('\n') %>% {.[[1]]} # kinda not necesarry
                oldConsole = consoleOut() %>% strsplit(split = '\n') %>% {.[[1]]}
                newConsole = c(oldConsole[(length(out)+1):consoleLength],out)
                console = newConsole %>% paste(collapse='\n')
                consoleOut(console)
            }

            textUpdate(textUpdate()+1)

            return(consoleOut())
        })

    })

    observeEvent(textUpdate(),{
        shinyjs::runjs(glue(
            "var objDiv =  document.getElementById('<session$ns('console')>');
            objDiv.scrollTop = objDiv.scrollHeight",.open = '<',.close = '>'))

    })

    return(consoleOut)
}
