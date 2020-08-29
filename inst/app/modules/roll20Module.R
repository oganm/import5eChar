startRoll20Session = function(email, password, gameID,view = FALSE){

    b <- chromote::ChromoteSession$new()
    if(view){
        b$view()
    }

    gameLink = glue::glue('https://app.roll20.net/editor/setcampaign/{gameID}')

    closeLink = glue::glue('https://app.roll20.net/sessions/destroy/')
    b$Page$navigate(closeLink)
    Sys.sleep(3)

    b$Page$navigate(gameLink)
    b$DOM$enable()
    Sys.sleep(5)
    page = b$DOM$getDocument()


    # check for already logged in sessions
    chatBox = b$DOM$querySelector(page$root$nodeId, "#textchat-input > textarea")
    if(chatBox$nodeId!=0){
        print('Already logged in')
        return(b)
    }


    # email section
    emailID = b$DOM$querySelector(page$root$nodeId, "#email")
    b$DOM$focus(emailID$nodeId)
    b$Input$insertText(text = email)

    # password section
    passwordID = b$DOM$querySelector(page$root$nodeId, "#password")
    b$DOM$focus(passwordID$nodeId)
    b$Input$insertText(text = password)

    # login button
    loginID = b$DOM$querySelector(page$root$nodeId, "#login")
    box = b$DOM$getBoxModel(nodeId = loginID$nodeId)
    b$Input$synthesizeTapGesture(x= round(box$model$content[[1]]),y = round(box$model$content[[2]]))

    # navigate to target page after login
    Sys.sleep(5)
    b$Page$navigate(gameLink)
    Sys.sleep(3)

    return(b)
}

sendMessage = function(session, message){
    b = session
    page = b$DOM$getDocument()

    chatBox = b$DOM$querySelector(page$root$nodeId, "#textchat-input > textarea")
    b$DOM$focus(chatBox$nodeId)

    b$Input$insertText(text = message)

    chatButton =  b$DOM$querySelector(page$root$nodeId, "#textchat-input > button")
    box = b$DOM$getBoxModel(nodeId = chatButton$nodeId)
    b$Input$synthesizeTapGesture(x= round(box$model$content[[1]]),y = round(box$model$content[[2]]))

}



roll20UI = function(id){
    ns = NS(id)
    tagList(
        shiny::tags$p('Connect to roll20 (beta and possibly unsafe)'),
        switchInput(ns('roll20'),label = 'Connect'),
        dropdownButton(
        textInput(ns('email'),label = 'email'),
        passwordInput(ns('password'),label = 'password'),
        textInput(ns('gameLink'),label = 'Game ID'),
        actionButton(ns('login'),label = 'login'),
        icon = icon("bars"),
        size=  'xs',
        width = '30px',
        right = TRUE,
        inputId = ns('roll20Dropdown'))
    )
}

roll20 = function(input, output, session,...){

    b = reactiveVal()
    loggedIn = reactiveVal(FALSE)
    observeEvent(input$login,{
        print('dasdas')
        withProgress(message = 'Logging into roll20',value = 1,{
            b(startRoll20Session(input$email, input$password, input$gameLink))
            loggedIn(TRUE)
        })
        print('dadsa')

    })

    observe({
        requestParams = list(...)
        out = ""
        for(i in seq_along(requestParams)){
            if(requestParams[[i]]()!=''){
                out  = requestParams[[i]]()
                break
            }
        }

        isolate({
            if(input$roll20 && loggedIn() && out != ''){

                out = out %>% stringr::str_replace('^(.*?)\n','**\\1**\n')

                tryCatch(sendMessage(b(),out),
                         error = function(e){
                             print('failed to send message')
                         })
            }
        })
    })
}
