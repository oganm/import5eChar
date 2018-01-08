characterDescription = function(char){
    fluidRow(
        column(4,
               wellPanel(h2(char$Name))),
        column(8,
               wellPanel(
                   fluidRow(
                       column(6, p(char$ClassField, class ='narrowElement field'),
                              hr(class = 'narrowElement'),
                              p('Class & Level', class = 'narrowElement minorText')),
                       column(6,p(char$Background, class ='narrowElement field'),
                              hr(class = 'narrowElement'),
                              p('Background', class = 'narrowElement minorText'))
                   ),
                   fluidRow(
                       column(6,p(char$Race, class ='narrowElement field'),
                              hr(class = 'narrowElement'),
                              p('Race',class='narrowElement minorText')),
                       column(6,p(char$Alignment, class ='narrowElement field'),
                              hr(class = 'narrowElement'),
                              p('Alignment',class='narrowElement minorText'))
                   )
               ))
    )

}

healthUI = function(id,char){
    ns = NS(id)
    tagList(
        progressBar(id = ns("healthBar"), value = char$currentHealth+char$currentTempHP,
                    total = char$maxHealth + char$currentTempHP),
        div(
        numericInput(inputId = ns('increment'),
                     label = '', value = 1,step =1,
                     width = '70px'),style="display:inline-block"),
        actionButton(ns('minusHealth'),'-', class = 'modButton'),
        actionButton(ns('plusHealth'),'+',class = 'modButton'),
        actionButton(ns('plusTempHealth'),
                     glue::glue('+ {char$currentTempHP}'),
                     class = 'modButton',style = 'background-color:#6959CD')

    )
}

health = function(input, output, session){
    char = reactiveValues(maxHealth = char$maxHealth,
                          currentHealth = char$currentHealth,
                          currentTempHP = char$currentTempHP)

    observeEvent(input$minusHealth,{
            if(char$currentTempHP>0){
                char$currentTempHP = char$currentTempHP -1
                updateActionButton(session,
                                   'plusTempHealth',
                                   label = glue::glue('+ {char$currentTempHP}'))
            } else if(char$currentHealth>0){
                char$currentHealth = char$currentHealth -1

            }
    })

    observeEvent(input$plusHealth,{
        if(char$currentHealth<char$maxHealth){
            char$currentHealth = char$currentHealth + 1
        }
    })

    observeEvent(input$plusTempHealth,{
        char$currentTempHP = char$currentTempHP + 1
        updateActionButton(session,
                           'plusTempHealth',
                           label = glue::glue('+ {char$currentTempHP}'))
    })

    observe({
        if(char$currentTempHP>0){
            status = 'primary'
        } else{
            status = 'danger'
        }

        updateProgressBar(session,
                          id = session$ns('healthBar'),
                          value =  char$currentHealth + char$currentTempHP,
                          total = char$maxHealth + char$currentTempHP,
                          status= status)
    })

}

