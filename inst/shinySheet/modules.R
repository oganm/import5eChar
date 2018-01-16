characterDescriptionUI = function(id){
    ns = NS(id)
    tagList(htmlOutput(ns('description')))
}

characterDescription = function(input,output,session,char){
    output$description = renderUI({
        tagList(
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
        )
    })
}

healthUI = function(id){
    ns = NS(id)
    tagList(
        fluidRow(column(11,
                        progressBar(id = ns("healthBar"), value = 1,
                                    total = 1)),
                 column(1,
                        dropdownButton(
                            numericInput(inputId = ns('maxHealth'),
                                         label = 'Max health',
                                         value = -1),
                            circle = TRUE,
                            icon = icon("bars"),
                            size=  'xs',
                            width = '30px',
                            right = TRUE))),
        div(
            numericInput(inputId = ns('increment'),
                         label = '', value = 1,step =1, min = 1,
                         width = '70px'),style="display:inline-block"),
        actionButton(ns('minusHealth'),'-', class = 'modButton'),
        actionButton(ns('plusHealth'),'+',class = 'modButton'),
        actionButton(ns('plusTempHealth'),'+ 0',
                     class = 'modButton',style = 'background-color:#6959CD')

    )
}

health = function(input, output, session,
                  char
){
    init = reactiveVal(FALSE)

    observe({
        if(!init()){
            init(TRUE)
            updateNumericInput(session,inputId = 'maxHealth',value = char$maxHealth,min= 1)
        }
    })

    observe({
        if(init() & input$maxHealth != -1){
            char$maxHealth = input$maxHealth
            isolate({
                if(char$currentHealth > char$maxHealth){
                    char$currentHealth = char$maxHealth
                }
            })
        }
    })


    observeEvent(input$minusHealth,{

        healthDamage = min(0,char$currentTempHP - input$increment)

        char$currentTempHP = max(0,char$currentTempHP - input$increment)

        char$currentHealth =   max(0,char$currentHealth + healthDamage)


    })

    observeEvent(input$plusHealth,{
        char$currentHealth = min(char$maxHealth,char$currentHealth + input$increment)

    })

    observeEvent(input$plusTempHealth,{
        char$currentTempHP = char$currentTempHP + input$increment

    })

    observe({
        if(char$currentTempHP>0){
            status = 'primary'
        } else{
            status = 'danger'
        }

        updateActionButton(session,
                           'plusTempHealth',
                           label = glue::glue('+ {char$currentTempHP}'))

        updateProgressBar(session,
                          id = session$ns('healthBar'),
                          value =  char$currentHealth + char$currentTempHP,
                          total = char$maxHealth + char$currentTempHP,
                          status= status)
    })

}
