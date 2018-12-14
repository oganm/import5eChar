characterDescriptionUI = function(id){
    ns = NS(id)
    tagList(htmlOutput(ns('description')))
}

characterDescription = function(input,output,session,char,charInitial){

    inputUserid <- function(inputId, value='') {
        #   print(paste(inputId, "=", value))
        tagList(
            singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
            singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
            tags$body(onload="setvalues()"),
            tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
        )
    }

    inputIp <- function(inputId, value='', ns){
        tagList(
            singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
            singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
            tags$body(onload="setvalues()"),
            tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
        )
    }
    saveCharacter = function(characterFile, consent, fingerprint = ''){
        randomName = tools::md5sum(characterFile)
        if(consent){
            dir.create('chars',showWarnings = FALSE)
            file.copy(characterFile, file.path('chars',paste0(fingerprint,'_',randomName)))
        } else {
            dir.create('naysayer',showWarnings = FALSE)
            file.create(file.path('naysayer',paste0(fingerprint,'_',randomName)))
        }
    }

    output$description = renderUI({
        descriptiveElement = function(field,label){
            tagList(
                p(field,class = 'narrowElement field'),
                hr(class = 'narrowElement'),
                p(label, class = 'narrowElement minorText')
            )
        }

        tagList(
            fluidRow(
                {
                    if(!is.null(getOption('ImTheWebClient'))){
                        tagList(inputIp(session$ns("ipid")),
                                inputUserid(session$ns("fingerprint")))
                    } else  {
                        NULL
                    }
                },

                column(5,
                       wellPanel(fluidRow(
                           column(5,h2(char$Name)),
                           column(4,
                                  {
                                      if(is.null(getOption('ImThePortableClient')) & is.null(getOption('ImTheWebClient'))){
                                          tagList(
                                              div(textInput(session$ns('driveInput'),label = 'G Drive Import',width = '150px') ,style= 'display: inline-block'),
                                              actionButton(session$ns('driveSubmit'),label = '',icon = icon('check'),class = 'modButton',style = 'display: inline-block'),
                                              bsTooltip(session$ns('driveSubmit'),'Search in Google Drive',placement = 'bottom')
                                          )
                                      } else{
                                          NULL
                                      }
                                  }
                           ),
                           column(3,
                                  fileInput(session$ns('charInput'),label = 'Local import'),
                                  div(id=session$ns('consentDiv') , checkboxInput(inputId = session$ns('consent'),label = 'Can I keep a copy?', value = TRUE), style = 'font-size:70%'),
                                  bsTooltip(session$ns('charInput'),'Load local file',placement = 'bottom'),
                                  bsTooltip(session$ns('consentDiv'),
                                            title = "If the box is checked I save a copy of the uploaded character sheet. I use these saved sheets as test cases when improving the application. I also plan to use them for some statistical analyses examining character building choices and release this analysis publicly. The characters remain your intellectual property. If you\\'d rather I didn\\'t save your character, uncheck this box. I won\\'t be mad. Only dissapointed"))
                       )
                       )
                ),
                column(7,
                       wellPanel(
                           fluidRow(
                               column(2, descriptiveElement(AC(char),'AC')),
                               column(2,
                                      actionButton(session$ns('init'), label = initBonus(char), class = 'skillButton'),
                                      hr(class = 'narrowElement'),
                                      p('Initiative', class = 'narrowElement minorText')
                               ),
                               column(4, descriptiveElement(char$ClassField,'Class & Level')),
                               column(4, descriptiveElement(char$Background,'Background'))

                           ),
                           fluidRow(
                               column(2, descriptiveElement(char$proficiencyBonus,'Proficiency')),
                               column(2, descriptiveElement(char$baseSpeed, 'Speed')),
                               column(4, descriptiveElement(char$Race, 'Race')),
                               column(4,descriptiveElement(char$Alignment, 'Alignment'))
                           )
                       ))
            )
        )
    })

    observe({
        input$charInput
        if(is.null(getOption('ImTheWebClient'))){
            hide('consentDiv')

        }
    })

    observe({
        input$charInput
        if(!is.null(input$charInput)){
            character = importCharacter(file = input$charInput$datapath)
            isolate({
                for(x in names(reactiveValuesToList(char))){
                    char[[x]] = character[[x]]
                }
                for(x in names(reactiveValuesToList(charInitial))){
                    charInitial[[x]] = character[[x]]
                }
                if(!is.null(getOption('ImTheWebClient'))){
                    saveCharacter(input$charInput$datapath, input$consent, paste0(input$fingerprint,'_',input$ipid))
                }

            })

        }
    })

    observe({
        input$driveSubmit
        isolate({
            if(!is.null(input$driveSubmit) && !is.null(input$driveInput) && input$driveInput != ''){
                withProgress({
                    googledrive::drive_auth(cache=TRUE)
                    character = importCharacter(regex = input$driveInput)
                    for(x in names(reactiveValuesToList(char))){
                        char[[x]] = character[[x]]
                    }
                    for(x in names(reactiveValuesToList(charInitial))){
                        charInitial[[x]] = character[[x]]
                    }
                },value  =0.5 ,message = 'Reading from Google Drive')
            }
        })
    })

    out = reactive({
        out = ''
        if(!is.null(input$init) && input$init > 0){
            out = paste0('Initiative:\n',
                         capture.output(init(char)) %>%
                             gsub('(\\[1\\] )|"','',.) %>%
                             paste(collapse = '\n'))
            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('init'))

        }
        return(out)
    })

    return(out)

}
