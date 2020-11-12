processHit = function(attackData,advantage = "Norm"){

    if(!is.null(attackData)){

        attackRoll = switch(advantage,
            Norm = roll('1d20'),
            Adv = roll('2d20d1'),
            DisAdv = roll('2d20dh1')
        )


        # attackRoll = roll('1d20')
        attack = attackRoll + attackData$attack

        if('' == attackData$damage){
            attackData$damage = '0'
        }


        if(attackRoll == 1){
            damageText = 'CRITICAL MISS'
            damage = 0
        } else if(attackRoll == 20){
            critDamage = stringr::str_replace_all(attackData$damage, "\\d+(?=d|D)", function(m) as.integer(m) * 2)

            damageText = capture.output({damage = diceSyntax::roll(critDamage)}) %>%
                gsub('(\\[1\\] )|"','',.) %>%  paste0(collapse = '\n')
        } else{

            damageText = capture.output({damage = diceSyntax::roll(attackData$damage)}) %>%
                gsub('(\\[1\\] )|"','',.)  %>% paste0(collapse = '\n')


        }

        roll = c('Attack'= attack, 'Damage' = damage,
                 crit = (attackRoll == 20)-(attackRoll == 1))

        if(advantage == 'Norm'){
            out = glue::glue("Pet's {attackData$name}:\n",
                             "{capture.output(roll) %>% paste0(collapse = '\n')}")
        } else{
            out = glue::glue("Pet's {attackData$name} {advantage}:\n",
                             "{capture.output(roll) %>% paste0(collapse = '\n')}")
        }


    } else{
        out = ''
    }


    return(out)

}

monsters = monsteR::monsters



monsters$`Homonculus Servant` = list(
    HP = 8, # minimum HP,
    AC = 13,
    abilityScores = c(Str = 4, Dex = 15, Con = 12, Int = 10, Wis = 10, Cha = 7),
    saves = c(Str = -3, Dex = 4, Con = 1, Int = 0, Wis = 0, Cha = -2),
    actions = list(
        `Force Strike` = list(
            name = 'Force Strike',
            attack_bonus = 4,
            damage_dice = '1d4',
            damage_bonus = 2
        )
    )
)
monsters = c(monsters[length(monsters)],monsters[-length(monsters)])

monsters$`Steel Defender` = list(
    HP = 17, # minimum hp
    AC = 15, # change at level 15
    abilityScores = c(Str = 14, Dex = 12, Con = 14, Int = 4, Wis = 10, Cha = 6),
    saves = c(Str = 2, Dex = 3, Con = 4, Int = -3, Wis = 0, Cha = -2),
    actions = list(
        `Force-Empowered Rend`= list(
            name = 'Force-Empowered Rend',
            attack_bonus = 4,
            damage_dice = '1d8',
            damage_bonus = 2,
            saves  = c(Str = 2, Dex = 3, Con = 4, Int = -4, Wis = 0, Cha = -2)),
        Repair = list(
            name = 'Repair',
            attack_bonus = 0,
            damage_dice = '2d8',
            damage_bonus = 2,
            saves  = c(Str = 2, Dex = 3, Con = 4, Int = -4, Wis = 0, Cha = -2)),
        Mending = list(
            name = 'Mending',
            attack_bonus = 0,
            damage_dice = '2d6',
            damage_bonus = 0,
            saves  = c(Str = 2, Dex = 3, Con = 4, Int = -4, Wis = 0, Cha = -2))
        )
    )

skillAttributes = c('Str',
                    rep('Dex',3),
                    rep('Int',5),
                    rep('Wis',5),
                    rep('Cha',4))


monsters$`Steel Defender`$skills = stat2mod(monsters$`Steel Defender`$abilityScores)[skillAttributes]
monsters$`Homonculus Servant`$skills =  stat2mod(monsters$`Homonculus Servant`$abilityScores)[skillAttributes]

names(monsters$`Steel Defender`$skills) = names(monsters$Aboleth$skills)
names(monsters$`Homonculus Servant`$skills) = names(monsters$Aboleth$skills)


monsters$`Steel Defender`$skills['Athletics'] = 4
monsters$`Steel Defender`$skills['Perception'] = 4

monsters$`Homonculus Servant`$skills['Perception'] = 4
monsters$`Homonculus Servant`$skills['Stealth'] = 4

monsters = c(monsters[length(monsters)],monsters[-length(monsters)])


monsters$`Animate Objects` =
    list(actions =
             list(
                 'Tiny' = list(
                     name = 'Tiny',
                     attack_bonus = 8,
                     damage_dice = '1d4',
                     damage_bonus = 5,
                     default_count = 10,
                     saves = c(Str = -3, Dex = 4, Con = 0, Int = -4, Wis = -4, Cha = -5)),
                 'Small' = list(
                     name = 'Small',
                     attack_bonus = 6,
                     damage_dice = '1d8',
                     damage_bonus = 2,
                     default_count = 10,
                     saves = c(Str = -2, Dex = 3, Con = 0, Int = -4, Wis = -4, Cha = -5)),
                 "Medium" = list(
                     name = 'Medium',
                     attack_bonus = 5,
                     damage_dice = '2d6',
                     damage_bonus = 1,
                     default_count = 5,
                     saves = c(Str = 0, Dex = 2, Con = 0, Int = -4, Wis = -4, Cha = -5)),
                 "Large" = list(
                     name = 'Large',
                     attack_bonus = 6,
                     damage_dice = '2d10',
                     damage_bonus = 2,
                     default_count = 2,
                     saves = c(Str = 2, Dex = 0, Con = 0, Int = -4, Wis = -4, Cha = -5)),
                 "Huge" = list(
                     name = 'Huge',
                     attack_bonus = 8,
                     damage_dice = '2d12',
                     damage_bonus = 2,
                     saves = c(Str = 4, Dex = -2, Con = 0, Int = -4, Wis = -4, Cha = -5))
             ),
         HP = 80
    )


monsters = c(monsters[length(monsters)],monsters[-length(monsters)])



petUI = function(id){
    ns = NS(id)
    tagList(
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
});"),
        wellPanel(style = 'overflow-y:scroll;max-height: 400px',
            fluidRow(
                column(6,
                       healthUI(ns('petHealth'))),
                       # htmlOutput(ns('HPSliderColorController')),
                       # div(class = ns('HPsliderDiv'),
                       #     sliderInput(inputId = ns('healthSlider'), value = 1, label = 'HP',min = 0,max = 1,step = 1, width = '100%')
                       # ),
                       # div(
                       #     numericInput(inputId = ns('increment'),
                       #                  label = '', value = 1,step =1, min = 1,
                       #                  width = '70px'),style="display:inline-block"),
                       # actionButton(ns('minusHealth'),'-', class = 'modButton'),
                       # actionButton(ns('plusHealth'),'+',class = 'modButton'),
                       # actionButton(ns('plusTempHealth'),'+ 0',
                       #              class = 'modButton',style = 'background-color:#6959CD'),
                       # actionButton(ns('setHealth'),'Set', class = 'modButton')),
                column(6,
                       shiny::selectInput(inputId = ns('monster'),label = 'Load SRD monster',choices = c('',names(monsters))),
                       numericInput(ns('AC'),label = 'AC',value = 10,width = '70px'),
                       radioGroupButtons(inputId = ns('advantage'),
                                         choices = c('DisAdv','Norm','Adv'),
                                         selected = 'Norm',
                                         status = "primary"))

            ),
            fluidRow(
                column(7,
                       fluidRow(
                           div(textInput(ns('action1-name'),label = 'Attack',width = '140px'),style="display:inline-block"),
                           div(numericInput(ns('action1-attack'),label = 'Atk Bonus',value = 0,width = '70px'),style="display:inline-block"),
                           div(textInput(ns('action1-damage'), label = 'Damage',width = '90px'),style="display:inline-block"),
                           actionButton(ns('action1-button'),label = 'Attack!', class = 'weaponButton')
                       ),
                       fluidRow(
                           div(textInput(ns('action2-name'),label = '',width = '140px'),style="display:inline-block"),
                           div(numericInput(ns('action2-attack'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           div(textInput(ns('action2-damage'), label = '',width = '90px'),style="display:inline-block"),
                           actionButton(ns('action2-button'),label = 'Attack!', class = 'weaponButton')
                       ),
                       fluidRow(
                           div(textInput(ns('action3-name'),label = '',width = '140px'),style="display:inline-block"),
                           div(numericInput(ns('action3-attack'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           div(textInput(ns('action3-damage'), label = '',width = '90px'),style="display:inline-block"),
                           actionButton(ns('action3-button'),label = 'Attack!', class = 'weaponButton')
                       ),
                       fluidRow(
                           div(textInput(ns('action4-name'),label = '',width = '140px'),style="display:inline-block"),
                           div(numericInput(ns('action4-attack'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           div(textInput(ns('action4-damage'), label = '',width = '90px'),style="display:inline-block"),
                           actionButton(ns('action4-button'),label = 'Attack!', class = 'weaponButton')
                       ),
                       fluidRow(
                           div(textInput(ns('action5-name'),label = '',width = '140px'),style="display:inline-block"),
                           div(numericInput(ns('action5-attack'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           div(textInput(ns('action5-damage'), label = '',width = '90px'),style="display:inline-block"),
                           actionButton(ns('action5-button'),label = 'Attack!', class = 'weaponButton')
                       )),
                column(5,
                       fluidRow(
                           actionButton(ns('str'), label='Str:',class = 'weaponButton',width = '60px',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                           div(numericInput(ns('str-mod'), label = '', value = 0, width = '70px'),style="display:inline-block")
                       ),
                       fluidRow(
                           actionButton(ns('dex'), label='Dex:',class = 'weaponButton',width = '60px',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                           div(numericInput(ns('dex-mod'), label = '', value = 0, width = '70px'),style="display:inline-block")
                       ),
                       fluidRow(
                           actionButton(ns('con'), label='Con:',class = 'weaponButton',width = '60px',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                           div(numericInput(ns('con-mod'), label = '', value = 0, width = '70px'),style="display:inline-block")
                       ),
                       fluidRow(
                           actionButton(ns('int'), label='Int:',class = 'weaponButton',width = '60px',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                           div(numericInput(ns('int-mod'), label = '', value = 0, width = '70px'),style="display:inline-block")
                       ),
                       fluidRow(
                           actionButton(ns('wis'), label='Wis:',class = 'weaponButton',width = '60px',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                           div(numericInput(ns('wis-mod'), label = '', value = 0, width = '70px'),style="display:inline-block")
                       ),
                       fluidRow(
                           actionButton(ns('cha'), label='Cha:',class = 'weaponButton',width = '60px',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                           div(numericInput(ns('cha-mod'), label = '', value = 0, width = '70px'),style="display:inline-block")
                       ),
                       fluidRow(
                           div(textInput(ns('extra1-name'),label = '',width = '120px'),style="display:inline-block"),
                           div(numericInput(ns('extra1-bonus'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           actionButton(ns('extra1-button'),label = div(img(src = 'icons/dice-twenty-faces-twenty.png',
                                                                            height = 20,
                                                                            width = 20)), class = 'weaponButton',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                       ),
                       fluidRow(
                           div(textInput(ns('extra2-name'),label = '',width = '120px'),style="display:inline-block"),
                           div(numericInput(ns('extra2-bonus'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           actionButton(ns('extra2-button'),label = div(img(src = 'icons/dice-twenty-faces-twenty.png',
                                                                            height = 20,
                                                                            width = 20)), class = 'weaponButton',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                       ),
                       fluidRow(
                           div(textInput(ns('extra3-name'),label = '',width = '120px'),style="display:inline-block"),
                           div(numericInput(ns('extra3-bonus'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           actionButton(ns('extra3-button'),label = div(img(src = 'icons/dice-twenty-faces-twenty.png',
                                                                            height = 20,
                                                                            width = 20)), class = 'weaponButton',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                       ),
                       fluidRow(
                           div(textInput(ns('extra4-name'),label = '',width = '120px'),style="display:inline-block"),
                           div(numericInput(ns('extra4-bonus'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           actionButton(ns('extra4-button'),label = div(img(src = 'icons/dice-twenty-faces-twenty.png',
                                                                            height = 20,
                                                                            width = 20)), class = 'weaponButton',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                       ),
                       fluidRow(
                           div(textInput(ns('extra5-name'),label = '',width = '120px'),style="display:inline-block"),
                           div(numericInput(ns('extra5-bonus'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           actionButton(ns('extra5-button'),label = div(img(src = 'icons/dice-twenty-faces-twenty.png',
                                                                            height = 20,
                                                                            width = 20)), class = 'weaponButton',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                       ),
                       fluidRow(
                           div(textInput(ns('extra6-name'),label = '',width = '120px'),style="display:inline-block"),
                           div(numericInput(ns('extra6-bonus'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           actionButton(ns('extra6-button'),label = div(img(src = 'icons/dice-twenty-faces-twenty.png',
                                                                            height = 20,
                                                                            width = 20)), class = 'weaponButton',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                       ),
                       fluidRow(
                           div(textInput(ns('extra7-name'),label = '',width = '120px'),style="display:inline-block"),
                           div(numericInput(ns('extra7-bonus'),label = '',value = 0,width = '70px'),style="display:inline-block"),
                           actionButton(ns('extra7-button'),label = div(img(src = 'icons/dice-twenty-faces-twenty.png',
                                                                            height = 20,
                                                                            width = 20)), class = 'weaponButton',
                                        onclick = glue('Shiny.onInputChange("',ns('d20button'),'",  this.id)')),
                       )

                )
            )


        )
    )
}

pet = function(input,output,session, char){




    reset = reactiveVal(value = FALSE)
    out = reactiveVal(value = '')

    pet = reactiveValues(currentHealth = 1,
                         currentTempHP = 0,
                         maxHealth = 1)
    petInitial = reactiveValues(currentHealth = 1,
                                currentTempHP = 0,
                                maxHealth = 1)

    actions = reactiveVal(value =
                              list(action1 = list(name = 'Attack',
                                                  attack_bonus = 0,
                                                  damage_dice = 0,
                                                  damage_bonus = 0),
                                   action1 = list(name = 'Attack',
                                                  attack_bonus = 0,
                                                  damage_dice = 0,
                                                  damage_bonus = 0),
                                   action2 = list(name = 'Attack',
                                                  attack_bonus = 0,
                                                  damage_dice = 0,
                                                  damage_bonus = 0),
                                   action3 = list(name = 'Attack',
                                                  attack_bonus = 0,
                                                  damage_dice = 0,
                                                  damage_bonus = 0),
                                   action4 = list(name = 'Attack',
                                                  attack_bonus = 0,
                                                  damage_dice = 0,
                                                  damage_bonus = 0),
                                   action5 = list(name = 'Attack',
                                                  attack_bonus = 0,
                                                  damage_dice = 0,
                                                  damage_bonus = 0)))

    observeEvent(char$classInfo,{
        if(any(char$classInfo[,'Archetype'] == 'Battle Smith')){
            updateSelectInput(session = session,
                              inputId = 'monster',
                              label = 'Load SRD monster',
                              choices = c('',names(monsters)),
                              selected = 'Steel Defender')
        }
    },priority = -9999)


    observeEvent(input$monster,{


        if(any(char$classInfo[,'Class']=='Artificer')){
            level = char$classInfo[char$classInfo[,'Class'] == 'Artificer','Level'] %>% as.numeric()
            proficiency = char$proficiencyBonus

            monsters$`Homonculus Servant`$HP = unname(level+ 1 + char$abilityMods['Int'])
            monsters$`Homonculus Servant`$saves['Dex'] = 2 + char$proficiencyBonus
            monsters$`Homonculus Servant`$skills['Perception'] = 2*char$proficiencyBonus
            monsters$`Homonculus Servant`$skills['Stealth'] = 2 + char$proficiencyBonus

            monsters$`Homonculus Servant`$actions$`Force Strike`$attack_bonus = unname(char$abilityMods['Int']) + char$proficiencyBonus
            monsters$`Homonculus Servant`$actions$`Force Strike`$damage_bonus = char$proficiencyBonus
        }

        if(any(char$classInfo[,'Archetype'] == 'Battle Smith')){
            level = char$classInfo[char$classInfo[,'Archetype'] == 'Battle Smith','Level'] %>% as.numeric()
            proficiency = char$proficiencyBonus

            monsters$`Steel Defender`$HP = unname(level*5+2+char$abilityMods['Int'])
            monsters$`Steel Defender`$actions$`Force-Empowered Rend`$attack_bonus = unname(char$abilityMods['Int']) +  char$proficiencyBonus
            monsters$`Steel Defender`$actions$`Force-Empowered Rend`$damage_bonus = char$proficiencyBonus
            monsters$`Steel Defender`$skills['Perception'] = 2*char$proficiencyBonus
            monsters$`Steel Defender`$skills['Athletics'] = 2 + char$proficiencyBonus
            monsters$`Steel Defender`$saves['Dex'] = 1 + char$proficiencyBonus
            monsters$`Steel Defender`$saves['Con'] = 2 + char$proficiencyBonus

            monsters$`Steel Defender`$actions$Repair$damage_bonus = char$proficiencyBonus

            if(level>=15){
                monsters$`Steel Defender`$AC = 17
            }
        }

        if(!is.null(input$monster) && input$monster!= ''){
            monster = monsters[[input$monster]]


            pet$currentHealth = monster$HP
            pet$maxHealth = monster$HP

            petInitial$currentHealth = monster$HP
            petInitial$maxHealth = monster$HP


            validActions = monsteR:::attackable(monster$actions)

            updateNumericInput(session,'AC',value = monster$AC)
            updateNumericInput(session,'str-mod',value = stat2mod(monster$abilityScores['Str']) %>% unname)
            updateNumericInput(session,'dex-mod',value = stat2mod(monster$abilityScores['Dex']) %>% unname)
            updateNumericInput(session,'con-mod',value = stat2mod(monster$abilityScores['Con']) %>% unname)
            updateNumericInput(session,'wis-mod',value = stat2mod(monster$abilityScores['Wis']) %>% unname)
            updateNumericInput(session,'int-mod',value = stat2mod(monster$abilityScores['Int']) %>% unname)
            updateNumericInput(session,'cha-mod',value = stat2mod(monster$abilityScores['Cha']) %>% unname)

            specialSaves = (monster$saves - stat2mod(monster$abilityScores)) %>% {.[!. %in% 0]} %>% names %>% {monster$saves[.]}

            specialSkills = (monster$skills - stat2mod(monster$abilityScores)[skillAttributes]) %>% {.[!. %in% 0]} %>% names %>% {monster$skills[.]}

            extraPosition = 1

            while(extraPosition<=7){
                for(i in seq_along(specialSaves)){
                    updateTextInput(session,inputId = glue::glue('extra{extraPosition}-name'),
                                    value = glue::glue('{names(specialSaves)[i]} save'))

                    updateNumericInput(session,glue::glue('extra{extraPosition}-bonus'),
                                       value = unname(specialSaves)[i])
                    extraPosition = extraPosition + 1

                }

                for(i in seq_along(specialSkills)){
                    updateTextInput(session,inputId = glue::glue('extra{extraPosition}-name'),
                                    value = glue::glue('{names(specialSkills)[i]}'))
                    updateNumericInput(session,glue::glue('extra{extraPosition}-bonus'),
                                       value = unname(specialSkills)[i])
                    extraPosition = extraPosition + 1

                }

                if(extraPosition<=7){
                    for(i in seq(extraPosition, 7,by = 1)){
                        updateTextInput(session,inputId = glue::glue('extra{extraPosition}-name'),
                                        value = '')
                        updateNumericInput(session,glue::glue('extra{extraPosition}-bonus'),
                                           value = 0)
                        extraPosition = extraPosition+1
                    }

                }
            }

            for(i in seq(1,5)){
                if(!is.na(validActions[i])){
                    action = monster$actions[[validActions[i]]]
                    if(is.null(action$damage_bonus)){
                        action$damage_bonus = 0
                    }

                    updateTextInput(session,inputId = glue::glue('action{i}-name'), value = action$name)
                    updateNumericInput(session,inputId = glue::glue('action{i}-attack'), value = action$attack_bonus)
                    updateTextInput(session,
                                    inputId = glue::glue('action{i}-damage'),
                                    value = glue::glue("{action$damage_dice} {ifelse(action$damage_bonus >0, '+','')} {ifelse(action$damage_bonus != 0, action$damage_bonus, '')}"))
                } else{
                    updateTextInput(session,inputId = glue::glue('action{i}-name'), value = '')
                    updateNumericInput(session,inputId = glue::glue('action{i}-attack'), value = 0)
                    updateTextInput(session,inputId = glue::glue('action{i}-damage'), value = '')

                }


            }


        }


    })


    callModule(health,id = 'petHealth',
               char = pet,
               charInitial = petInitial)


    observeEvent(input$`action1-button`,{
        attackData = list(name = input$`action1-name`,
                          attack = input$`action1-attack`,
                          damage = input$`action1-damage`)

        attack = processHit(attackData, input$advantage)
        out(attack)
        reset(TRUE)

    })

    observeEvent(input$d20button,{
        print(input$d20button)

        if(!is.null(input$d20button)){
            roll = switch(input$advantage,
                                Norm = roll('1d20'),
                                Adv = roll('2d20d1'),
                                DisAdv = roll('2d20dh1')
            )

            baseButton = strsplit(input$d20button,'-') %>% {.[[1]]} %>% {.[length(.)]}

            if(baseButton %in% c('str','dex','con','wis','int','cha')){
                mod = input[[paste0(baseButton,'-mod')]]
                rollName = baseButton %>% str_to_title()

            } else{
                whichExtra = str_extract(input$d20button,'(?<=extra)[0-9](?=\\-button$)') %>% as.integer()
                mod = input[[glue::glue('extra{whichExtra}-bonus')]]
                rollName = input[[glue::glue('extra{whichExtra}-name')]]
            }


            if(input$advantage !='Norm'){
                text = glue::glue('Pet\'s {rollName} {input$advantage}:\n{roll+mod}')
            } else{
                text = glue::glue('Pet\'s {rollName}:\n{roll+mod}')
            }
            out(text)
            reset(TRUE)
        }

        session$sendCustomMessage(type = 'resetInputValue',
                                  message =  session$ns('d20button'))
    })

    observeEvent(input$`action1-button`,{
        attackData = list(name = input$`action1-name`,
                          attack = input$`action1-attack`,
                          damage = input$`action1-damage`)

        attack = processHit(attackData, input$advantage)
        out(attack)
        reset(TRUE)

    })

    observeEvent(input$`action2-button`,{
        attackData = list(name = input$`action2-name`,
                          attack = input$`action2-attack`,
                          damage = input$`action2-damage`)

        attack = processHit(attackData, input$advantage)
        out(attack)
        reset(TRUE)

    })

    observeEvent(input$`action3-button`,{
        attackData = list(name = input$`action3-name`,
                          attack = input$`action3-attack`,
                          damage = input$`action3-damage`)

        attack = processHit(attackData, input$advantage)
        out(attack)
        reset(TRUE)

    })

    observeEvent(input$`action4-button`,{
        attackData = list(name = input$`action4-name`,
                          attack = input$`action4-attack`,
                          damage = input$`action4-damage`)

        attack = processHit(attackData, input$advantage)
        out(attack)
        reset(TRUE)

    })

    observeEvent(input$`action5-button`,{
        attackData = list(name = input$`action5-name`,
                          attack = input$`action5-attack`,
                          damage = input$`action5-damage`)

        attack = processHit(attackData, input$advantage)
        out(attack)
        reset(TRUE)

    })

    observe({
        if(reset()){
            out('')
            reset(FALSE)
        }
    })


    return(out)


}
