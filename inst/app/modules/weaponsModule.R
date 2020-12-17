
weaponsUI = function(id){
    ns = NS(id)
    tagList(
        # not strictly necesary as the function is already defined. before. keeping
        # it to ensure it'll work on its own
        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                    Shiny.onInputChange(variableName, null);
});"),
        fluidRow(column(6,
                        radioGroupButtons(inputId = ns('advantage'),
                                          choices = c('DisAdv','Norm','Adv'),
                                          selected = 'Norm',
                                          status = "primary"),
                        actionButton(ns('attackCounter'),label = 'Attack counter: 0', class = 'modButton'),
                        bsTooltip(ns('attackCounter'),'Click to reset',placement = 'bottom')),
                 column(6,
                        switchInput(ns('sharpshoot'),label = 'SharpS/GreatWM'),
                        fluidRow(column(6,
                                        textInput(ns('attackBonus'), placeholder = 'eg. 1d4 or 2',label = 'Attack bonus')
                        ),
                        column(6,
                               textInput(ns('damageBonus'), placeholder = 'eg. 1d4 or 2',label = 'Damage bonus'))))),
        dataTableOutput(ns('weaponsTable'))
    )
}

weapons =function(input, output,session,char){

    attackCount = reactiveVal(value = 0)

    imagePick = function(weapon){
        if(grepl('crossbow', tolower(weapon$name))){
            icon = 'icons/crossbow.png'
        } else if(grepl('bow', tolower(weapon$name))){
            icon = 'icons/high-shot.png'
        } else if(grepl('net', tolower(weapon$name))){
            icon = 'icons/fishing-net.png'
        } else if(grepl('blowgun|dart',tolower(weapon$name))){
            icon = 'icons/dart.png'
        } else if(grepl('whip',tolower(weapon$name))){
            icon = 'icons/whip.png'
        } else if(grepl('hammer|pick|maul', tolower(weapon$name))){
            icon = 'icons/claw-hammer.png'
        } else if(grepl('trident|wave', tolower(weapon$name))){
            icon = 'icons/trident.png'
        } else if(grepl('sword|rapier|scimitar|dagger', tolower(weapon$name))){
            icon = 'icons/pointy-sword.png'
        } else if(grepl('pike|spear|lance|glaive', tolower(weapon$name))){
            icon = 'icons/spear-hook.png'
        } else if(grepl('halberd', tolower(weapon$name))){
            icon = 'icons/sharp-halberd.png'
        } else if(grepl('axe',tolower(weapon$name))){
            icon = 'icons/battered-axe.png'
        } else if(grepl('flail',tolower(weapon$name))){
            icon = 'icons/flail.png'
        } else if(grepl('sling',tolower(weapon$name))){
            icon = 'icons/sling.png'
        } else if(grepl('sickle',tolower(weapon$name))){
            icon = 'icons/sickle.png'
        } else if(grepl('quarterstaff',tolower(weapon$name))){
            icon = 'icons/bo.png'
        } else if(grepl('mace|morningstar',tolower(weapon$name))){
            icon = 'icons/flanged-mace.png'
        } else if(grepl('javelin',tolower(weapon$name))){
            icon = 'icons/thrown-spear.png'
        } else if(grepl('club',tolower(weapon$name))){
            icon = 'icons/wood-club.png'
        } else if(grepl('unarmed',tolower(weapon$name))){
            icon = 'icons/fist.png'
        } else if(weapon$type =='ranged'){
            icon = 'icons/high-shot.png'
        } else if(weapon$damageType =='Bludgeoning'){
            icon = 'icons/flanged-mace.png'
        } else{
            icon = 'icons/pointy-sword.png'
        }

        return(img(src = icon,width = 20,height = 20))
    }
    output$weaponsTable = renderDataTable({
        if(length(char$weapons) == 0){
            return(NULL)
        }
        weaponTable = char$weapons %>% sapply(function(x){



            c(actionButton(label = div(imagePick(x),x$name),
                           inputId=x$name,
                           onclick = glue('Shiny.onInputChange("',session$ns('weaponButton'),'",  this.id)'),
                           class = 'weaponButton')%>%
                  as.character,
              weaponBonus(x,char=char)['weaponTypeAttackBonus'] +
                  x$proficient*char$proficiencyBonus +
                  char$abilityMods[x$attackStat],
              paste0(paste(x$dice,collapse=' + '),
                     '+',
                     weaponBonus(x,char=char)['weaponTypeDamageBonus'] +
                         char$abilityMods[x$attackStat]),
              x$damageType,
              x$range
            )
        }) %>% t %>% as.data.frame()
        names(weaponTable) = c('name','Atk Bonus','Damage','Type','Range')


        table = datatable(weaponTable,escape = FALSE,selection = 'none',rownames = FALSE,
                          options = list(bFilter = 0,
                                         bLengthChange = 0,
                                         paging = 0,
                                         ordering = 0,
                                         bInfo = 0,
                                         pageLength = nrow(weaponTable)))
        return(table)
    })

    observeEvent(input$attackCounter,{
        attackCount(0)
        updateActionButton(session = session, 'attackCounter',label = 'Attack counter: 0')
    })




    out = reactive({
        out = ''

        if(!is.null(input$weaponButton)){
            advantage = switch(input$advantage,
                               Norm = 0,
                               DisAdv = -1,
                               Adv = 1)

            w = char$weapons
            modToHit = 0
            modToDamage = 0
            hitBonutText = ''
            damageBonusText = ''
            if(input$attackBonus!=''){
                modToHit = tryCatch(roll(input$attackBonus),
                         error = function(e){
                             as.integer(input$attackBonus)
                         })

                if(is.na(modToHit)){
                    modToHit = 0
                } else{
                    hitBonutText = glue::glue('Adding {modToHit} to attack roll\n\n')
                }
            }

            if(input$damageBonus!=''){
                modToDamage = tryCatch(roll(input$damageBonus),
                                    error = function(e){
                                        as.integer(input$attackBonus)
                                    })

                if(is.na(modToDamage)){
                    modToDamage = 0
                } else{
                    damageBonusText = glue::glue('Adding {modToDamage} to damage roll\n\n')
                }
            }


            out = glue(input$weaponButton,':\n',hitBonutText,damageBonusText,
                       capture.output(weaponAttack(w[[input$weaponButton]],
                                                   modToHit = modToHit,
                                                   modToDamage = modToDamage,
                                                   sharpShoot =input$sharpshoot,
                                                   adv = advantage,
                                                   char = char)) %>%
                           gsub('(\\[1\\] )|"','',.) %>%
                           paste(collapse = '\n'))

            session$sendCustomMessage(type = 'resetInputValue',
                                      message =  session$ns('weaponButton'))


            isolate({
                attackCount(attackCount()+1)
                updateActionButton(session = session,'attackCounter',label = glue::glue('Attack counter: {attackCount()}'))

            })
        }

        return(out)
    })

    return(out)
}
