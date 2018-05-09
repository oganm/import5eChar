#' @export
improvedInitiativeJSON = function(char = getOption('defaultCharacter')){

    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    saves = saveBonus(char) %>% {.[. != 0]}
    skills = skillBonus(char) %>% {.[. != 0]}

    iiJSON = list(Name = char$Name,
                  Source = '',
                  Type = '',
                  HP = list(Value = char$maxHealth,
                            Notes = ''),
                  AC = list( Value = AC(char = char),
                             Notes = ''),
                  InitiativeModifier = unname(initBonus(char) - char$abilityMods["Dex"]),
                  InitiativeAdvantage = FALSE,
                  Speed = c(char$baseSpeed + char$speedMiscMod,''),
                  Abilities = list(Str = char$abilityScores['Str'],
                                   Dex = char$abilityScores['Dex'],
                                   Con = char$abilityScores['Con'],
                                   Cha = char$abilityScores['Cha'],
                                   Int = char$abilityScores['Int'],
                                   Wis = char$abilityScores['Wis']),
                  DamageVulnerabilities = NULL,
                  DamageResistances = NULL,
                  ConditionImmunities = NULL,
                  Saves = lapply(seq_along(saves), function(i){
                      list(Name = names(saves)[i],
                           Modifier = unname(saves[i]))
                  }),
                  Skills = lapply(seq_along(skills), function(i){
                      list(Name = names(skills)[i],
                           Modifier = unname(skills[i]))
                  }),
                  Senses = NULL,
                  Languages = char$LanguagesKnown %>% strsplit('\n') %>% {c(.[[1]],'')},
                  Challenge = '',
                  Traits = NULL,
                  Actions = NULL,
                  Reactions = NULL,
                  LegendaryActions = NULL,
                  Description = '',
                  ImageURL =''
                  ) %>% jsonlite::toJSON(pretty = TRUE,auto_unbox = TRUE)

    return(iiJSON)
}
