#' @export
improvedInitiativeJSON = function(char = getOption('defaultCharacter')){

    if(is.character(char)){
        char = char %>% parse(text = .) %>% eval(envir = parent.frame())
    }
    saves = saveBonus(char) %>% {.[. != 0]}
    skills = skillBonus(char) %>% {.[. != 0]}

    iiJSON = list(Source = '',
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
                                   Int = char$abilityScores['Int'],
                                   Wis = char$abilityScores['Wis'],
                                   Cha = char$abilityScores['Cha']),
                  DamageVulnerabilities = list(),
                  DamageResistances =  list(),
                  ConditionImmunities = list(),
                  Saves = lapply(seq_along(saves), function(i){
                      list(Name = names(saves)[i],
                           Modifier = unname(saves[i]))
                  }),
                  Skills = lapply(seq_along(skills), function(i){
                      list(Name = names(skills)[i],
                           Modifier = unname(skills[i]))
                  }),
                  Senses = list(),
                  Languages = char$LanguagesKnown %>% strsplit('\n') %>% {c(.[[1]],'')},
                  Challenge = list(),
                  Traits = list(),
                  Actions = list(),
                  Reactions = list(),
                  LegendaryActions = list(),
                  Description = list(),
                  Player = 'player',
                  Version = '2.3.2',
                  ImageURL =list()
                  ) %>% jsonlite::toJSON(pretty = TRUE,auto_unbox = TRUE)

    return(iiJSON)
}
