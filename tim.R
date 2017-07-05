library(import5eChar)

char = importCharacter('Tim_Fighter')

# by default the name getOption('defaultCharacter') returns 'char'.
# If another name is used use options(defaultCharacter = 'whatever') to set or manually
# specify char when running functions

# set a default attacking function for Tim the fighter.
timAttack = function(adv = 0,
                     sharpShoot = F,
                     attackStat = 'Dex',
                     damageDice = '1d6',
                     proficient = TRUE,
                     modToHit = 2, # tim's archery
                     modToDamage = 0,
                     useAmmo = TRUE,
                     ammo = 'bolt',
                     vocal = TRUE,
                     char = getOption('defaultCharacter')){

    attack(adv = adv,
            sharpShoot = sharpShoot,
            attackStat = attackStat,
            damageDice = damageDice,
            proficient = proficient,
            modToHit = modToHit, # tim's archery
            modToDamage = modToDamage,
            useAmmo = useAmmo,
            ammo = ammo,
            vocal = vocal,
            char = char)

}

# initiative --------------
init()
# ability scores ----------
char$abilityMods
# saves -------------
quickSave()

# ability checks
abilityCheck('Str')
abilityCheck('Dex')
abilityCheck('Con')
abilityCheck('Chr')
abilityCheck('Int')
abilityCheck('Wis')

# checks -----------
quickCheck()

skillCheck(athletics)

skillCheck(acrobatics)
skillCheck(sleight)
skillCheck(stealth)

skillCheck(arcana)
skillCheck(history)
skillCheck(investigation)
skillCheck(nature)
skillCheck(religion)

skillCheck(animal)
skillCheck(insight)
skillCheck(medicine)
skillCheck(perception)
skillCheck(survival)

skillCheck(deception)
skillCheck(intimidation)
skillCheck(persuasion)
skillCheck(performance)



# health ------------
char$maxHealth

# second wind *
# d10+figter level
r(r1d10) + 2

# action surge *

# attack --------------
# ammuntion update at the end of session
char$bolt = 49
char$arrow = 20

# hand crossbow
timAttack()
timAttack(1)
timAttack(-1)

timAttack(sharpShoot = TRUE)
timAttack(1,sharpShoot = TRUE)
timAttack(-1,sharpShoot = TRUE)

# dagger
timAttack(damageDice= '1d4',useAmmo = FALSE,modToHit = 0)
timAttack(1,damageDice= '1d4',useAmmo = FALSE,modToHit = 0)
timAttack(-1,damageDice= '1d4',useAmmo = FALSE,modToHit = 0)

# longbow
timAttack(damageDice = '1d8',ammo = 'arrow')
timAttack(1,damageDice = '1d8',ammo = 'arrow')
timAttack(-1,damageDice = '1d8',ammo = 'arrow')

timAttack(damageDice = '1d8',ammo = 'arrow',sharpShoot = TRUE)
timAttack(1,damageDice = '1d8',ammo = 'arrow',sharpShoot = TRUE)
timAttack(-1,damageDice = '1d8',ammo = 'arrow',sharpShoot = TRUE)

# other notes below
