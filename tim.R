library(import5eChar)

char = importCharacter('Tim_Fighter')

# initiative --------------
init()
# ability scores ----------
char$abilityMods
# saves -------------
save('Str')
save('Dex')
save('Con')
save('Chr')
save('Int')
save('Wis')

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

# second wind
# d10+figter level
r(r1d10) + 2

# attack --------------
# weapon attacks can be automated if details are provided within the app
# for more costumization see attack() function
w = char$weapons

char$bolt = 49
char$arrow = 20

# hand crossbow
weaponAttack(w$`Crossbow, hand`,ammo = 'bolt')
weaponAttack(w$`Crossbow, hand`,1,ammo = 'bolt')
weaponAttack(w$`Crossbow, hand`,-1,ammo = 'bolt')

weaponAttack(w$`Crossbow, hand`,sharpShoot = TRUE,ammo = 'bolt')
weaponAttack(w$`Crossbow, hand`,sharpShoot = TRUE,1,ammo = 'bolt')
weaponAttack(w$`Crossbow, hand`,sharpShoot = TRUE,-1,ammo = 'bolt')
# dagger
weaponAttack(w$Dagger)
weaponAttack(w$Dagger,1)
weaponAttack(w$Dagger,-1)

# longbow
weaponAttack(w$Longbow,useAmmo = TRUE,ammo = 'arrow')
weaponAttack(w$Longbow,1,useAmmo = TRUE,ammo = 'arrow')
weaponAttack(w$Longbow,-1,useAmmo = TRUE,ammo = 'arrow')

weaponAttack(w$Longbow,sharpShoot = TRUE,ammo = 'arrow')
weaponAttack(w$Longbow,1,sharpShoot = TRUE,ammo = 'arrow')
weaponAttack(w$Longbow,-1,sharpShoot = TRUE,ammo = 'arrow')

# inventory and stuff goes below


