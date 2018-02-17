
import5echar
============

Walter Kammerer's [Fifth Edition Character Sheet](https://play.google.com/store/apps/details?id=com.wgkammerer.testgui.basiccharactersheet.app&hl=en) app allows you to upload your character to google drive. This package parses that file and allows you to use character data for automation.

Installation
------------

    devtools::install_github('oganm/import5echar')

Basics
------

First thing that need to be done is to import yoour character. You can either use a regex or a google drive file id (acquirable using `googledrive` package). If a regex is used, last modified file that matches the regex is pulled which is probably your latest export. First time you run the line below, you'll have to authenticate with google drive.

``` r
char = importCharacter('Tim_Fighter')
```

If you don't want to authenticate with google drive, a local file can be used

``` r
characterFile <- system.file("Tim_Fighter5", package = "import5eChar")
char = import5eChar::importCharacter(file = characterFile)
```

Once a character is loaded convenience functions can be used

``` r
init() # initiative roll
```

    ## [1] "Rolls: [ 6 ]"

    ## [1] 10

``` r
skillCheck('medicine') # medicine skill check
```

    ## [1] "Rolls: [ 8 ]"

    ## [1] 9

``` r
weaponAttack(char$weapons$Dagger) # a weapon attack with a dagger
```

    ## [1] "Rolls: [ 12 ]"
    ## [1] "Rolls: [ *4* ]"
    ## [1] "Piercing damage"

    ## Attack Damage   crit 
    ##     19      8      0

All these conveninece functions require an imported character as an input. If you use the variable name `char`, providing this will not be necesarry. If you want to use a different name you can either do `options(defaultCharacter='newVariableName')` to continue using the functions as before, or specify the object.

``` r
myBestCharacter = importCharacter('Tim_Fighter')
```

    ## Auto-refreshing stale OAuth token.

``` r
init(char = myBestCharacter)
```

    ## [1] "Rolls: [ 5 ]"

    ## [1] 10

PDF character sheets
--------------------

A rmarkdown template is available to print out pdf character sheets. To use it, create a new markdown document with `CharacterSheet` template. Replace the character import line with a character of your own. See [example.pdf](inst/skeleton.pdf) to see what it looks like. Alternatively use `prettyPDF` function to get a proper fillable character sheet.

Shiny character sheet
---------------------

To run the interactive character sheet, just use `shinySheet()`.

Use example
-----------

See [tim.R](tim.R) for example usage. I keep a file like that open during session and ctrl+R the relevant lines as needed. If you are a caster see if you'd be interested in the [wizaRd](https://github.com/oganm/wizaRd) package. If you turn into animals a lot see [monsteR](https://github.com/oganm/monsteR) package.
