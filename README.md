
import5echar
============

Walter Kammerer's [Fifth Edition Character Sheet](https://play.google.com/store/apps/details?id=com.wgkammerer.testgui.basiccharactersheet.app&hl=en) app allows you to upload your character to google drive. This package parses that file and allows you to use character data for automation.

Installation
------------

    devtools::install_github('oganm/diceSyntax)
    devtools::install_gtihbu('oganm/import5echar)

Basics
------

First thing that need to be done is to import yoour character. You can either use a regex or a google drive file id (acquirable using `googledrive` package). If a regex is used, last modified file that matches the regex is pulled which is probably your latest export. First time you run the line below, you'll have to authenticate with google drive.

``` r
char = importCharacter('Tim_Fighter')
```

Once a character is loaded convenience functions can be used

``` r
init() # initiative roll
```

    ## [1] "Rolls: [ 6 ]"

    ## [1] 10

``` r
skillCheck(medicine) # medicine skill check
```

    ## [1] "Rolls: [ 8 ]"

    ## Medicine 
    ##        9

All these conveninece functions require an imported character as an input. If you use the variable name `char`, providing this will not be necesarry. If you want to use a different name you can either do `options(defaultCharacter='newVariableName')` to continue using the functions as before, or specify the object.

``` r
myBestCharacter = importCharacter('Tim_Fighter')

init(char = myBestCharacter)
```

    ## [1] "Rolls: [ 12 ]"

    ## [1] 16

Use example
-----------

See [tim.R](tim.R) for example usage. I keep a file like that open during session and ctrl+R the relevant lines as needed. If you are a caster see if you'd be interested in the [wizaRd](https://github.com/oganm/wizaRd) package.
