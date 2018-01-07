characterDescription = function(char)
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
