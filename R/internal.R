.onAttach <- function(libname, pkgname) {
    options(defaultCharacter='char')
}

splitAt = function(x, pos){
    unname(split(x, cumsum(seq_along(x) %in% pos)))
    }


logicConvert = function(x){
    x %>%  ogbox::replaceElement(c('true'=TRUE,'false'=FALSE)) %$% newVector %>% as.logical()
}

#' @export
stat2mod = function(x){
    floor(x/2-5)
}
