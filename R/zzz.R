.onLoad <- function(libname, pkgname) {
  djpr_girafe <<- memoise::memoise(djpr_girafe)
}
