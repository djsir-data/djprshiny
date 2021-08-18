.onLoad <- function(libname, pkgname) {
  addResourcePath("djprshiny", system.file("www", package = "djprshiny"))

  djpr_girafe_mem <<- memoise::memoise(djpr_girafe,
                                       cache = cachem::cache_disk())
}
