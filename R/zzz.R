.onLoad <- function(libname, pkgname) {
  addResourcePath("djprshiny", system.file("www", package = "djprshiny"))

  djpr_shiny_cache <- cachem::cache_disk(
    dir = file.path(dirname(tempdir()), "djpr-shiny-cache")
  )
  djpr_girafe_mem <<- memoise::memoise(djpr_girafe,
                                       cache = djpr_shiny_cache)
}
