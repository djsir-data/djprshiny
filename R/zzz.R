.onLoad <- function(libname, pkgname) {
  addResourcePath("djprshiny", system.file("www", package = "djprshiny"))

  if (requireNamespace("memoise")) {
    cache_dir <- ifelse(dir.exists("app-cache"),
                        "app-cache",
                        ".")

    djpr_girafe <<- memoise::memoise(djpr_girafe,
                                     cache = cachem::cache_disk(dir = cache_dir))
  }

}
