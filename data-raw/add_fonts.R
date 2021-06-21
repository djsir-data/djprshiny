# Download font(s) and add to package
# https://rstudio.github.io/bslib/articles/recipes.html#fonts-1

library(gfonts)
font_dir <- "inst/www/fonts"
if (!dir.exists(font_dir)) dir.create(font_dir, recursive = T)
download_font("roboto", output_dir = font_dir)


# setup_font("roboto", font_dir)
