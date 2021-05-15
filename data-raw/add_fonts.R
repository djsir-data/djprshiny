# Download font(s) and add to package
# https://rstudio.github.io/bslib/articles/recipes.html#fonts-1

library(gfonts)
font_dir <- "www/fonts"
if (!dir.exists(font_dir)) dir.create(font_dir, recursive = T)
download_font("roboto", output_dir = font_dir)

# Add this to theme:
# bs_theme_fonts(base = "Roboto")
# bs_theme_add(rules = generate_css("roboto", font_dir = font_dir))
# shiny::runApp()
