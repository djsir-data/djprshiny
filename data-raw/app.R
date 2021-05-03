# This script is an environment in which to manually play around with Shiny
# functionality

library(shiny)
library(ggplot2)

econ_plot <- function(data = ggplot2::economics,
                      title = "A title",
                      subtitle = "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum",
                      caption = "A caption") {

  data %>%
    ggplot(aes(x = date, y = unemploy)) +
    geom_line() +
    labs(title  = title,
         subtitle = subtitle,
         caption = caption) +
    theme_minimal(base_size = 16)
}


my_sample_card <- function() {

  #
  # bs4cards::sample_card(image_align = "bottom",
  #                       # margin = bs4cards::bs_mar(1),
  #                       # width = bs4cards::bs_col(large = 2,
  #                       #                medium = 4),
  #                       pad = bs4cards::bs_pad(1))

  print(temp_image)

  bs4cards::card(title = "Lorem ipsum",
                 text = bs4cards::sample_lorem(),
                 image = here::here("temp_card.jpeg"),
                 image_align = "bottom",
                 pad = bs4cards::bs_pad(1))
}

ui <- fluidPage(
  theme = djpr_shiny_theme(),
  title = "Some title",
  navbarPage( title = "Overview",
              position = "fixed-top",
              centred_row(uiOutput("cards")),
              centred_row(titlePanel(title = "Example")),
              centred_row(tagList(h4(text_active("The unemployment rate went up by XX to XX.",
                                      c("0.2 percentage points", "6.1 per cent")),
                                  bs4cards::sample_lorem()),
                                  ),
                          left_space = 1),
              br(),
              centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
              djpr_plot_ui("plot1"),
              djpr_plot_ui("plot2"),
              centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
              br())


)

server <- function(input, output, session) {

  output$cards <- renderUI({


    make_card <- function() {
      bs4cards::card(title = "Lorem ipsum",
                   text = bs4cards::sample_lorem(),
                   image = bs4cards::card_image("temp_card.jpeg"),
                   image_align = "bottom",
                   pad = bs4cards::bs_pad(1))
    }


    bs4cards::card_grid(
      list(
        make_card(),
        make_card(),
        make_card()
        )
    )
  })


  purrr::pmap(.l = list(id = list("plot1",
                                  "plot2"),
                        plot_function = list(econ_plot,
                                             econ_plot),
                        date_slider = list(FALSE,
                                           TRUE)
                        ),
              .f = djpr_plot_server)

}

shinyApp(ui, server)

