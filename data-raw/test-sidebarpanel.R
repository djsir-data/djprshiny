library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
      sliderInput("bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("dimension_display"),
      plotOutput("distPlot")
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  output$dimension_display <- renderText({
    paste(input$dimension[1], input$dimension[2], input$dimension[2] / input$dimension[1])
  })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
})

# Run the application
shinyApp(ui = ui, server = server)
