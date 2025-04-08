library(shiny)
suppressPackageStartupMessages(library(ggplot2))

ui <- fluidPage(
  titlePanel("Download plot"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_points", "Number of points:", 100, min = 10, max = 1000),
      textInput("filename", "Output file name (without extension):", "my_plot"),
      numericInput("fig_width", "Figure width (inches):", 6),
      numericInput("fig_height", "Figure height (inches):", 4),
      numericInput("fig_dpi", "Figure DPI:", 300),
      downloadButton("downloadPlot", "Download plot")
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {

  # Reactive expression to generate the plot data
  plot_data <- reactive({
    set.seed(1984)
    data.frame(
      x = runif(input$num_points),
      y = runif(input$num_points)
    )
  })

  # Reactive expression to generate the plot
  scatter_plot <- reactive({
    ggplot(plot_data(), aes(x = x, y = y)) +
      geom_point(color = "orange") +
      labs(title = "Scatter plot") +
      theme_minimal()
  })

  # Render the plot in the UI
  output$scatterPlot <- renderPlot({
    scatter_plot()
  })

  #
  # downloadHandler is a special Shiny function that creates downloadable content from the app.
  # It takes two key arguments:
  #
  # filename: A function that returns the name of the downloaded file.
  # content: A function that generates the file and saves it to the given path.
  #          Shiny provides file as a temporary file path to write to.
  #          Shiny then automatically moves it to the user's download location using the filename() name.
  #
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".png")
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = scatter_plot(),
        width = input$fig_width,
        height = input$fig_height,
        dpi = input$fig_dpi
      )
    }
  )
}

shinyApp(ui = ui, server = server)