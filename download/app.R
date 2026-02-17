suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(colourpicker)
})

ui <- fluidPage(
  titlePanel("Download plot"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Plot",
          numericInput("num_points", "Number of points:", 100, min = 10, max = 1000),
          sliderInput("point_size", "Point size:", min = 0.5, max = 10, value = 2, step = 0.5),
          colourInput("point_colour", "Point colour:", value = "orange")
        ),
        tabPanel("Labels",
          textInput("plot_title", "Plot title:", "Scatter plot"),
          numericInput("title_size", "Title size:", 14, min = 6, max = 36),
          textInput("plot_subtitle", "Subtitle:", ""),
          numericInput("subtitle_size", "Subtitle size:", 11, min = 6, max = 36),
          textInput("x_label", "X-axis label:", "x"),
          textInput("y_label", "Y-axis label:", "y"),
          numericInput("axis_label_size", "Axis label size:", 11, min = 6, max = 36),
          numericInput("tick_label_size", "Tick label size:", 9, min = 6, max = 36)
        ),
        tabPanel("Export",
          textInput("filename", "Output file name (without extension):", "my_plot"),
          selectInput("fig_format", "Output format:", choices = c("png", "pdf", "svg")),
          numericInput("fig_width", "Figure width (inches):", 6),
          numericInput("fig_height", "Figure height (inches):", 4),
          numericInput("fig_dpi", "Figure DPI:", 300),
          downloadButton("downloadPlot", "Download plot")
        )
      )
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
      geom_point(color = input$point_colour, size = input$point_size) +
      labs(title = input$plot_title, subtitle = input$plot_subtitle,
           x = input$x_label, y = input$y_label) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = input$title_size),
        plot.subtitle = element_text(size = input$subtitle_size),
        axis.title = element_text(size = input$axis_label_size),
        axis.text = element_text(size = input$tick_label_size)
      )
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
      paste0(input$filename, ".", input$fig_format)
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