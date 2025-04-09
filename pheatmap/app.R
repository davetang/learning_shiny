library(shiny)
library(pheatmap)
library(grid)

# https://davetang.github.io/muse/pheatmap.html
save_pheatmap_png <- function(x, filename, width = 2000, height = 2400, res = 150) {
  png(filename, width = width, height = height, res = res)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}

# Generate sample data
set.seed(1984)
gene_data <- matrix(rnorm(100 * 8), nrow = 100)
rownames(gene_data) <- paste0("Gene", 1:100)
colnames(gene_data) <- paste0("Sample", 1:8)

# Assign sample conditions
conditions <- rep(c("Control", "Test"), each = 4)
names(conditions) <- colnames(gene_data)

# UI
ui <- fluidPage(
  titlePanel("Heatmap with pheatmap"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("samples", "Select samples to keep:",
                         choices = colnames(gene_data),
                         selected = colnames(gene_data)),
      
      h4("Options"),
      checkboxInput("cluster_rows", "Cluster rows", value = TRUE),
      checkboxInput("cluster_cols", "Cluster columns", value = TRUE),
      selectInput("scale_data", "Scale data:",
                  choices = c("none", "row", "column"), selected = "none"),
      
      hr(),
      h4("Download"),
      numericInput("plot_width", "Width (px):", value = 2000),
      numericInput("plot_height", "Height (px):", value = 2400),
      numericInput("plot_res", "Resolution (dpi):", value = 150),
      downloadButton("download_plot", "Download Heatmap")
    ),
    
    mainPanel(
      plotOutput("heatmap_plot", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    gene_data[, input$samples, drop = FALSE]
  })
  
  filtered_annotation <- reactive({
    data.frame(Condition = conditions[input$samples], row.names = input$samples)
  })
  
  heatmap_plot <- reactive({
    pheatmap(filtered_data(),
             cluster_rows = input$cluster_rows,
             cluster_cols = input$cluster_cols,
             annotation_col = filtered_annotation(),
             scale = input$scale_data,
             silent = TRUE)
  })
  
  output$heatmap_plot <- renderPlot({
    grid::grid.newpage()
    grid::grid.draw(heatmap_plot()$gtable)
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(
        "heatmap_",
        "w", input$plot_width,
        "_h", input$plot_height,
        "_res", input$plot_res,
        "_rowclust-", ifelse(input$cluster_rows, "yes", "no"),
        "_colclust-", ifelse(input$cluster_cols, "yes", "no"),
        "_scale-", input$scale_data,
        ".png"
      )
    },
    content = function(file) {
      save_pheatmap_png(
        heatmap_plot(),
        file,
        width = input$plot_width,
        height = input$plot_height,
        res = input$plot_res
      )
    }
  )
}

shinyApp(ui, server)