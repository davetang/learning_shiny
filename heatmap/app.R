# app.R
library(shiny)
library(plotly)
library(dplyr)

ui <- fluidPage(
  titlePanel("Heatmap"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload RSEM gene TPM file",
                accept = c(".txt", ".tsv", ".csv", ".gz")),
      
      selectizeInput("selected_genes", "Search and select genes",
                     choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Type to search genes...')),
      
      numericInput("fontsize_row", "Font size (rows):", value = 10, min = 1, max = 30),
      numericInput("fontsize_col", "Font size (columns):", value = 10, min = 1, max = 30),
      
      checkboxInput("cluster_rows", "Cluster rows", value = TRUE),
      checkboxInput("cluster_cols", "Cluster columns", value = TRUE),
      
      actionButton("plot_btn", "Generate heatmap")
    ),
    mainPanel(
      plotlyOutput("heatmap", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  rsem_data <- reactive({
    req(input$file)
    df <- read.delim(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    
    validate(
      need(ncol(df) > 2, "Input file must have at least two metadata columns and one sample column"),
      need(all(c("gene_id", "transcript_id(s)") %in% colnames(df)), 
           "Input file must contain 'gene_id' and 'transcript_id(s)' columns")
    )
    
    df <- df[, !colnames(df) %in% "transcript_id(s)"]
    df
  })
  
  observe({
    df <- rsem_data()
    gene_names <- df$gene_id
    updateSelectizeInput(session, "selected_genes",
                         choices = gene_names,
                         server = TRUE)
  })
  
  output$heatmap <- renderPlotly({
    input$plot_btn
    isolate({
      req(input$selected_genes)
      df <- rsem_data()
      filtered_df <- df[df$gene_id %in% input$selected_genes, ]
      
      gene_ids <- filtered_df$gene_id
      expr_matrix <- filtered_df[, -1]  # Remove gene_id column
      expr_matrix <- as.matrix(sapply(expr_matrix, as.numeric))
      
      # Optionally cluster rows/columns
      if (input$cluster_rows && nrow(expr_matrix) > 1) {
        row_order <- hclust(dist(expr_matrix))$order
        expr_matrix <- expr_matrix[row_order, ]
        gene_ids <- gene_ids[row_order]
      }
      if (input$cluster_cols && ncol(expr_matrix) > 1) {
        col_order <- hclust(dist(t(expr_matrix)))$order
        expr_matrix <- expr_matrix[, col_order]
        sample_labels <- colnames(expr_matrix)[col_order]
      } else {
        sample_labels <- colnames(expr_matrix)
      }
      
      # Create interactive heatmap
      plot_ly(
        x = sample_labels,
        y = gene_ids,
        z = scale(expr_matrix),
        type = "heatmap",
        colors = "RdBu",
        reversescale = TRUE
      ) %>%
        layout(
          xaxis = list(title = "", tickfont = list(size = input$fontsize_col)),
          yaxis = list(title = "", tickfont = list(size = input$fontsize_row)),
          margin = list(l = 120, r = 50, b = 100, t = 50)
        )
    })
  })
}

shinyApp(ui, server)