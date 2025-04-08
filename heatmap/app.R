# app.R
library(shiny)
library(pheatmap)

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
      plotOutput("heatmap")
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
    
    df <- df[, !colnames(df) %in% "transcript_id(s)"]  # Drop transcript_id(s) column
    df
  })
  
  observe({
    df <- rsem_data()
    gene_names <- df$gene_id
    updateSelectizeInput(session, "selected_genes",
                         choices = gene_names,
                         server = TRUE)
  })
  
  output$heatmap <- renderPlot({
    input$plot_btn
    isolate({
      req(input$selected_genes)
      df <- rsem_data()
      
      filtered_df <- df[df$gene_id %in% input$selected_genes, ]
      
      expr_matrix <- filtered_df[, -1]
      expr_matrix <- as.matrix(sapply(expr_matrix, as.numeric))
      rownames(expr_matrix) <- filtered_df$gene_id
      
      pheatmap(expr_matrix,
               scale = "row",
               fontsize_row = input$fontsize_row,
               fontsize_col = input$fontsize_col,
               cluster_rows = input$cluster_rows,
               cluster_cols = input$cluster_cols)
    })
  })
}

shinyApp(ui, server)