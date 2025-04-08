# app.R
library(shiny)
library(pheatmap)

ui <- fluidPage(
  titlePanel("Heatmap"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload RSEM gene TPM file",
                accept = c(".txt", ".tsv", ".csv", ".gz")),
      selectizeInput("selected_genes", "Select genes for heatmap",
                     choices = NULL, multiple = TRUE),
      actionButton("plot_btn", "Generate heatmap")
    ),
    mainPanel(
      plotOutput("heatmap")
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression to load and preprocess data
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
  
  # Update gene selector dynamically
  observe({
    df <- rsem_data()
    gene_names <- df$gene_id
    updateSelectizeInput(session, "selected_genes",
                         choices = gene_names,
                         server = TRUE)
  })
  
  # Generate heatmap
  output$heatmap <- renderPlot({
    input$plot_btn  # triggers on click
    isolate({
      req(input$selected_genes)
      df <- rsem_data()
      
      # Filter to selected genes
      filtered_df <- df[df$gene_id %in% input$selected_genes, ]
      
      # Extract expression matrix
      rownames(filtered_df) <- filtered_df$gene_id
      expr_matrix <- filtered_df[, -1]  # remove gene_id column
      
      # Convert to numeric matrix
      expr_matrix <- as.matrix(sapply(expr_matrix, as.numeric))
      
      # Plot heatmap
      pheatmap(expr_matrix, scale = "row", fontsize_row = 8, fontsize_col = 10)
    })
  })
}

shinyApp(ui, server)