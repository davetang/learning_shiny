# app.R
library(shiny)

ui <- fluidPage(
  titlePanel("Delimited file upload and display"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload delimited file",
                accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double quote" = '"',
                               "Single quote" = "'"),
                   selected = '"')
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  output$contents <- renderTable({
    req(input$file)  # Ensure a file is uploaded
    
    tryCatch({
      df <- read.table(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      df  # return data frame to display
    }, error = function(e) {
      return(data.frame(Error = "Error reading file"))
    })
  })
}

shinyApp(ui, server)