library(shiny)
library(tidyverse)

ui <- fluidPage(
  selectizeInput("blah",
              "Choose a color",
              choices = unique(diamonds$color),
              multiple = TRUE),
  dataTableOutput("blah2")
)

server <- function(input, output, session) {
  
  output$blah2 <- renderDataTable({
    diamonds |>
      filter(color %in% input$blah)
  })
  
}

shinyApp(ui, server)