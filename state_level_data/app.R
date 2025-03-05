# Load necessary libraries
library(shiny)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)

source("state_data_functions.R")

vt_age_groups_2023 <- age_distribution_data(2023)
vt_age_groups_2020 <- age_distribution_data(2020)

# Define UI
ui <- fluidPage(
  titlePanel("Vermont's Population"),
  sidebarLayout(
    sidebarPanel(
      p("This dashboard displays the age distribution of Vermont's population based on 2023 ACS data.")
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("age_plot_2023")),
        column(6, plotOutput("age_plot_2020"))
      )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  output$age_plot_2023 <- renderPlot({
    plot_age_distribution(vt_age_groups_2023)
  })
  
  output$age_plot_2020 <- renderPlot({
    plot_age_distribution(vt_age_groups_2020)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
