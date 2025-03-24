library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(censusapi)

source("/Users/carlymcadam/Desktop/College/Midd2024-25/Spring2025/STAT711/vt-futures/state_data_functions.R")

vt_age_groups_2023 <- age_distribution_data(2023)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      body { font-family: Georgia, serif; }
      .section-title {
        text-align: center; 
        font-size: 36px; 
        font-weight: bold; 
        text-decoration: underline; 
        color: #2C6E49;  
        margin-top: 40px; 
        margin-bottom: 20px;
      }
    "))
  ),
  
  ### STATE LEVEL DATA ### 
  
  div(class = "section-title", "Vermont's Population"),
  sidebarLayout(
    sidebarPanel(
      p("In 2000, Vermont had a high proportion of prime working-age adults
        relative to the number of children and elderly. There were significantly
        more children than there are today. In 2023, (pictured right) Vermont's
        working-age population (25-49 years old) is much smaller relative to 
        the number of children and elderly. The fertility rate is too low
        to grow the future workforce and tax base. 
        This demographic shift has also led to an imbalance in the workforce,
        where the supply of working-age individuals is insufficient to meet
        the demand for labor. As a result, employers struggle to fill positions,
        which limits productivity and economic growth. For consumers, this
        shortage leads to reduced access to goods and services such as adequate
        childcare, dining at local restaurants, getting automotive repairs, or
        healthcare appointments. Growing the prime working-age population
        is essential to closing the workforce gap, improving affordability, and
        strengthening communities to better meet the needs of all Vermonters.")
    ),
    mainPanel(
      plotOutput("age_plot_2023")
    )
  ),
  
  br(), br(), hr(),
  
  ### COUNTY GOAL BREAKDOWNS ### 
  
  div(class = "section-title", "County-Level Breakdown of VFP Population Goals"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_county", "Select a County:", 
                  choices = NULL, 
                  selected = NULL)
    ),
    mainPanel(
      plotOutput("county_plot")
    )
  )
)


server <- function(input, output, session) {
  county_caps_df <- reactive({
    build_county_caps_df()
  })
  
  observe({
    updateSelectInput(session, "selected_county", 
                      choices = unique(county_caps_df()$County),
                      selected = unique(county_caps_df()$County)[1])
  })
  
  filtered_data <- reactive({
    county_caps_df() %>%
      filter(County == input$selected_county) %>%
      pivot_longer(cols = -County, names_to = "Metric", values_to = "Value")
  })
  
  # Age Distribution Plot
  output$age_plot_2023 <- renderPlot({
    plot_age_distribution(vt_age_groups_2023)
  })
  
  # County Capacity Plot
  output$county_plot <- renderPlot({
    ggplot(filtered_data() %>% 
             mutate(Metric = factor(Metric, levels = c("pop_goal", "latent_cap", "jobs_homes_diff", "latent_cap_school"))),
           aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("County Capacity Limitations:", input$selected_county, "\n"), 
           x = "\nMetric", y = "Value") +
      theme_minimal() +
      scale_x_discrete(labels = c(
        "latent_cap" = "Latent Capacity",
        "jobs_homes_diff" = "Jobs-Homes Difference",
        "latent_cap_school" = "School Latency",
        "pop_goal" = "Population Goal"
      )) + 
      scale_fill_manual(values = c(
        "latent_cap" = "deepskyblue1",
        "jobs_homes_diff" = "orange",
        "latent_cap_school" = "red",
        "pop_goal" = "aquamarine4"
      )) +
    theme(legend.position = "none", 
          text = element_text(family = "Georgia"),
          plot.margin = margin(t = 10, r = 10, b = 40, l = 10),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 22, face = "bold"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)