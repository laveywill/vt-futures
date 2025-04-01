
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(tigris)
library(censusapi)
library(tidycensus)
library(bslib)
library(scales)
library(readxl)
library(DT)
library(readxl)

pth <- getwd()
source(paste0(pth, "/read_data.R"))
source(paste0(pth, "/population.R"))
source(paste0(pth, "/jobs.R"))
source(paste0(pth, "/homes.R"))

#### Global Variables ####
Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")
year <- 2023
state_fips <- 50

#### Read in data ####
census_data <- census_data(year)
census_variables <- get_census_variables()
state <- census_data$state
county <- census_data$county
town <- census_data$place

housing <- get_housing_units_data(year)
state_housing_data <- housing$state
county_housing_data <- housing$county

state_age_data <- build_state_age_df(state)
vt_map <- county_level_map(county)

theme <- bs_theme(
  primary = "darkgreen", secondary = "#2c3e50",
  base_font = c("Georgia", "sans-serif"),
  heading_font = c("Georgia", "sans-serif"),
  "input-border-color" = "darkgreen"
)

#### UI #### 

ui <- page_fluid(
  theme = theme,
  titlePanel("Vermont Futures Project: Interactive Dashboard"),
  p("This is the main page for the data exploration dashboard. This should be placed right below the title."),
  
  navset_card_pill(
    nav_panel("Population",
      layout_column_wrap(  
        width = 1,
        card(
          card_header(class = "bg-primary", "State Population"),
          card_body(
            sidebarLayout(
              sidebarPanel(
                p("In 2000, Vermont had a high proportion of prime working-age adults
          relative to the number of children and elderly. There were significantly
          more children than there are today. In 2023 (pictured right), Vermont's
          working-age population (25–49 years old) is much smaller relative to 
          the number of children and elderly."),
                p("The fertility rate is too low to grow the future workforce and tax base. 
          This demographic shift has led to an imbalance in the workforce, where
          the supply of working-age individuals is insufficient to meet the demand
          for labor."),
                p("As a result, employers struggle to fill positions, which limits productivity 
          and economic growth. Consumers face reduced access to services like 
          childcare, dining, repairs, and healthcare."),
                p("Growing the prime working-age population is essential to closing the 
          workforce gap, improving affordability, and strengthening communities 
          to better meet the needs of all Vermonters.")
              ),
              mainPanel(
                plotOutput("age_plot", height = "600px")
              )
            )
          )
        ),
        card(
          card_header(class = "bg-primary", "County Level Exploration"),
          layout_sidebar(
            sidebar = sidebar(
              bg = "lightgrey",
              selectInput("pop_county_col", 
                          label = "Select a Variable to Explore",
                          choices = census_variables$title),
              sidebarPanel(
                strong("National Benchmarks"),
                p("\n"),
                p("Average capita income: $37,683"),
                p("Median age: 38.7"), 
                p("Poverty Rate: 11.1%"), 
                p("Median home value: $420,000"), 
                p("Average labor force participation rate: 62%"),
                , width = "150px"
              ),
            ),
            plotOutput("pop")
          )
        ),
        card(
          card_header(class = "bg-primary", "County-Level Breakdown of VFP Population Goals"),
          card_body(
            layout_sidebar(
              sidebar = sidebar(
                bg = "lightgrey",
                width = "300px",
                selectInput("selected_county", "Select a County:", 
                            choices = NULL, 
                            selected = NULL),
                p("VFP has a goal of increasing Vermont’s population to 802,000
           residents by 2035 by recruiting and retaining working-age people."),
                p("Here, we can see the population goal for each county compared
           with its current capacities for adding new population
           in different areas."),
                p("It might make sense to add some more description here
           of the different capacity metrics.")
              ),
                plotOutput("county_plot", height = "600px"),
                plotOutput("jobs_homes_gauge", height = "100px")
            )
          )
        ),
      )
    ),
 
    nav_panel("Homes",
      layout_column_wrap(
        width = 1,
        card(
          card_header(class = "bg-primary", "State Homes"),
          card_body(
            sidebarLayout(
              sidebarPanel(
                p("Vermont has some of the oldest housing stock in the country. 
                  A quarter of homes were built before 1940. 
                  Rates of housing construction were healthy in the 1970s and 
                  1980s relative to the needs of the population at the time. 
                  Vermont's current housing shortage is the result of decades of 
                  decelerating housing construction.")
              ),
              mainPanel(
                p("Estimated Housing Units by Year Structure Built"),
                plotOutput("job_plot", height = "600px")
              )
            )
          )
        )
      )
    ),
    
    nav_panel("Jobs", p("Jobs data at the state level."), tableOutput("jobs")),
    
    nav_panel("Etc...", p("Etc..."), tableOutput("etc"))
    
  )
)

#### Server ####

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
      pivot_longer(cols = c("pop_goal", "latent_cap", "latent_cap_school"),
                   names_to = "Metric", values_to = "Value")
  })
  
  output$pop <- renderPlot({
    plot_county_map(vt_map, input$pop_county_col)
  })
  
  output$age_plot <- renderPlot({
    plot_age_distribution(state_age_data)
  })
  
  output$county_plot <- renderPlot({
    ggplot(filtered_data() %>% 
             mutate(Metric = factor(Metric, levels = c("pop_goal", "latent_cap", "latent_cap_school"))),
           aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("County Capacity Limitations:", input$selected_county, "\n"), 
           x = "\nMetric", y = "Value") +
      theme_minimal() +
      scale_x_discrete(labels = c(
        "latent_cap" = "Latent Capacity",
        "latent_cap_school" = "School Latency",
        "pop_goal" = "Population Goal"
      )) + 
      scale_fill_manual(values = c(
        "latent_cap" = "deepskyblue1",
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
  
  output$jobs_homes_gauge <- renderPlot({
    data <- county_caps_df() %>%
      filter(County == input$selected_county)
    
    if (nrow(data) == 0 || is.na(data$jobs_homes_index)) return(NULL)
    
    val <- round(data$jobs_homes_index, 2)
    gradient_data <- data.frame(x = seq(0, 2, length.out = 200))
    
    ggplot() +
      geom_tile(data = gradient_data, aes(x = x, y = 1, fill = x), height = 0.3) +
      
      geom_segment(aes(x = val, xend = val, y = 0.85, yend = 1.15), 
                   color = "black", size = 1.5) +
      
      annotate("text", x = val, y = 1.3, label = paste(input$selected_county, ":", val),
               size = 4.5, fontface = "bold", hjust = 0.5, family = "Georgia") +
      annotate("text", x = 0, y = 0.8, label = "0 (More homes)", hjust = 0, size = 5, family = "Georgia") +
      annotate("text", x = 1, y = 0.8, label = "1 (Balanced)", hjust = 0.5, size = 5, family = "Georgia") +
      annotate("text", x = 2, y = 0.8, label = "2 (More jobs)", hjust = 1, size = 5, family = "Georgia") +
      
      scale_fill_gradient(low = "lightgreen", high = "orange") +
      
      scale_x_continuous(limits = c(0, 2), breaks = c(0, 0.5, 1, 1.5, 2)) +
      coord_cartesian(clip = "off") +
      labs(title = "Jobs-Homes Index\n") +
      theme_void() +
      theme(
        plot.title = element_text(size = 20, face = "bold", family = "Georgia"),
        legend.position = "none",
        plot.margin = margin(5, 10, 5, 10)
      )
  })
  
  output$job_plot <- renderPlot({
    plot_state_housing_units(state_housing_data)
  })

}

shinyApp(ui, server)

