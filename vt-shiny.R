
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
                plotOutput("county_plot", height = "600px")
            )
          )
        ),
      )
    ),
 
    nav_panel("Jobs",
      layout_column_wrap(
        width = 1,
        card(
          card_header(class = "bg-primary", "State Jobs"),
          card_body(
            sidebarLayout(
              sidebarPanel(
                p("placeholder text")
              ),
              mainPanel(
                p("placeholder plot")
                # plotOutput("job_plot", height = "600px")
              )
            )
          )
        )
      )
    ),
    
    nav_panel("Homes", p("Homes data at the state level."), tableOutput("homes")),
    
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
      pivot_longer(cols = -County, names_to = "Metric", values_to = "Value")
  })
  
  output$pop <- renderPlot({
    plot_county_map(vt_map, input$pop_county_col)
  })
  
  output$age_plot <- renderPlot({
    plot_age_distribution(state_age_data)
  })
  
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

shinyApp(ui, server)

