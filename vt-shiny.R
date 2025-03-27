
library(shiny)

library(leaflet)
library(sf)
library(tidyverse)
library(tigris)
library(censusapi)
library(bslib)
library(DT)

pth <- getwd()
source(paste0(pth, "/population.R"))
source(paste0(pth, "/state_data_functions.R"))

Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")
state_fips <- 50 # VT

#### Data Prep ####

census_variables <- data.frame(
  code = c(
    "B01003_001E", "B01002_001E", "B01001_002E", "B01001_026E", "B02001_002E", "B02001_003E", "B02001_005E", "B03001_003E",
    "B19013_001E", "B19001_002E", "B19301_001E", "B17001_002E", "B25077_001E", "B25064_001E",
    "B23025_002E", "B23025_005E", "B23006_002E", "B24011_001E",
    "B15003_017E", "B15003_021E", "B15003_022E", "B15003_023E", "B15003_024E",
    "B25001_001E", "B25002_002E", "B25002_003E", "B25003_002E", "B25003_003E",
    "B08006_001E", "B08006_003E", "B08006_008E", "B08013_001E",
    "B27001_001E", "B27001_005E", "B27001_008E", "B27001_012E"
  ),
  title = c(
    "Total Population", "Median Age", "Total Male Population", "Total Female Population", "White Alone", 
    "Black or African American Alone", "Asian Alone", "Hispanic or Latino Population",
    "Median Household Income", "Household Income Brackets", "Per Capita Income", "Population Below Poverty Level", 
    "Median Home Value", "Median Gross Rent",
    "Labor Force", "Unemployed Population", "Civilian Employed Population", "Industry for Civilian Employed Population",
    "High School Graduate or Equivalent", "Bachelor's Degree", "Master's Degree", "Professional School Degree", "Doctorate Degree",
    "Total Housing Units", "Occupied Housing Units", "Vacant Housing Units", "Owner-Occupied Housing Units", "Renter-Occupied Housing Units",
    "Total Workers", "Workers Who Drive Alone", "Workers Using Public Transport", "Mean Travel Time to Work (Minutes)",
    "Total Population for Health Insurance Coverage", "Population with Public Health Insurance", 
    "Population with Private Health Insurance", "Population with No Health Insurance"
  ),
  stringsAsFactors = FALSE
)

census_data_raw <- getCensus(
  name = "acs/acs5",
  vintage = 2022,
  vars = c("NAME", census_variables$code),
  region = "county:*",
  regionin = paste0("state:", state_fips)
)

census_data <- census_data_raw |> 
  rename_with(~ census_variables$title, .cols = any_of(census_variables$code)) |> 
  mutate(
    NAME = gsub(" County, Vermont", "", NAME)
  )

vt_counties <- counties(state = "VT", cb = TRUE, class = "sf", year = "2022")

vt_map <- left_join(vt_counties, census_data, by = "NAME")

theme <- bs_theme(
  primary = "darkgreen", secondary = "#2c3e50",
  base_font = c("Georgia", "sans-serif"),
  heading_font = c("Georgia", "sans-serif"),
  "input-border-color" = "darkgreen"
)

#### UI #### 

ui <- page_fluid(
  theme = theme,
  titlePanel("Vermont Futures: Interactive Dashboard"),
  p("This is the main page for the data exploration dashboard. This should be placed right below the title."),
  
  navset_card_pill(
    # Population tab is an example layout of what we want
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
 
    nav_panel("Jobs", p("Jobs data at the state level."), tableOutput("jobs")),
    
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
  
  
  output$pop_text <- renderText({
    "This is some test text for the page"
  })
  
  output$pop_state <- renderDataTable({
    census_data |> 
      group_by(NAME) |> 
      select(NAME, `Total Population`)
  })
  
  output$pop <- renderPlot({
    vt_map |>
      ggplot() +
      geom_sf(aes(fill = !!as.symbol(input$pop_county_col))) + 
      geom_sf_label(aes(label = !!as.symbol(input$pop_county_col))) +
      geom_sf_label(aes(label = NAME), nudge_y = -0.1) + 
      scale_fill_gradient(
        low = "honeydew", 
        high = "darkgreen", 
        name = input$pop_county_col
      ) +
      theme_void()
  })
  
  output$age_plot <- renderPlot({
    age_data <- age_distribution_data(2023)
    plot_age_distribution(age_data)
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
  
  output$jobs <- renderDataTable(
    vt_map |> 
      filter()
  )
}

shinyApp(ui, server)

