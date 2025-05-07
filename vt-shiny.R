
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
library(shinydashboard)
library(rlang)
library(forcats)
library(data.table)
library(plotly)
library(geojsonsf)

pth <- getwd()
source(paste0(pth, "/read_data.R"))
source(paste0(pth, "/population.R"))
source(paste0(pth, "/jobs.R"))
source(paste0(pth, "/homes.R"))

#### Global Variables ####
Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")
Sys.setenv(MAPBOX_TOKEN = "")
year <- 2023
state_fips <- 50

population_variables = c(
  "Total Population", "Median Age", "Total Male Population", "Total Female Population", "White Alone", 
  "Black or African American Alone", "Asian Alone", "Hispanic or Latino Population"
)

homes_variables = c(
  "Median Home Value", "Median Gross Rent", "Total Housing Units", "Occupied Housing Units", 
  "Vacant Housing Units", "Owner-Occupied Housing Units", "Renter-Occupied Housing Units"
)

jobs_variables = c(
  "Labor Force", "Unemployed Population", 
  "High School Graduate or Equivalent", "Bachelor's Degree", "Master's Degree", "Professional School Degree", "Doctorate Degree",
  "Total Workers", "Workers Who Drive Alone", "Workers Using Public Transport", "Mean Travel Time to Work (Minutes)"
)

zoning_variables = c(
  "1F Allowance", "2F Allowance", "3F Allowance", "4F Allowance", "5F Allowance"
)

#### Read in data ####
census_variables <- get_census_variables()
census_data <- census_data(year)
state <- census_data$state
county <- census_data$county
town <- census_data$place
natl <- census_data$natl
collierFL <- census_data$collierFL

housing <- get_housing_data(year)
zoning <- get_zoning_data()

labor_force_df <- get_lf_data()
prime_age_df <- get_prime_age_data(labor_force_df)
dependency_df <- get_dependency_data(labor_force_df)
job_opening_df <- get_job_openings_data()
county_job_opening_df <- get_county_job_openings_data()
rank_df <- get_rank_data()

state_age_data <- build_age_df(state)
natl_age_data <- build_age_df(natl)
collierFL_age_data <- build_age_df(collierFL)
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
  card(
    card_header(class = "bg-primary", "Vermont Futures Project: Interactive Dashboard"),
    card_body(p("An interactive dashboard to make Vermont's publicly available information digestable"))
  ),
  
  #### POPULALTION PAGE ####

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
              selectInput(
                "pop_var_col", 
                label = "Select a Variable to Explore",
                choices = population_variables
              ),
              conditionalPanel(
                condition = "input.pop_var_col != 'Total Population'",
                checkboxInput("show_natl_diff", "Show Difference From National Average", value = FALSE)
              )
            ),
            layout_columns(
              col_widths = c(7, 5), 
              plotOutput("pop_county_map", height = "500px"),
              card(
                class = "bg-light p-3 shadow-sm",
                card_header("How Does Your County Compare to National Stats? ", class = "bg-secondary text-white"),
                div(class = "mb-2", strong("Median age:"), "38.7"),
                div(class = "mb-2", strong("Male population:"), "49.5%"),
                div(class = "mb-2", strong("Female population:"), "50.5%"),
                div(class = "mb-2", strong("White population:"), "61%"),
                div(class = "mb-2", strong("Black or African American population:"), "14%"),
                div(class = "mb-2", strong("Asian population:"), "7%"),
                div(class = "mb-2", strong("Hispanic or Latino population:"), "19%")
              )
            )
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
                strong("Latent capacity:"),
                p("The difference between the current population and the 
                  maximum population that the county has supported historically"),
                strong("School latency:"),
                p("The difference between the current school enrollment and the 
                  enrollment if the student-teacher ratio was increased to 18:1")
              ),
                plotOutput("county_plot", height = "600px"),
                plotOutput("jobs_homes_gauge", height = "100px"),
              p("The jobs-homes index is a measure of the ratio of jobs
                to homes in a county. Counties that have a ratio less than 
                1 have more homes than jobs. This usually means that most people
                who work in the county are able to find housing there, and some
                people commute outside of the county for work. Counties with a ratio
                very close to 0 are known as `bedroom communities` because most
                of the residents of the county do not work there -- there are many more
                homes than jobs. On the other
                hand, counties with a ratio greater than 1 have more jobs than
                homes. Most workers have to commute into the county because 
                there is not enough housing for everyone who works in the county.
                This ratio can help us understand which counties are able to support
                 an influx of population, and where counties can focus on development
                to help support a population increase. For example, bedroom 
                communities may want to work on adding jobs, while counties with
                more jobs than housing would want to prioritize building housing.")
            )
          )
        ),
      )
    ),
 
 #### HOMES PAGE ####
    
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
                  decelerating housing construction."),
                p("Act 250 was passed in 1970"),
                p("Less than 20% of Vermont's housing stock has been built in the last 20 years"),
                checkboxInput("show_homes_county_view", "View by County", value = FALSE),
                conditionalPanel(
                  condition = "input.show_homes_county_view == true",
                  selectInput("selected_homes_county", "Select a County",
                              choices = unique(housing$NAME),
                              selected = NULL)
                )
              ),
              mainPanel(
                plotOutput("home_plot", height = "600px")
              )
            )
          )
        ),
        card(
          card_header(class = "bg-primary", "County Level Exploration"),
          layout_sidebar(
            sidebar = sidebar(
              bg = "lightgrey",
              selectInput(
                "homes_var_col", 
                label = "Select a Variable to Explore",
                choices = homes_variables
              ),
              conditionalPanel(
                condition = "input.homes_var_col != 'Total Housing Units'",
                checkboxInput("show_natl_diff", "Show Difference From National Average", value = FALSE)
              ),
              card(
                class = "bg-light p-3 shadow-sm",
                card_header("How Does Your County Compare to National Stats? ", class = "bg-secondary text-white"),
                div(class = "mb-2", strong("Median Home Value:"), "$348,000"),
                div(class = "mb-2", strong("Median Gross Rent:"), "$1,348"),
                div(class = "mb-2", strong("Occupied Housing Units:"), "65%"),
                div(class = "mb-2", strong("Vacant Housing Units:"), "10%"),
                div(class = "mb-2", strong("Owner-Occupied Housing Units:"), "59%"),
                div(class = "mb-2", strong("Renter-Occupied Housing Units:"), "31%")
              )
            ),
            plotOutput("homes_county_map", click = "homes_map_click", height = "500px"),
            conditionalPanel(
              condition = "output.zoning_county_selected",
              layout_columns(
                col_widths = c(10, 2),
                plotlyOutput("zoning_map"),
                selectInput(
                  "zoning_var_col",
                  label = "Select a Variable to Explore",
                  choices = zoning_variables
                )
              )
            )
          )
        ),
      )
    ),
    
 #### JOBS PAGE #### 
 
    nav_panel(
      "Jobs",
      layout_column_wrap(
        width = 1,
        card(
          card_header(class = "bg-primary", "State Jobs"),
          card_body(
            sidebarLayout(
              sidebarPanel(
                p("Jobs Placeholder Text")
              ),
              mainPanel(
                plotOutput("jobs_plot", height = "600px")
              )
            )
          )
        ),
        card(
          card_header(class = "bg-primary", "County Level Exploration"),
          layout_sidebar(
            sidebar = sidebar(
              bg = "lightgrey",
              selectInput("job_county_col", 
                          label = "Select a County to Explore",
                          choices = county$NAME)
            ),
            plotOutput("jobs_county")
          )
        ),
        card(
          card_header(class = "bg-primary", "County Level Exploration"),
          layout_sidebar(
            sidebar = sidebar(
              bg = "lightgrey",
              selectInput(
                "jobs_var_col", 
                label = "Select a Variable to Explore",
                choices = jobs_variables
              ),
              conditionalPanel(
                condition = "input.jobs_var_col != `Labor Force`",
                checkboxInput("show_natl_diff", "Show Difference From National Average", value = FALSE)
              )
            ),
            layout_columns(
              col_widths = c(7, 5), 
              plotOutput("jobs_county_map", height = "500px"),
              card(
                class = "bg-light p-3 shadow-sm",
                card_header("How Does Your County Compare to National Stats? ", class = "bg-secondary text-white"),
                div(class = "mb-2", strong("Labor Force:"), "X%"),
                div(class = "mb-2", strong("Unemployed Population:"), "4.2%"),
                div(class = "mb-2", strong("High School Graduate or Equivalent*:"), "27.9%"),
                div(class = "mb-2", strong("Bachelor's Degree*:"), "23.5%"),
                div(class = "mb-2", strong("Master's Degree*:"), "9.4%"),
                div(class = "mb-2", strong("Doctorate Degree*:"), "2.1%"),
                div(class = "mb-2", strong("Professional School Degree*:"), "1.5%"),
                div(class = "mb-2", strong("Workers Who Drive Alone:"), "77%"),
                div(class = "mb-2", strong("Workers Using Public Transport:"), "5%"),
                div(class = "mb-2", strong("Mean Travel Time to Work (Minutes):"), "27"),
                div(class = "mb-2", "*indicates highest level of education at this level"),
              )
            )
          )
        ),
        card(
          card_header(class = "bg-primary", "Dependency Ratio"),
          card_body(
            sidebarLayout(
              sidebarPanel(
                p("Dependency Ratio")
              ),
              mainPanel(
                plotOutput("dependency_plot", height = "600px")
              )
            )
          )
        ),
        
        card(
          card_header(class = "bg-primary", "Job Openings"),
          card_body(
            plotOutput("job_opening_plot", height = "600px")
          )
        ), 
        card(
          card_header(class = "bg-primary", "County Rankings"),
          card_body(
            plotOutput("county_rank_plot", height = "600px")
          )
        ),
        
      )
    )
    
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
  
  output$pop_county_map <- renderPlot({
    req(input$pop_var_col)
    
    show_diff <- isTRUE(input$show_natl_diff)
    
    plot_county_map_population(
      df = vt_map,
      county_col = input$pop_var_col,
      show_diff = show_diff
    )
  })
  
  output$jobs_county_map <- renderPlot({
    req(input$jobs_var_col)
    
    show_diff <- isTRUE(input$show_natl_diff)
    
    plot_county_map_jobs(df = vt_map, 
                         county_col = input$jobs_var_col, 
                         show_diff = show_diff)
  })
  
  output$age_plot <- renderPlot({
    plot_age_distribution(state_age_data, natl_age_data, collierFL_age_data)
  })
  
  output$county_plot <- renderPlot({
    plot_county_capacities(filtered_data(), county = input$selected_county)
  })
  
  output$jobs_homes_gauge <- renderPlot({
    jobs_homes_index_scale(county_caps_df(), county = input$selected_county)
  })
  
  output$home_plot <- renderPlot({
    
    view_county <- isTRUE(input$show_homes_county_view)
    
    if (view_county) {
      plot_county_housing(housing, input$selected_homes_county)
    } else {
    plot_state_housing(housing)
    }
    
  })
  
  output$jobs_plot <- renderPlot({
    plot_prime_working_age(prime_age_df)
  })
  
  output$jobs_county <- renderPlot({
    plot_lf_county(labor_force_df, input$job_county_col)
  })
  
  output$dependency_plot <- renderPlot({
    plot_dependency_ratio(dependency_df)
  })
  
  
  # Zoning interactive map
  output$homes_county_map <- renderPlot({
    req(input$homes_var_col)
    
    show_diff <- isTRUE(input$show_natl_diff)
    
    plot_county_map_homes(df = vt_map, 
                          county_col = input$homes_var_col,
                          show_diff = show_diff)
  })
  
  selected_zoning_county <- reactiveVal(NULL)
  observeEvent(input$homes_map_click, {
    click <- input$homes_map_click
    if (is.null(click)) return()
    
    # Convert click to sf point
    click_point <- st_sfc(st_point(c(click$x, click$y)), crs = st_crs(vt_map))
    
    # Find which county was clicked
    clicked_index <- st_intersects(click_point, vt_map, sparse = FALSE)
    
    if (any(clicked_index)) {
      clicked_name <- vt_map$NAME[which(clicked_index)[1]]
      selected_zoning_county(clicked_name)
    }
  })
  output$zoning_county_selected <- reactive({
    !is.null(selected_zoning_county())
  })
  outputOptions(output, "zoning_county_selected", suspendWhenHidden = FALSE)
  
  output$zoning_map <- renderPlotly({
    req(selected_zoning_county())
    plot_county_zoning(
      zoning, 
      county_selection = selected_zoning_county(), 
      var_selected = input$zoning_var_col
    )
  })
  
  
  output$job_opening_plot <- renderPlot({
    plot_job_opening_rate(job_opening_df)
  })
  
  output$county_rank_plot <- renderPlot({
    plot_rank(rank_df)
  })

}

shinyApp(ui, server)

