
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
library(purrr)
library(gridExtra)
library(googledrive)

folder_id <- "12n9suUDVN7xsxzblbCsEY1zwJPmeY2KK"
csv_files <- drive_ls(as_id(folder_id), type = "csv")
data_list <- lapply(csv_files$id, function(file_id) {
  temp_path <- tempfile(fileext = ".csv")
  drive_download(as_id(file_id), path = temp_path, overwrite = TRUE)
  read_csv(temp_path)
})
names(data_list) <- csv_files$name

pth <- getwd()
# source(paste0(pth, "/pull_data.R"))
# url <- "https://drive.google.com/uc?export=download&id="
source(paste0(pth, "/process_data.R"))
source(paste0(pth, "/read_data.R"))
source(paste0(pth, "/population.R"))
source(paste0(pth, "/jobs.R"))
source(paste0(pth, "/housing.R"))
source(paste0(pth, "/recommendation_model.R"))

#### Global Variables ####
Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")
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
state <- read_state_data()
county <- read_county_data()
town <- read_town_data()
natl <- read_natl_data()
collierFL <- read_collierFL_data()
county_pop_df <- read_county_pop_data()

housing <- read_housing_data() |> 
  process_housing_data()
zoning <- read_zoning_data()

labor_force_df <- read_lf_data()

job_opening_df <- read_job_openings_data()
county_job_opening_df <- read_county_job_openings_data()
rank_df <- read_rank_data()

vt_map <- county_level_map(county)
town_map <- town_level_map()

#### Process data ####
state_age_data <- build_age_df(state)
county_age_data <- build_county_age_df(county)
natl_age_data <- build_age_df(natl)
collierFL_age_data <- build_age_df(collierFL)
county_town_association <- town_map |> data.frame() |> select(TOWNNAMEMC, NAME) 

prime_age_df <- process_prime_age_data(labor_force_df)
dependency_df <- process_dependency_data(labor_force_df)

#### UI #### 

theme <- bs_theme(
  primary = "darkgreen", secondary = "#2c3e50",
  base_font = c("Georgia", "sans-serif"),
  heading_font = c("Georgia", "sans-serif"),
  "input-border-color" = "darkgreen"
)

#### HOME PAGE ####

ui <- page_fluid(
  theme = theme,
  card(
    card_header(
      class = "bg-primary",
      tags$div(
        style = "font-size: 36px; text-align: center; width: 100%;",
        "Vermont Futures Project: Interactive Dashboard"
      )
    )
  ),
  navset_card_pill(
    nav_panel("Home Page",
              div(  
                style = "display: flex; justify-content: center; gap: 20px;",
                card(
                  style = "width: 250px; height: 250px;",
                  card_image(
                    file = "vt-futures-logo.png",
                    href = "https://vtfuturesproject.org/"
                  ),
                ),
                card(
                  style = "width: 250px; height: 250px;",
                  card_image(
                    file = "midd_math_stat.png",
                    href = "https://www.middlebury.edu/college/academics/mathematics"
                  )
                ),
              ),
              card_body(
                p("Welcome to the Vermont Futures Project interactive dashboard! Click into the people, housing, and jobs
    pages to learn more about these categories both at the state and county levels in Vermont. Check out the recommendations
    page to explore our data-driven recommendations to steer each county towards a thriving economy.", style = "text-align: center; width: 50%; margin: 0 auto; font-size:24px")
              ),
              card(
                card_header(class = "bg-primary", "About the project"),
                card_body(
                  p("This project is a collaboration between Vermont Futures Projet and the senior seminar for the statistics major
    at Middlebury College. Will Lavey, Eujin Chae, Carly McAdam (all Middlebury '25), and Alex Lyford (Middlebury College Department of Statistics) 
      worked with Kevin Chu to create this dashboard for VFP in spring 2025.", style = "width: 100%; font-size:16px")
                )
              )
    ),
    
    #### POPULALTION PAGE ####
    nav_panel("People",
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
          to better meet the needs of all Vermonters."),
                        checkboxInput("show_pop_county_view", "View by County", value = FALSE),
                        conditionalPanel(
                          condition = "input.show_pop_county_view == true",
                          selectInput("selected_pop_county", "Select a County",
                                      choices = unique(county_age_data$NAME),
                                      selected = NULL)
                        )
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
                      width = 425,
                      bg = "lightgrey",
                      selectInput(
                        "pop_var_col", 
                        label = "Select a Variable to Explore",
                        choices = population_variables
                      ),
                      conditionalPanel(
                        condition = "input.pop_var_col != 'Total Population'",
                        checkboxInput("show_natl_diff", "Show Difference From National Average", value = FALSE)
                      ),
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
                    ),
                    plotOutput("pop_county_map", height = "500px")
                  )
                )
              )
    ),
    
    #### HOUSING PAGE ####
    
    nav_panel(
      "Housing",
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
            width = 425,
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
          plotOutput("housing_map_plot", height = "900px"),
        )
      ),
      card(
        card_header(class = "bg-primary", "Zoning Exploration"),
        layout_sidebar(
          sidebar = sidebar(
            width = 425,
            bg = "lightgrey",
            card(
              class = "bg-light p-3 shadow-sm",
              card_header("Town-Level Zoning Exploration", class = "bg-secondary text-white"),
              selectInput(
                "zoning_county",
                label = "Select a County",
                choices = unique(vt_map$NAME),
                selected = "Addison"
              ),
              selectInput(
                "zoning_town",
                label = "Select a Town",
                choices = county_town_association %>%
                  filter(NAME == "Addison") %>%
                  pull(TOWNNAMEMC),
                selected = "Middlebury"
              ),
              selectInput(
                "zoning_var_col",
                label = "Select a Variable to Explore",
                choices = zoning_variables,
                selected = "1F Allowance"
              )
            )
          ),
          plotOutput("county_town_map", height = "400px"),
          leafletOutput("town_leaflet", height = "500px")
        )
      )
    ),
    
    #### JOBS PAGE #### 
    
    nav_panel(
      "Jobs",
      card(
        card_header(class = "bg-primary", "State Jobs"),
        card_body(
          sidebarLayout(
            sidebarPanel(
              p("Vermonters’ top economic concern is affordability. Demographics are the
                key factor increasing cost of living. According to the United Nations, a high dependency ratio
                indicates that the economically active population and the overall economy face a greater burden to support and
                provide the social services needed by children and by older persons who are often economically dependent.
                In the past, Vermont had a large working-age population relative to the young and elderly, providing a robust
                workforce and a healthy tax base to support demand on public services. While the overall population size has
                remained relatively stagnant since 2000, the composition of Vermont’s population has shifted dramatically."
              ),
            ),
            mainPanel(
              plotOutput("jobs_plot", height = "500px")
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
            width = 425,
            bg = "lightgrey",
            selectInput(
              "jobs_var_col", 
              label = "Select a Variable to Explore",
              choices = jobs_variables
            ),
            conditionalPanel(
              condition = "input.jobs_var_col != `Labor Force`",
              checkboxInput("show_natl_diff", "Show Difference From National Average", value = FALSE)
            ),
            card(
              class = "bg-light p-3 shadow-sm",
              card_header("How Does Your County Compare to National Stats? ", class = "bg-secondary text-white"),
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
          ),
          plotOutput("jobs_county_map", height = "400px")
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
              plotOutput("dependency_plot", height = "400px")
            )
          )
        )
      ),
      
      card(
        card_header(class = "bg-primary", "Job Openings"),
        card_body(
          plotOutput("job_opening_plot", height = "400px")
        )
      ), 
      card(
        card_header(class = "bg-primary", "County Rankings"),
        card_body(
          plotOutput("county_rank_plot", height = "400px")
        )
      ),
    ),
    nav_panel(
      "Recommendations",
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
      )
    )
  )
)


#### SUMMARY PAGE ####





#### Server ####

server <- function(input, output, session) {
  
  #### POPULATION PLOTS ####
  
  county_caps_df <- reactive({
    build_county_caps_df(county_pop_df)
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
    view_county <- isTRUE(input$show_pop_county_view)
    
    if (view_county) {
      plot_county_age_distribution(input$selected_pop_county, county_age_data, natl_age_data, collierFL_age_data)
    } else {
      plot_age_distribution(state_age_data, natl_age_data, collierFL_age_data)
    }
  })
  
  output$county_plot <- renderPlot({
    plot_county_capacities(filtered_data(), county = input$selected_county)
  })
  
  output$jobs_homes_gauge <- renderPlot({
    jobs_homes_index_scale(county_caps_df(), county = input$selected_county)
  })
  
  #### HOUSING PLOTS ####
  
  output$home_plot <- renderPlot({
    view_county <- isTRUE(input$show_homes_county_view)
    if (view_county) {
      plot_county_housing(housing, input$selected_homes_county)
    } else {
      plot_state_housing(housing)
    }
  })
  
  observeEvent(input$zoning_county, {
    towns <- county_town_association %>%
      filter(NAME == input$zoning_county) %>%
      pull(TOWNNAMEMC)
    
    updateSelectInput(
      session,
      "zoning_town",
      choices = towns,
      selected = towns[1]
    )
  })
  
  output$housing_map_plot <- renderPlot({
    
    req(input$homes_var_col)
    show_diff <- isTRUE(input$show_natl_diff)
    plot_county_map_homes(df = vt_map,
                          county_col = input$homes_var_col,
                          show_diff = show_diff)
  })
  
  output$county_town_map <- renderPlot({
    req(input$zoning_county)
    plot_county_map(town_level_df = town_map,
                    county_selection = input$zoning_county)
  })
  
  output$town_leaflet <- renderLeaflet({
    req(input$zoning_town)
    plot_town_zoning(zoning_df = zoning,
                     county_town_association = county_town_association,
                     county_selection = input$zoning_county,
                     town_selection = input$zoning_town,
                     var_selected = input$zoning_var_col)
  })
  
  
  #### JOB PLOTS ####
  
  output$job_opening_plot <- renderPlot({
    plot_job_opening_rate(job_opening_df)
  })
  
  output$county_rank_plot <- renderPlot({
    plot_rank(rank_df)
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
  
}

#### run ####
shinyApp(ui, server)

