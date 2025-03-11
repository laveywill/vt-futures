
library(shiny)

library(leaflet)
library(sf)
library(tidyverse)
library(tigris)
library(censusapi)
library(bslib)
library(DT)

Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")
state_fips <- 50 # VT

# Data preparation

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


# Shiny App

ui <- fluidPage(
  titlePanel("Vermont Futures: Interactive Dashboard"),
  p("This is the main page for the data exploration dashboard. This should be placed right below the title."),
  
  div(
    style = "display: flex; justify-content: center;",
    navset_card_underline(
      
      nav_panel(
        title = "Population", 
        fluidRow(
          title = "State Population",
          column(4, textOutput("pop_text")),
          column(6, dataTableOutput("pop_state"), offset = 2)
          ),
        fluidRow(
          title = "County Population",
          column(12, plotOutput("pop"))
        )
        ),
      
      
      nav_panel("Jobs", p("Jobs data at the state level."), tableOutput("jobs")),
      nav_panel("Homes", p("Homes data at the state level."), tableOutput("homes")),
      nav_panel("Etc...", p("Etc..."), tableOutput("etc"))
    )
  )
)


server <- function(input, output, session) {
  
  output$pop_text <- renderText({
    "This is some test text for the page"
  })
  
  output$pop_state <- renderDataTable({
    vt_map |> 
      group_by(NAME) |> 
      select(NAME, `Total Population`)
  })
  
  output$pop <- renderPlot({
    vt_map |>
      ggplot() +
      geom_sf(aes(fill = `Total Population`)) +
      theme_void()
  })
  
  output$jobs <- renderDataTable(
    vt_map |> 
      filter()
  )
}

shinyApp(ui, server)
