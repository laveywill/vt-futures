#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(leaflet)
library(sf)
library(tidyverse)
library(tigris)

vt_counties <- counties(state = "VT", cb = TRUE, class = "sf")

census_data <- data.frame(
  NAME = vt_counties$NAME,
  population = sample(10000:70000, length(vt_counties$NAME), replace = TRUE) # Replace with actual data
)

vt_map <- left_join(vt_counties, census_data, by = "NAME")


ui <- fluidPage(
  titlePanel("Vermont Census Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Select Variable:", choices = c("population")),
      sliderInput("opacity", "Map Opacity:", min = 0.2, max = 1, value = 0.7)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)


server <- function(input, output, session) {
  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = "Blues", domain = vt_map[[input$var]])
    leaflet(vt_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(get(input$var)),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = input$opacity,
        highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE),
        label = ~paste(NAME, "-", get(input$var))
      ) %>%
      addLegend(pal = pal, values = vt_map[[input$var]], title = input$var)
  })
}

shinyApp(ui, server)
