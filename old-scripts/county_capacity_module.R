countyCapacityUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "section-title", "County-Level Breakdown of VFP Population Goals"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("selected_county"), "Select a County:", 
                    choices = NULL, 
                    selected = NULL)
      ),
      mainPanel(
        plotOutput(ns("county_plot"))
      )
    )
  )
}

# Module Server
countyCapacityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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
  })
}
