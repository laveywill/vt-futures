library(shiny)

create_county_plot_9 <- function(df, county) {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(tools)
  library(stringr)
  
  county_full <- paste0(toTitleCase(county), " County, Vermont")
  
  county_data <- df %>% 
    filter(NAME == county_full)
  
  # Prepare data
  county_data_long <- county_data %>%
    mutate(`Non Labor Force Count` = Count - `Labor Force Count`) %>%
    pivot_longer(
      cols = c(`Labor Force Count`, `Non Labor Force Count`),
      names_to = "Population_Type",
      values_to = "Population"
    ) %>%
    mutate(
      Population_Type = factor(
        Population_Type, 
        levels = c("Labor Force Count", "Non Labor Force Count")
      ),
      `Age Groups` = gsub("_", " ", `Age Groups`)
    )
  
  county_data_long <- county_data_long %>%
    mutate(age_lower = as.numeric(str_extract(`Age Groups`, "^[0-9]+"))) %>%
    mutate(`Age Groups` = factor(
      `Age Groups`, 
      levels = unique(`Age Groups`[order(age_lower)])
    ))
  
  county_data_long <- county_data_long %>%
    group_by(`Age Groups`) %>%
    mutate(total_population = sum(Population)) %>%
    ungroup() %>%
    mutate(labor_percent = ifelse(
      Population_Type == "Labor Force Count",
      Population / total_population * 100, 
      NA
    ))
  
  
  county_data_long <- county_data_long %>%
    mutate(is_prime = str_detect(`Age Groups`, "25 to 34|35 to 44|45 to 54"))
  
  
  county_data_long <- county_data_long %>%
    mutate(fill_factor = if_else(is_prime, 
                                 paste0("prime_", Population_Type), 
                                 as.character(Population_Type)))
  
  max_population <- county_data_long %>%
    group_by(`Age Groups`) %>%
    summarise(total_population = sum(Population)) %>%
    summarise(max_total = max(total_population)) %>%
    pull(max_total)
  
  upper_limit <- max_population * 1.1
  
  
  p <- ggplot(county_data_long, aes(x = `Age Groups`, y = Population, fill = fill_factor)) +
    geom_bar(
      stat = "identity", 
      position = position_stack(reverse = TRUE), 
      width = 0.7
    ) +
    
    geom_text(
      data = filter(county_data_long, Population_Type == "Labor Force Count"),
      aes(label = paste0(round(labor_percent, 1), "%")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      color = "white", 
      size = 5
    ) +
    coord_flip() +
    labs(
      title = paste0(county_full, " Labor Force and Population by Age"),
      subtitle = "Comparison of Labor Force vs. Non-Labor Force across Age Groups",
      x = NULL, 
      y = "Population",
      caption = "Source: American Community Survey (ACS)"
    ) +
    
    scale_fill_manual(
      name = "Population Type",
      breaks = c("Labor Force Count", "prime_Labor Force Count", "Non Labor Force Count"),
      values = c("Labor Force Count" = "#225ea8", 
                 "Non Labor Force Count" = "#41b6c4",
                 "prime_Labor Force Count" = "#08306b", 
                 "prime_Non Labor Force Count" = "#41b6c4"),
      labels = c("Labor Force", "Labor Force (Prime Age)", "Not in Labor Force")
    ) +
    scale_y_continuous(
      limits = c(0, upper_limit),
      expand = expansion(mult = c(0, 0.05))
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
      plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 10)),
      axis.text = element_text(color = "black"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major.y = element_blank(),   
      panel.grid.minor = element_blank()     
    )
  
  return(p)
}

create_prime_working_age_lfpr <- function(data) {
  ggplot(prime_working_age_lfpr, aes(x = reorder(NAME, prime_labor_pr), y = prime_labor_pr, fill = prime_labor_pr)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.2f", prime_labor_pr)), 
              hjust = -0.1, 
              size = 4, 
              color = "black") +
    coord_flip() +
    labs(title = "Prime Working-Age Adults Labor Force Participation Rate by County",
         x = "County",
         y = "Labor Force Participation Rate (%)" ,
         caption = "Source: American Community Survey (ACS)")+
    scale_fill_gradient(low = "steelblue", high = "darkred") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(labels = function(x) sub(" County,.*", "", x)) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          legend.position = "none")
}



#creating the dependency ratio plot
plot_dependency_ratio <- function(data, title = "Dependency Ratio by County") {
  ggplot(data, aes(x = reorder(NAME, dependency_ratio), y = dependency_ratio, fill = dependency_ratio)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.2f", dependency_ratio)), 
              hjust = -0.1, 
              size = 3, 
              color = "black") +
    coord_flip() +
    labs(
      title = title,
      x = "County",
      y = "Dependency Ratio",
      caption = "Note: Dependency ratio = (Age 0–14 + Age 65+) / (Age 16–64)\nSource: American Community Survey (ACS)"
    ) +
    scale_fill_gradient(low = "steelblue", high = "darkred") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(labels = function(x) sub(" County,.*", "", x)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 19 ),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      plot.caption = element_text(hjust = 0, size = 8),
      legend.position = "none"
    )
}


ui <- fluidPage(
  selectInput(inputId = "county",
              label = "Choose a county",
              choices = c("Addison County" = "addison",
                          "Orleans County" = "orleans",
                          "Bennington County"= "bennington",
                          "Caledonia County"= "caledonia",
                          "Chittenden County"= "chittenden",
                          "Essex County"= "essex",
                          "Franklin County"= "franklin",
                          "Grand Isle County"= "grand isle",
                          "Lamoille County"= "lamoille",
                          "Orange County"= "orange",
                          "Rutland County"="rutland",
                          "Washington County"="washington",
                          "Windham County"= "windham",
                          "Windsor County"= "windsor")),
  plotOutput(outputId = "plot1"),
  br(),  # Adds a simple line break
  tags$div(style = "height: 10px;"),
  plotOutput(outputId = "plot2"),
  plotOutput(outputId = "dependencyPlot")
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    create_county_plot_9(cleaned_df_age_distrbution, input$county)
  })
  
  output$plot2 <- renderPlot({
    #function place
    create_prime_working_age_lfpr(prime_working_age_lfpr)
  })
  
  output$dependencyPlot <- renderPlot({
    plot_dependency_ratio(dependency_ratio)
  })
  
}

shinyApp(ui, server)