census_api_key("d2c6932eca5b04592aaa4b32840c534b274382dc", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

plot_lf_county <- function(labor_force_df, county) {
  
  county_full <- paste0(tools::toTitleCase(county), " County, Vermont")
  
  county_data <- labor_force_df %>% 
    filter(NAME == county_full)
  
  # Prepare data
  county_data_long <- county_data %>%
    mutate(non_labor_force_count = count - labor_force_count) %>%
    pivot_longer(
      cols = c(labor_force_count, non_labor_force_count),
      names_to = "Population_Type",
      values_to = "Population"
    ) %>%
    mutate(
      Population_Type = factor(
        Population_Type, 
        levels = c("labor_force_count", "non_labor_force_count")
      ),
      age_group = gsub("_", " ", age_group)
    )
  
  county_data_long <- county_data_long %>%
    mutate(age_lower = as.numeric(str_extract(age_group, "^[0-9]+"))) %>%
    mutate(age_group = factor(
      age_group, 
      levels = unique(age_group[order(age_lower)])
    ))
  
  county_data_long <- county_data_long %>%
    group_by(age_group) %>%
    mutate(total_population = sum(Population)) %>%
    ungroup() %>%
    mutate(labor_percent = ifelse(
      Population_Type == "labor_force_count",
      Population / total_population * 100, 
      NA
    ))
  
  
  county_data_long <- county_data_long %>%
    mutate(is_prime = str_detect(age_group, "25 to 34|35 to 44|45 to 54"))
  
  
  county_data_long <- county_data_long %>%
    mutate(fill_factor = if_else(is_prime, 
                                 paste0("prime_", Population_Type), 
                                 as.character(Population_Type)))
  
  max_population <- county_data_long %>%
    group_by(age_group) %>%
    summarise(total_population = sum(Population)) %>%
    summarise(max_total = max(total_population)) %>%
    pull(max_total)
  
  upper_limit <- max_population * 1.1
  
  
  p <- ggplot(county_data_long, aes(x = age_group, y = Population, fill = fill_factor)) +
    geom_bar(
      stat = "identity", 
      position = position_stack(reverse = TRUE), 
      width = 0.7
    ) +
    
    geom_text(
      data = filter(county_data_long, Population_Type == "labor_force_count"),
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
      breaks = c("labor_force_count", "prime_Labor Force Count", "non_labor_force_count"),
      values = c("labor_force_count" = "#225ea8", 
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

plot_prime_working_age <- function(prime_age_df) {
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

plot_dependency_ratio <- function(dependency_df, title = "Dependency Ratio by County") {
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

get_lf_data <- function() {
  age_vars <- paste0("B01001_", sprintf("%03d", 2:49))
  labor_force_vars <- paste0("B23001_", sprintf("%03d", 1:173))
  
  vermont_age_data <- get_acs(
    geography = "county",
    state = "VT",
    variables = age_vars,
    year = 2020,
    survey = "acs5"
  )
  
  vermont_lf_age_data <- get_acs(
    geography = "county",
    state = "VT",
    variables = labor_force_vars,
    year = 2020,
    survey = "acs5"
  )
  
  acs_vars <- load_variables(2020, "acs5", cache = TRUE)
  
  acs_vars <- rename(acs_vars, variable =  name)
  
  binned_age_df <- left_join(vermont_age_data, acs_vars, by = "variable") %>%
    group_by(NAME) %>%
    summarise(
      age_0_to_14 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!Under 5 years", "Estimate!!Total:!!Female:!!Under 5 years",
        "Estimate!!Total:!!Male:!!5 to 9 years", "Estimate!!Total:!!Female:!!5 to 9 years",
        "Estimate!!Total:!!Male:!!10 to 14 years", "Estimate!!Total:!!Female:!!10 to 14 years"
      )], na.rm = TRUE),
      
      age_16_to_24 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!15 to 17 years", "Estimate!!Total:!!Female:!!15 to 17 years",
        "Estimate!!Total:!!Male:!!18 and 19 years", "Estimate!!Total:!!Female:!!18 and 19 years",
        "Estimate!!Total:!!Male:!!20 years", "Estimate!!Total:!!Female:!!20 years",
        "Estimate!!Total:!!Male:!!21 years", "Estimate!!Total:!!Female:!!21 years",
        "Estimate!!Total:!!Male:!!22 to 24 years", "Estimate!!Total:!!Female:!!22 to 24 years"
      )], na.rm = TRUE),
      
      age_25_to_34 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!25 to 29 years", "Estimate!!Total:!!Female:!!25 to 29 years",
        "Estimate!!Total:!!Male:!!30 to 34 years", "Estimate!!Total:!!Female:!!30 to 34 years"
      )], na.rm = TRUE),
      
      age_35_to_44 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!35 to 39 years", "Estimate!!Total:!!Female:!!35 to 39 years",
        "Estimate!!Total:!!Male:!!40 to 44 years", "Estimate!!Total:!!Female:!!40 to 44 years"
      )], na.rm = TRUE),
      
      age_45_to_54 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!45 to 49 years", "Estimate!!Total:!!Female:!!45 to 49 years",
        "Estimate!!Total:!!Male:!!50 to 54 years", "Estimate!!Total:!!Female:!!50 to 54 years"
      )], na.rm = TRUE),
      
      age_55_to_64 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!55 to 59 years", "Estimate!!Total:!!Female:!!55 to 59 years",
        "Estimate!!Total:!!Male:!!60 and 61 years", "Estimate!!Total:!!Female:!!60 and 61 years",
        "Estimate!!Total:!!Male:!!62 to 64 years", "Estimate!!Total:!!Female:!!62 to 64 years"
      )], na.rm = TRUE),
      
      age_65_and_over = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!65 and 66 years", "Estimate!!Total:!!Female:!!65 and 66 years",
        "Estimate!!Total:!!Male:!!67 to 69 years", "Estimate!!Total:!!Female:!!67 to 69 years",
        "Estimate!!Total:!!Male:!!70 to 74 years", "Estimate!!Total:!!Female:!!70 to 74 years",
        "Estimate!!Total:!!Male:!!75 to 79 years", "Estimate!!Total:!!Female:!!75 to 79 years",
        "Estimate!!Total:!!Male:!!80 to 84 years", "Estimate!!Total:!!Female:!!80 to 84 years",
        "Estimate!!Total:!!Male:!!85 years and over", "Estimate!!Total:!!Female:!!85 years and over"
      )], na.rm = TRUE)
    )
  
  binned_lf_age_df <- left_join(vermont_lf_age_data, acs_vars, by = "variable") %>%
    group_by(NAME) %>% 
    summarise(
      age_16_to_24 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!16 to 19 years:!!In labor force:", "Estimate!!Total:!!Female:!!16 to 19 years:!!In labor force:",
        "Estimate!!Total:!!Male:!!20 and 21 years:!!In labor force:", "Estimate!!Total:!!Female:!!20 and 21 years:!!In labor force:",
        "Estimate!!Total:!!Male:!!22 to 24 years:!!In labor force:", "Estimate!!Total:!!Female:!!22 to 24 years:!!In labor force:"
      )], na.rm = TRUE),
      
      age_25_to_34 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!25 to 29 years:!!In labor force:", "Estimate!!Total:!!Female:!!25 to 29 years:!!In labor force:",
        "Estimate!!Total:!!Male:!!30 to 34 years:!!In labor force:", "Estimate!!Total:!!Female:!!30 to 34 years:!!In labor force:"
      )], na.rm = TRUE),
      
      age_35_to_44 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!35 to 44 years:!!In labor force:", "Estimate!!Total:!!Female:!!35 to 44 years:!!In labor force:"
      )], na.rm = TRUE),
      
      age_45_to_54 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!45 to 54 years:!!In labor force:", "Estimate!!Total:!!Female:!!45 to 54 years:!!In labor force:"
      )], na.rm = TRUE),
      
      age_55_to_64 = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!55 to 59 years:!!In labor force:", "Estimate!!Total:!!Female:!!55 to 59 years:!!In labor force:",
        "Estimate!!Total:!!Male:!!60 and 61 years:!!In labor force:", "Estimate!!Total:!!Female:!!60 and 61 years:!!In labor force:",
        "Estimate!!Total:!!Male:!!62 to 64 years:!!In labor force:", "Estimate!!Total:!!Female:!!62 to 64 years:!!In labor force:"
      )], na.rm = TRUE),
      
      age_65_and_over = sum(estimate[label %in% c(
        "Estimate!!Total:!!Male:!!65 and 69 years:!!In labor force:", "Estimate!!Total:!!Female:!!65 and 69 years:!!In labor force:",
        "Estimate!!Total:!!Male:!!70 to 74 years:!!In labor force:", "Estimate!!Total:!!Female:!!70 to 74 years:!!In labor force:",
        "Estimate!!Total:!!Male:!!75 years and over:!!In labor force:", "Estimate!!Total:!!Female:!!75 years and over:!!In labor force:"
      )], na.rm = TRUE)
    )
  
  pivoted_binned_age_df <- binned_age_df |> 
    pivot_longer(
      cols = starts_with("age"),
      names_to = "age_group", 
      values_to = "count"
    )
  
  pivoted_binned_lf_age_df <- binned_lf_age_df |> 
    pivot_longer(
      cols = starts_with("age"),
      names_to = "age_group", 
      values_to = "labor_force_count"
    )
  
  cleaned_age_distribution <- left_join(pivoted_binned_age_df, pivoted_binned_lf_age_df, by = c("NAME", "age_group")) |> 
    mutate_if(is.numeric,coalesce,0)
  
  return(cleaned_age_distribution)
}

# takes the lf_data as input df
get_prime_age_data <- function(labor_force_df) {
  df <- labor_force_df %>%
    filter(age_group== "age_25_to_34"|age_group == "age_35_to_44"|age_group == "age_45_to_54") %>% 
    group_by(NAME) %>% 
    summarise(prime_labor_pr = sum(labor_force_count)/sum(count)) %>% 
    arrange(desc(prime_labor_pr))
  
  return(df)
}

get_dependency_data <- function(labor_force_df) {
  
  df <- labor_force_df %>%
    group_by(NAME) %>%
    summarise(
      dependents = sum(count[age_group %in% c("age_0_to_14", "age_65_and_over")], na.rm = TRUE),
      working_age = sum(count[age_group %in% c(
        "age_16_to_24", "age_25_to_34", "age_35_to_44", 
        "age_45_to_54", "age_55_to_64"
      )], na.rm = TRUE),
      dependency_ratio = dependents / working_age
    )
  
  return(df)
}
