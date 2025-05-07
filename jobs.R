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
                                 as.character(Population_Type))) |> 
    filter(!(age_group %in% c("age 0 to 14")))
  
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
      breaks = c("labor_force_count", "prime_labor_force_count", "non_labor_force_count"),
      values = c("labor_force_count" = "#225ea8", 
                 "non_labor_force_count" = "#41b6c4",
                 "prime_labor_force_count" = "#08306b", 
                 "prime_non_labor_force_count" = "#41b6c4"),
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
  ggplot(prime_age_df, aes(x = reorder(NAME, prime_labor_pr), y = prime_labor_pr, fill = prime_labor_pr)) +
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
    scale_fill_gradient(high = "steelblue", low = "darkred") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(labels = function(x) sub(" County,.*", "", x)) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          legend.position = "none") + 
    geom_hline(yintercept = 0.833, color = "black", linetype = "dashed", size = 1) +
    annotate("text", x = 1, y = 0.84, label = "National Average", hjust = 0, vjust = 2, size = 4, fontface = "italic")
}

plot_dependency_ratio <- function(dependency_df) {
  ggplot(dependency_df, aes(x = reorder(NAME, dependency_ratio), y = dependency_ratio, fill = dependency_ratio)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.2f", dependency_ratio)), 
              hjust = -0.1, 
              size = 4, 
              color = "black") +
    coord_flip() +
    labs(
      title = "Dependency Ratio by County",
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
      axis.text.x = element_text(color = "black", size = 10),
      axis.text.y = element_text(color = "black", size = 10),
      plot.caption = element_text(hjust = 0, size = 8),
      legend.position = "none"
    )
}

plot_county_map_jobs <- function(df, county_col, show_diff) {
  percent_cols <- c(
    "Labor Force", "Unemployed Population",
    "High School Graduate or Equivalent", "Bachelor's Degree", 
    "Master's Degree", "Professional School Degree", "Doctorate Degree",
    "Workers Who Drive Alone", "Workers Using Public Transport"
  )
  
  national_averages <- c(
    "Labor Force" = 0, 
    "Unemployed Population" = 0.042,
    "High School Graduate or Equivalent" = 0.279, 
    "Bachelor's Degree" = 0.235, 
    "Master's Degree" = 0.094, 
    "Professional School Degree" = 0.015, 
    "Doctorate Degree" = 0.021,
    "Workers Who Drive Alone" = 0.77, 
    "Workers Using Public Transport" = 0.05,
    "Mean Travel Time to Work (Minutes)" = 27
  )
  
  county_sym <- sym(county_col)
  is_percent <- as_string(county_sym) %in% percent_cols

  if (show_diff && county_col %in% names(national_averages)) {
    national_value <- national_averages[[county_col]]
    
    df <- df %>%
      mutate(
        !!county_sym := as.numeric(gsub("[^0-9.]", "", !!county_sym)),
        diff = !!county_sym - national_value,
        value_label = if (is_percent) {
          case_when(
            diff > 0 ~ paste0("⬆️", round(diff * 100, 2), "%"),
            diff < 0 ~ paste0("⬇️", round(diff * 100, 2), "%"),
            TRUE ~ "0%"
          )
        } else {
          case_when(
            diff > 0 ~ paste0("⬆️", formatC(diff, format = "f", digits = 2)),
            diff < 0 ~ paste0("⬇️", formatC(diff, format = "f", digits = 2)),
            TRUE ~ "0"
          )
        }
      )
    
    fill_aes <- aes(fill = diff)
    label_aes <- aes(label = value_label)
    
    max_diff <- max(df$diff, na.rm = TRUE)
    max_range <- max_diff + 0.25*max_diff
    
    min_diff <- min(df$diff, na.rm = TRUE)
    min_range <- min_diff - 0.25*min_diff
    
    fill_scale <- scale_fill_viridis_c(name = "Difference from\nnational average", 
                                       option = "D",
                                       limits = c(min_range, max_range),
                                       oob = scales::squish,
                                       labels = if (is_percent) percent_format(accuracy = 0.01) else waiver())
    
  } else {
    df <- df %>%
      mutate(
        value_label = if (is_percent) paste0(round(!!county_sym * 100, 2), "%") else !!county_sym
      )
    
    fill_aes <- aes(fill = !!county_sym)
    label_aes <- aes(label = value_label)
    
    fill_scale <- scale_fill_gradient(
      low = "honeydew", high = "darkgreen",
      name = NULL, 
      labels = if (is_percent) percent_format(accuracy = 0.01) else waiver()
    )
  }
  
  map <- ggplot(df) +
    geom_sf(fill_aes) +
    geom_sf_label(label_aes) +
    geom_sf_label(aes(label = NAME), nudge_y = -0.1, size = 5.5) +
    fill_scale +
    labs(title = paste0(county_col, "\n")) +
    coord_sf(expand = FALSE) +
    theme_void() + 
    theme(
      plot.title = element_text(size = 20, face = "bold", family = "Georgia", hjust = 0.5),
      plot.margin = margin(5, 10, 5, 10),
      legend.position="top",
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(3,"cm") 
    )
  
  return(map)
}

plot_job_opening_rate <- function(job_openings_long) {
  ggplot(job_openings_long, aes(x = date, y = OpeningRate, color = Region, linetype = Region)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("Vermont" = "darkgreen", "United States" = "orange")) +
    scale_linetype_manual(values = c("Vermont" = "solid", "United States" = "dashed")) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b\n%Y"
    ) +
    labs(
      title = "Job Opening Rate: Vermont vs US",
      x = NULL,
      y = NULL,
      color = NULL,
      linetype = NULL,
      caption = "Note: The job openings rate is computed by dividing the number of job openings\nby the sum of employment and job openings and multiplying that quotient by 100.\nSource: U.S. Bureau of Labor Statistics."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
      legend.position = "top",
      axis.text.x = element_text(size = 10),  
      axis.text.y = element_text(size = 10)
    )
}

plot_county_job_opening <- function(county_job_opening_df) {
  ggplot(county_job_opening_df, aes(x = reorder(county, job_opening_rate), y = job_opening_rate, fill = job_opening_rate)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.2f", job_opening_rate)), 
              hjust = -0.1, 
              size = 4, 
              color = "black") +
    coord_flip() +
    labs(
      title = "Job Opening Rate By County",
      x = "County",
      y = "Job Opening Rate",
      caption = "Note: Job Opening rate = (active job openings for county) / (active job openings + total employed in county)\nSource: Vermont JobLink & U.S. Bureau of Labor Statistics."
    ) +
    scale_fill_gradient(low = "darkgreen", high = "darkgreen") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(labels = function(x) sub(" County,.*", "", x)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 19 ),
      axis.text.x = element_text(color = "black", size = 10),
      axis.text.y = element_text(color = "black", size = 10),
      plot.caption = element_text(hjust = 0, size = 8),
      legend.position = "none"
    )
}

plot_rank <- function(rank_df) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  
  rank_df |>
    pivot_longer(-county, names_to = "variable") |>
    mutate(variable = str_replace_all(variable, "_", " ")) |>
    group_by(variable) |>
    mutate(rank = case_when(
      variable %in% c("job opening rate", "prime labor pr") ~ rank(-value),
      variable == "dependency ratio" ~ rank(value),
      TRUE ~ NA_real_
    )) |>
    ggplot() +
    geom_tile(aes(x = variable, y = county, fill = rank)) +
    scale_fill_gradientn(
      colors = c("darkblue", "white", "darkred"),
      values = scales::rescale(c(1, 7, 13))
    ) +
    geom_text(aes(x = variable, y = county, label = rank, color = rank)) +
    scale_color_gradientn(
      colors = c("white", "black", "white"),
      values = scales::rescale(c(1, 7, 14)),
      guide = "none"  # hide color legend for text
    )+
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # title bigger and centered
      legend.title = element_text(size = 14),  # legend title size
      legend.text = element_text(size = 12)    # legend label size
    )
}

