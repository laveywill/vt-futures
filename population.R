
pth <- getwd()
source(paste0(pth, "/read_data.R"))


# STATE LEVEL POPULATION DATA

plot_age_distribution <- function(state_df, national_df, compare_df) {
  process_age_data <- function(df) {
    df %>%
      mutate(
        age_bucket = case_when(
          age_group %in% c("Under 5 years", "5 to 9 years", "10 to 14 years") ~ "0-14 years",
          age_group %in% c("15 to 19 years", "20 to 24 years") ~ "15-24 years",
          age_group %in% c("25 to 29 years","30 to 34 years","35 to 39 years",
                           "40 to 44 years","45 to 49 years") ~ "25-49 years",
          age_group %in% c("50 to 54 years", "55 to 59 years", "60 to 64 years") ~ "50-64 years",
          age_group %in% c("65 to 69 years", "70 to 74 years", "75 to 79 years",
                           "80 to 84 years", "85 years and over") ~ "65+ years",
          TRUE ~ "Unknown"
        ),
        age_label = case_when(
          age_group == "Under 5 years" ~ "Under 5",
          age_group == "85 years and over" ~ "85+",
          TRUE ~ gsub(" to ", " - ", gsub(" years", "", age_group))
        )
      )
  }
  state_df <- process_age_data(state_df)
  national_df <- process_age_data(national_df)
  compare_df <- process_age_data(compare_df)
  
  age_levels <- rev(c("85+", "80 - 84", "75 - 79", "70 - 74",
                      "65 - 69", "60 - 64", "55 - 59", "50 - 54",
                      "45 - 49", "40 - 44", "35 - 39", "30 - 34", 
                      "25 - 29", "20 - 24", "15 - 19", "10 - 14", 
                      "5 - 9", "Under 5"))
  state_df$age_label <- factor(state_df$age_label, levels = age_levels)
  national_df$age_label <- factor(national_df$age_label, levels = age_levels)
  compare_df$age_label <- factor(compare_df$age_label, levels = age_levels)
  
  # Scale US/Collier FL density to match size of VT
  scale_factor_us <- sum(state_df$total_population) / sum(national_df$total_population)
  scale_factor_compare <- sum(state_df$total_population) / sum(compare_df$total_population)
  
  national_density <- national_df %>%
    mutate(age_numeric = as.numeric(age_label),
           scaled_population = total_population * scale_factor_us) %>%
    complete(age_label = age_levels, fill = list(`Total Population` = 0)) %>%
    arrange(age_label)
  
  compare_density <- compare_df %>%
    mutate(age_numeric = as.numeric(age_label),
           scaled_population = total_population * scale_factor_compare) %>%
    complete(age_label = age_levels, fill = list(`Total Population` = 0)) %>%
    arrange(age_label)
  
  ggplot(state_df, aes(x = age_label, y = total_population, fill = age_bucket)) +
    geom_bar(stat = "identity",show.legend = FALSE) +
    geom_line(data = national_density, aes(x = age_label, y = scaled_population, group = 1, color = "US (scaled)"),
              size = 1.2) +
    geom_line(data = compare_density, aes(x = age_label, y = scaled_population, group = 1, color = "Collier FL (scaled)"),
              size = 1.2, linetype = 3) +
    labs(
      x = "Age Group", 
      y = "\nTotal Population", 
      title = "Vermont Age Group Distribution (2023)",
      color = NULL
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c(
      "0-14 years" = "deepskyblue1", 
      "15-24 years" = "aquamarine2", 
      "25-49 years" = "aquamarine4", 
      "50-64 years" = "orange", 
      "65+ years" = "red"
    )) +
    scale_color_manual(values = c("US (scaled)" = "black", "Collier FL (scaled)" = "black")) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12),
      text = element_text(family = "Georgia"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
}

plot_county_map_population <- function(df, county_col, show_diff = FALSE) {
  percent_cols <- c(
    "White Alone",
    "Black or African American Alone",
    "Asian Alone",
    "Hispanic or Latino Population",
    "Total Male Population", 
    "Total Female Population"
  )
  
  national_averages <- c(
    "Median Age" = 38.7,
    "Total Male Population" = 0.495,
    "Total Female Population" = 0.505,
    "White Alone" = 0.61,
    "Black or African American Alone" = 0.14,
    "Asian Alone" = 0.07,
    "Hispanic or Latino Population" = 0.19
  )
  
  county_sym <- sym(county_col)
  is_percent <- as_string(county_sym) %in% percent_cols
  
  if (show_diff && county_col %in% names(national_averages)) {
    national_value <- national_averages[[county_col]]
    
    df <- df %>%
      mutate(
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
    max_range <- max_diff + 0.2*max_diff
    
    min_diff <- min(df$diff, na.rm = TRUE)
    min_range <- min_diff - 0.2*min_diff
    
    fill_scale <- scale_fill_viridis_c(name = "Difference from\nnational average", 
                                       option = "D",
                                       limits = c(min_range, max_range),
                                       oob = scales::squish,
                                       labels = if (is_percent) percent_format(accuracy = 0.01) else waiver())
  } else {
    df <- df %>%
      mutate(
        value_label = if (is_percent) paste0(round(!!county_sym * 100, 2), "%") else scales::comma(!!county_sym)
      )
    
    fill_aes <- aes(fill = !!county_sym)
    label_aes <- aes(label = value_label)
    
    fill_scale <- scale_fill_gradient(
      low = "honeydew", 
      high = "darkgreen",
      name = NULL, 
      labels = if (is_percent) percent_format(accuracy = 0.01) else scales::comma_format(accuracy = 1)
    )
  }
  
  map <- ggplot(df) +
    geom_sf(fill_aes) +
    geom_sf_label(label_aes, check_overlap = T) +
    geom_sf_label(aes(label = NAME), nudge_y = -0.1, size = 5.5, check_overlap = T) +
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

plot_county_age_distribution <- function(county_selection, county_df, national_df, compare_df) {
  process_age_data <- function(df, county_processing = FALSE) {
    if (county_processing) {
      df <- df |> filter(NAME == county_selection)
    }
    df <- df %>%
      mutate(
        age_bucket = case_when(
          age_group %in% c("Under 5 years", "5 to 9 years", "10 to 14 years") ~ "0-14 years",
          age_group %in% c("15 to 19 years", "20 to 24 years") ~ "15-24 years",
          age_group %in% c("25 to 29 years","30 to 34 years","35 to 39 years",
                           "40 to 44 years","45 to 49 years") ~ "25-49 years",
          age_group %in% c("50 to 54 years", "55 to 59 years", "60 to 64 years") ~ "50-64 years",
          age_group %in% c("65 to 69 years", "70 to 74 years", "75 to 79 years",
                           "80 to 84 years", "85 years and over") ~ "65+ years",
          TRUE ~ "Unknown"
        ),
        age_label = case_when(
          age_group == "Under 5 years" ~ "Under 5",
          age_group == "85 years and over" ~ "85+",
          TRUE ~ gsub(" to ", " - ", gsub(" years", "", age_group))
        )
      )
  }
  county_df <- process_age_data(county_df, county_processing = TRUE)
  national_df <- process_age_data(national_df)
  compare_df <- process_age_data(compare_df)
  
  age_levels <- rev(c("85+", "80 - 84", "75 - 79", "70 - 74",
                      "65 - 69", "60 - 64", "55 - 59", "50 - 54",
                      "45 - 49", "40 - 44", "35 - 39", "30 - 34", 
                      "25 - 29", "20 - 24", "15 - 19", "10 - 14", 
                      "5 - 9", "Under 5"))
  county_df$age_label <- factor(county_df$age_label, levels = age_levels)
  national_df$age_label <- factor(national_df$age_label, levels = age_levels)
  compare_df$age_label <- factor(compare_df$age_label, levels = age_levels)
  
  # Scale US/Collier FL density to match size of VT
  scale_factor_us <- sum(county_df$total_population) / sum(national_df$total_population)
  scale_factor_compare <- sum(county_df$total_population) / sum(compare_df$total_population)
  
  national_density <- national_df %>%
    mutate(age_numeric = as.numeric(age_label),
           scaled_population = total_population * scale_factor_us) %>%
    complete(age_label = age_levels, fill = list(`Total Population` = 0)) %>%
    arrange(age_label)
  
  compare_density <- compare_df %>%
    mutate(age_numeric = as.numeric(age_label),
           scaled_population = total_population * scale_factor_compare) %>%
    complete(age_label = age_levels, fill = list(`Total Population` = 0)) %>%
    arrange(age_label)
  
  ggplot(county_df, aes(x = age_label, y = total_population, fill = age_bucket)) +
    geom_bar(stat = "identity",show.legend = FALSE) +
    geom_line(data = national_density, aes(x = age_label, y = scaled_population, group = 1, color = "US (scaled)"),
              size = 1.2) +
    geom_line(data = compare_density, aes(x = age_label, y = scaled_population, group = 1, color = "Collier FL (scaled)"),
              size = 1.2, linetype = 3) +
    labs(
      x = "Age Group", 
      y = "\nTotal Population", 
      title = paste("Vermont Age Group Distribution", county_selection, "(2023)"),
      color = NULL
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c(
      "0-14 years" = "deepskyblue1", 
      "15-24 years" = "aquamarine2", 
      "25-49 years" = "aquamarine4", 
      "50-64 years" = "orange", 
      "65+ years" = "red"
    )) +
    scale_color_manual(values = c("US (scaled)" = "black", "Collier FL (scaled)" = "black")) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12),
      text = element_text(family = "Georgia"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
}
