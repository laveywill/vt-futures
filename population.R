<<<<<<< HEAD
=======
pth <- getwd()
source(paste0(pth, "/read_data.R"))
>>>>>>> refs/remotes/origin/main

# STATE LEVEL POPULATION DATA

plot_age_distribution <- function(df) {
  age_groups <- df %>%
    mutate(age_bucket = case_when(
      age_group %in% c("Under 5 years", "5 to 9 years", "10 to 14 years") ~ "0-14 years",
      age_group %in% c("15 to 19 years", "20 to 24 years") ~ "15-24 years",
      age_group %in% c("25 to 29 years","30 to 34 years","35 to 39 years","40 to 44 years","45 to 49 years") ~ "25-49 years",
      age_group %in% c("50 to 54 years", "55 to 59 years", "60 to 64 years") ~ "50-64 years",
      age_group %in% c("65 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 84 years", "85 years and over") ~ "65+ years",
      TRUE ~ "Unknown"
    ))
  
  age_groups$age_group <- factor(age_groups$age_group, levels = c(
    "85 years and over", "80 to 84 years", "75 to 79 years", "70 to 74 years",
    "65 to 69 years", "60 to 64 years", "55 to 59 years", "50 to 54 years",
    "45 to 49 years", "40 to 44 years", "35 to 39 years",
    "30 to 34 years", "25 to 29 years","20 to 24 years","15 to 19 years","10 to 14 years", 
    "5 to 9 years", "Under 5 years"
  ))
  
  ggplot(age_groups, aes(y = age_group, x = total_population, fill = age_bucket)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = total_population), vjust = 0.3, hjust = 3, color="white", size = 3) +
    labs(x = "Total Population", y = "\nAge Group", title = "Vermont Age Group Distribution (2023)\n") +
    theme_minimal() +
    scale_fill_manual(values = c("0-14 years" = "deepskyblue1", "15-24 years" = "aquamarine2", 
                                 "25-49 years" = "aquamarine4", "50-64 years" = "orange", 
                                 "65+ years" = "red")) +
    scale_x_continuous(labels = scales::comma) +
    theme(legend.position = "none", 
          text = element_text(family = "Georgia"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 22, face = "bold"))
}

plot_county_map <- function(df, county_col) {
  map <- df |>
    ggplot() +
    geom_sf(aes(fill = !!as.symbol(county_col))) + 
    geom_sf_label(aes(label = !!as.symbol(county_col)), parse = T) +
    geom_sf_label(aes(label = NAME), nudge_y = -0.1) + 
    scale_fill_gradient(
      low = "honeydew", 
      high = "darkgreen",
      name = county_col
    ) +
    theme_void()
  
  return(map)
}
