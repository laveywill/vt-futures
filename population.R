
pth <- getwd()
source(paste0(pth, "/read_data.R"))


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

plot_county_capacities <- function(df, county) {
  ggplot(df %>% 
           mutate(Metric = factor(Metric, levels = c("pop_goal", "latent_cap", "latent_cap_school"))),
         aes(x = Metric, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("County Capacity Limitations:", county, "\n"), 
         x = "\nMetric", y = "Value") +
    theme_minimal() +
    scale_x_discrete(labels = c(
      "latent_cap" = "Latent Capacity",
      "latent_cap_school" = "School Latency",
      "pop_goal" = "Population Goal"
    )) + 
    scale_fill_manual(values = c(
      "latent_cap" = "deepskyblue1",
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
}

jobs_homes_index_scale <- function(df, county) {
  data <- df %>%
    filter(County == county)
  val <- round(data$jobs_homes_index, 2)
  gradient_data <- data.frame(x = seq(0, 2, length.out = 200))
  
  ggplot() +
    geom_tile(data = gradient_data, aes(x = x, y = 1, fill = x), height = 0.3) +
    
    geom_segment(aes(x = val, xend = val, y = 0.85, yend = 1.15), 
                 color = "black", size = 1.5) +
    
    annotate("text", x = val, y = 1.3, label = paste(county, ":", val),
             size = 4.5, fontface = "bold", hjust = 0.5, family = "Georgia") +
    annotate("text", x = 0, y = 0.8, label = "0 (More homes)", hjust = 0, size = 5, family = "Georgia") +
    annotate("text", x = 1, y = 0.8, label = "1 (Balanced)", hjust = 0.5, size = 5, family = "Georgia") +
    annotate("text", x = 2, y = 0.8, label = "2 (More jobs)", hjust = 1, size = 5, family = "Georgia") +
    
    scale_fill_gradient(low = "lightgreen", high = "orange") +
    
    scale_x_continuous(limits = c(0, 2), breaks = c(0, 0.5, 1, 1.5, 2)) +
    coord_cartesian(clip = "off") +
    labs(title = "Jobs-Homes Index\n") +
    theme_void() +
    theme(
      plot.title = element_text(size = 20, face = "bold", family = "Georgia"),
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 10)
    )
  
}

national_averages <- c(
  "Median Age" = 38.7,
  "Total Male Population" = 0.495,
  "Total Female Population" = 0.505,
  "White Alone" = 0.61,
  "Black or African American Alone" = 0.14,
  "Asian Alone" = 0.07,
  "Hispanic or Latino Population" = 0.19
)

plot_county_map <- function(df, county_col, show_diff = FALSE) {
  percent_cols <- c(
    "White Alone",
    "Black or African American Alone",
    "Asian Alone",
    "Hispanic or Latino Population",
    "Total Male Population", 
    "Total Female Population"
  )
  
  county_sym <- sym(county_col)
  is_percent <- as_string(county_sym) %in% percent_cols

    df <- df %>%
      mutate(value_label = if (is_percent) paste0(round(!!county_sym * 100, 2), "%") else !!county_sym)
    
    label_aes <- aes(label = value_label)
    fill_aes <- aes(fill = !!county_sym)
    
    fill_scale <- scale_fill_gradient(
      low = "honeydew", 
      high = "darkgreen",
      name = county_col,
      labels = if (is_percent) percent_format(accuracy = 0.01) else waiver()
    )
  
  map <- ggplot(df) +
    geom_sf(fill_aes) +
    geom_sf_label(label_aes) +
    geom_sf_label(aes(label = NAME), nudge_y = -0.1, size = 5.5) +
    fill_scale +
    theme_void()
  
  return(map)
}
