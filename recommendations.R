
create_scaled_df <- function(weights = c(0.25, 0.25, 0.25, 0.25), 
                             goal = 802000, curr_pop = 647464,
                             county_caps_df, zoning_df, county_job_opening_df) {
  
  goal_increase <- goal - curr_pop
  
  zoning_clean <- clean_scale_zoning(zoning_df)
  county_cap_clean <- clean_scale_capacity(county_caps_df)
  jobs_clean <- clean_scale_jobs(county_job_opening_df)
  
  # Combine data frames
  joined_df <- left_join(county_cap_clean, zoning_clean, by = "County") |> 
    left_join(jobs_clean, by = "County")
  
  county_cols <- joined_df |> select("County", "pop_goal", "population", "proportion")
  metrics <- joined_df[,5:length(names(joined_df))]
  
  scored <- data.frame(County = county_cols[1],
                       pop_goal = county_cols[2],
                       population = county_cols[3],
                       proportion = county_cols[4],
                       score = rowSums(mapply("*", metrics, weights)))
  
  fx <- function(x) {
    (1/6) * x + 1
  }
  
  out <- scored |> 
    mutate(
      scaled_score = fx(score),
      adjusted_pop = scaled_score * (proportion*goal_increase),
      adj_pop_proportion = adjusted_pop / sum(adjusted_pop),
      rescaled_pop = floor(goal_increase * adj_pop_proportion)
    )
    
    
  return(out)
}

plot_pop_adjustments <- function(scaled_df) {
  
  # each county is plotted here
  # plotting pop_goal as its own bar, 
  # directly next to that bar
  # if rescaled_pop > pop_goal,
  #   plot pop_goal as blank and rescaled_pop-pop_goal as color above it
  # if rescaled_pop < pop_goal,
  #   plot rescaled_pop as blank and pop_goal-rescaled_pop as color above it
  
  df_base <- scaled_df |> 
    mutate(
      diff = rescaled_pop - pop_goal,
      County = factor(County)
    ) |> 
    select(County, pop_goal, rescaled_pop, diff)
  
  df_plot <- bind_rows(
    df_base |> 
      mutate(bar_type = "Goal", ymin = 0, ymax = pop_goal),

    df_base |> 
      mutate(
        bar_type = "Adjustment",
        ymin = pop_goal,
        ymax = pop_goal + diff
      )
  )
  
  df_plot <- df_plot |> 
    mutate(ymin_adj = pmin(ymin, ymax),
           ymax_adj = pmax(ymin, ymax),
           fill_color = case_when(
             bar_type == "Goal" ~ "Proportional Goal",
             diff >= 0 ~ "Increase",
             TRUE ~ "Decrease"
           ))
  
  df_plot |> 
    ggplot(aes(x = County, fill = fill_color)) +
    geom_rect(aes(xmin = as.numeric(County) - 0.3 + 
                    ifelse(bar_type == "Adjustment", 0.35, 0),
                  xmax = as.numeric(County) - 0.3 + 
                    ifelse(bar_type == "Adjustment", 0.65, 0.3),
                  ymin = ymin_adj, ymax = ymax_adj)) +
    scale_x_discrete(breaks = 1:length(levels(df_plot$County)), labels = levels(df_plot$County)) +
    scale_fill_manual(values = c("Proportional Goal" = "grey70", "Increase" = "forestgreen", "Decrease" = "firebrick")) +
    labs(x = "County", y = "Population", fill = "", 
         title = "Population Goal and Adjustment by County") +
    theme_minimal()
  
  return(p)
}



clean_scale_jobs <- function(job_opening_df) {
  
  out <- job_opening_df |> 
    mutate(job_opening_score = scale(job_opening_rate),
           County = str_to_title(county)) |> 
    select(c("job_opening_score", "County"))
  
  return(out)
  
}

clean_scale_capacity <- function(county_caps_df) {
  county_caps_df <- county_caps_df %>%
    mutate(goal_over_latency = pop_goal - latent_cap,
           goal_over_school_latency = ( (pop_goal/2) * 1.89 ) - latent_cap_school,
           goal_over_latency_scaled = scale(goal_over_latency),
           goal_over_school_latency_scaled = scale(goal_over_school_latency),
           proportion = population / sum(population)
    ) |> 
    select(c("County", "pop_goal", "population", "proportion", "goal_over_latency_scaled", "goal_over_school_latency_scaled"))
  return(county_caps_df)
}

clean_scale_zoning <- function(zoning_df) {
  
  zoning_clean <- zoning |> 
    select(c("County", "District Name",
             "1F Allowance", "2F Allowance", "3F Allowance", "4F Allowance", "5F Allowance",
             "Shape_Area", "Acres", "geometry"))
  
  zoning_scored <- zoning_clean |>
    mutate(across(c("1F Allowance", "2F Allowance", "3F Allowance", "4F Allowance", "5F Allowance"),
                  ~case_when(
                    . == "NA" ~ 0,
                    . == "Permitted" ~ 1,
                    . == "Public Hearing" ~ 0.5,
                    . == "Prohibited" ~ 0,
                    . == "Overlay" ~ 0.5,
                    . == "Allowed/Conditional" ~ 0.5,
                    TRUE ~ NA_real_
                  ))) |> 
    mutate(across(c("1F Allowance", "2F Allowance", "3F Allowance", "4F Allowance", "5F Allowance"),
                  ~replace_na(., 0)))
  
  final <- zoning_scored |> 
    group_by(County) |> 
    mutate(
      housing_score = 1*`1F Allowance` + 2*`2F Allowance` + 3*`3F Allowance` + 4*`4F Allowance` + 5*`5F Allowance`
    ) |> 
    summarize(
      zoning_score = sum(housing_score)
    ) |> 
    ungroup() |> 
    mutate(zoning_score = scale(zoning_score))
  
  return(final)
}

plot_pop_recommendations <- function(scored_df) {
  
  df_long <- scored_df |> 
    mutate(difference = rescaled_pop - pop_goal,
           pop_goal = if_else(difference < 0, rescaled_pop)) |> 
    pivot_longer(
      cols = c(pop_goal, difference),
      names_to = "type",
      values_to = "value"
    )
  
  df_long |> 
    ggplot() +
    geom_col(aes(x = County, y = value, fill = type), position = "stack")
  
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
    
    scale_fill_gradient(low = "lightblue", high = "yellow") +
    
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
