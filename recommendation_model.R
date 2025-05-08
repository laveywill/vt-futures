
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
    mutate(difference = rescaled_pop - pop_goal) |> 
    pivot_longer(
      cols = c(pop_goal, difference),
      names_to = "type",
      values_to = "value"
    )
  
  df_long |> 
    ggplot() +
    geom_col(aes(x = County, y = value, fill = type), position = "stack")
  
}
