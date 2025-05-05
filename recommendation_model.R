
create_scaled_df <- function(weights, county_caps_df, zoning_df, job_opening_df) {
  
  zoning_clean <- clean_scale_zoning(zoning_df)
  county_cap_clean <- clean_scale_capacity(couty_caps_df)
  jobs_clean <- clean_scale_jobs(job_opening_df)
  
  # Combine dataframes
  joined_df <- left_join(county_cap_clean, zoning_clean, by = "County") |> 
    left_join(., jobs_clean, by = "County") 
  
  county <- joined_df[,1]
  metrics <- joined_df[,2:length(names(joined_df))]
  
  out <- data.frame(mapply("*", joined_df, weights)) |> 
    cbind(county, .)
    
  return(out)
}


clean_scale_jobs <- function(job_opening_df) {
  
  out <- job_opening_df |> 
    mutate(job_opening_score = scale(job_opening_rate),
           County = toupper(county)) |> 
    select(c("job_opening_score", "County"))
  
  return(out)
  
}

clean_scale_capacity <- function(county_caps_df) {
  county_caps_df <- county_caps_df %>%
    mutate(goal_over_latency = pop_goal - latent_cap,
           goal_over_school_latency = ( (pop_goal/2) * 1.89 ) - latent_cap_school,
           goal_over_latency_scaled = scale(goal_over_latency),
           goal_over_school_latency_scaled = scale(goal_over_school_latency)
    ) 
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