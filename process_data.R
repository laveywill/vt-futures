
build_age_df <- function(df) {
  # Filter for age demographic data
  age <- df %>%
    select(starts_with("B01001_"))  %>%
    summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))
  
  age_long <- as.data.frame(t(age))
  colnames(age_long) <- "population"
  age_long$variable <- rownames(age_long) 
  age_long$population <- as.numeric(age_long$population)  
  
  # Age group pattern mapping
  age_group_patterns <- list(
    "Under 5 years" = "B01001_0(03|27)E",  
    "5 to 9 years" = "B01001_0(04|28)E",  
    "10 to 14 years" = "B01001_0(05|29)E",  
    "15 to 19 years" = "B01001_0(06|07|30|31)E",  
    "20 to 24 years" = "B01001_0(08|09|10|32|33|34)E",  
    "25 to 29 years" = "B01001_0(11|35)E",  
    "30 to 34 years" = "B01001_0(12|36)E",  
    "35 to 39 years" = "B01001_0(13|37)E",  
    "40 to 44 years" = "B01001_0(14|38)E",  
    "45 to 49 years" = "B01001_0(15|39)E",  
    "50 to 54 years" = "B01001_0(16|40)E",  
    "55 to 59 years" = "B01001_0(17|41)E",  
    "60 to 64 years" = "B01001_0(18|19|42|43)E",  
    "65 to 69 years" = "B01001_0(20|21|44|45)E",  
    "70 to 74 years" = "B01001_0(22|46)E",  
    "75 to 79 years" = "B01001_0(23|47)E",  
    "80 to 84 years" = "B01001_0(24|48)E",  
    "85 years and over" = "B01001_0(25|49)E"
  )
  
  age_long$age_group <- sapply(age_long$variable, function(var) {
    matched_group <- names(Filter(function(pattern) str_detect(var, pattern), age_group_patterns))
    if (length(matched_group) > 0) return(matched_group) else return(NA)
  })
  
  age_summary <- age_long %>%
    group_by(age_group) %>%
    summarise(total_population = sum(population, na.rm = TRUE)) %>%
    filter(!is.na(age_group)) 
  return(age_summary)
}

build_county_age_df <- function(df) {
  
  age <- df %>%
    select(NAME, starts_with("B01001_")) |> 
    pivot_longer(
      cols = -NAME,
      names_to = "age_group",
      values_to = "population"
    ) 
  
  # Age group pattern mapping
  age_group_patterns <- list(
    "Under 5 years" = "B01001_0(03|27)E",  
    "5 to 9 years" = "B01001_0(04|28)E",  
    "10 to 14 years" = "B01001_0(05|29)E",  
    "15 to 19 years" = "B01001_0(06|07|30|31)E",  
    "20 to 24 years" = "B01001_0(08|09|10|32|33|34)E",  
    "25 to 29 years" = "B01001_0(11|35)E",  
    "30 to 34 years" = "B01001_0(12|36)E",  
    "35 to 39 years" = "B01001_0(13|37)E",  
    "40 to 44 years" = "B01001_0(14|38)E",  
    "45 to 49 years" = "B01001_0(15|39)E",  
    "50 to 54 years" = "B01001_0(16|40)E",  
    "55 to 59 years" = "B01001_0(17|41)E",  
    "60 to 64 years" = "B01001_0(18|19|42|43)E",  
    "65 to 69 years" = "B01001_0(20|21|44|45)E",  
    "70 to 74 years" = "B01001_0(22|46)E",  
    "75 to 79 years" = "B01001_0(23|47)E",  
    "80 to 84 years" = "B01001_0(24|48)E",  
    "85 years and over" = "B01001_0(25|49)E"
  )
  
  proc <- age |> 
    mutate(age_group = map_chr(age_group, function(code) {
      matched <- keep(age_group_patterns, ~ str_detect(code, .x))
      if (length(matched) > 0) names(matched)[1] else NA_character_
    })) |> 
    group_by(NAME, age_group) |> 
    summarize(total_population = sum(population, na.rm = TRUE), .groups = "drop_last")
  
  return(proc)
}

process_prime_age_data <- function(labor_force_df) {
  df <- labor_force_df %>%
    filter(age_group== "age_25_to_34"|age_group == "age_35_to_44"|age_group == "age_45_to_54") %>% 
    group_by(NAME) %>% 
    summarise(prime_labor_pr = sum(labor_force_count)/sum(count)) %>% 
    arrange(desc(prime_labor_pr))
  
  return(df)
}

process_dependency_data <- function(labor_force_df) {
  
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

process_housing_data <- function(housing_df) {
  
  housing_df$Tenure <- fct_rev(housing_df$Tenure)
  housing_df$Year_Built <- fct_rev(housing_df$Year_Built)
  
  out <- housing_df
  
  return(out)
}

build_county_caps_df <- function(pop_df, latent_capacity_df, jobs_homes_map_df, teacher_info) {
  
  vt_pop <- sum(pop_df$B01001_001E)
  
  pop_df <- pop_df %>% 
    rename( "population" = "B01001_001E") %>%
    mutate(pop_goal = floor((population/vt_pop)*(802000 - 647464)),
           County = str_trim(str_remove(NAME, "County, Vermont"))
    ) %>% select(County, pop_goal)
  
  latent_cap <- 
    latent_capacity_df %>%
    rename(latent_cap = `Latent Capacity`) %>%
    group_by(County) %>%
    summarise(latent_cap = sum(latent_cap))
  
  jobs_homes <- 
    jobs_homes_map_df %>%
    drop_na() %>%
    rename(jobs_homes_index = `Jobs-Homes Index`) %>%
    group_by(County) %>%
    summarise(jobs_homes_index = mean(jobs_homes_index)) %>%
    mutate(County = str_trim(str_remove(County, "County")))
  
  school_latency_raw <- teacher_info
  school_latency <- school_latency_raw[names(school_latency_raw) != ""]
  
  school_latency <- school_latency %>%
    mutate(latent_cap_school = (num_teachers*18)-(num_teachers*student_teacher_ratio)) %>%
    distinct(County, school_district, latent_cap_school) %>% 
    group_by(County) %>%
    summarise(latent_cap_school = sum(latent_cap_school))  
  
  # Merge all datasets
  county_caps <- 
    left_join(latent_cap, jobs_homes, by = "County") %>%
    left_join(school_latency, by = "County") %>%
    left_join(pop_df, by = "County") %>% select (
      County, pop_goal, latent_cap, jobs_homes_index, latent_cap_school
    ) %>% 
    drop_na() 
  return(county_caps)
}

process_job_opening_df <- function(job_openings_long) {
  
  out <- job_openings_long
  out$date <- as.Date(out$date, format = "%Y-%m-%d")
  
  return(out)
}


