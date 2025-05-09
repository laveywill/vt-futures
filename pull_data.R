
pull_census_data <- function(year, census_variables) {
  Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")
  
  # Pull df at the national level
  natl_census_data_raw <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = c("NAME", census_variables$code, paste0("B01001_", str_pad(3:49, 3, pad="0"), "E")),
    region = "us:1"
  )
  
  natl_census_data <- natl_census_data_raw |> 
    rename_with(~ census_variables$title, .cols = any_of(census_variables$code))
  
  # Pull df at the state level
  state_census_data_raw <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = c("NAME", census_variables$code, paste0("B01001_", str_pad(3:49, 3, pad="0"), "E")),
    region = "state:50",
  )
  
  state_census_data <- state_census_data_raw |> 
    rename_with(~ census_variables$title, .cols = any_of(census_variables$code))
  
  # Pull df at the county level
  county_census_data_raw <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = c("NAME", census_variables$code, paste0("B01001_", str_pad(3:49, 3, pad="0"), "E")),
    region = "county:*",
    regionin = "state:50",
  ) 
  
  county_census_data <- county_census_data_raw |> 
    rename_with(~ census_variables$title, .cols = any_of(census_variables$code)) |> 
    mutate(
      NAME = gsub(" County, Vermont", "", NAME),
      `White Alone` = `White Alone` / `Total Population`,
      `Black or African American Alone` = `Black or African American Alone` / `Total Population`,
      `Asian Alone` = `Asian Alone` / `Total Population`,
      `Hispanic or Latino Population` = `Hispanic or Latino Population` / `Total Population`,
      `Total Male Population` = `Total Male Population` / `Total Population`,
      `Total Female Population` = `Total Female Population` / `Total Population`,
      `Labor Force` = `Labor Force` / `Total Population`,
      `Unemployed Population` = `Unemployed Population` / `Total Population`,
      `High School Graduate or Equivalent` = `High School Graduate or Equivalent` / `Total Population`,
      `Bachelor's Degree` = `Bachelor's Degree` / `Total Population`,
      `Professional School Degree` = `Professional School Degree` / `Total Population`,
      `Doctorate Degree` = `Doctorate Degree` / `Total Population`,
      `Master's Degree` = `Master's Degree` / `Total Population`,
      `Workers Who Drive Alone` = `Workers Who Drive Alone` / `Total Workers`,
      `Workers Using Public Transport` = `Workers Using Public Transport` / `Total Workers`,
      `Occupied Housing Units` = `Occupied Housing Units` / `Total Housing Units`,
      `Vacant Housing Units` = `Vacant Housing Units` / `Total Housing Units`,
      `Owner-Occupied Housing Units` = `Owner-Occupied Housing Units` / `Total Housing Units`,
      `Renter-Occupied Housing Units` = `Renter-Occupied Housing Units` / `Total Housing Units`
    )
  
  # Pull df at the town ("place") level
  place_census_data_raw <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = c("NAME", census_variables$code),
    region = "place:*",
    regionin = "state:50",
  )
  
  place_census_data <- place_census_data_raw |> 
    rename_with(~ census_variables$title, .cols = any_of(census_variables$code)) |> 
    mutate(
      NAME = gsub(" village, Vermont", "", NAME),
      NAME = gsub(" CDP, Vermont", "", NAME),
      NAME = gsub(" city, Vermont", "", NAME),
    )
  
  # Pull df for county in FL (age comparison)
  county_census_data_collier_raw <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = c("NAME", census_variables$code,  paste0("B01001_", str_pad(3:49, 3, pad="0"), "E")),
    region = "county:021",
    regionin = "state:12"
  )
  
  county_census_data_collier <- county_census_data_collier_raw |> 
    rename_with(~ census_variables$title, .cols = any_of(census_variables$code))
  

  # Return all of the dataframes
  return(list(state = state_census_data, 
              county = county_census_data, 
              town = place_census_data,
              natl = natl_census_data,
              collierFL = county_census_data_collier))
}

pull_lf_data <- function(year = 2020) {
  age_vars <- paste0("B01001_", sprintf("%03d", 2:49))
  labor_force_vars <- paste0("B23001_", sprintf("%03d", 1:173))
  
  vermont_age_data <- get_acs(
    geography = "county",
    state = "50",
    variables = age_vars,
    year = year,
    survey = "acs5"
  )
  
  vermont_lf_age_data <- get_acs(
    geography = "county",
    state = "50",
    variables = labor_force_vars,
    year = year,
    survey = "acs5"
  )
  
  acs_vars <- load_variables(year, "acs5", cache = TRUE)
  
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
    mutate(labor_force_count = if_else(is.na(labor_force_count), 0, labor_force_count))
  
  return(cleaned_age_distribution)
}

pull_housing_data <- function(year = 2023) {
  
  total_vars <- c(
    "B25034_002E", "B25034_003E", "B25034_004E", "B25034_005E", 
    "B25034_006E", "B25034_007E", "B25034_008E", "B25034_009E", 
    "B25034_010E", "B25034_011E"
  )
  
  owner_vars <- c(
    "B25036_003E", "B25036_004E", "B25036_005E", "B25036_006E", 
    "B25036_007E", "B25036_008E", "B25036_009E", "B25036_010E", 
    "B25036_011E", "B25036_012E"
  )
  
  renter_vars <- c(
    "B25036_014E", "B25036_015E", "B25036_016E", "B25036_017E", 
    "B25036_018E", "B25036_019E", "B25036_020E", "B25036_021E", 
    "B25036_022E", "B25036_023E"
  )
  
  all_vars <- c("NAME", total_vars, owner_vars, renter_vars)
  
  housing_data <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = all_vars,
    region = "county:*",
    regionin = "state:50"
  )
  
  # Reshape to long format and categorize
  tidy_data <- housing_data |>
    pivot_longer(cols = -c(NAME, state, county), names_to = "Variable", values_to = "Count") |>
    mutate(
      Category = case_when(
        Variable %in% total_vars ~ "Total",
        Variable %in% owner_vars ~ "Owner",
        Variable %in% renter_vars ~ "Renter"
      ),
      Year_Built = case_when(
        Variable %in% c("B25034_002E", "B25036_003E", "B25036_014E") ~ "2014 or later",
        Variable %in% c("B25034_003E", "B25036_004E", "B25036_015E") ~ "2010-2013",
        Variable %in% c("B25034_004E", "B25036_005E", "B25036_016E") ~ "2000-2009",
        Variable %in% c("B25034_005E", "B25036_006E", "B25036_017E") ~ "1990-1999",
        Variable %in% c("B25034_006E", "B25036_007E", "B25036_018E") ~ "1980-1989",
        Variable %in% c("B25034_007E", "B25036_008E", "B25036_019E") ~ "1970-1979",
        Variable %in% c("B25034_008E", "B25036_009E", "B25036_020E") ~ "1960-1969",
        Variable %in% c("B25034_009E", "B25036_010E", "B25036_021E") ~ "1950-1959",
        Variable %in% c("B25034_010E", "B25036_011E", "B25036_022E") ~ "1940-1949",
        Variable %in% c("B25034_011E", "B25036_012E", "B25036_023E") ~ "1939 or earlier"
      ),
      NAME = gsub(" County, Vermont", "", NAME)
    ) |>
    select(NAME, county, Year_Built, Category, Count)
  
  final_data <- tidy_data |>
    pivot_wider(names_from = Category, values_from = Count) |>
    mutate(
      `Seasonal or Vacant` = Total - Owner - Renter
    ) |>
    pivot_longer(cols = c(Owner, Renter, `Seasonal or Vacant`), names_to = "Tenure", values_to = "Count")
  
  final_data$Tenure <- fct_rev(final_data$Tenure)
  final_data$Year_Built <- fct_rev(final_data$Year_Built)
  
  out <- final_data
  
  return(out)
}

pull_total_pop_county_data <- function(year = 2020) {
  
  out <- getCensus(
    name = "acs/acs5",
    vintage = 2020,
    vars = c("NAME", "B01001_001E"),
    region = "county:*",
    regionin = "state:50"
  )

  return(out)
}

make_dfs <- function() {
  census <- pull_census_data()
  state <- census$state
  county <- census$county
  town <- census$town
  natl <- census$natl
  collierFL <- census$collierFL
  
  lf <- pull_lf_data()
  
  housing <- pull_housing_data()
  
  county_pop <- pull_housing_data()
  
  out <- list(state,
              county,
              state,
              town,
              natl,
              collierFL,
              lf,
              housing,
              county_pop)
  
  return(out)
}

write_dfs <- function(dfs = list(), path_to_write) {
  
  lapply(dfs, function(df) {write.csv(df, paste0(path_to_write, "/", df, ".csv"))})
  
}