
get_housing_data <- function(year = 2023) {
  
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
      Vacant = Total - Owner - Renter
    ) |>
    pivot_longer(cols = c(Owner, Renter, Vacant), names_to = "Tenure", values_to = "Count")
  
  final_data$Tenure <- fct_rev(final_data$Tenure)
  final_data$Year_Built <- fct_rev(final_data$Year_Built)
  
  out <- final_data
  
  return(out)
}

plot_county_housing <- function(full_df, county_selection) {
  
  maximum <- full_df |> 
    group_by(NAME, Year_Built) |> 
    summarize(s = sum(Count), .groups = "drop_last") |> 
    ungroup() |> 
    pull(s) |> 
    max()
  
  p <- full_df |> 
    filter(NAME == county_selection) |> 
    ggplot() + 
    geom_bar(aes(x = Year_Built, y = Count, fill = Tenure), stat = "identity") + 
    theme_minimal() + 
    labs(title = paste("Number of Housing Units by Year Built in", county_selection),
         x = "Year Built",
         y = "Number of Units") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    coord_flip(ylim = c(0, maximum)) 
  
  return(p)
}

plot_state_housing <- function(full_df) {
  
  processed <- full_df |> 
    group_by(Year_Built, Tenure) |> 
    summarize(Count = sum(Count), .groups = "drop_last") |> 
    ungroup()
  
  p <- processed |> 
    ggplot() + 
    geom_bar(aes(x = Year_Built, y = Count, fill = Tenure), stat = "identity") +
    theme_minimal() +
    labs(title = "Number of Housing Units by Year Built",
         x = "Year Built",
         y = "Number of Units") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    coord_flip()
    
  return(p)
}

plot_county_map_homes <- function(df, county_col) {
  dollar_cols <- c(
    "Median Home Value",
    "Median Gross Rent"
  )
  
  county_sym <- sym(county_col)
  is_dollar <- as_string(county_sym) %in% dollar_cols
  
  df <- df %>%
    mutate(value_label = if (is_dollar) dollar(!!county_sym) else !!county_sym)
  
  label_aes <- aes(label = value_label)
  fill_aes <- aes(fill = !!county_sym)
  
  fill_scale <- scale_fill_gradient(
    low = "honeydew", 
    high = "darkgreen",
    name = county_col,
    labels = if (is_dollar) dollar else waiver()
  )
  
  map <- ggplot(df) +
    geom_sf(fill_aes) +
    geom_sf_label(label_aes) +
    geom_sf_label(aes(label = NAME), nudge_y = -0.1, size = 5.5) +
    fill_scale +
    theme_void()
  
  return(map)
}

