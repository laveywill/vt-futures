
get_housing_units_data <- function(year) {
  
  var_names <- data.frame(
    code = c("B25034_001E", "B25034_002E", "B25034_003E", "B25034_004E",
                      "B25034_005E", "B25034_006E", "B25034_007E", "B25034_008E",
                      "B25034_009E", "B25034_010E", "B25034_011E"),
    title = c("Total Housing Units",
                    "Built 2020 or later",
                    "Built 2010 to 2019",
                    "Built 2000 to 2009",
                    "Built 1990 to 1999",
                    "Built 1980 to 1989",
                    "Built 1970 to 1979",
                    "Built 1960 to 1969",
                    "Built 1950 to 1959",
                    "Built 1940 to 1949",
                    "Built 1939 or earlier")
  )
  
  county_by_year <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = c("NAME", "B25034_001E", "B25034_002E", "B25034_003E", "B25034_004E",
             "B25034_005E", "B25034_006E", "B25034_007E", "B25034_008E",
             "B25034_009E", "B25034_010E", "B25034_011E"),
    region = "county:*",
    regionin = "state:50"
  )

   county <- county_by_year |> 
     rename_with(~ var_names$title, .cols = any_of(var_names$code)) |> 
     mutate(
       NAME = gsub(" County, Vermont", "", NAME)
     )
   
   state_by_year <- getCensus(
     name = "acs/acs5",
     vintage = year,
     vars = c("NAME", "B25034_001E", "B25034_002E", "B25034_003E", "B25034_004E",
              "B25034_005E", "B25034_006E", "B25034_007E", "B25034_008E",
              "B25034_009E", "B25034_010E", "B25034_011E"),
     region = "state:50"
   )
   
   state <- state_by_year |> 
     select(-c("NAME", "state")) |> 
     rename_with(~ var_names$title, .cols = any_of(var_names$code)) |> 
     select(-c("Total Housing Units")) |> 
     pivot_longer(cols = everything(), names_to = "Year_Built", values_to = "Count") |> 
     mutate(Year_Built = fct_rev(factor(Year_Built)))
   
   out <- list(state = state, county = county)
   return(out)
  
}

plot_state_housing_units <- function(state_df) {
  
  p <- state_df |> 
    ggplot() + 
    geom_bar(aes(x = Year_Built, y = Count), fill="steelblue", stat = "identity") +
    theme_minimal() +
    labs(title = "Number of Housing Units by Year Built",
         x = "Year Built",
         y = "Number of Units") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="none") +  # Rotate x-axis labels
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

