
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

plot_county_map_homes <- function(df, county_col, show_diff = FALSE) {
  dollar_cols <- c(
    "Median Home Value",
    "Median Gross Rent"
  )
  
  percent_cols <- c(
    "Occupied Housing Units",
    "Vacant Housing Units",
    "Owner-Occupied Housing Units",
    "Renter-Occupied Housing Units"
  )
  
  national_averages <- c(
    "Median Home Value" = 348000,
    "Median Gross Rent" = 1348,
    "Occupied Housing Units" = 0.65,
    "Vacant Housing Units" = 0.10,
    "Owner-Occupied Housing Units" = 0.59,
    "Renter-Occupied Housing Units" = 0.31
  )
  
  county_sym <- sym(county_col)
  is_dollar <- as_string(county_sym) %in% dollar_cols
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
        } 
        else if (is_dollar) {
          case_when(
            diff > 0 ~ paste0("⬆️", dollar(diff)),
            diff < 0 ~ paste0("⬇️", dollar(diff)),
            TRUE ~ "$0"
          )
        } 
        else {
          case_when(
            diff > 0 ~ paste0("⬆️", formatC(diff, format = "f", digits = 2)),
            diff < 0 ~ paste0("⬇️", formatC(diff, format = "f", digits = 2)),
            TRUE ~ "0"
          )
        }
      )
    
    fill_aes <- aes(fill = diff)
    label_aes <- aes(label = value_label)
    
    fill_scale <- scale_fill_viridis_c(name = NULL)
    } else {
      df <- df %>%
        mutate(value_label = case_when(
          is_dollar ~ dollar(!!county_sym),
          is_percent ~ paste0(round(!!county_sym * 100, 2), "%"),
          TRUE ~ as.character(!!county_sym)
        )
        )
    
    fill_aes <- aes(fill = !!county_sym)
    label_aes <- aes(label = value_label)
  
    fill_scale <- scale_fill_gradient(
      low = "honeydew", 
      high = "darkgreen",
      name = NULL,
      labels = if (is_dollar) dollar else waiver()
    )
    }
  map <- ggplot(df) +
    geom_sf(fill_aes) +
    geom_sf_label(label_aes) +
    geom_sf_label(aes(label = NAME), nudge_y = -0.1, size = 5.5) +
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

plot_county_zoning <- function(zoning_df, county_selection) {
  
  p <- zoning_df |> 
    filter(County == county_selection) |> 
    ggplot() +
    geom_sf(aes(fill = `1F Allowance`,
                geometry = geometry))
  
    return(ggplotly(p))
}
  


