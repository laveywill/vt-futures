
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
    labs(title = paste("Estimated Housing Units by Year Structure Built:", county_selection),
         x = "Year Built",
         y = "Number of Units") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    coord_flip(ylim = c(0, maximum)) +
    scale_fill_manual(values = c(
      "Owner" = "#00dca5",
      "Renter" = "#18a0cd",
      "Seasonal or Vacant" = "#c4c4c4"
    )) +
    scale_y_continuous(labels = scales::comma) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12),
      text = element_text(family = "Georgia"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
  
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
    labs(title = "Estimated Housing Units by Year Structure Built",
         x = "Year Built",
         y = "Number of Units") +
    coord_flip() + 
    scale_fill_manual(values = c(
      "Owner" = "#00dca5",
      "Renter" = "#18a0cd",
      "Seasonal or Vacant" = "#c4c4c4"
    )) + 
    scale_y_continuous(labels = scales::comma) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12),
      text = element_text(family = "Georgia"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
    
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
    
    max_diff <- max(df$diff, na.rm = TRUE)
    max_range <- max_diff + 0.2*max_diff
    
    min_diff <- min(df$diff, na.rm = TRUE)
    min_range <- min_diff - 0.2*min_diff
    
    fill_scale <- scale_fill_viridis_c(
      name = "Difference from\nnational average", 
      option = "D",
      limits = c(min_range, max_range),
      oob = scales::squish,
      labels = if (is_percent) {
        percent_format(accuracy = 0.01)
      } else if (is_dollar) {
        dollar
      } else {
        waiver()
      }
    )
    
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

plot_county_map <- function(town_level_df, county_selection) {
  # County map of towns in selected county
  
  if (is.null(county_selection)) {
    county_selection <- "Addison"
  }
  
  p <- town_level_df |> 
    filter(NAME == county_selection) |> 
    ggplot() +
    geom_sf() + 
    geom_sf_label(aes(label = TOWNNAMEMC), nudge_y = -0.1, size = 5) +
    labs(title = paste0(county_selection, "\n")) +
    coord_sf(expand = FALSE) +
    theme_void() + 
    theme(
      plot.title = element_text(size = 20, face = "bold", family = "Georgia", hjust = 0.5),
      plot.margin = margin(5, 10, 5, 10),
      legend.position="top",
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(3,"cm") 
    )
     
    return(p)
}
  
plot_town_zoning <- function(zoning_df, county_town_association, county_selection, town_selection, var_selected) {
  
  if (is.null(county_selection)) {
    county_selection <- "Addison"
  }
  
  possible_towns <- county_town_association |>
    filter(NAME == county_selection) |>
    pull(TOWNNAMEMC)
  
  if (is.null(town_selection) || !(town_selection %in% possible_towns)) {
    town_selection <- possible_towns[1]
  }
  
  df_filtered <- zoning_df |>
    filter(Jurisdiction == town_selection) |>
    st_as_sf()
  
  pal <- colorFactor("YlOrRd", domain = df_filtered[[var_selected]])
  
  p <- df_filtered |> 
    leaflet() |>
    addProviderTiles(providers$OpenStreetMap) |>
    addPolygons(
      fillColor = ~pal(df_filtered[[var_selected]]),
      fillOpacity = 0.7,
      weight = 1,
      label = ~`Jurisdiction District Name`
    ) |>
    addLegend("bottomright", pal = pal, values = df_filtered[[var_selected]], title = var_selected)
  
  return(p)
}

plot_grobs <- function(main_map, county_town_map) {
  
  grobs <- list(ggplotGrob(main_map), ggplotGrob(county_town_map))
  
  return(grid.arrange(grobs[[1]], grobs[[2]], ncol = 1))
}




