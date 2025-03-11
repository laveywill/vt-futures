age_distribution_data <- function(year) {
  # I will later add functionality to filter by county so we can
  # get the age distribution by county in the app 
  age <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = paste0("B01001_", str_pad(3:49, 3, pad="0"), "E"),
    region = "state:50"
  ) %>%
    select(-c("state"))
  
  age_long <- as.data.frame(t(age))
  
  age_group_patterns <- list(
    "Under 5 years" = "B01001_00(3|27)E",  
    "5 to 9 years" = "B01001_00(4|28)E",  
    "10 to 14 years" = "B01001_00(5|29)E",  
    "15 to 19 years" = "B01001_00(6|7|30|31)E",  
    "20 to 24 years" = "B01001_00(8|9|10|32|33|34)E",  
    "25 to 29 years" = "B01001_00(11|35)E",  
    "30 to 34 years" = "B01001_00(12|36)E",  
    "35 to 39 years" = "B01001_00(13|37)E",  
    "40 to 44 years" = "B01001_00(14|38)E",  
    "45 to 49 years" = "B01001_00(15|39)E",  
    "50 to 54 years" = "B01001_00(16|40)E",  
    "55 to 59 years" = "B01001_00(17|41)E",  
    "60 to 64 years" = "B01001_00(18|19|42|43)E",  
    "65 to 69 years" = "B01001_00(20|21|44|45)E",  
    "70 to 74 years" = "B01001_00(22|46)E",  
    "75 to 79 years" = "B01001_00(23|47)E",  
    "80 to 84 years" = "B01001_00(24|48)E",  
    "85 years and over" = "B01001_00(25|49)E"
  )
  
  
  
  age_long <- as.data.frame(age_long)
  age_long$code <- rownames(age_long)
  colnames(age_long) <- c("population", "code") 
  
  age_groups <- data.frame(
    age_group = names(age_group_patterns),
    total_population = sapply(age_group_patterns, function(cols) {
      sum(age_long$population[age_long$code %in% cols], na.rm = TRUE)
    })
  ) %>% select("total_population") %>%
    rownames_to_column(var = "age_group")
  
  return(age_groups)
}

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
  
  # Convert age_group to a factor with correct ordering
  age_groups$age_group <- factor(age_groups$age_group, levels = c(
    "85 years and over", "80 to 84 years", "75 to 79 years", "70 to 74 years",
    "65 to 69 years", "60 to 64 years", "55 to 59 years", "50 to 54 years",
    "45 to 49 years", "40 to 44 years", "35 to 39 years",
    "30 to 34 years", "25 to 29 years","20 to 24 years","15 to 19 years","10 to 14 years", 
    "5 to 9 years", "Under 5 years"
  ))
  
  # Create the bar chart with color-coded buckets
  ggplot(age_groups, aes(y = age_group, x = total_population, fill = age_bucket)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = total_population), vjust = 0.3, hjust = 3, color="white", size = 3) +
    labs(x = "Total Population", y = "Age Group", title = "Vermont Age Group Distribution from 2023") +
    theme_minimal() +
    scale_fill_manual(values = c("0-14 years" = "deepskyblue1", "15-24 years" = "aquamarine2", 
                                 "25-49 years" = "aquamarine4", "50-64 years" = "orange", 
                                 "65+ years" = "red")) +
    theme(legend.position = "none")
}