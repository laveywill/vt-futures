Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")

pth <- getwd()

# STATE LEVEL POPULATION DATA
age_distribution_data <- function(year) {
  age <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = paste0("B01001_", str_pad(3:49, 3, pad="0"), "E"),
    region = "state:50"
  ) %>%
    select(-c("state"))
  
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
    filter(!is.na(age_group))  # Remove any unmatched rows
  
  return(age_summary)
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
  
  age_groups$age_group <- factor(age_groups$age_group, levels = c(
    "85 years and over", "80 to 84 years", "75 to 79 years", "70 to 74 years",
    "65 to 69 years", "60 to 64 years", "55 to 59 years", "50 to 54 years",
    "45 to 49 years", "40 to 44 years", "35 to 39 years",
    "30 to 34 years", "25 to 29 years","20 to 24 years","15 to 19 years","10 to 14 years", 
    "5 to 9 years", "Under 5 years"
  ))

  ggplot(age_groups, aes(y = age_group, x = total_population, fill = age_bucket)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = total_population), vjust = 0.3, hjust = 3, color="white", size = 3) +
    labs(x = "Total Population", y = "\nAge Group", title = "Vermont Age Group Distribution (2023)\n") +
    theme_minimal() +
    scale_fill_manual(values = c("0-14 years" = "deepskyblue1", "15-24 years" = "aquamarine2", 
                                 "25-49 years" = "aquamarine4", "50-64 years" = "orange", 
                                 "65+ years" = "red")) +
    scale_x_continuous(labels = scales::comma) +
    theme(legend.position = "none", 
          text = element_text(family = "Georgia"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 22, face = "bold"))
}

# COUNTY BREAKDOWNS
build_county_caps_df <- function() {
  pop <- getCensus(
    name = "acs/acs5",
    vintage = 2020,
    vars = c("NAME", "B01001_001E"),
    region = "county:*",
    regionin = paste0("state:", state_fips)
  )  
  vt_pop <- sum(pop$B01001_001E)
  
  pop <- pop %>% 
    rename( "population" = "B01001_001E") %>%
    mutate(pop_goal = floor((population/vt_pop)*(802000 - 647464)),
           County = str_trim(str_remove(NAME, "County, Vermont"))
    ) %>% select(County, pop_goal)
  
  latent_cap <- 
    read_csv(paste0(pth, "/latent-capacity.csv")) %>%
    rename(latent_cap = `Latent Capacity`) %>%
    group_by(County) %>%
    summarise(latent_cap = sum(latent_cap))
  
  jobs_homes <- 
    read_csv(paste0(pth, "/JobsHomesMap_data_test.csv")) %>%
    drop_na() %>%
    mutate(jobs_homes_diff = `Occupied homes` - Jobs) %>%
    group_by(County) %>%
    summarise(jobs_homes_diff = sum(jobs_homes_diff)) %>%
    mutate(County = str_trim(str_remove(County, "County")))
  
  school_latency <- 
    read_excel(paste0(pth, "/school_latency.xlsx"), sheet = "Data")  %>%
    rename(latent_cap = `Latent Capacity`) %>%
    drop_na() %>%
    group_by(County) %>%
    summarise(latent_cap_school = sum(latent_cap))   
  
  # Merge all datasets
  county_caps <- 
    left_join(latent_cap, jobs_homes, by = "County") %>%
      left_join(school_latency, by = "County") %>%
      left_join(pop, by = "County") %>% select (
        County, pop_goal, latent_cap, jobs_homes_diff, latent_cap_school
      ) %>% 
      drop_na() 
  return(county_caps)
}
