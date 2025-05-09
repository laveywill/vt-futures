
get_census_variables <- function() {
  census_variables <- data.frame(
    code = c(
      "B01003_001E", "B01002_001E", "B01001_002E", "B01001_026E", "B02001_002E", "B02001_003E", "B02001_005E", "B03001_003E",
      "B19013_001E", "B19001_002E", "B19301_001E", "B17001_002E", "B25077_001E", "B25064_001E",
      "B23025_002E", "B23025_005E", "B23006_002E", "B24011_001E",
      "B15003_017E", "B15003_021E", "B15003_022E", "B15003_023E", "B15003_024E",
      "B25001_001E", "B25002_002E", "B25002_003E", "B25003_002E", "B25003_003E",
      "B08006_001E", "B08006_003E", "B08006_008E", "B08013_001E",
      "B27001_001E", "B27001_005E", "B27001_008E", "B27001_012E"
    ),
    title = c(
      "Total Population", "Median Age", "Total Male Population", "Total Female Population", "White Alone", 
      "Black or African American Alone", "Asian Alone", "Hispanic or Latino Population",
      "Median Household Income", "Household Income Brackets", "Per Capita Income", "Population Below Poverty Level", 
      "Median Home Value", "Median Gross Rent",
      "Labor Force", "Unemployed Population", "Civilian Employed Population", "Industry for Civilian Employed Population",
      "High School Graduate or Equivalent", "Bachelor's Degree", "Master's Degree", "Professional School Degree", "Doctorate Degree",
      "Total Housing Units", "Occupied Housing Units", "Vacant Housing Units", "Owner-Occupied Housing Units", "Renter-Occupied Housing Units",
      "Total Workers", "Workers Who Drive Alone", "Workers Using Public Transport", "Mean Travel Time to Work (Minutes)",
      "Total Population for Health Insurance Coverage", "Population with Public Health Insurance", 
      "Population with Private Health Insurance", "Population with No Health Insurance"
    ),
    stringsAsFactors = FALSE
  ) 
  # do we want to include health insurance variables? we don't talk about them much in our recommendations
  return(census_variables)
}

county_level_map <- function(df){
  counties_all <- st_read("data/cb_2022_us_county_500k.shp")
  vt_counties <- counties_all[counties_all$STATEFP == "50", ]
  vt_map <- left_join(vt_counties, df, by = "NAME")
  return(vt_map)
}

town_level_map <- function() {
  
  county_codes <- data.frame(num = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27),
                             NAME = c("Addison", "Bennington", "Caledonia", "Chittenden", "Essex", "Franklin", "Grand Isle",
                                      "Lamoille", "Orange", "Orleans", "Rutland", "Washington", "Windham", "Windsor"))
  
  vt_towns <- st_read("data/VT_town_sf 2/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1.shp") |> 
    left_join(county_codes, by = c("CNTY" = "num"))
  
  return(vt_towns)
}

build_county_caps_df <- function(vt_pop = 624340) { # pulled at 2020 vintage acs
  # pop <- getCensus(
  #   name = "acs/acs5",
  #   vintage = 2020,
  #   vars = c("NAME", "B01001_001E"),
  #   region = "county:*",
  #   regionin = "state:50"
  # )  
  # vt_pop <- sum(pop$B01001_001E)
  
  pop <- pop %>% 
    rename( "population" = "B01001_001E") %>%
    mutate(pop_goal = floor((population/vt_pop)*(802000 - 647464)),
           County = str_trim(str_remove(NAME, "County, Vermont"))
    ) %>% select(County, pop_goal)
  
  latent_cap <- 
    read_csv(paste0(pth, "/data/latent-capacity.csv"), show_col_types = F) %>%
    rename(latent_cap = `Latent Capacity`) %>%
    group_by(County) %>%
    summarise(latent_cap = sum(latent_cap))
  
  jobs_homes <- 
    read_csv(paste0(pth, "/data/JobsHomesMap_data_formatted.csv"), name_repair = make.names) %>%
    drop_na() %>%
    rename(jobs_homes_index = Jobs.Homes.Index) %>%
    group_by(County) %>%
    summarise(jobs_homes_index = mean(jobs_homes_index)) %>%
    mutate(County = str_trim(str_remove(County, "County")))
  
  school_latency <- 
    read_excel(paste0(pth, "/data/teacher_information.xlsx")) %>%
    mutate(latent_cap_school = (num_teachers*18)-(num_teachers*student_teacher_ratio)) %>%
    distinct(County, school_district, latent_cap_school) %>% 
    group_by(County) %>%
    summarise(latent_cap_school = sum(latent_cap_school))  
  
  # Merge all datasets
  county_caps <- 
    left_join(latent_cap, jobs_homes, by = "County") %>%
    left_join(school_latency, by = "County") %>%
    left_join(pop, by = "County") %>% select (
      County, pop_goal, latent_cap, jobs_homes_index, latent_cap_school
    ) %>% 
    drop_na() 
  return(county_caps)
}

read_zoning_data <- function() {
  
  directory_path <- paste0(pth, "/data/zoning_data")
  
  files <- list.files(directory_path, full.names = T)
  
  list_out <- lapply(files, read_sf)
  
  out <- rbindlist(list_out, fill = TRUE) |> 
    select(-`Bylaw Date`) |> 
    mutate(Jurisdiction = trimws(Jurisdiction, which = "right"))
  
  return(out)
}

read_job_openings_data <- function() {
  
  directory_path <- paste0(pth, "/data/job_openings_long.csv")
  
  out <- read.csv(directory_path) %>% 
    mutate(date = as.Date(date, format = "%Y-%m-%d"))
  
  return(out)
}

read_county_job_openings_data <- function() {
  out <- readRDS(paste0(pth, "/data/county_job_opening.rds"))
  
  return(out)
}

read_rank_data <- function() {
  out <- readRDS(paste0(pth, "/data/rank_df.rds"))
  
  return(out)
}

read_state_data <- function() {
  out <- read.csv(paste0(pth, "/data/generated_dfs/state.csv"), check.names = F)
}

read_county_data <- function() {
  
}

read_town_data <- function() {
  
}

read_housing_data <- function() {
  
}

read_lf_data <- function() {
  
}