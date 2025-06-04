
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

county_level_map <- function(county_shp_df, county_df){
  vt_counties <- county_shp_df[county_shp_df$STATEFP == "50", ]
  vt_map <- left_join(vt_counties, county_df, by = "NAME")
  return(vt_map)
}

town_level_map <- function(town_shp_df) {
  
  county_codes <- data.frame(num = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27),
                             NAME = c("Addison", "Bennington", "Caledonia", "Chittenden", "Essex", "Franklin", "Grand Isle",
                                      "Lamoille", "Orange", "Orleans", "Rutland", "Washington", "Windham", "Windsor"))
  
  vt_towns <- left_join(town_shp_df, county_codes, by = c("CNTY" = "num"))
  
  return(vt_towns)
}

# get_zoning_data <- function(geojson_files) {
#   
#   zoning_list <- lapply(geojson_files$id, function(file_id) {
#     temp_path <- tempfile(fileext = ".geojson")
#     drive_download(as_id(file_id), path = temp_path, overwrite = T)
#     out <- read_sf(temp_path)
#   })
#   
#   out <- rbindlist(zoning_list, fill = TRUE) |> 
#     select(-`Bylaw Date`) |> 
#     mutate(Jurisdiction = trimws(Jurisdiction, which = "right"))
#   
#   return(out)
# }

get_zoning_data <- function(geojson_files) {
  zoning_list <- lapply(geojson_files$id, function(file_id) {
    temp_path <- tempfile(fileext = ".geojson")
    download_from_drive(file_id, temp_path)
    sf::read_sf(temp_path)
  })
  
  out <- data.table::rbindlist(zoning_list, fill = TRUE) |>
    dplyr::select(-`Bylaw Date`) |>
    dplyr::mutate(Jurisdiction = trimws(Jurisdiction, which = "right"))
  
  return(out)
}

# get_sf_data <- function(file_list) {
#   temp_dir <- tempdir()
#   
#   lapply(seq_len(nrow(file_list)), function(i) {
#     drive_download(as_id(file_list$id[i]),
#                    path = file.path(temp_dir, file_list$name[i]),
#                    overwrite = TRUE)
#   })
#   
#   shp_paths <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
#   
#   out <- lapply(shp_paths, st_read)
#   names(out) <- c("county", "town")
#   
#   return(out)
# }
get_sf_data <- function(file_list) {
  temp_dir <- tempdir()
  
  # Download all files in file_list
  lapply(seq_len(nrow(file_list)), function(i) {
    file_id <- file_list$id[i]
    file_name <- file_list$name[i]
    dest_path <- file.path(temp_dir, file_name)
    
    download_url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
    curl::curl_download(url = download_url, destfile = dest_path, mode = "wb")
  })
  
  # List all .shp files in temp_dir
  shp_paths <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
  
  out <- lapply(shp_paths, sf::st_read)
  names(out) <- c("county", "town")
  
  return(out)
}

# get_csv_data <- function(file_list) {
#   
#   csv_list <- lapply(file_list$id, function(file_id) {
#     temp_path <- tempfile(fileext = ".csv")
#     drive_download(as_id(file_id), path = temp_path, overwrite = T)
#     out <- read.csv(temp_path, check.names = F)
#   })
#   
#   names(csv_list) <- gsub("\\.csv$", "", csv_files$name)
#   
#   out <- lapply(csv_list, function(df) {
#     df <- df[, names(df) != ""]
#     return(df)
#   })
#   
#   return(out)
# }

get_csv_data <- function(file_list) {
  csv_list <- lapply(seq_along(file_list$id), function(i) {
    file_id <- file_list$id[i]
    temp_path <- tempfile(fileext = ".csv")
    
    # Construct public download URL
    download_url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
    
    # Download using curl
    curl::curl_download(url = download_url, destfile = temp_path, mode = "wb")
    
    # Read CSV
    out <- read.csv(temp_path, check.names = FALSE)
    return(out)
  })
  
  # Use file names from input file_list
  names(csv_list) <- gsub("\\.csv$", "", file_list$name)
  
  # Remove empty-named columns, if any
  csv_list <- lapply(csv_list, function(df) {
    df <- df[, names(df) != ""]
    return(df)
  })
  
  return(csv_list)
}

download_from_drive <- function(file_id, destfile) {
  # Use httr to follow redirects
  url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
  req <- httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::progress())
  
  if (httr::http_error(req)) {
    stop("Failed to download file: ", httr::status_code(req))
  }
}
