# Function Documentation
## population.R
- plot_age_distribution

  Creates the state-level age distribution plot seen at the top of the population page that compares Vermont's age distribution
  with the national age distribution and the age distribution in Collier County, FL. Requires dataframes built by our
  build_age_df function. 

- plot_county_age_distribution

  Creates the county-level age distribution plot when the user clicks a specific county at the top of the population page. Includes
  the same comparison data as the state-level plot. Requires dataframes built by our build_age_df function. 

- plot_county_map_population

  Creates the map on the population page. Requires the county-level census dataframe. National averages, which come from
  the [census bureau](https://www.census.gov/quickfacts/fact/table/US/PST045224) for each variable are hard-coded into a list as part of 
  this function. To update these averages, replace them in this function. 

## homes.R
- plot_county_housing
- plot_state_housing
- plot_county_map_homes
- plot_county_map
- plot_town_zoning
- plot_grobs

## jobs.R




## pull_data.R


