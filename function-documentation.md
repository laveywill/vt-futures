# Function Documentation
## population.R
- **plot_age_distribution**

  Creates the state-level age distribution plot seen at the top of the population page that compares Vermont's age distribution
  with the national age distribution and the age distribution in Collier County, FL. Requires dataframes built by our
  build_age_df function. 

- **plot_county_age_distribution**

  Creates the county-level age distribution plot when the user clicks a specific county at the top of the population page.     Includes the same comparison data as the state-level plot. Requires dataframes built by our build_age_df function. 

- **plot_county_map_population**

  Creates the map on the population page. Requires the county-level census dataframe. National averages, which come from
  the [census bureau](https://www.census.gov/quickfacts/fact/table/US/PST045224) for each variable are hard-coded into a list as    part of this function. To update these averages, replace them in this function. 

## homes.R
- **plot_state_housing**

  Creates plot of when housing was built in Vermont. Requires full dataframe of housing information in Vermont. 

- **plot_county_housing**

  Creates plot of when housing was built in each county. Depends on user county selection.
  Requires full dataframe of housing information in Vermont.
  
- **plot_county_map_homes**

  Creates the map on the population page. Requires the county-level census dataframe. National averages, which come from
  the [census bureau](https://www.census.gov/quickfacts/fact/table/US/PST045224) for each variable are hard-coded into a list as    part of this function. To update these averages, replace them in this function. 

- **plot_county_map**
 
  Creates plot of towns in each county that appears below state map. Depends on user county selection. Requires town-level
  dataframe created using our town_level_map function. 

- **plot_town_zoning**

  Creates plot of the zoning in each town. Depends on user county and town selection. Requires zoning dataframe created by
  our read_zoning_df function and dataframe with the towns in each county that comes from our town_level_map function.  
  
- **plot_grobs**

  Lays out graph objects (grobs) for formatting within the county-level card on the housing page. 

## jobs.R




## pull_data.R


