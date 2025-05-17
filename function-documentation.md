# Function Documentation
## population.R
- **plot_age_distribution**

  Creates the state-level age distribution plot seen at the top of the population page that compares Vermont's age distribution
  with the national age distribution and the age distribution in Collier County, FL. Requires dataframes built by our
  build_age_df function. 

- **plot_county_age_distribution**

  Creates the county-level age distribution plot when the user clicks a specific county at the top of the population page.     Includes the same comparison data as the state-level plot. Requires dataframes built by our build_age_df function. 

- **plot_county_map_population**

  Creates the state map on the population page. Requires the county-level census dataframe. National averages, which come from
  the [census bureau](https://www.census.gov/quickfacts/fact/table/US/PST045224) for each variable are hard-coded into a list as    part of this function. To update these averages, replace them in this function. 

## homes.R
- **plot_state_housing**

  Creates plot of when housing was built in Vermont. Requires full dataframe of housing information in Vermont. 

- **plot_county_housing**

  Creates plot of when housing was built in each county. Depends on user county selection.
  Requires full dataframe of housing information in Vermont.
  
- **plot_county_map_homes**

  Creates the state map on the homes page. Requires the county-level census dataframe. National averages, which come from
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
- **plot_lf_county**

  Create plot for labor force participation rate for each county in Vermont. Depends on user county selection. Requires
  labor force dataframe from our pull_lf_data function. 

- **plot_prime_working_age**

  Create plot of participation rate of prime working age adults by county and comparison to national average. Requires dataset
  of prime working age participation rate created from our process_prime_age_data function and the labor force dataframe from
  our pull_lf_data function. 

- **plot_county_map_jobs**

  Creates the state map on the jobs page. Requires the county-level census dataframe. National averages, which come from
  the [census bureau](https://www.census.gov/quickfacts/fact/table/US/PST045224) for each variable are hard-coded into a list as
  part of this function. To update these averages, replace them in this function. 

- **plot_job_opening_rate**

  Creates plot that compares the job opening rate in Vermont to the national job opening rate over time. Requires dataframe of job
  openings obtained from U.S. Bureau of Labor Statistics. 

- **plot_county_job_opening**

  Creates plot of job opening rates by county. Requires county job opening dataframe from Vermont JobLink &
  U.S. Bureau of Labor Statistics. 

- **plot_rank**

  Creates plot of rankings of each county in jobs categories. Requires dataframe with our custom rankings using job metrics
  (rank_df). 

## pull_data.R

The app does not depend on the API calls this code executes. It is included as a reference to how the data was generated 
and so that the data can be updated in the future if needed. 

- **pull_census_data**

  Pulls full data from the census (ACS survey). Takes year as an input so that data can be updated. Includes census calls
  for different levels of granularity (national, state-level, county-level) Variables to pull are
  coded within each census call. Update the vars list within the individual call to pull additionally variables.  

- **pull_lf_data**

  Pulls labor force data from the census (ACS survey). Takes year as an input so that data can be updated. Variables to pull are
  coded within each census call. Update the vars list within the individual call to pull additionally variables.  

- **pull_housing_data**

  Pulls housing data from the census (ACS survey). Takes year as an input so that data can be updated. Variables to pull are
  coded within each census call. Update the vars list within the individual call to pull additionally variables.  


- **pull_total_pop_county_data**

  Pulls VT population information from the census (ACS survey). Takes year as an input so that data can be updated. Variables to pull are
  coded within the census call. Update the vars list within the individual call to pull additionally variables.  


- **make_dfs**

  Aggreagates list of all dataframes generated from the pull_data.R script. 

- **write_dfs**

  Writes all dataframes to CSVs. Requires the list of dataframes generated using the pull_data.R script. 
  

