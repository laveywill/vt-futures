
# install.packages("tidycensus")
# install.packages("censusapi")
library(tidycensus)
library(tidyverse)
library(censusapi)

Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")

state_fips <- 50

test <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region = "county:*",
  regionin = paste0("state:", state_fips)
)
