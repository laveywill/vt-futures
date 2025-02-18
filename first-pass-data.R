
# install.packages("tidycensus")
# install.packages("censusapi")
library(tidycensus)
library(tidyverse)
library(censusapi)

Sys.setenv(CENSUS_KEY = "d2c6932eca5b04592aaa4b32840c534b274382dc")

test <- getCensus(
  
)