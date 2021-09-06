library(tidyverse)


goanna_scripts <- read_rds("//mvpc76-mv/C$/Users/may159/GOANNA/goanna-ag-api/data/early-season-canopy-temps.RDS")

locations <- unique(goanna_scripts$location)


mutate(drum = if(count(read == 3))
  
  mutate(drum = if_else(count(read == 3), "drum", "plot")