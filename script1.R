library(tidyverse)
library(rio)
library(janitor)

initial <- import(here("data", "food_carbon_footprint_data.xlsx") %>% 
  clean_names() %>%
    as_tibble()
  