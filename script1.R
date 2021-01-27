library(tidyverse)
library(rio)
library(janitor)
library(ggplot2)
library(here)
library(maps)
library(dplyr)

initial <- import(here("data", "food_carbon_footprint_data.xlsx")) %>% 
  clean_names() %>%
    as_tibble()

#bring in map
country_data <- map_data("world") %>% 
  rename(country=region)

#left join initial and country_data by country
data_map<-full_join(initial, country_data)

#maybe use a map type other than albers; mercator?
#need outlines for all countries; full join? 
#now countries without data are filled grey

#plot of difference by country on map
ggplot(data_map, aes(long, lat))+
  geom_polygon(aes(group=group, color = "red", fill=animal_nonanimal_difference))
               +coord_map("albers", at0 = 45.5, lat1 = 29.5)

ggplot(initial, aes(total_animal_products,country)) +
  geom_point(aes(color=ranking))
  
ggplot(initial, aes(total_animal_products,ranking)) +
  geom_point() +
  labs(title = "kg CO2 per person")