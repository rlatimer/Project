library(tidyverse)
library(rio)
library(janitor)
library(ggplot2)
library(here)
library(maps)
library(dplyr)
library(gghighlight)

initial <- import(here("data", "food_carbon_footprint_data.xlsx")) %>% 
  clean_names() %>%
    as_tibble()

#change ranking variable to numeric
#filter out the top ten highest CO2/person/year
subset <- initial %>% 
  mutate(ranking = as.numeric(ranking)) %>% 
  filter(ranking < 11 |country == "all"| country =="Canada"|country =="Japan")

#line plot work: CO2/person/year produced by country

initial_longer <- subset %>%
  pivot_longer(cols = 3:16,
               names_to = "product",
               values_to = "CO2_person_year")

animal <- subset %>%
  pivot_longer(cols = 3:9,
               names_to = "product",
               values_to = "CO2_person_year")

non_animal <- subset %>%
  pivot_longer(cols = 11:14,
               names_to = "product",
               values_to = "CO2_person_year")


  #plot1: animal products
#draft

a1 <- animal %>% 
  ggplot(aes(product, CO2_person_year, group=country)) +
  geom_line(aes(color = country))
a1

#final
a2 <- animal %>% 
  ggplot(aes(product, CO2_person_year, group=country)) +
  geom_line(aes(color = country), size = 2) +
  gghighlight(country == "all" |country == "USA" | country =="Canada"| country =="Japan", 
              unhighlighted_params = list(size = 1)) +
  scale_color_viridis_d() +
  scale_x_discrete(expand = c(0, 0)) +
  labs(title = "CO2/person/year for animal products",
       subtitle = "",
       x = "animal product",
       y = "Co2/person/year (in 10,000lbs)") +
  theme_minimal()
a2


  #plot2: non-animal products
#draft
na1 <- non_animal %>% 
  ggplot(aes(product, CO2_person_year, group=country)) +
  geom_line(aes(color = country))
na1

#final
na2 <- non_animal %>% 
  ggplot(aes(product, CO2_person_year, group=country)) +
  geom_line(aes(color = country), size = 2) +
  gghighlight(country == "all" |country == "USA" | country =="Canada"| country =="Japan", 
              unhighlighted_params = list(size = 1)) +
  scale_color_viridis_d() +
  scale_x_discrete(expand = c(0, 0)) +
  labs(title = "CO2/person/year for non-animal products",
       subtitle = "",
       x = "non-animal product",
       y = "Co2/person/year (in 10,000lbs)") +
  theme_minimal()
na2

  #plot3: difference between animal and non-animal products
#draft

#final


#geographic plot work
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