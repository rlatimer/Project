library(tidyverse)
library(rio)
library(janitor)
library(ggplot2)
library(here)
library(maps)
library(dplyr)
library(gghighlight)
library(plotly)
library(RColorBrewer)
library(colorspace)
library(reactable)
#install.packages("RColorBrewer")
#install.packages("colorspace")
#install.packages("reactable")

initial <- import(here("data", "food_carbon_footprint_data.xlsx")) %>% 
  clean_names() %>%
    as_tibble()

#change ranking variable to numeric
#filter out the top ten highest CO2/person/year
subset <- initial %>% 
  mutate(ranking = as.numeric(ranking)) %>% 
  filter(ranking < 7 |country == "average"| country =="Canada"|country =="Japan"
         |country == "Germany"|country =="Mexico"|country =="South Korea"|country =="China")

#line plot work: CO2/person/year produced by country

initial_longer <- subset %>%
  select("ranking", "country","total_animal_products", "total_nonanimal_products", 
         "animal_nonanimal_difference") %>% 
  pivot_longer(cols = 3:5,
               names_to = "product",
               values_to = "CO2_person_year")

nadiff <- subset %>%
  arrange(desc(animal_nonanimal_difference)) %>% 
  select("ranking", "country", "animal_nonanimal_difference") %>% 
  pivot_longer(cols = 3,
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

#vector for highlighted countries
focus <- c("average","USA","Canada","Japan")

a2 <- animal %>% 
  ggplot(aes(product, CO2_person_year, group=country),show.legend = FALSE) +
  geom_line(color="grey",show.legend = FALSE) + 
  geom_line(aes(color = country),
            data = filter(animal, country %in% focus)) + 
  scale_color_viridis_d() +
  scale_x_discrete(expand = c(0, 0)) +
  labs(title = "CO2/person/year for animal products",
       subtitle = "",
       x = "animal product",
       y = "Co2/person/year (in Kg)") +
  theme_minimal()
a2
ggplotly(a2, tooltip = c("country","product","CO2_person_year")) 

 

  #plot2: non-animal products
#draft
na1 <- non_animal %>% 
  ggplot(aes(product, CO2_person_year, group=country)) +
  geom_line(aes(color = country))
na1

#final
na2 <- non_animal %>% 
  ggplot(aes(product, CO2_person_year, group=country),show.legend = FALSE) +
  geom_line(color="grey",show.legend = FALSE) +
  geom_line(aes(color = country),
            data = filter(non_animal, country %in% focus)) + 
  scale_color_viridis_d() +
  scale_x_discrete(expand = c(0, 0)) +
  labs(title = "CO2/person/year for non-animal products",
       subtitle = "",
       x = "non-animal product",
       y = "Co2/person/year (in Kg)") +
  theme_minimal()
na2
ggplotly(na2, tooltip = c("country","product","CO2_person_year"))
 

  #plot3: difference between animal and non-animal products
#draft
d1 <- nadiff %>% 
  ggplot(aes(CO2_person_year, country)) +
  geom_col(aes(fill = country))
d1
#final
colourCount = length(unique(nadiff$country))
getPalette = colorRampPalette(brewer.pal(13, "Set1"))


d2 <- nadiff %>% 
  ggplot(aes(CO2_person_year, reorder(country, CO2_person_year))) +
  geom_col(aes(fill = CO2_person_year)) +
  scale_fill_continuous_diverging(
    "Green-Brown",
    rev = FALSE,
    mid = mean(initial$animal_nonanimal_difference, na.rm = TRUE)) +
  labs(title = "Animal v. Non-Animal Products difference",
       fill =  "CO2/person/year (Kg)",
       subtitle = "",
       x = "",
       y = "") +
  theme_minimal()

d2
ggplotly(d2, tooltip = c("CO2_person_year"))


#geographic plot work
#bring in map
country_data <- map_data("world") %>% 
  rename(country=region)

#left join initial and country_data by country
data_map<-full_join(initial, country_data)

#maybe use a map type other than albers; mercator?
#need outlines for all countries; full join? 
#now countries without data are filled grey

#draft
#plot of difference by country on map
m0 <- ggplot(data_map, aes(long, lat))+
  geom_polygon(aes(group=group, color = "black", fill=animal_nonanimal_difference)) +
 coord_map("albers", at0 = 45.5, lat1 = 29.5)
m0

#final
m0 <- ggplot(data = data_map,
             mapping = aes(x = long, y = lat,
                           group = group, fill = animal_nonanimal_difference))
m1 <- m0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
m2 <- m1 + scale_fill_continuous_diverging(
  "Green-Brown",
  rev = FALSE,
  mid = mean(initial$animal_nonanimal_difference, na.rm = TRUE)) +
  labs(title = "",
       fill =  "CO2/person/year (Kg)",
       subtitle = "",
       x = "",
       y = "") +
  theme_minimal()



#final plot of difference by country on map

m0 <- ggplot(data = data_map,
             mapping = aes(x = long, y = lat,
                           group = group, fill = animal_nonanimal_difference))
m1 <- m0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
m2 <- m1 + scale_fill_continuous_diverging(
  "Green-Brown",
  rev = FALSE,
  mid = mean(initial$animal_nonanimal_difference, na.rm = TRUE)) +
  labs(title = "Animal v. Non-Animal Products difference",
       fill =  "CO2/person/year (Kg)",
       subtitle = "",
       x = "",
       y = "") +
  theme_minimal()

ggplotly(m2)

#table draft
initial %>% 
  reactable(
    columns = list(
      lamb_goat = colDef(name="lamb/goat"),
      total_animal_products = colDef(name="Total (animal products)"),
      total_nonanimal_products = colDef(name="Total (non-animal products)"),
      animal_nonanimal_difference = colDef(name="Difference (Animal-Non-animal)")
    ),
  )

#table final
initial %>% 
  reactable(
    searchable = TRUE,
    filterable = TRUE,
    columns = list(
      lamb_goat = colDef(name="lamb/goat"),
      total_animal_products = colDef(name="Total (animal products)"),
      total_nonanimal_products = colDef(name="Total (non-animal products)"),
      animal_nonanimal_difference = colDef(name="Difference (Animal-Non-animal)")
    ),
    defaultPageSize = 15
)

