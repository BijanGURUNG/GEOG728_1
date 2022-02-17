#Thursday, 02/17/2022 ------------------------------------------

install.packages("sf")
library(sf)
library(tidyverse)
library(USAboundaries)
library(USAboundariesData)

bea <- readRDS("C:/Users/bijangurung/Documents/GEOG728_1/clean_KS_BEA_data.rds")

counties <- us_counties(states = "KS")

head(counties)

ggplot(counties)+
  geom_sf()

st_crs(counties)

bea <- bea %>%
  mutate(GEOID = str_extract(GeoFIPS, "\\d+"))
# d for digits and + for all digits

bea_sf <- left_join(counties[,-9], bea, by=c("geoid" = "GEOID"))
# there were two names with "state_name", so need to remove one of them. 

head(bea_sf)

ggplot()+
  geom_sf(data=bea_sf, aes(fill = total))

bea_sf %>%
  filter(Year == 2012) %>%
  ggplot(.)+
  geom_sf(aes(fill = total))

bea_sf %>%
  filter(Year == 2012) %>%
  ggplot(.)+
  geom_sf(aes(fill = log(total)))

# normalize the data