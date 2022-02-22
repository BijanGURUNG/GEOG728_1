#Tuesday, 02/22/2022 ----------------------------------------------------------------

library(tidyverse)
library(sf)

bea <- readRDS("C:/Users/bijangurung/Documents/GEOG728_1/clean_KS_BEA_data.rds")

counties <- st_read("C:/Users/bijangurung/Documents/GEOG728_1/KS_counties/KS_counties.shp")

head(counties)
head(bea)

bea <- bea %>%
  mutate(GEOID = str_extract(GeoFIPS, "\\d+"))

bea_sf <- left_join(counties, bea, by = c("geoid" = "GEOID"))
# geometry file or spatial file is first or x, so geometry is retained in the joined file. 

head(bea_sf)

ggplot()+
  geom_sf(data = bea_sf, aes(fill = total))

bea_sf %>%
  filter(Year == 2012) %>%
  ggplot(.)+
  geom_sf(data = bea_sf, aes(fill = total))

bea_sf %>%
  mutate(perc_total = total/sum(total)) %>%
  filter(Year == 2012) %>%
  ggplot(.)+
  geom_sf(aes(fill = perc_total))

bea_test <- bea_sf %>%
  group_by(Year) %>%
  mutate(perc_total = total/sum(total)*100)

head(bea_test)  

bea_test %>%
  filter (Year == 2012) %>%
  summarize(sum = sum(perc_total))

bea_test %>%
  filter (Year == 2012) %>%
  ggplot(.)+
  geom_sf(aes(fill = perc_total))

cnty_pts <- st_centroid(bea_test)

st_crs(bea_test)

bea_test %>%
  ggplot(.)+
  geom_sf(aes(fill = perc_total), alpha = 0.7)+
  facet_wrap(~Year)
  theme_minimal()+
  theme(axis.title = element_blank())+
  scale_fill_viridis_c()

# geom_sf(data = cnty_pts, aes(size=Ag))+ to add this layer to the current map  
# the centroid layer is added to the original layer
# xlab(NULL) + ylab(NULL) is added to add the lat-log in the above map
# filter (Year == 2012) %>% to get only year 2012