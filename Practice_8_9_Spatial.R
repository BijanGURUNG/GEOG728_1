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
  facet_wrap(~Year)+
  theme_minimal()+
  theme(axis.title = element_blank())+
  scale_fill_viridis_c()

# geom_sf(data = cnty_pts, aes(size=Ag))+ to add this layer to the current map  
# the centroid layer is added to the original layer
# xlab(NULL) + ylab(NULL) is added to add the lat-log in the above map
# filter (Year == 2012) %>% to get only year 2012
  
#Thursday, 02/24/2022------------------------------------------------------------

bea_test %>%
  ggplot(.)+
  geom_sf(aes(fill = perc_total), alpha = 0.7)+
  facet_wrap(~Year)+
  theme_minimal()+
  theme(axis.title = element_blank())+
  scale_fill_viridis_c()

bea_sf %>%
  filter(Year == 2012) %>%
  ggplot(.)+
  geom_sf(aes(fill = Mining))+
  geom_sf_label(aes(label = name),
                label.padding=unit(0.25, "lines"), size = 3)
# size gives the font size

bea_sf %>%
  filter(Year == 2012) %>%
  ggplot(.)+
  geom_sf(aes(fill = Mining))+
  geom_sf_text(aes(label = name), size=2)+
  scale_fill_viridis_c(direction = -1)

# see the difference in between geom_sf_label vs geom_sf_text
# scale_fill_viridis_c(direction = -1) reverses the legend color

st_crs(bea_sf)

bea_sf %>%
  filter(Year == 2012) %>%
  st_transform(., "+proj=lcc +lat_1=38.71666666666667 +lat_2=39.78333333333333 +lat_0=38.33333333333334 +lon_0=-98 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")%>%
  ggplot(.)+
  geom_sf(aes(fill = Mining))+
  geom_sf_text(aes(label = name), size=2)+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()+
  theme(axis.text = element_blank(),
        legend.position = "bottom")

# go to https://spatialreference.org/

#Using only the base plot function
plot(bea_sf["total"], 
     pal = c('#e5f5f9','#99d8c9','#2ca25f'),
     nbreaks = 3,
     breaks = "jenks",
     main = "Some title",
     key.pos = 2)

# go to https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
# key.pos for the position of legend on the map

install.packages("tmap")
library(tmap)

tmap_mode("plot")

qtm(bea_sf, fill = "Mining", 
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    fill.title = "Mining Economic Prod",
    style = "gray")

?tm_layout
?tm_style

tm_shape(bea_sf %>% filter(Year == 2012))+
  tm_polygons("Mining", title = "Mining $")+
  tm_bubbles("Ag", title.size = "Ag$")+
  tm_layout(legend.outside = TRUE,
            inner.margins = c(0.2, 0.05, 0.05, 0.1))+
  tm_compass(size = 0.5, type = "rose", 
             position = c("right", "bottom"))

tmap_mode("view")
tm_shape(bea_sf %>% filter(Year == 2012))+
  tm_polygons("Mining", title = "Mining $")

