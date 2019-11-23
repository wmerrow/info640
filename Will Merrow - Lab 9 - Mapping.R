rm(list = ls(all = TRUE))

#install.packages("ggmap")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("sf")
#install.packages("sp")
#install.packages("ggthemes")
#install.packages("tigris")
#install.packages("tmap")
#install.packages("leaflet")
#install.packages("rgdal")
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggmap)
library(tmap)
library(tigris)
library(sp)
library(ggthemes)
library(maps)
library(mapdata)
library(sf)
library(stringr)
library(rgdal)

register_google(key = "[my api key]", write=TRUE)

nyc <- c(lon = -73.950, lat = 40.6971)
nyc_map <- get_map(location = nyc, zoom = 10, scale=1)
ggmap(nyc_map)

nyc_map <- get_map(location = nyc, zoom = 5, scale=1)
ggmap(nyc_map)

nyc_map <- get_map(location = nyc, zoom = 13, scale=1)
ggmap(nyc_map)

noise_311 <- read.csv("DataSets/311_Service_Requests_from_2010_to_present.csv")
glimpse(noise_311)
dim(noise_311)

not_all_na <- function(x) any(!is.na(x))
noise_311_clean <- noise_311 %>%
  select_if(not_all_na)
glimpse(noise_311_clean)

noise_311_clean$Created.Date <- mdy_hms(noise_311_clean$Created.Date)
noise_311_clean$Closed.Date <- mdy_hms(noise_311_clean$Closed.Date)
glimpse(noise_311_clean)

ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = noise_311_clean)

unique(noise_311_clean$Descriptor)
as.character(noise_311_clean$Descriptor)
icecream_311 <- noise_311_clean %>%
  filter(str_detect(Descriptor, "Ice Cream"))
dim(icecream_311)
unique(icecream_311$Descriptor)
glimpse(icecream_311)

ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = icecream_311)

nyc_map <- get_map(location = nyc, zoom = 11, scale=2)
ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = icecream_311)

ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude, color=Address.Type), data = icecream_311)

ggmap(nyc_map,
      base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Address.Type))

ggmap(nyc_map, base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Address.Type)) +
  facet_wrap(~Address.Type)

ggmap(nyc_map,
      base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Borough)) +
  theme_void()+
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
 # labs(title="Ice Cream Noise Complaints by Address Type", caption="Source: NYC Open Data")
facet_wrap(~Address.Type)

# exercise

str(icecream_311)
glimpse(icecream_311)

ggmap(nyc_map,
      base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Borough)) +
  theme_void()+
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
#  labs(title="Ice Cream Noise Complaints by Address Type", caption="Source: NYC Open Data")
facet_wrap(~Borough)

# It is interesting to distinguish by borough, athough not necessary to do this with both color and faceting (either one would be enough to distinguish)
# The best approach is probably to facet by borough and color by address type:

ggmap(nyc_map,
      base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Address.Type)) +
  theme_void()+
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  #  labs(title="Ice Cream Noise Complaints by Address Type", caption="Source: NYC Open Data")
  facet_wrap(~Borough)


# thematic maps

nyc_counties <- c("New York", "Kings", "Queens", "Bronx", "Richmond")
nyc_tracts <- tracts(state = "NY", nyc_counties, cb = TRUE)
summary(nyc_tracts)
plot(nyc_tracts)

head(nyc_tracts, n=1)

proj4string(nyc_tracts)

income <- read.csv("DataSets/medianhouseholdincomecensustract.csv", header = TRUE)
glimpse(income)
glimpse(nyc_tracts)

any(duplicated(income$TRACTCE10))
any(duplicated(income$GEOID10))

nyc_tracts_merge <- sp::merge(nyc_tracts, income, by.x = "GEOID", by.y = "GEOID10")
glimpse(nyc_tracts_merge)

tm_shape(nyc_tracts_merge) +
  tm_fill(col = "MHI")

nyc_water <- area_water("NY", nyc_counties)
tm_shape(nyc_tracts_merge) +
  tm_fill(col = "MHI", title="Median Income NYC") +
  tm_shape(nyc_water)+
  tm_fill(col = "grey90")

nta <- readOGR("DataSets/Neighborhood Tabulation Areas-20191108/geo_export_482dad1a-b1a1-4d72-89d2-86f4a6b590e5.shp")
proj4string(nta)

nta <- spTransform(nta, proj4string(nyc_tracts))

tm_shape(nyc_tracts_merge) +
  tm_fill(col = "MHI") +
  tm_shape(nyc_water)+
  tm_fill(col = "grey90") +
  tm_shape(nta) +
  tm_borders()

tm_shape(nyc_tracts_merge) +
  tm_fill(col = "MHI", title="Median Income NYC", palette = "Reds")+
  tm_shape(nyc_water) +
  tm_fill(col = "grey90") +
  tm_shape(nta) +
  tm_borders(col= "grey30", lwd=2) +
  tm_credits("Source: 2010 Census, 10 year Estimates")

save_tmap(width=6, heigth=10)

# interactive maps

leaflet(nta) %>%
  addTiles() %>%
  addPolygons(popup = ~ntaname) %>%
  addProviderTiles("CartoDB.Positron")

leaflet(nta) %>%
  addTiles() %>%
  addPolygons(popup = ~ntaname) %>%
  addMarkers(~Longitude, ~Latitude, data = icecream_311) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)
