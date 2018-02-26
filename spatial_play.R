library(sp)
library(rgdal)
library(sf)
library(maptools)
library(tidyverse)

cr <-readOGR("CR/CR_2012.shp")
nc <- st_read("CR/CR_2012.shp")

class(nc)
glimpse(nc)
attr(nc, 'CR_ID')
print(nc$CR_NAME)

nc$CR_NAME[nc$CR_NAME=='Vancouver Island'] 
print(nc1[1:3], n = 15)

# convert to SpatialPolygonsDataFrame
nc_sp <- as(nc, "Spatial")

# change variable
nc1<-nc %>% 
  mutate(CR_NAME = recode(CR_NAME,'Vancouver Island' = 'Some Stuff')) 


# http://strimas.com/r/tidy-sf/
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
ggplot(nc) +
  geom_sf(aes(fill = CR_NAME)) +
  # scale__viridis("Area") +
  ggtitle("Area of counties in North Carolina") +
  theme_bw()
