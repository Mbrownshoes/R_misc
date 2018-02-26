soil <- st_read(system.file("external/lux.shp", package="raster")) %>% 
  # add in some fake soil type data
  mutate(soil = LETTERS[c(1:6,1:6)]) %>% 
  select(soil)

# field polygons
field <- c("POLYGON((6 49.75,6 50,6.4 50,6.4 49.75,6 49.75))",
           "POLYGON((5.8 49.5,5.8 49.7,6.2 49.7,6.2 49.5,5.8 49.5))") %>% 
  st_as_sfc(crs = st_crs(soil)) %>% 
  st_sf(field = c('x','y'), geoms = ., stringsAsFactors = FALSE)

# intersect - note that sf is intelligent with attribute data!
pi <- st_intersection(soil, field)
plot(soil$geometry, axes = TRUE)
plot(field$geoms, add = TRUE)
plot(pi$geometry, add = TRUE, col = 'red')


##load libraries
library(rgdal)
library(raster)

#read in first polygon shapefile
# 2016 Police Jurisdictions

# 2016 Police Jurisdictions
nc <- st_read("police/Resp_Codes_Cartographic2016/Resp_Codes_Cartographic2016.shp")

# add projection
p4s <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
st_crs(nc) <- p4s
rcmp <- st_transform(nc, 3005)

subs1 <- rcmp %>% 
  as("Spatial")

# CSD
csd <- st_read("police/CSD/CSD_2011.shp")
csd<-st_transform(csd, 3005)
plot(csd$geometry)

# filter on Fernie
# potential - https://stackoverflow.com/questions/40631947/merging-two-spatialpolygonsdataframe-objects
fernie <- csd %>%  
  filter(CSDNAME == 'Kitimat-Stikine F')

f<- fernie %>%
  as("Spatial")


#read in second polygon shapefile
subs2 <- f
summary(subs1)
summary(subs2)
# subs1 <- st_as_sf(subs1)
subs_union <- raster::union(subs1, subs2)
merged <- st_as_sf(subs_union)
plot(merged$geometry)

merged <- merged %>% 
  mutate(RESP_CODE=replace(RESP_CODE, CSDNAME == 'Kitimat-Stikine F', 999))


plot(subs1, col='blue')
plot(subs2, add=TRUE, col='red')

##### try merging two polys

temp <- merged %>% 
  group_by(District) %>% summarize()

a <- merged %>% 
  filter(RESP_CODE == 59039)

b <- merged %>% 
  filter(RESP_CODE == 59825)

c <- merged %>% 
  filter(RESP_CODE == 59039 | RESP_CODE == 59825) %>%
  st_union()
  
  group_by(District) %>% 
  summarize(RESP_CODE = 59825, RESP=59825)
