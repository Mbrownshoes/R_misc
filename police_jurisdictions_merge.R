library(sf)
library(tidyverse)
# library(bcmaps) #for BC regional district map

# 2016 Police Jurisdictions
nc <- st_read("police/Resp_Codes_Cartographic2016/Resp_Codes_Cartographic2016.shp")

# add projection
p4s <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
st_crs(nc) <- p4s

rcmp <- st_transform(nc, 3005) %>% 
  as("Spatial")

# CSD
csd <- st_read("police/CSD/CSD_2011.shp")
# https://stackoverflow.com/questions/40631947/merging-two-spatialpolygonsdataframe-objects
st_crs(csd) <- p4s

# Get the three CSDs we need
csd<-st_transform(csd, 3005) %>%  
  filter(CSDNAME == 'Fernie' | CSDNAME == 'Armstrong' | CSDNAME == 'Osoyoos') %>% 
  as("Spatial")

# wasn't able to do in sf
subs_union <- raster::union(rcmp, csd)

merged <- st_as_sf(subs_union) %>% 
  mutate(RESP_CODE=replace(RESP_CODE, RESP_CODE == 59796 & CSDUID == 5901012, 59043),
         RESP=replace(RESP_CODE, RESP_CODE == 59796 & CSDUID == 5901012, 59043),
         RESP_CODE=replace(RESP_CODE, RESP_CODE == 59752 & CSDUID == 5937028, 59041),
         RESP=replace(RESP_CODE, RESP_CODE == 59752 & CSDUID == 5937028, 59041),
         RESP_CODE=replace(RESP_CODE, RESP_CODE == 59736 & CSDUID == 5907005, 59042),
         RESP=replace(RESP_CODE, RESP_CODE == 59736 & CSDUID == 5907005, 59042)) %>% 
  select(-one_of(names(csd))) %>% 
  mutate(RESP_CODE = ifelse(RESP_CODE == 59039 | RESP_CODE == 59825,59825, RESP_CODE))

# merge Northern Rockies Muni back into Northern Rockies Rural
c <- merged %>%
  group_by(RESP_CODE) %>% 
  summarise(Location = first(Location),District = first(District), n = n()) %>% 
  ungroup()

st_write(c, "out/merged.shp")

c <- ms_simplify(c)
# c<-st_transform(c, 3005) 

ggplot(c) +
  geom_sf(aes(fill = RESP_CODE))+
  theme_minimal() +
  theme(
    # panel.background = element_rect(fill = 'lightblue', colour = 'lightblue'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    # legend.title = element_text(size = 14, face = "bold"),
    # legend.text = element_text(size = 13),
    # legend.position = c(0.15, 0.2),
    text = element_text(family = "Verdana"),
    plot.title = element_text(vjust=-13,hjust = .99,size = 12),
    plot.margin = unit(c(5, 5, 5, 5), "mm"))

# north_union <- raster::union(as(merged,"Spatial"), as(c,"Spatial"))

# north_merged <- st_as_sf(north_union) %>% 
  # filter(RESP_CODE.1 != 59039)

# north_union <- st_union(merged, c)

# make sure it worked
filter(c, RESP_CODE == 59041 | RESP_CODE == 59042 | RESP_CODE == 59043)





