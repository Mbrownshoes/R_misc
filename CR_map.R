
library(readr) #data import
library(dplyr) #data munging
library(ggplot2)
# library(bcmaps) #for BC regional district map
library(sf) #sf map object
library(stringr) #modifying character strings
library(units) #unit conversion
library(rmapshaper)
library(RColorBrewer) #for colour palette
# library(rnaturalearth)
library(sp)
# library(raster)

# crop spatial object
# https://stackoverflow.com/questions/13982773/crop-for-spatialpolygonsdataframe/13986029#13986029

#### communities
cm <- st_read("CR/communities/Communities_CR.shp")
# st_crs(cm) <- p4s
cm<-st_transform(cm, 3005)
## converting sf to sp and then to df
cm <- as(cm, "Spatial")
cm.df <- as.data.frame(coordinates(cm))

communities <- data.frame(id = cm$NAME, cm.df) %>% 
  filter(id != 'Kamloops')
names(communities) <- c("Name","Longitude", "Latitude")  #more sensible column names
## Use for cities, but they're locations are odd
# communities <- data.frame(id = cm$PLACE, cm.df) %>% 
#   filter(id != 'Duncan', id != 'Richmond', id != 'Burnaby', id != 'New Westminster', id != 'Surrey')
# names(communities) <- c("Name","Longitude", "Latitude")  #more sensible column names

#### load US
us <- st_read("CR/cb_2014_us_nation_5m/cb_2014_us_nation_5m.shp")

us_simp <- st_transform(us,3005)
us_simp <- ms_simplify(us_simp)

### Ocean
ocean50 <-st_read("CR/ne_10m_ocean/ne_10m_ocean.shp")
# o<- as(ocean50,"Spatial")
# out <- raster::crop(o, extent(-129.45,-122.45,47.4 49.999))
# try with sf
pac<-st_intersection(ocean50, st_set_crs(st_as_sf(as(raster::extent(-129.45,-120.45,44.49, 55.99), "SpatialPolygons")), st_crs(ocean50)))


ocean <- pac %>% 
  st_transform(3005) %>% 
  as("Spatial")
ocean <- fortify(ocean) 

# %>% 
  # filter(hole==TRUE)

ggplot(data = plotmapdf, aes(x = long, y = lat, group = group,fill=VI)) +
  geom_polygon(alpha = 0.9) +
  geom_path(colour = "grey50", size = 0.3) +
  coord_fixed(xlim = c(1370571.8, 1003875.7),  ylim = c(369045, 649045), ratio = 0.79)+
  geom_polygon(data = ocean, aes(long, lat,fill=hole),inherit.aes = FALSE)
  
# ocean <- st_read("CR/ne_10m_ocean/ne_10m_ocean.shp")
# 
# ocean_simp <- st_transform(ocean,3005)
# ocean_simp <- ms_simplify(ocean_simp)


#### college regions
cr_simp <- st_read("CR/CR_2012.shp")

# cr_simp <- ms_simplify(nc)
# add projection
p4s <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
st_crs(cr_simp) <- p4s

# plot(cr_simp)
# st_transform(cr_simp, 3005)

# BC Albers projection
cr_simp<-st_transform(cr_simp, 3005)

#plot sf
ggplot(ocean_simp) +
  geom_sf(aes(fill = 'blue')) +
  # scale_fill_viridis("Area") +
  ggtitle("College Districts") +
  theme_bw()

## converting sf to sp and then to df
plotmap <- as(cr_simp, "Spatial")
plotmapdf <- fortify(plotmap, region = "CR_NAME") %>% 
  mutate(VI = ifelse(id== 'Vancouver Island','Vancouver Island','other'))

us <- as(us_simp,"Spatial")%>% 
  fortify(region ="NAME" )

# ocean <- fortify(ocean50@data,region="id")


#Get centroids of regions for labels
centroids.df <- as.data.frame(coordinates(plotmap))
names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column names
idList <- plotmap@data$CR_NAME
places <- data.frame(id = idList, centroids.df)
## plotting chloropleth
## creating a colour brewer palette from http://colorbrewer2.org/

pal <- c("#FED98E",'white',"#FFFFD4",'ocean'='lightblue')

# pal<- (brewer.pal(5, "YlOrBr")[5:1])

ggplot(data = plotmapdf, aes(x = long, y = lat, group = group,fill=VI)) +
  geom_polygon(data = ocean, aes(long, lat, group = group,fill=hole),inherit.aes = FALSE) +
  geom_polygon(alpha = 0.9) +
  geom_path(colour = "grey50", size = 0.3) +
  coord_fixed(xlim = c(1370571.8, 1003875.7),  ylim = c(369045, 649045), ratio = 0.79)  + 
  scale_fill_manual(values = rev(pal))+
  guides(fill=FALSE)  +
  # geom_polygon(data=us,fill = 'grey50' ) +
  # geom_polygon(data = ocean, aes(long, lat, group = group,fill=hole),inherit.aes = FALSE) +
  geom_text(data = places,aes(label = id, x = Longitude, y = Latitude),size=3.5,color='red',inherit.aes = FALSE) +
  geom_text(data = communities,aes(label = Name, x = Longitude, y = Latitude),size = 2.5,inherit.aes = FALSE) +
  labs(title = "Vancouver Island \n College Region") +
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
  # geom_polygon(data=subset(plotmapdf,id='Vancouver Island'),fill='blue')
                     
plot(rd_plot)

## saving plots as SVG

## create a folder to store the output plots
if (!exists("out")) dir.create('out', showWarnings = FALSE)

ggsave("out/VI_rd.png", width = 40, height = 35.8, units = "cm")

svg_px("popn_pctplot.svg", width = 650, height = 550)
plot(rd_plot)
dev.off()


