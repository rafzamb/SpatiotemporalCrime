
library(osmdata)
library(sf)
library(rangeBuilder)

# Calles

CABA_OS <- getbb("Ciudad Autonoma de Buenos Aires")

CABA_OS_poly <- getbb("Ciudad Autonoma de Buenos Aires", format_out = "sf_polygon")

sf::st_crs(CABA_OS_poly)

#test <- CABA <- opq(CABA_OS) 

st_crs(CABA_OS_poly)
st_crs(calles)

#st_transform(CABA_OS_poly, 4326)

#st_transform(conos, 5344)


CABA <- opq(CABA_OS) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

ggplot() +
  geom_sf(data = CABA$osm_lines, color=ggplot2::alpha("black",0.2)) +
  theme_minimal() +
  ggtitle("Principales calles y avenidas de CABA")


calles <- CABA$osm_lines

calles <- sf::st_intersection(calles, CABA_OS_poly$multipolygon)

intercepcion_calles <- st_cast(calles, "POINT") %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  distinct() %>% 
  rename(lat = Y, long = X) %>% 
  rownames_to_column("id")

intercepcion_calles_1 <- intercepcion_calles

coordinates(intercepcion_calles_1) <- c('long', 'lat')
proyeccion <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(intercepcion_calles_1) <- CRS(proyeccion)
intercepcion_calles_1 <- spTransform(intercepcion_calles_1, CRS(proyeccion))

intercepcion_calles_2 <- filterByProximity(intercepcion_calles_1,dist=0.3, mapUnits=F)

intercepcion_calles_def <- intercepcion_calles_2 %>% 
  coordinates %>% 
  as.data.frame() %>% 
  rownames_to_column("id")


save.image("03_calles.RData")
