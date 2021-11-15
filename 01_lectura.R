
library(tidyverse)
library(rgdal)


delitos_2020 <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/delitos/delitos_2020.csv")

delitos_2019 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv")

delitos_2018 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv")

delitos_2017 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2017.csv")

universidades <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/universidades/universidades.csv")

local_bailables <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/agencia-gubernamental-de-control/locales-bailables/locales-bailables.csv") 

centros_de_salud_privados <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-salud-privados/centros-de-salud-privados.csv")

hospitales <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/hospitales/hospitales.csv")

comisarias <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv")

embajadas <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/embajadas/embajadas.csv")

bocas_de_subte <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/bocas-de-subte/bocas-de-subte.csv")

atm <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/cajeros-automaticos/cajeros-automaticos.csv")

bancos <- read_csv2("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bancos/accesibilidad-en-bancos.csv")

gasolina <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-de-servicio/estaciones_servicio_caba.csv")

gastronomica<- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/oferta-gastronomica/oferta_gastronomica.csv") 

hotel<- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/alojamientos-turisticos/alojamientos-turisticos.csv") %>% 
  drop_na(long,lat)

metrobus <-  read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/metrobus/estaciones-de-metrobus.csv") 

taxi <-  read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/paradas-de-taxis/paradas-de-taxis.csv")

carteles <-  read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/carteles-luminosos/carteles-luminosos.csv")

colectivo <-  read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/colectivos/paradas-de-colectivo.csv") 

farmacias <-  read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/farmacias/farmacias.csv")

centros_salud_comunitarios <-  read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/centros-salud-accion-comunitaria-cesac/centros_de_salud_nivel_1_BADATA_WGS84.csv") 

centros_medicos_barriales <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-medicos-barriales/centros-medicos-barriales.csv")

estadios <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/estadios/estadios.csv")

skate <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/pistas-de-skate/pistas-de-skate.csv")

gimnasios <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/gimnasios/gimnasios.csv")

wifi <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/puntos-de-wi-fi-publicos/sitios-de-wifi.csv")

garajes <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/garajes-comerciales/garajes-comerciales.csv") 

educativos <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/establecimientos-educativos/establecimientos-educativos.csv")

bibliotecas <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bibliotecas/bibliotecas.csv")

basilicas <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/basilicas/basilicas.csv")

espacios_culturales <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/espacios-culturales/espacios-culturales.csv")

organizaciones_sociales <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/organizaciones-sociales/organizaciones-sociales.csv")

estaciones_ferrocarril <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-de-ferrocarril/estaciones-de-ferrocarril.csv")

temperatura_mes_histoico <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/direccion-general-de-estadisticas-y-censos/registro-temperatura-ciudad/historico_temperaturas.csv") 

lluvia_mes_histoico <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/direccion-general-de-estadisticas-y-censos/registro-precipitaciones-ciudad/historico_precipitaciones.csv") 

viento_mes_histoico <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/direccion-general-de-estadisticas-y-censos/velocidad-maxima-del-viento/velocidad-max-viento.csv")
                                #skip = 1) 

hogares_paradores <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/hogares-y-paradores/hogares-y-paradores.csv") %>% 
  drop_na(X, Y)

bus_turistico <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/paradas-y-recorridos-bus-turistico/paradas_bus_turistico.csv") 

consulados <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/consulados/consulados.csv") 

bomberos <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/cuarteles-y-destacamentos-de-bomberos/cuarteles-y-destacamentos-de-bomberos-de-policia-federal-argentina.csv") 



proyeccion <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Barrios Caba
dir.create(file.path(getwd(), "shape_barrios"))  

download.file("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios-zip.zip",
              destfile="shape_barrios/barrios-zip.zip")

unzip("shape_barrios/barrios-zip.zip",exdir= "shape_barrios")

shape_barrios <- readOGR("shape_barrios/barrios_badata_wgs84.shp") #Limites de Barrios CABA

shape_barrios_caba <- spTransform(shape_barrios, CRS(proyeccion))


# barrios-populares
dir.create(file.path(getwd(), "shape_barrios_populares"))  

download.file("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/desarrollo-humano-y-habitat/barrios-populares/barrios-populares-badata.zip", destfile<-"shape_barrios_populares/barrios-populares-badata.zip")

unzip("shape_barrios_populares/barrios-populares-badata.zip",exdir= "shape_barrios_populares")

shape_barrios_populares <- readOGR("shape_barrios_populares/barrios_vulnerables.shp") 


save.image("01_lectura.RData")
