
library(spdplyr)
library(tidyverse)
library(rgdal)
library(lubridate)
library(janitor)
library(zoo)

load("01_lectura.RData")

barrios_populares <- spTransform(shape_barrios_populares, CRS(proyeccion)) %>% 
  filter(stringr::str_detect(OBSERV,
                             "Complejos de Edificios ya incorporados en la trama urbana",
                             negate = TRUE))


delitos_2020 <- delitos_2020 %>% 
  rename(lat = latitud ,long = longitud,
         tipo_delito = tipo, subtipo_delito = subtipo) %>% 
  drop_na(long,lat) %>%
  mutate(fecha = lubridate::dmy(fecha)) %>% 
  filter(fecha < "2020-03-01")

delitos_2019 <- delitos_2019 %>% drop_na(long,lat)

delitos_2018 <- delitos_2018 %>% drop_na(long,lat)

delitos_2017 <- delitos_2017 %>% drop_na(long,lat)

universidades <- universidades %>% distinct(universida,long,lat)

local_bailables <- local_bailables %>%
  rename(long = longitud,lat = latitud) %>% 
  drop_na(long,lat)

bancos <- bancos %>% rename(long = X, lat = Y)


hotel_baja <- hotel %>% filter(categoria %in% c("1*","2*","3*", "APART", "APART 1*", "APART 2*","Hosp. B"))

hotel_alta <- hotel %>% filter(categoria %in% c("4*","5*","Hosp. A", "APART 3*"))

librerias <- espacios_culturales %>% 
  filter(funcion_principal == "LIBRERIA") %>% 
  rename(long = longitud,lat = latitud)

teatros <- espacios_culturales %>% 
  filter(subcategoria == "SALA DE TEATRO") %>% 
  rename(long = longitud,lat = latitud)


gimnasios <- gimnasios %>% drop_na(long,lat)

hospitales <- hospitales %>% 
  mutate(coordenadas = str_trim(gsub("\\(|\\)|[[:alpha:]]","",WKT))) %>% 
  select(coordenadas) %>% 
  separate(coordenadas,c("long","lat"), sep = " ") %>% 
  mutate_all(as.double)

metrobus <- metrobus %>% 
  mutate(coordenadas = str_trim(gsub("\\(|\\)|[[:alpha:]]","",WKT))) %>% 
  select(coordenadas) %>% 
  separate(coordenadas,c("long","lat"), sep = " ") %>% 
  mutate_all(as.double)

centros_salud_comunitarios <- centros_salud_comunitarios %>% 
  mutate(coordenadas = str_trim(gsub("\\(|\\)|[[:alpha:]]","",WKT))) %>% 
  select(coordenadas) %>% 
  separate(coordenadas,c("long","lat"), sep = " ") %>% 
  mutate_all(as.double)

colectivo <- colectivo %>% rename(long = stop_lon, lat = stop_lat)

barrios_populares_centroides <- coordinates(barrios_populares) %>% 
  as.data.frame() %>% 
  `colnames<-`(c("long","lat"))


delitos <- bind_rows(delitos_2020,
                     delitos_2019,
                     delitos_2018,
                     delitos_2017 %>% mutate(franja_horaria = as.double(franja_horaria)))

skim(delitos)

table(delitos$tipo_delito, delitos$subtipo_delito, useNA = "always")


delitos <- delitos %>% 
  filter(tipo_delito == "Robo (con violencia)") %>% 
  mutate(cantidad_registrada = ifelse(is.na(cantidad_registrada), 1, cantidad_registrada)) %>% 
  uncount(cantidad_registrada)


delitos_def <- delitos %>% mutate(id = as.character(id)) %>% 
  mutate(id = ifelse(is.na(id), id_mapa, id)) %>% 
  mutate(fecha_split = zoo::as.yearmon(fecha, "%Y-%m")) %>% 
  dplyr::select(id,  lat, long, fecha_split) %>% 
  mutate(across(c(lat,long), ~ ifelse(.x < -100, .x/1000 ,.x)))



meses <- temperatura_mes_histoico$mes %>% unique()
  
temperatura_mes_histoico <- temperatura_mes_histoico %>% 
  clean_names() %>% 
  bind_cols(meses = match(.$mes,meses)) %>% 
  mutate(fecha = paste0(ano,"-",meses)) %>%
  mutate(fecha = as.yearmon(fecha, "%Y-%m")) %>% 
  select(fecha, maxima,minima,media) %>% 
  filter(fecha < "mar. 2020")

lluvia_mes_histoico <- lluvia_mes_histoico %>% 
  clean_names() %>% 
  bind_cols(meses = match(.$mes,meses)) %>% 
  mutate(fecha = paste0(ano,"-",meses)) %>%
  mutate(fecha = as.yearmon(fecha, "%Y-%m")) %>% 
  select(-c(ano,meses,mes)) %>% 
  filter(fecha < "mar. 2020")

viento_mes_histoico <- viento_mes_histoico %>% 
  clean_names() %>% 
  select(-3) %>% 
  setNames((c("fecha","veloc_viento"))) %>% 
  mutate(fecha = as.yearmon(fecha, "%Y-%m")) %>% 
  filter(fecha < "mar. 2020")


save.image("02_clean.RData")
