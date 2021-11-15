

# Home --------------------------------------------------------------------

delitos_def_4 <- readRDS("04_delitos.rds")
load("02_clean.RData")
load("03_calles.RData")

library(tidyverse)
library(sp)
library(geosphere)


delitos_def_5 <- delitos_def_4 %>% 
  left_join(
    intercepcion_calles_def %>% 
      mutate(esquina = paste0("esquina_",id)) %>% 
      select(-id)
  ) %>% 
  relocate(esquina,long,lat)


F.distancia.punto <- function(referencia, data,metros){
  
  conteo <- lapply(data, function(x){
    
    vector_localizacion <- distm(x, referencia, fun = distHaversine)
    n_total <- sum(vector_localizacion <= metros)
    n_total
  })
  conteo  %>% unlist()
}


split_esquina <- delitos_def_5 %>% 
  select(long,lat) %>% 
  split(1:nrow(delitos_def_5))

# Generales ---------------------------------------------------------------

datas_400 <- erer::listn(
  
  wifi,
  teatros,
  gimnasios,
  colectivo,
  atm,
  bancos,
  gasolina,
  gastronomica,
  taxi,
  farmacias,
  garajes,
  educativos,
  organizaciones_sociales
)

datas_400 <- lapply(datas_400, function(x){
  
  x %>% select(c("long","lat"))
  
})

entorno_400 <- lapply(datas_400, F.distancia.punto, metros = 400, data = split_esquina) 

entorno_400 <- bind_cols(entorno_400 %>% as.data.frame())

Data_set_final_1 <- bind_cols(delitos_def_5, entorno_400)

# 1000 --------------------------------------------------------------------

datas_1000 <- erer::listn(
  
  bomberos,
  estaciones_ferrocarril, 
  bibliotecas,    
  centros_medicos_barriales,
  centros_salud_comunitarios,          
  hospitales   
)

datas_1000 <- lapply(datas_1000, function(x){
  
  x %>% select(c("long","lat"))
  
})

datas_1000 <- lapply(datas_1000, F.distancia.punto, metros = 1000, data = split_esquina) 

datas_1000 <- bind_cols(datas_1000 %>% as.data.frame())

Data_set_final_1 <- bind_cols(Data_set_final_1, datas_1000)

# 500 ---------------------------------------------------------------------

datas_500 <- erer::listn(
  
  consulados                    ,       
  bus_turistico       ,
  carteles,
  bocas_de_subte,
  embajadas               ,
  comisarias ,
  centros_de_salud_privados  ,
  metrobus    ,
  hotel_baja,
  hotel_alta,
  local_bailables,
  universidades 
)

datas_500 <- lapply(datas_500, function(x){
  
  x %>% select(c("long","lat"))
  
})

datas_500 <- lapply(datas_500, F.distancia.punto, metros = 500, data = split_esquina) 

datas_500 <- bind_cols(datas_500 %>% as.data.frame())

Data_set_final_1 <- bind_cols(Data_set_final_1, datas_500)


saveRDS(Data_set_final_1, "Data_set_final_1.rds")
