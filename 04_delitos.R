

# Home --------------------------------------------------------------------

load("C:/Users/Lenovo/Desktop/UBA/Esp_MC/articulo/paper_robos/02_clean.RData")
load("C:/Users/Lenovo/Desktop/UBA/Esp_MC/articulo/paper_robos/03_calles.RData")

library(tidyverse)
library(future)
library(furrr)
library(zoo, include.only = "as.yearmon")
library(janitor)

pertenencia_punto <- function(data, referencia, metros){
  
  crime <- data %>%
    dplyr::select(.data$long,.data$lat) %>%
    split(1:nrow(data))
  
  esquinas <- referencia[,c("long","lat")]
  
  vector_esquinas<- c()
  
  #Esquina <- parallel::parLapply(crime, function(x){
    
  Esquina <- lapply(crime, function(x){
    
    for (i in 1:nrow(esquinas)) {
      
      vector_localizacion <- geosphere::distm(x,  esquinas[i,], fun = geosphere::distHaversine)
      
      vector_esquinas[i] <- vector_localizacion
    }
    
    vector <- list(which(vector_esquinas <= metros))
    
    n_total <- ifelse(length(vector[[1]]) == 0, 0, vector)
    
  })
  return(Esquina)
}


tictoc::tic()

set.seed(123)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)
clusterEvalQ(cl, {library(tidyverse);library(geosphere)})


esquinas_1_25000 <- pertenencia_punto(data = delitos_def[1:25000,],
                              referencia = intercepcion_calles_def,
                              metros = 200)

doParallel::stopImplicitCluster()

tictoc::toc()
beepr::beep(8)

saveRDS(esquinas_1_25000, "esquinas_1_25000.rds")

delitos_def_1 <- delitos_def %>% head(25) %>% 
  mutate(esquina = Esquina ) %>% 
  mutate(train = map(esquina,~ pluck(.x,1) %>% .[1])) %>% 
  filter(train != 0) %>% 
  select(-train) %>% 
  unnest(esquina) %>% unnest(esquina) %>% 
  mutate(esquina = paste0("esquina_",esquina)) %>% 
  group_by(esquina,fecha_split) %>% 
  tally(name = "delitos")

# Prueba con furr ---------------------------------------------------------


# #1 - 50000 ----------------------------------------------------------

crime <- delitos_def[1:50000,] %>%
  dplyr::select(.data$long,.data$lat) %>%
  split(1:nrow(delitos_def[1:50000,]))

esquinas <- intercepcion_calles_def[,c("long","lat")]

metros <- 200

vector_esquinas<- c()


tictoc::tic()

future::plan(multisession, workers = 4)

Esquina_1_50000 <- furrr::future_map(crime, function(.x){
  
  for (i in 1:nrow(esquinas)) {
    
    vector_localizacion <- geosphere::distm(.x,  esquinas[i,], fun = geosphere::distHaversine)
    
    vector_esquinas[i] <- vector_localizacion
  }
  
  vector <- list(which(vector_esquinas <= metros))
  
  n_total <- ifelse(length(vector[[1]]) == 0, 0, vector)
  
})

tictoc::toc()

beepr::beep(8)

saveRDS(Esquina_1_50000, "Esquina_1_50000.rds")
  

  delitos_def_1 <- delitos_def %>% slice(1:50000) %>% 
    mutate(esquina = Esquina_1_50000 ) %>% 
    mutate(train = map(esquina,~ pluck(.x,1) %>% .[1])) %>% 
    filter(train != 0) %>% 
    dplyr::select(-train) %>% 
    unnest(esquina) %>% unnest(esquina) %>% 
    mutate(esquina = paste0("esquina_",esquina)) %>% 
    group_by(esquina,fecha_split) %>% 
    tally(name = "delitos")





  
  







# #50000 - 150000 ----------------------------------------------------------
  
  crime <- delitos_def[50001:150000,] %>%
    dplyr::select(.data$long,.data$lat) %>%
    split(1:nrow(delitos_def[50001:150000,]))
  
  esquinas <- intercepcion_calles_def[,c("long","lat")]
  
  metros <- 200
  
  vector_esquinas<- c()
  
  
  tictoc::tic()
  
  future::plan(multisession, workers = 4)
  
  Esquina_50001_150000 <- furrr::future_map(crime, function(.x){
    
    for (i in 1:nrow(esquinas)) {
      
      vector_localizacion <- geosphere::distm(.x,  esquinas[i,], fun = geosphere::distHaversine)
      
      vector_esquinas[i] <- vector_localizacion
    }
    
    vector <- list(which(vector_esquinas <= metros))
    
    n_total <- ifelse(length(vector[[1]]) == 0, 0, vector)
    
  })
  
  tictoc::toc()
  
  beepr::beep(8)
  
  saveRDS(Esquina_50001_150000, "Esquina_50001_150000.rds")
  
  
  delitos_def_1 <- delitos_def %>% slice(50001:150000) %>% 
    mutate(esquina = Esquina_50001_150000 ) %>% 
    mutate(train = map(esquina,~ pluck(.x,1) %>% .[1])) %>% 
    filter(train != 0) %>% 
    select(-train) %>% 
    unnest(esquina) %>% unnest(esquina) %>% 
    mutate(esquina = paste0("esquina_",esquina)) %>% 
    group_by(esquina,fecha_split) %>% 
    tally(name = "delitos")
  
  
# # 150000 - tail ----------------------------------------------------------
  
  crime <- delitos_def[150001:208214,] %>%
    dplyr::select(.data$long,.data$lat) %>%
    split(1:nrow(delitos_def[150001:208214,]))
  
  esquinas <- intercepcion_calles_def[,c("long","lat")]
  
  metros <- 200
  
  vector_esquinas<- c()
  
  
  tictoc::tic()
  
  future::plan(multisession, workers = 4)
  
  Esquina_150001_tail <- furrr::future_map(crime, function(.x){
    
    for (i in 1:nrow(esquinas)) {
      
      vector_localizacion <- geosphere::distm(.x,  esquinas[i,], fun = geosphere::distHaversine)
      
      vector_esquinas[i] <- vector_localizacion
    }
    
    vector <- list(which(vector_esquinas <= metros))
    
    n_total <- ifelse(length(vector[[1]]) == 0, 0, vector)
    
  })
  
  tictoc::toc()
  
  beepr::beep(8)
  
  saveRDS(Esquina_150001_tail, "Esquina_150001_tail.rds")
  
  
  delitos_def_1 <- delitos_def %>% slice(150001:208214) %>% 
    mutate(esquina = Esquina_150001_tail ) %>% 
    mutate(train = map(esquina,~ pluck(.x,1) %>% .[1])) %>% 
    filter(train != 0) %>% 
    select(-train) %>% 
    unnest(esquina) %>% unnest(esquina) %>% 
    mutate(esquina = paste0("esquina_",esquina)) %>% 
    group_by(esquina,fecha_split) %>% 
    tally(name = "delitos")
  
  
  
  
# Unificacion de datasets -------------------------------------------------
  
 Esquina_1_50000 <- readRDS("Esquina_1_50000.rds")
 Esquina_50001_150000 <- readRDS("Esquina_50001_150000.rds")
 Esquina_150001_tail <- readRDS("Esquina_150001_tail.rds")
  
esquinas_delitos <- c(
    Esquina_1_50000,
    Esquina_50001_150000,
    Esquina_150001_tail
  )

meses <- c(
  "delitos_ene_2017",
  "delitos_feb_2017",      "delitos_mar_2017",     
  "delitos_abr_2017",      "delitos_may_2017",      "delitos_jun_2017" ,    
  "delitos_jul_2017",      "delitos_ago_2017",      "delitos_sep_2017" ,
  "delitos_oct_2017",
  "delitos_nov_2017",      "delitos_dic_2017",      "delitos_ene_2018" ,    
  "delitos_feb_2018",      "delitos_mar_2018",      "delitos_abr_2018" ,    
  "delitos_may_2018",      "delitos_jun_2018",      "delitos_jul_2018" ,    
  "delitos_ago_2018",      "delitos_sep_2018",      "delitos_oct_2018" ,    
  "delitos_nov_2018",      "delitos_dic_2018",      "delitos_ene_2019" ,    
  "delitos_feb_2019",      "delitos_mar_2019",      "delitos_abr_2019" ,    
  "delitos_may_2019",      "delitos_jun_2019",      "delitos_jul_2019" ,    
  "delitos_ago_2019",      "delitos_sep_2019",      "delitos_oct_2019" ,    
  "delitos_nov_2019",      "delitos_dic_2019",      "delitos_ene_2020" ,    
  "delitos_feb_2020"  
)

  
  delitos_def_1 <- delitos_def %>% 
    mutate(esquina = esquinas_delitos) %>% 
    mutate(train = map(esquina,~ pluck(.x,1) %>% .[1])) %>% 
    filter(train != 0) %>% 
    select(-train) %>% 
    unnest(esquina) %>% unnest(esquina) %>% 
    mutate(esquina = paste0("esquina_",esquina)) %>% 
    group_by(esquina,fecha_split) %>% 
    tally(name = "delitos")
  
  saveRDS(delitos_def_1, "delitos_def_1.rds")
  
  
  delitos_def_2 <- delitos_def_1 %>% 
    pivot_wider(names_from = fecha_split,
                values_from = delitos, 
                values_fill = 0) %>% 
    pivot_longer(!esquina, names_to = "fecha_split", values_to = "delitos") %>% 
    mutate(fecha_split = as.yearmon(fecha_split, "%b %Y")) %>% 
    left_join(
      temperatura_mes_histoico %>% 
        select(fecha, media), by = c("fecha_split"="fecha")
    ) %>% 
    left_join(
      lluvia_mes_histoico, by = c("fecha_split"="fecha")
    ) %>% 
    left_join(
      viento_mes_histoico, by = c("fecha_split"="fecha")
    ) %>% 
    ungroup() %>% 
    dplyr::rename(temperatura = media, mm_agua = mm, dias_lluvia = dias)
  
  
  delitos_def_3 <- delitos_def_2 %>% 
    pivot_wider(names_from = fecha_split,
                values_from = c(delitos, temperatura, mm_agua, dias_lluvia, veloc_viento), 
                values_fill = 0) %>% 
    clean_names() %>% 
    relocate(esquina, all_of(meses))

  
  
  delitos_def_4 <- delitos_def_3 %>% 
    dplyr::mutate(meses_sin_delitos = rowSums(dplyr::select(., delitos_dic_2017:delitos_feb_2020) == 0)) %>% 
    relocate(meses_sin_delitos)
  
  
  saveRDS(delitos_def_4, "04_delitos.rds")
  
