
library(tidyverse)
library(data.table)

Data_set_final_1 <- readRDS("Data_set_final_1.rds")

pliegues <- 1:24
names(pliegues) <- pliegues

variables <- c("delitos", "temperatura", "mm_agua", "lluvia", "viento")
names(variables) <- variables


my_sliding_window <- function (data, inicio, pliegues, variables) {
  
  list_data <- lapply(variables, function(x) {data %>% dplyr::select(1, dplyr::contains(x))})
  
  list_sliding_window <- lapply(list_data, function(y) {
    
    splits <- lapply(pliegues, function(x) {
      
      nombre <- names(y)[2]
      
      nombre <- sub("_.*", "", nombre)
      
      a <- y %>% 
        
        dplyr::transmute(.data$esquina,
                         
                         `:=`("{nombre}_last_year", rowSums(.[(inicio + x - 12)])),
                         
                         `:=`("{nombre}_last_12", rowSums(.[(inicio + x - 12):(inicio + x - 1)])), 
                         
                         `:=`("{nombre}_last_6", rowSums(.[(inicio + x - 6):(inicio + x - 1)])), 
                         
                         `:=`("{nombre}_last_3", rowSums(.[(inicio + x - 3):(inicio + x - 1)])), 
                         
                         `:=`("{nombre}_last_1", rowSums(.[(inicio + x - 1)])), 
                         
                         `:=`("{nombre}", rowSums(.[(inicio + x)])),
                         
                         `:=`("{nombre}_next_3", rowSums(.[(inicio + x):(inicio + x + 2)])), 
                         
                         pliegue = x)
      
    })
    
    splits
    
  })
  
  data_sliding_window <- purrr::map(list_sliding_window, dplyr::bind_rows)
  
  drop_pliegue <- ncol(data_sliding_window[[1]])
  
  id <- data_sliding_window[[1]][[1]]
  
  id_pliegue <- data_sliding_window[[1]][[drop_pliegue]]
  
  data_def <- do.call(dplyr::bind_cols, lapply(data_sliding_window, 
                                               `[`, -c(1, drop_pliegue))) %>% dplyr::mutate(id = id, 
                                                                                            pliegue = id_pliegue) %>% dplyr::relocate(id, .data$pliegue)
  return(data_def)
}


Data_set_final_2 <- my_sliding_window(data = Data_set_final_1 %>% select(-c(long,lat, meses_sin_delitos)),
                                   inicio = 13,
                                   pliegues = pliegues,  
                                   variables = variables)


Data_set_modelos <- Data_set_final_2 %>% 
  left_join(
    Data_set_final_1 %>% 
      select(esquina, long, lat, meses_sin_delitos, wifi:universidades), by =c("id" = "esquina")
  ) %>% 
  select(
    c(id,pliegue,contains("delitos"), mm_last_3, mm_last_1, temperatura_last_3, temperatura_last_1, dias_last_3,
      dias_last_1, veloc_last_3, veloc_last_1, long:universidades))

saveRDS(Data_set_modelos, "Data_set_modelos.rds")

