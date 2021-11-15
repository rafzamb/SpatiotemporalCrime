
# home --------------------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(treesnip)
library(sknifedatar)
library(modeltime)
library(timetk)
library(clock)
library(erer)

Data_set_final_1 <- readRDS("Data_set_final_1.rds")

Sys.setlocale(locale='Spanish_Spain')

base_line_ts <-  Data_set_final_1 %>% 
  select(-meses_sin_delitos) %>% 
  select(esquina, contains("delitos")) %>% 
  pivot_longer(!esquina, names_to = "Tiempo", values_to = "Delitos") %>% 
  mutate(Tiempo =  gsub("delitos_","",Tiempo)) %>% 
  mutate(Tiempo =  gsub("sep","sept",Tiempo)) %>% 
  mutate(Tiempo =  gsub("_",".",Tiempo)) %>% 
  mutate(Tiempo = paste0("1_", Tiempo)) %>% 
  mutate(Tiempo = clock::date_parse(Tiempo, format = "%d_%B%Y", locale = clock_locale("es"))) %>% 
  filter(Tiempo < "2019-12-01" & Tiempo > "2017-11-01") %>% 
  arrange(esquina,Tiempo)


data <- base_line_ts %>% 
  rename(value = Delitos, date = Tiempo) %>% 
  nest(nested_column=-esquina )

# Funciones mdoficadas ----------------------------------------------------

my_multifif <- function (serie, .prop, ...) {
  
  variables <- serie %>% dplyr::select(nested_column) %>% purrr::pluck(1, 
                                                                       1) %>% names()
  if ("value" %in% variables == FALSE) 
    stop("No 'value' column was found. Please specify a column named 'value'.")
  if ("date" %in% variables == FALSE) 
    stop("No 'date' column was found. Please specify a column named 'date'.")
  nest_fit <- function(serie, model, .proportion = .prop) {
    if (tune::is_workflow(model) == TRUE) {
      model %>% parsnip::fit(data = rsample::training(rsample::initial_time_split(serie, 
                                                                                  prop = .proportion)))
    }
    else {
      model %>% parsnip::fit(value ~ date, data = rsample::training(rsample::initial_time_split(serie, 
                                                                                                prop = .proportion)))
    }
  }
  
  exprs <- substitute(list(...))
  list_model <- list(...)
  names(list_model) <- vapply(as.list(exprs), deparse, "")[-1]
  names_list_model <- names(list_model)
  
  models_fits <- mapply(function(.model, name_model, prop) {
    
    table_models <- serie %>% dplyr::mutate(`:=`("{name_model}", 
                                                 purrr::map(.data$nested_column, ~nest_fit(serie = .x, 
                                                                                           model = .model, .proportion = prop)))) %>% dplyr::select(3)
  }, list_model, names_list_model, prop = .prop, SIMPLIFY = F)
  
  time_data <- dplyr::bind_cols(serie, models_fits)
  exp1 <- colnames(time_data)[3:ncol(time_data)]
  exp2 <- paste("list(", paste(exp1, collapse = ","), 
                ")")
  exp3 <- parse(text = exp2)
  
  table_time <- time_data %>% dplyr::mutate(nested_model = purrr::pmap(eval(exp3), 
                                                                       .f = function(...) {
                                                                         modeltime::modeltime_table(...)
                                                                       })) %>% dplyr::mutate(calibration = purrr::pmap(list(.data$nested_model, 
                                                                                                                            .data$nested_column), .f = function(x = .data$nested_model, 
                                                                                                                                                                y = .data$nested_column) {
                                                                                                                              x %>% modeltime::modeltime_calibrate(new_data = rsample::testing(rsample::initial_time_split(y, 
                                                                                                                                                                                                                           
                                                                                                                                                                                                                           prop = .prop)))                                                                                                                       }))
  models_accuracy <- mapply(function(calibracion, name_ts) {
    
    calibracion %>% 
      modeltime::modeltime_accuracy(metric_set = extended_forecast_accuracy_metric_set()) %>%
      dplyr::mutate(name_serie = name_ts) %>% 
      dplyr::relocate(.data$name_serie)
    
  }, table_time$calibration, table_time[[1]], SIMPLIFY = F) %>% 
    dplyr::bind_rows()
  cli::cat_line()
  cli::cli_h1(paste0(nrow(table_time), " models fitted ", 
                     cli::symbol$heart))
  list(table_time = table_time, models_accuracy = models_accuracy)
}

my_modeltime_multibestmodel <- function (.table, .metric = NULL, .minimize = TRUE, .forecast = TRUE) {
  
  if (.forecast == TRUE & "nested_forecast" %in% colnames(.table) == 
      FALSE) 
    stop("The object entered in the parameter \n  \".table\" was not applied the \"modeltime_multiforecast\" function, therefore it does not have the column \"nested_forecast\"\n  and the best forecasting cannot be selected, change the parameter from \".forecast\" to \"FALSE\"")
  if (.minimize == TRUE) {
    .optimization <- "dplyr::slice_min"
  }
  else {
    .optimization <- "dplyr::slice_max"
  }
  if (is.null(.metric)) 
    .metric = "rmse"
  if (!.metric %in% c("mae", "mape", "mase", 
                      "smape", "rmse", "rsq")) 
    cat("A metric is being supplied that is outside of those defined by defaluutl(mae, mape, mase, smape, rmse, rsq)")
  calibration_table_best <- .table %>% dplyr::mutate(best_model = purrr::map(.data$calibration, 
                                                                             function(table_time = .data$calibration) {
                                                                               table_time %>% modeltime::modeltime_accuracy(metric_set = extended_forecast_accuracy_metric_set()) %>% 
                                                                                 eval(parse(text = .optimization))(eval(parse(text = .metric)), 
                                                                                                                   n = 1) %>% head(1) %>% dplyr::pull(.model_id)
                                                                             })) %>% dplyr::mutate(calibration = purrr::map2(.data$calibration, 
                                                                                                                             .data$best_model, function(x, y) x %>% dplyr::filter(.model_id == 
                                                                                                                                                                                    y)))
  if (.forecast == TRUE) {
    calibration_table_best <- calibration_table_best %>% 
      dplyr::mutate(nested_forecast = purrr::map2(.data$nested_forecast, 
                                                  .data$best_model, function(x, y) x %>% dplyr::filter(.model_id %in% 
                                                                                                         c(NA, y))))
  }
  
  return(calibration_table_best)
  
}

library(rlang)

rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  rmsle_impl <- function(truth, estimate) {
    sqrt(mean((log(truth + 1) - log(estimate + 1))^2))
  }
  
  metric_vec_template(
    metric_impl = rmsle_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

rmsle <- function(data, ...) {
  UseMethod("rmsle")
}
rmsle <- new_numeric_metric(rmsle, direction = "minimize")

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}

# Last 3 months -----------------------------------------------------------

Baseline_TS_last3 <- bind_cols(
  observado = Data_set_final_1 %>% 
    mutate(next_3 = delitos_dic_2019 +delitos_ene_2020 + delitos_feb_2020) %>% 
    .$next_3,
  prediccion =  Data_set_final_1 %>% 
    mutate(last_3 = delitos_nov_2019 +delitos_oct_2019 + delitos_sep_2019) %>% 
    .$last_3 
)


Metricas_ts_last3 <- multieval(.dataset = Baseline_TS_last3,
                             .observed = "observado",
                             .predictions = "prediccion",
                             .metrics = listn(rmse, mae, maape))

Metricas_ts_last3

rmsle_vec(Baseline_TS_last3$observado, Baseline_TS_last3$prediccion)

# Autoarima ---------------------------------------------------------------

m_auto_arima <- arima_reg() %>% set_engine('auto_arima')

model_table_aua <- my_multifif(serie = data,
                                .prop = 0.90,
                                m_auto_arima
                                )

saveRDS(model_table_aua, "model_table_aua.rds")


refit_aua <- modeltime_multirefit(models_table = model_table_aua$table_time)

saveRDS(refit_aua, "refit_aua.rds")


forecast_aua <- modeltime_multiforecast(
  refit_aua,
  .prop = 0.9,
  .h = "3 month"
)


pronosticos_aua <-
  forecast_aua %>% 
  select(esquina, nested_forecast) %>% 
  unnest(nested_forecast) %>% 
  filter(.key == "prediction") %>% 
  select(esquina, .model_desc, .value) %>% 
  group_by(esquina) %>% 
  summarise(.value = sum(.value))

saveRDS(pronosticos_aua, "pronosticos_aua.rds")


Baseline_TS_aua <- bind_cols(
  observado = Data_set_final_1 %>% 
    mutate(next_3 = delitos_dic_2019 +delitos_ene_2020 + delitos_feb_2020) %>% 
    .$next_3,
  pronosticos_aua
)


Metricas_ts_aua <- multieval(.dataset = Baseline_TS_aua,
                         .observed = "observado",
                         .predictions = ".value",
                         .metrics = listn(rmse, mae, maape))

Metricas_ts_aua

rmsle_vec(Baseline_TS_aua$observado, Baseline_TS_aua$.value)

# Autoarima + croston -----------------------------------------------------

m_auto_arima <- arima_reg() %>% set_engine('auto_arima')

m_croston <- exp_smoothing() %>% set_engine("croston")


model_table_aua_cro <- my_multifif(serie = data,
                                .prop = 0.90,
                                m_auto_arima,
                                m_croston
)

saveRDS(model_table_aua_cro, "model_table_aua_cro.rds")


best_model_aua_cro <- my_modeltime_multibestmodel(
  .table = model_table_aua_cro$table_time,
  .metric = "maape",
  .minimize = TRUE,
  .forecast = FALSE
)

refit_aua_cro <- modeltime_multirefit(models_table = best_model_aua_cro)

saveRDS(refit_aua_cro, "refit_aua_cro.rds")

forecast_aua_cro <- modeltime_multiforecast(
  refit_aua_cro,
  .prop = 0.9,
  .h = "3 month"
)

pronosticos_aua_cro <-
  forecast_aua_cro %>% 
  select(esquina, nested_forecast) %>% 
  unnest(nested_forecast) %>% 
  filter(.key == "prediction") %>% 
  select(esquina, .model_desc, .value) %>% 
  group_by(esquina, .model_desc) %>% 
  summarise(.value = sum(.value))

saveRDS(pronosticos_aua_cro, "pronosticos_aua_cro.rds")


Baseline_TS_aua_cro <- bind_cols(
  observado = Data_set_final_1 %>% 
    mutate(next_3 = delitos_dic_2019 +delitos_ene_2020 + delitos_feb_2020) %>% 
    .$next_3,
  pronosticos_aua_cro
)


Metricas_ts_aua_cro <- multieval(.dataset = Baseline_TS_aua_cro,
                         .observed = "observado",
                         .predictions = ".value",
                         .metrics = listn(rmse, mae, maape))


Metricas_ts_aua_cro

rmsle_vec(Baseline_TS_aua_cro$observado, Baseline_TS_aua_cro$.value)

