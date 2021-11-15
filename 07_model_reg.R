
# Home --------------------------------------------------------------------

library(tidyverse)
library(leaflet)
library(ggforce)

Data_set_modelos <- readRDS("Data_set_modelos.rds") %>% 
  select(-meses_sin_delitos)

# Particion ---------------------------------------------------------------

library(tidymodels)
library(spatialsample)

dataset <- Data_set_modelos %>% filter(pliegue >= 12)

train <- dataset %>% filter(pliegue != 24)

test <- dataset %>% filter(pliegue == 24)

ind <- list(analysis = seq(nrow(train)),assessment = nrow(train) + seq(nrow(test)))

splits <- make_splits(ind, dataset)
splits$id <- tibble(id = "Resample1")

train <- training(splits)
test <- testing(splits)


set.seed(123)
cv_folds <- vfold_cv(train, v = 10)

set.seed(123)
spatial_folds <- spatial_clustering_cv(train, coords = c("lat", "long"), v = 10)

# Visualizacion de particiones --------------------------------------------

plot_splits <- function(x,y){
  
  analysis(x) %>%
    mutate(partition = "train") %>%
    bind_rows(
      assessment(x) %>%
        mutate(partition = "test")
      ) %>%
    
    ggplot(aes(long, lat, color = partition)) + 
    
    geom_point(alpha = 0.5) +
    
    theme_minimal() +
    
    scale_color_grey() +
    
    labs(title = y) +
    
    rremove("ylab") +
    
    rremove("xlab") +
    
    labs(color='Partición') 
}


splits_no_s <- cv_folds %>% mutate(graficos = map2(splits, id, plot_splits))


library(ggpubr)
ggarrange(plotlist = splits_no_s$graficos, common.legend = TRUE) %>% 
  annotate_figure(top = text_grob("Validación cruzada no espacial",
                                  color = "black", face = "bold", size = 16),
                  left = text_grob("Latitud", rot = 90, vjust = 1),
                  bottom = text_grob("Longitud"))


splits_s <- spatial_folds %>% mutate(graficos = map2(splits, id, plot_splits))

ggarrange(plotlist = splits_s$graficos, common.legend = TRUE) %>% 
  annotate_figure(
    top =text_grob("Validación cruzada espacial", 
                   color = "black", face = "bold", size = 16),
    left = text_grob("Latitud", rot = 90, vjust = 1),
    bottom = text_grob("Longitud")
  )


# Parametros adicionales XGBoost ------------------------------------------

colsample_bytree <- function(range = c(1L, unknown()), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(colsample_bytree = "# Sample variables by tree"),
    finalize = get_p
  )
}

lambda <- function(range = c(-10, 1), trans = log10_trans()) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(penalty_L2 = "Amount of L2 Regularization"),
    finalize = NULL
  )
}


alpha <- function(range = c(-10, 1), trans = log10_trans()) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(penalty_L1 = "Amount of L1 Regularization"),
    finalize = NULL
  )
}


# Metrica rmsle -----------------------------------------------------------

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

# Modeling all HO ----------------------------------------------------------------

Receta_regresion <- 
  
  recipe(formula = delitos_next_3 ~ ., data =  train) %>%
  
  step_mutate(bancos_atm = bancos + atm) %>% 
  
  step_rm(id, pliegue, bancos, atm, delitos, lat, long)


xgb_spec_H <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                     
  learn_rate = tune(),
  mtry = tune(),
  stop_iter = tune(),
  sample_size = tune()
) %>% 
  set_engine("xgboost", 
             colsample_bytree = tune(),
             lambda = tune(),
             alpha = tune()
             ) %>% 
  set_mode("regression")


xgb_wf_H <- workflow() %>%
  add_recipe(Receta_regresion) %>%
  add_model(xgb_spec_H)


xgb_param <- 
  xgb_spec_H %>%
  parameters() %>% 
  update(trees = trees(c(10L, 50L)),
         mtry = mtry(c(28L, 42L)),
         colsample_bytree = colsample_bytree(c(28L, 42L)),
         lambda = penalty_L2(range = c(-10, -1)),
         alpha = penalty_L1(range = c(-10, -1))
         )


ctrl <- control_bayes(no_improve = 10, verbose = T, save_pred = T, seed = 123)

options(tidymodels.dark = TRUE)

tictoc::tic()
set.seed(123)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)
clusterEvalQ(cl, {library(tidymodels)})


xgb_res <- tune_bayes(xgb_wf_H, 
                      resamples = cv_folds,
                      iter = 15,
                      param_info = xgb_param,
                      metrics = metric_set(rmse),
                      control = ctrl,
                      initial = 15
)

doParallel::stopImplicitCluster()
tictoc::toc()
beepr::beep(8)

autoplot(xgb_res) + theme(legend.position = "top")

saveRDS(xgb_res, "xgb_res.rds")

modelo_xgb <- finalize_workflow(xgb_wf_H, parameters = select_best(xgb_res))

validacion_xgb  <- fit_resamples(
  object       = modelo_xgb,
  resamples    = cv_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_resamples(save_pred = TRUE, verbose = T)
) 

metricas<- c()
for (i in 1:10) {
  
  metricas[i] = rmsle_vec(validacion_xgb$splits[[i]] %>% testing() %>% .$delitos_next_3,
                          validacion_xgb$.predictions[[i]]$.pred)
}
mean(metricas)

final_xgb_H <- last_fit(modelo_xgb, splits, metrics = metric_set(rmse, mae))

final_xgb_H %>% collect_predictions() %>% rmsle(delitos_next_3,.pred)

bind_rows(
  collect_metrics(validacion_xgb) %>% 
    dplyr::rename(valor = mean) %>% 
    mutate(Error = "validación cruzada"),
  collect_metrics(final_xgb_H) %>% 
    dplyr::rename(valor = .estimate) %>% 
    mutate(Error = "Febrero 2021")) %>% 
  mutate(modelo = "XGB regresión") %>% 
  dplyr::select(modelo,.metric,valor,std_err,Error) %>% 
  arrange(.metric)


predicciones_reg <- final_xgb_reg %>%
  collect_predictions() %>% 
  dplyr::select(-.row)

plot_reg <- predicciones_reg %>% 
  ggplot(aes(x = delitos, y = .pred))+
  geom_point()+ 
  geom_abline(intercept = 0, col = "red") +
  theme_minimal()

densidad <- predicciones_reg %>% 
  pivot_longer(!c(id,.config),names_to = "Variable", values_to = "n") %>% 
  ggplot(aes(x=n, volor = Variable, fill = Variable)) + 
  geom_density(alpha=.2)+ 
  theme_minimal() +
  labs(x = "", y = "")

plot_reg / densidad

# Modeling all HO Spatial ----------------------------------------------------------------

xgb_wf_s_H <- workflow() %>%
  add_recipe(Receta_regresion) %>%
  add_model(xgb_spec_H)

ctrl <- control_bayes(no_improve = 10, verbose = T, save_pred = T, seed = 42)

options(tidymodels.dark = TRUE)

tictoc::tic()
set.seed(42)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)
clusterEvalQ(cl, {library(tidymodels)})


xgb_res_s <- tune_bayes(xgb_wf_s_H, 
                      resamples = spatial_folds,
                      iter = 15,
                      param_info = xgb_param,
                      metrics = metric_set(rmse),
                      control = ctrl,
                      initial = 15
)

doParallel::stopImplicitCluster()
tictoc::toc()
beepr::beep(8)

autoplot(xgb_res_s) + theme(legend.position = "top")

saveRDS(xgb_res_s, "xgb_res_s.rds")

modelo_xgb_s <- finalize_workflow(xgb_wf_s_H, parameters = select_best(xgb_res_s))

validacion_xgb_s  <- fit_resamples(
  object       = modelo_xgb_s,
  resamples    = spatial_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_resamples(save_pred = TRUE, verbose = T)
) 

metricas_s <- c()

for (i in 1:10) {
  
  metricas_s[i] = rmsle_vec(validacion_xgb_s$splits[[i]] %>% testing() %>% .$delitos_next_3,
                          validacion_xgb_s$.predictions[[i]]$.pred)
  
}

mean(metricas_s)

final_xgb_s_H <- last_fit(modelo_xgb_s, splits, metrics = metric_set(rmse, mae))

final_xgb_s_H %>% collect_predictions() %>% rmsle(delitos_next_3,.pred)

bind_rows(
  collect_metrics(validacion_xgb_s) %>% 
    dplyr::rename(valor = mean) %>% 
    mutate(Error = "validación cruzada"),
  collect_metrics(final_xgb_s_H) %>% 
    dplyr::rename(valor = .estimate) %>% 
    mutate(Error = "Febrero 2021")) %>% 
  mutate(modelo = "XGB regresión") %>% 
  dplyr::select(modelo,.metric,valor,std_err,Error) %>% 
  arrange(.metric)


predicciones_s <- final_xgb_s_H %>%
  collect_predictions() %>% 
  dplyr::select(-.row)

# Modeling delitos HO ----------------------------------------------------------------

Receta_regresion_d <- 
  
  recipe(formula = delitos_next_3 ~ ., data =  train) %>%
  
  step_select(contains("delitos")) %>% 
  
  step_rm(delitos)


xgb_spec_d_H <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                     
  learn_rate = tune(),
  mtry = tune(),
  stop_iter = tune(),
  sample_size = tune()
) %>% 
  set_engine("xgboost", 
             colsample_bytree = tune(),
             lambda = tune(),
             alpha = tune()
  ) %>% 
  set_mode("regression")


xgb_wf_d <- workflow() %>%
  add_recipe(Receta_regresion_d) %>%
  add_model(xgb_spec_d_H)


xgb_param_d <- 
  xgb_spec_d_H %>%
  parameters() %>% 
  update(trees = trees(c(10L, 50L)),
         mtry = mtry(c(4L, 5L)),
         colsample_bytree = colsample_bytree(c(4L, 5L)),
         lambda = penalty_L2(range = c(-10, -1)),
         alpha = penalty_L1(range = c(-10, -1))
  )


ctrl <- control_bayes(no_improve = 10, verbose = T, save_pred = T, seed = 123)

options(tidymodels.dark = TRUE)

tictoc::tic()
set.seed(123)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)
clusterEvalQ(cl, {library(tidymodels)})


xgb_res_d <- tune_bayes(xgb_wf_d, 
                      resamples = cv_folds,
                      iter = 15,
                      param_info = xgb_param_d,
                      metrics = metric_set(rmse),
                      control = ctrl,
                      initial = 15
)

doParallel::stopImplicitCluster()
tictoc::toc()
beepr::beep(8)


xgb_res_d

autoplot(xgb_res_d) + theme(legend.position = "top")

saveRDS(xgb_res_d, "xgb_res_d.rds")

modelo_xgb_d <- finalize_workflow(xgb_wf_d, parameters = select_best(xgb_res_d))

validacion_xgb_d  <- fit_resamples(
  object       = modelo_xgb_d,
  resamples    = cv_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_resamples(save_pred = TRUE, verbose = TRUE)
) 

metricas_d <- c()
for (i in 1:10) {
  
  metricas_d[i] = rmsle_vec(validacion_xgb_d$splits[[i]] %>% testing() %>% .$delitos_next_3,
                            validacion_xgb_d$.predictions[[i]]$.pred)
}
mean(metricas_d)


final_xgb_d <- last_fit(modelo_xgb_d, splits, metrics = metric_set(rmse, mae))

final_xgb_d %>% collect_predictions() %>% rmsle(delitos_next_3,.pred)

bind_rows(
  collect_metrics(validacion_xgb_d) %>% 
    dplyr::rename(valor = mean) %>% 
    mutate(Error = "validación cruzada"),
  collect_metrics(final_xgb_d) %>% 
    dplyr::rename(valor = .estimate) %>% 
    mutate(Error = "Diciembre 2019")) %>% 
  mutate(modelo = "XGB regresión") %>% 
  dplyr::select(modelo,.metric,valor,std_err,Error) %>% 
  arrange(.metric) 


predicciones_reg_d <- final_xgb_reg_d %>%
  collect_predictions() %>% 
  dplyr::select(-.row)

# Modeling delitos HO Spatial----------------------------------------------------------------

xgb_wf_d_s <- workflow() %>%
  add_recipe(Receta_regresion_d) %>%
  add_model(xgb_spec_d_H)


ctrl <- control_bayes(no_improve = 10, verbose = T, save_pred = T, seed = 123)

options(tidymodels.dark = TRUE)

tictoc::tic()
set.seed(123)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)
clusterEvalQ(cl, {library(tidymodels)})


xgb_res_d_s <- tune_bayes(xgb_wf_d_s, 
                        resamples = spatial_folds,
                        iter = 15,
                        param_info = xgb_param_d,
                        metrics = metric_set(rmse),
                        control = ctrl,
                        initial = 15
)

doParallel::stopImplicitCluster()
tictoc::toc()
beepr::beep(8)

autoplot(xgb_res_d_s) + theme(legend.position = "top")

saveRDS(xgb_res_d_s, "xgb_res_d_s.rds")

modelo_xgb_d_s <- finalize_workflow(xgb_wf_d_s, parameters = select_best(xgb_res_d_s))


validacion_d_s  <- fit_resamples(
  object       = modelo_xgb_d_s,
  resamples    = spatial_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_resamples(verbose  = TRUE, save_pred = TRUE)
) 


final_xgb_d_s <- last_fit(modelo_xgb_d_s, splits, metrics = metric_set(rmse, mae))

metricas_d_s <- c()

for (i in 1:10) {
  
  metricas_d_s[i] = rmsle_vec(validacion_d_s$splits[[i]] %>% testing() %>% .$delitos_next_3,
                          validacion_d_s$.predictions[[i]]$.pred)
  
}

mean(metricas_d_s)

final_xgb_d_s %>% collect_predictions() %>% rmsle(delitos_next_3,.pred)

bind_rows(
  collect_metrics(validacion_d_s) %>% 
    dplyr::rename(valor = mean) %>% 
    mutate(Error = "validación cruzada"),
  collect_metrics(final_xgb_d_s) %>% 
    dplyr::rename(valor = .estimate) %>% 
    mutate(Error = "Diciembre 2019")) %>% 
  mutate(modelo = "XGB regresión") %>% 
  dplyr::select(modelo,.metric,valor,std_err,Error) %>% 
  arrange(.metric)


predicciones_reg_d <- final_xgb_reg_d %>%
  collect_predictions() %>% 
  dplyr::select(-.row)

# Shap values funcion modificada ------------------------------------------

plot_shap_my <- function(data_long,
                         x_bound = NULL,
                         dilute = FALSE,
                         scientific = FALSE,
                         my_format = NULL,
                         min_color_bound = "#FFCC33",
                         max_color_bound = "#6600CC"){
  
  if (scientific){label_format = "%.1e"} else {label_format = "%.3f"}
  if (!is.null(my_format)) label_format <- my_format
  # check number of observations
  N_features <- data.table::setDT(data_long)[,uniqueN(variable)]
  if (is.null(dilute)) dilute = FALSE
  
  nrow_X <- nrow(data_long)/N_features # n per feature
  if (dilute!=0){
    # if nrow_X <= 10, no dilute happens
    dilute <- ceiling(min(nrow_X/10, abs(as.numeric(dilute)))) # not allowed to dilute to fewer than 10 obs/feature
    set.seed(1234)
    data_long <- data_long[sample(nrow(data_long),
                                  min(nrow(data_long)/dilute, nrow(data_long)/2))] # dilute
  }
  
  
  plot1 <- ggplot(data = data_long) +
    coord_flip(ylim = x_bound) +
    geom_hline(yintercept = 0) + # the y-axis beneath
    # sina plot:
    ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
                       method = "counts", maxwidth = 0.7, alpha = 0.7) +
    # print the mean absolute value:
    geom_text(data = unique(data_long[, c("variable", "mean_value")]),
              aes(x = variable, y=-Inf, label = sprintf(label_format, mean_value)),
              size = 3, alpha = 0.7,
              hjust = -0.2,
              fontface = "bold",
              check_overlap = TRUE) + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) +
    scale_color_gradient(low=min_color_bound, high=max_color_bound,
                         breaks=c(0,1), labels=c(" Low", "High "),
                         guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
    theme_bw() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(), # remove axis line
          axis.text.y = element_text(size = 15),
          legend.position="bottom",
          legend.title=element_text(size=10),
          legend.text=element_text(size=8),
          axis.title.x= element_text(size = 10)) +
    # reverse the order of features, from high to low
    # also relabel the feature using `label.feature`
    scale_x_discrete(limits = rev(levels(data_long$variable)),
                     labels = SHAPforxgboost:::label.feature(rev(levels(data_long$variable))))+
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value  ")
  return(plot1)
}


# Shap Values -------------------------------------------------------------

library(SHAPforxgboost)
library(data.table)
library(fastshap)

receta_workflow <- extract_recipe(final_xgb_s_H $.workflow[[1]])

train_data <- receta_workflow %>% bake(training(splits)) %>% dplyr::select(-delitos_next_3) %>% 
  as.matrix()

shap_values <- explain(final_xgb_s_H $.workflow[[1]] %>% extract_fit_parsnip() %>% .$fit,
                       X = train_data, exact = TRUE)

autoplot(shap_values)


var_delitos <- names(shap_values) %>% stringr::str_detect("delitos", negate = TRUE)
prueba <- shap_values[,var_delitos] 
autoplot(prueba)


shap_xgb <- shap.prep(final_lg_reg$.workflow[[1]] %>% extract_fit_parsnip() %>% .$fit,
                      X_train = train_data)

# Top 10 variales
top_10 <- shap_xgb %>%
  distinct(variable, mean_value) %>%
  arrange(desc(mean_value)) %>%
  .$variable %>%
  head(10)

top_10_data <- shap_xgb %>%
  dplyr::filter(variable %in% top_10) %>%  
  mutate(variable = as.character(variable)) %>% 
  mutate(variable =  factor(variable, levels = top_10))

top_10_plot <- plot_shap_my(top_10_data, dilute = TRUE, x_bound = c(-3,18)) +
  ggtitle("Principales variables") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  scale_color_gradient(low="gray", high="black",
                       breaks=c(0,1), labels=c(" Bajo", "Alto "),
                       guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
  labs(y = "SHAP value (impacto en la salida del modelo)", x = "", color = "Valor de la variable  ")

top_10_plot

# sin variables delitos

var_delitos <- names(shap_values)[names(shap_values) %>% stringr::str_detect("delitos", negate = TRUE)]

top_nd <- shap_xgb %>% dplyr::filter(variable %in% var_delitos) 

top_10_nd_levels <- top_nd %>%
  distinct(variable, mean_value) %>%
  arrange(desc(mean_value)) %>%
  .$variable %>%
  head(10)


top_10_nd <- top_nd %>%
  dplyr::filter(variable %in% top_10_nd_levels) %>%  
  mutate(variable = as.character(variable)) %>% 
  mutate(variable =  factor(variable, levels = top_10_nd_levels))

top_10_nd_plot <- plot_shap_my(top_10_nd, dilute = TRUE, x_bound = c(-0.4,0.5)) +
  ggtitle("Principales variables no delictivas") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  scale_color_gradient(low="gray", high="black",
                       breaks=c(0,1), labels=c(" Bajo", "Alto "),
                       guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
  labs(y = "SHAP value (impacto en la salida del modelo)", x = "", color = "Valor de la variable  ")



ggarrange(top_10_plot, top_10_nd_plot, common.legend = TRUE, ncol = 2, 
          legend = "bottom")
