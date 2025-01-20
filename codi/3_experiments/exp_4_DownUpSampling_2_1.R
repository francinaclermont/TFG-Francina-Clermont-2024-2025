source("programacio/codi/3_experiements/llibreries_dataset.R")

experiment_label <- '4_DownUpSampling_2_1'
file_output <- 'exp_4_DownUpSampling_2_1'

skimr::skim(data_set_a_modelar)

# Crear 200 bootstraps estratificados a partir de las variables originales ----

# Se crean tantas muestras bootstrap ESTRATIFICADAS SEGUN LA RESPUESTA 
# requeridas como se pidan y se generan los datasets de training y test 

n_boot <- 1000

mostres_bootsrap <- rsample::bootstraps(data_set_a_modelar, times = n_boot, strata = CMVD_rev) %>%
  mutate(data_training = map(splits, ~ training(..1))) %>%
  mutate(data_testing = map(splits, ~ testing(..1)))


# Crear la receta ----

# Aqui se dejan los pasos de preprocesamiento. En este caso, el step adasyn
# pàra diferencia un experimento dejaremos que se haga con 2 vecinos.

eina_de_treball <- mostres_bootsrap %>%
  mutate(receta = map(data_training, 
                      ~ recipes::recipe(CMVD_rev ~ ., data = ..1) %>%
                        recipes::step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
                        themis::step_downsample(CMVD_rev, under_ratio = 2, skip = T) %>%
                        themis::step_upsample(CMVD_rev, over_ratio = 1, , skip = T)
  ))


# Cocinando recetas ----
# Aqui se actualizan los datasets de train y test 

## Dataset de train ----
safe_prep_bake_train <- safely(
  function(receta) { receta %>% prep() %>%bake(new_data = NULL)},
  otherwise="NO"
)

eina_de_treball <- eina_de_treball %>%  
  mutate(data_training = map(receta, ~safe_prep_bake_train(..1) ))


## Dataset de test  ----
safe_prep_bake_test <- safely(
  function(receta, dataset) {receta %>% prep() %>% bake(new_data = dataset) },
  otherwise="NO"
)

eina_de_treball <- eina_de_treball %>%  
  mutate(data_testing = map2(receta,data_testing, ~safe_prep_bake_test(..1,..2) ) )


# Filtrado de no convergencias ----
# Eliminamos todos aquellos datasets que por lo que sea no hayan convergido en el proceso de la receta

## Filtrado en train ----
eina_de_treball <- eina_de_treball %>%
  filter( map_lgl(data_training, ~ is.null(.x$error)==TRUE)) %>%
  mutate(data_training= map(data_training, "result")) 

## Filtrado en test ----
eina_de_treball <- eina_de_treball %>%
  filter(map_lgl(data_testing, ~ is.null(.x$error)==TRUE)) %>%
  mutate(data_testing= map(data_testing, "result")) 



# Ajuste del modelo-----
# un sencillo modelo de regresiuon lineal  usando como formula todas las variables disponibles

eina_de_treball <- eina_de_treball %>% 
  mutate( modelo_ajustado = map( data_training, ~ glm(formula = 'CMVD_rev ~ .', data= ..1, family = 'binomial' )))

# Predicciones ----

## Creación de predicciones training ----
eina_de_treball <- eina_de_treball %>%
  mutate(
    '.pred_train' = pmap(
      list(modelo_ajustado, data_training),
      ~ tibble(
        '.pred_0' =     predict(..1, newdata = ..2, type= 'response'),
        '.pred_1' = 1 - predict(..1, newdata = ..2, type= 'response')
      ) %>% 
        mutate(
          .pred_class = factor(
            case_when(
              .pred_1 > (0.5 - 0.0001) ~ 1,
              .pred_1 <= 0.5 ~ 0,
              TRUE ~ NA_real_),
            levels = c("1", "0")
          )
        ) %>% 
        mutate('CMVD_rev' = ..2$CMVD_rev)  # Etiquetas reales
    )
  )

## Creación de predicciones testing ----
eina_de_treball <- eina_de_treball %>%
  mutate(
    '.pred_test' = pmap(
      list(modelo_ajustado, data_testing),
      ~ tibble(
        '.pred_0' =     predict(..1, newdata = ..2, type= 'response'),
        '.pred_1' = 1 - predict(..1, newdata = ..2, type= 'response')
      ) %>% 
        mutate(
          .pred_class = factor(
            case_when(
              .pred_1 > (0.5 - 0.0001) ~ 1,
              .pred_1 <= 0.5 ~ 0,
              TRUE ~ NA_real_
            ),
            levels = c("1", "0")
          )) %>% 
        mutate('CMVD_rev' = ..2$CMVD_rev)  # Etiquetas reales
    )
  )

eina_de_treball


# Model Metrics ----

# Metrics definitions -----
Metricas_evaluacion <- metric_set(
  yardstick::accuracy,
  yardstick::sensitivity,
  yardstick::spec,
  yardstick::f_meas,
  yardstick::kap,
  yardstick::ppv,
  yardstick::mcc,
  yardstick::bal_accuracy,
  yardstick::detection_prevalence
)  


# Metrics train -----

eina_de_treball <- eina_de_treball %>% 
  mutate('.metricas_train'= map( .pred_train, 
                                 ~ Metricas_evaluacion(..1, truth = CMVD_rev, estimate = .pred_class) )) %>%
  mutate('.metricas_train'= pmap(
    list(.metricas_train,.pred_train),
    ~ ..1 %>% 
      bind_rows(yardstick::roc_auc(..2,  .pred_1, truth = CMVD_rev, estimator= 'binary'))))

# Metrics test -----

eina_de_treball <- eina_de_treball %>% 
  mutate('.metricas_test'= map(
    .pred_test, 
    ~ Metricas_evaluacion(..1, truth = CMVD_rev, estimate = .pred_class) )) %>%
  mutate('.metricas_test'= pmap(
    list(.metricas_test,.pred_test),
    ~ ..1 %>% 
      bind_rows(yardstick::roc_auc(..2,  .pred_1, truth = CMVD_rev, estimator= 'binary'))))



eina_de_treball

# Metricas 100 modelos ----

# Metricas training ----

eina_de_treball$.metricas_train %>% 
  bind_rows() %>% 
  group_by(.metric ) %>% 
  summarize(
    'value' = mean(.estimate, na.rm = T ) ) %>% 
  pivot_wider(names_from =  .metric, values_from = value  ) %>% 
  mutate(n_models = length(eina_de_treball$id)  ) %>% 
  mutate(Avg_number_obs = eina_de_treball$data_training %>%  map_dbl(~dim(..1)[1]) %>%  mean() ) %>% 
  mutate(Experiment = experiment_label) %>%  
  select(Experiment,n_models, Avg_number_obs, roc_auc, sensitivity, spec, accuracy, everything() ) %>% 
  write_rds(., file = paste0('programacio/outputs/Experiments/',file_output,'/Taula_Metriques_train.rds' ))

# Metricas test ----

eina_de_treball$.metricas_test %>% 
  bind_rows() %>% 
  group_by(.metric ) %>% 
  summarize(
    'value' = mean(.estimate, na.rm = T ) ) %>% 
  pivot_wider(names_from =  .metric, values_from = value  ) %>% 
  mutate(n_models = length(eina_de_treball$id)  ) %>% 
  mutate(Avg_number_obs = eina_de_treball$data_testing %>%  map_dbl(~dim(..1)[1]) %>%  mean() ) %>% 
  mutate(Experiment = experiment_label) %>%  
  select(Experiment,n_models, Avg_number_obs, roc_auc, sensitivity, spec, accuracy, everything() ) %>% 
  write_rds(., file = paste0('programacio/outputs/Experiments/',file_output,'/Taula_Metriques_test.rds' ))

# report experimento ----

# coeficientes del modelo experimental  ----

eina_de_treball$modelo_ajustado %>% 
  map(tidy) %>%  
  bind_rows(.id = 'modelo') %>% 
  mutate(experimento = experiment_label) %>%  
  relocate(experimento, .before= modelo) %>% 
  write_rds(., file = paste0('programacio/outputs/Experiments/',file_output,'/Tibble_Coefs_models.rds' ))


# metricas del experimento ----

eina_de_treball$.metricas_test %>% 
  bind_rows(.id = 'modelo') %>% 
  mutate(experimento = experiment_label) %>%  
  relocate(experimento, .before= modelo)  %>% 
  write_rds(., file = paste0('programacio/outputs/Experiments/',file_output,'/Tibble_plot_Metriques_train.rds' ))

