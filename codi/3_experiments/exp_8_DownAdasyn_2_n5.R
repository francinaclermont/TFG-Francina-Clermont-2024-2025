source("programacio/codi/3_experiements/llibreries_dataset.R")

experiment_label <- '8_DownAdasyn_2_n5'
file_output <- 'exp_8_DownAdasyn_2_n5'

skimr::skim(data_set_a_modelar)

# Creem 1000 bootstraps estratificats a partir de las variables originals ----
# generació dels datasets de training y test 

n_boot <- 1000

mostres_bootsrap <- rsample::bootstraps(data_set_a_modelar, times = n_boot, strata = CMVD_rev) %>%
  mutate(data_training = map(splits, ~ training(..1))) %>%
  mutate(data_testing = map(splits, ~ testing(..1)))

# Creem recepta ----

# Passos de preprocessament, downsampling (majoritària duplicada) i adasyn (5 veïns)

eina_de_treball <- mostres_bootsrap %>%
  mutate(receta = map(data_training, 
                      ~ recipes::recipe(CMVD_rev ~ ., data = ..1) %>%
                        recipes::step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
                        themis::step_downsample(CMVD_rev, under_ratio = 2, skip = T) %>% 
                        themis::step_adasyn(CMVD_rev, neighbors = 5, over_ratio = 1, skip = T)
                      
  ))


# Cuinat deceptes ----
# Actualització de test i train 

## Dataset train ----
safe_prep_bake_train <- safely(
  function(receta) { receta %>% prep() %>%bake(new_data = NULL)},
  otherwise="NO"
)

eina_de_treball <- eina_de_treball %>%  
  mutate(data_training = map(receta, ~safe_prep_bake_train(..1) ))


## Dataset test  ----
safe_prep_bake_test <- safely(
  function(receta, dataset) {receta %>% prep() %>% bake(new_data = dataset) },
  otherwise="NO"
)

eina_de_treball <- eina_de_treball %>%  
  mutate(data_testing = map2(receta,data_testing, ~safe_prep_bake_test(..1,..2) ) )

# Filtrem models no convergits ----
# Eliminem tots els datasets que no han convergit durant el procés de recepta.

## Filtrem train ----
eina_de_treball <- eina_de_treball %>%
  filter( map_lgl(data_training, ~ is.null(.x$error)==TRUE)) %>%
  mutate(data_training= map(data_training, "result")) 

## Filtrem test ----
eina_de_treball <- eina_de_treball %>%
  filter(map_lgl(data_testing, ~ is.null(.x$error)==TRUE)) %>%
  mutate(data_testing= map(data_testing, "result")) 

# Ajustem model-----
# model de regressió lineal a partir de model complet

eina_de_treball <- eina_de_treball %>% 
  mutate( modelo_ajustado = map( data_training, ~ glm(formula = 'CMVD_rev ~ .', data= ..1, family = 'binomial' )))

# Prediccions ----

## Creem prediccions training ----
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

## Creem prediccions testing ----
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


# Mètriques dels models ----

# Funció mètriques -----
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

# Mètriques train -----

eina_de_treball <- eina_de_treball %>% 
  mutate('.metricas_train'= map( .pred_train, 
                                 ~ Metricas_evaluacion(..1, truth = CMVD_rev, estimate = .pred_class) )) %>%
  mutate('.metricas_train'= pmap(
    list(.metricas_train,.pred_train),
    ~ ..1 %>% 
      bind_rows(yardstick::roc_auc(..2,  .pred_1, truth = CMVD_rev, estimator= 'binary'))))

# Mètriques test -----

eina_de_treball <- eina_de_treball %>% 
  mutate('.metricas_test'= map(
    .pred_test, 
    ~ Metricas_evaluacion(..1, truth = CMVD_rev, estimate = .pred_class) )) %>%
  mutate('.metricas_test'= pmap(
    list(.metricas_test,.pred_test),
    ~ ..1 %>% 
      bind_rows(yardstick::roc_auc(..2,  .pred_1, truth = CMVD_rev, estimator= 'binary'))))



eina_de_treball

# Guardem mètriques training ----

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

# Guardem mètriques test ----

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

# guardem els coeficients del model  ----

eina_de_treball$modelo_ajustado %>% 
  map(tidy) %>%  
  bind_rows(.id = 'modelo') %>% 
  mutate(experimento = experiment_label) %>%  
  relocate(experimento, .before= modelo) %>% 
  write_rds(., file = paste0('programacio/outputs/Experiments/',file_output,'/Tibble_Coefs_models.rds' ))


# l'afegim al model ----

eina_de_treball$.metricas_test %>% 
  bind_rows(.id = 'modelo') %>% 
  mutate(experimento = experiment_label) %>%  
  relocate(experimento, .before= modelo)  %>% 
  write_rds(., file = paste0('programacio/outputs/Experiments/',file_output,'/Tibble_plot_Metriques_train.rds' ))

