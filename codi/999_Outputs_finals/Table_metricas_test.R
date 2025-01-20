library(tidyverse)
# Llista d'arxius en el directori
archivos_paths <- list.files(path = 'programacio/outputs/Experiments/') %>%  as.list()

# mètriques de tots els experiments del dataset test

archivos_paths %>% 
  map(~read_rds(paste0('programacio/outputs/Experiments/',..1,'/Taula_Metriques_test.rds') )) %>%  
  bind_rows() %>% 
  arrange(Experiment) %>% 
  mutate(order = case_when(
    Experiment %in% c("10_SMOTE_n5", "11_DownSMOTE_2_n2", "12_DownSMOTE_2_n5") ~ 2,
    TRUE ~ 1 
  )) %>% 
  arrange(order, Experiment) %>%  
  select(-order, -Avg_number_obs) %>%  
  writexl::write_xlsx(path='programacio/outputs/Finals/Experiments_Metriques_test.xlsx')
