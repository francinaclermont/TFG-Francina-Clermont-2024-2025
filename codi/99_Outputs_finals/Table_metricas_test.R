library(tidyverse)
# Lista todos los archivos en el directorio
archivos_paths <- list.files(path = 'programacio/outputs/Experiments/') %>%  as.list()


archivos_paths %>% 
  map(~read_rds(paste0('programacio/outputs/Experiments/',..1,'/Taula_Metriques_test.rds') )) %>%  
  bind_rows() %>% 
  arrange(Experiment) %>% 
  mutate(order = case_when(
    Experiment %in% c("10_SMOTE_n5", "11_DownSMOTE_2_n2", "12_DownSMOTE_2_n5") ~ 2, # Asignar prioridad alta (2)
    TRUE ~ 1 # Prioridad baja para el resto (1)
  )) %>% 
  arrange(order, Experiment) %>%  # Ordenar primero por 'order', luego por 'Experiment'
  select(-order, -Avg_number_obs) %>%  
  writexl::write_xlsx(path='programacio/outputs/Finals/Experiments_Metriques_test.xlsx')