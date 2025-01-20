library(tidyverse)
# Lista todos los archivos en el directorio
archivos_paths <- list.files(path = 'programacio/outputs/Experiments/') %>%  as.list()



archivos_paths %>% 
  map(~read_rds(paste0('programacio/outputs/Experiments/', ..1, '/Tibble_Coefs_models.rds'))) %>%  
  bind_rows() %>% 
  arrange(experimento) %>% 
  mutate(order = case_when(
    experimento %in% c("10_SMOTE_n5", "11_DownSMOTE_2_n2", "12_DownSMOTE_2_n5") ~ 2, # Asignar prioridad alta (2)
    TRUE ~ 1 # Prioridad baja para el resto (1)
  )) %>% 
  arrange(order, experimento) %>%  # Ordenar primero por 'order', luego por 'experimento'
  mutate(experimento = factor(experimento, levels = unique(experimento))) %>% # Convertir a factor con niveles en el orden actual
  select(-order, -statistic) %>%
  
  group_by(experimento, term) %>% 
  summarize(across(estimate:p.value, ~mean(., na.rm = TRUE))) %>%
  pivot_longer(estimate:p.value, names_to = 'metric', values_to = 'value') %>% 
  pivot_wider(names_from = 'term', values_from = 'value') %>%
  as.data.frame() %>% 
  
  # Usar `format()` para evitar notación científica y limitar a 2 decimales
  mutate(across(where(is.numeric), ~ format(., digits = 2, nsmall = 0, scientific = FALSE))) %>% 
  arrange(metric) %>% 
  writexl::write_xlsx(.,'programacio/outputs/Finals/Reporting_coefs.xlsx')



archivos_paths %>% 
  map(~read_rds(paste0('programacio/outputs/Experiments/', ..1, '/Tibble_Coefs_models.rds'))) %>%  
  bind_rows() %>% 
  arrange(experimento) %>% 
  mutate(order = case_when(
    experimento %in% c("10_SMOTE_n5", "11_DownSMOTE_2_n2", "12_DownSMOTE_2_n5") ~ 2, # Asignar prioridad alta (2)
    TRUE ~ 1 # Prioridad baja para el resto (1)
  )) %>% 
  arrange(order, experimento) %>%  # Ordenar primero por 'order', luego por 'experimento'
  mutate(experimento = factor(experimento, levels = unique(experimento))) %>% # Convertir a factor con niveles en el orden actual
  select(-order, -statistic) %>%
  
  group_by(experimento, term) %>% 
  summarize(across(estimate:p.value, ~mean(., na.rm = TRUE))) %>%  print(n=500) %>% 
  writexl::write_xlsx('programacio/outputs/Finals/Reporting_coefs_2.xlsx')

  
  
  
  
  