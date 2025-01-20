library(tidyverse)
# Llista d'arxius en el directori
archivos_paths <- list.files(path = 'programacio/outputs/Experiments/') %>%  as.list()

# creem taula amb els coeficients dels models generats en els experiments

archivos_paths %>% 
  map(~read_rds(paste0('programacio/outputs/Experiments/', ..1, '/Tibble_Coefs_models.rds'))) %>%  
  bind_rows() %>% 
  arrange(experimento) %>% 
  mutate(order = case_when(
    experimento %in% c("10_SMOTE_n5", "11_DownSMOTE_2_n2", "12_DownSMOTE_2_n5") ~ 2,
    TRUE ~ 1
  )) %>% 
  arrange(order, experimento) %>%  # ordenem 'order',i 'experimento'
  mutate(experimento = factor(experimento, levels = unique(experimento))) %>% convertim a factor
  select(-order, -statistic) %>%
  
  group_by(experimento, term) %>% 
  summarize(across(estimate:p.value, ~mean(., na.rm = TRUE))) %>%
  pivot_longer(estimate:p.value, names_to = 'metric', values_to = 'value') %>% 
  pivot_wider(names_from = 'term', values_from = 'value') %>%
  as.data.frame() %>% 
  
  # `format()` evita notació científica i limita a 2 decimals
  mutate(across(where(is.numeric), ~ format(., digits = 2, nsmall = 0, scientific = FALSE))) %>% 
  arrange(metric) %>% 
  writexl::write_xlsx(.,'programacio/outputs/Finals/Reporting_coefs.xlsx')



archivos_paths %>% 
  map(~read_rds(paste0('programacio/outputs/Experiments/', ..1, '/Tibble_Coefs_models.rds'))) %>%  
  bind_rows() %>% 
  arrange(experimento) %>% 
  mutate(order = case_when(
    experimento %in% c("10_SMOTE_n5", "11_DownSMOTE_2_n2", "12_DownSMOTE_2_n5") ~ 2, 
    TRUE ~ 1 
  )) %>% 
  arrange(order, experimento) %>% 
  mutate(experimento = factor(experimento, levels = unique(experimento))) %>% 
  select(-order, -statistic) %>%
  
  group_by(experimento, term) %>% 
  summarize(across(estimate:p.value, ~mean(., na.rm = TRUE))) %>%  print(n=500) %>% 
  writexl::write_xlsx('programacio/outputs/Finals/Reporting_coefs_2.xlsx')


  
  
  
