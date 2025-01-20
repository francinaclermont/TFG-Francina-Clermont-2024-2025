library(tidyverse)
# Lista todos los archivos en el directorio
archivos_paths <- list.files(path = 'programacio/outputs/Experiments/') %>%  as.list()


archivos_paths %>% 
  map(~read_rds(paste0('programacio/outputs/Experiments/',..1,'/Tibble_plot_Metriques_train.rds'))) %>%  
  bind_rows() %>% 
  arrange(experimento) %>% 
  mutate(order = case_when(
    experimento %in% c("10_SMOTE_n5", "11_DownSMOTE_2_n2", "12_DownSMOTE_2_n5") ~ 2, # Asignar prioridad alta (2)
    TRUE ~ 1 # Prioridad baja para el resto (1)
  )) %>% 
  arrange(order, experimento) %>%  # Ordenar primero por 'order', luego por 'experimento'
  mutate(experimento = factor(experimento, levels = unique(experimento))) %>% # Convertir a factor con niveles en el orden actual
  select(-order) %>% 
  ggplot(aes(x = .metric, y = .estimate, fill = .metric)) + # Añadimos fill = .metric
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~experimento) +
  # Agregar líneas horizontales para 0.5 y 0.75
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "purple", size = 1) + # Línea en 0.5
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "magenta", size = 1) + # Línea en 0.75
  scale_fill_manual(values = c(
    "sensitivity" = "skyblue",  # Color para sensitivity
    "roc_auc" = "orange"        # Otros niveles no especificados se quedarán sin color
  ), na.translate = FALSE) + # Evita mostrar niveles sin color en la leyenda
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename= 'Metricas_experimentos.png',
  plot = last_plot(),
  path = 'programacio/outputs/Finals/')



