library(tidyverse)
# Llista d'arxius en el directori
archivos_paths <- list.files(path = 'programacio/outputs/Experiments/') %>%  as.list()

# guardem en una taula els coeficients dels experiments

archivos_paths %>% 
  map(~read_rds(paste0('programacio/outputs/Experiments/',..1,'/Tibble_plot_Metriques_train.rds'))) %>%  
  bind_rows() %>% 
  arrange(experimento) %>% 
  mutate(order = case_when(
    experimento %in% c("10_SMOTE_n5", "11_DownSMOTE_2_n2", "12_DownSMOTE_2_n5") ~ 2, 
    TRUE ~ 1 
  )) %>% 
  arrange(order, experimento) %>%  # ordenem 'order', i 'experimento'
  mutate(experimento = factor(experimento, levels = unique(experimento))) %>% # convertim a factor
  select(-order) %>% 
  ggplot(aes(x = .metric, y = .estimate, fill = .metric)) + # Añadimos fill = .metric
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~experimento) +
  # Afegim línies horitzontals en 0.5 y 0.75
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "purple", size = 1) + # Línia en 0.5
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "magenta", size = 1) + # Línia en 0.75
  scale_fill_manual(values = c(
    "sensitivity" = "skyblue",  # Color sensitivity
    "roc_auc" = "orange"        # Color roc_auc
  ), na.translate = FALSE) + 
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename= 'Metricas_experimentos.png',
  plot = last_plot(),
  path = 'programacio/outputs/Finals/')



