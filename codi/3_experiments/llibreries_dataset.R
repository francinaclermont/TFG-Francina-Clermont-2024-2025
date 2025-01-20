
library(readr)
library(tidymodels)
library(tidyverse)
library(themis)
library(probably)

# Importació del dataset -----

data_set_a_modelar<-read_rds(file="programacio/dades/1_arreglada/dataset_complet.rds") 


# Canviem l'ordre del factor CMVD_rev ----
data_set_a_modelar <- data_set_a_modelar %>%
  mutate(CMVD_rev = factor(CMVD_rev, levels = c("1", "0")))

# Re-neteja de variables -----
# reduïm als factors de >2 nivells a 2

data_set_a_modelar<-data_set_a_modelar %>%
  mutate(
    CMV0Cat4=factor(
      case_when(
        CMV0Cat4%in%c("0. <=35", "1. >35 to <=500")~"<500", 
        CMV0Cat4=="2. >500"~">500"),
      levels=c("<500",">500"))) %>%
  rename("CMV0"="CMV0Cat4") %>%
  mutate(
    modo_infeccion=factor(
      case_when(
        modo_infeccion=="HMSX"~"HMSX", 
        TRUE~"no_HMSX"), 
      levels=c("no_HMSX","HMSX")))
