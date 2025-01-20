library(sjlabelled)
library(dplyr)
library(haven)
library(tableone)
library(rsample)
library(tidyverse)
library(purrr)
library(recipes)
library(tibble)
library(parsnip)
library(yardstick)
library(forestplot)

# $$$$$$$$$$$$$$$$
# BASES DE DADES + DICCIONARI ----
# $$$$$$$$$$$$$$$$

# carreguem la base de dades amb els seus formats
data_original<-haven::read_sas(
  data_file ='programacio/dades/1_arreglada/baseline.sas7bdat', 
  catalog_file = 'programacio/dades/1_arreglada/formats.sas7bdat')

# creem en un tibble els noms que tenim a la base de dades que utilitzarem (escurçats) i els noms extensos
diccionari_noms <- tibble::tibble( 
  Nombres_originales = data_original %>%  names(),
  Nombres_alterados = data_original %>% sjlabelled::label_to_colnames() %>% names()
) 

# base de dades amb noms complets
data_original_fullnames<-haven::read_sas(
  data_file ='programacio/dades/1_arreglada/baseline.sas7bdat', 
  catalog_file = 'programacio/dades/1_arreglada/formats.sas7bcat') %>% sjlabelled::label_to_colnames()

# seleccionem les variables tipus factor
variables_factor<-data_original %>%
  select(CMVD_rev, ReconsImm, mITT, PP, Treat, Sexo, modo_infeccion, OrigenEtnico, AntecEnfHepatica, EnfermHepatica_activa,
         AntecEnfRenal, OtrasEnfer, DiagSIDA, EventoDefiSIDA, VirologicalFailure, IgG_anti_CMV, IgM_anti_CMV, CMV0le500,
         CMV0le1000, CMV0Pos, CMV0Cat, CMV0Cat2, CMV0Cat3, CMV0Cat4, CMV0Cat5, IRIS, CMVNegative, IRIS_rev, CMVPosIsolated_rev,
         PrematureFinalisation, Reason, PatCode) %>% names()

# les transformem a factor
data_original_amb_classes<-data_original%>%
  mutate(across(all_of(variables_factor), as.factor))

# eliminem aquelles variables que sabem que no seran útils per l'estudi
# son les variables que depenen de que cmvd_rev sigui positiu i les temporals
data_original_amb_classes_retallada<-data_original_amb_classes%>%
  select(-c(CMVD_revTm, PatCode, IrisTm, CmvEventAnyTm, OnStudyTime_, 
            PatNo, SidaTime, FollowUpTime, ResistenceTime, OnStudyTime))
