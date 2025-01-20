source("programacio/codi/0_importacio/importacio.R")

library(mice)

#mirem quines dades tenen valors perduts
skimr::skim(data_original_amb_classes_retallada)

# tria de mètode de imputació ----

#opció 1: random forest
imputades_per_rf<-mice(
  data=data_original_amb_classes_retallada,
  m=5, 
  maxit=5,
  method="rf", 
  seed=500)
data_imputada_per_rf<-mice::complete(imputades_per_rf)

plot_density_imputation_rf<-densityplot(imputades_per_rf)

png(filename = 'programacio/outputs/descriptiva/distribucio_imputacio_randomforest.png')
print(plot_density_imputation_rf)
dev.off()

# mètode 2: cart
imputades_per_cart<-mice(
  data=data_original_amb_classes_retallada,
  m=5, 
  maxit=5,
  method="cart", 
  seed=500)
data_imputada_per_cart<-mice::complete(imputades_per_cart)

densityplot(imputades_per_cart)

plot_density_imputation_cart<-densityplot(imputades_per_cart)

png(filename = 'programacio/outputs/descriptiva/distribucio_imputacio_cart.png')
print(plot_density_imputation_rf)
dev.off()
#visualitzem la distribució de les variables seleccionades abans i després de la imputació

# mètode final: rf -> provem amb una imputació més forta per solucionar el na restant
data_imputada <- mice(
  data=data_original_amb_classes_retallada, 
  m=5, 
  maxit=25, 
  meth='rf', 
  seed=500)
densityplot(data_imputada)
completed_imputed_data<-complete(data_imputada)
which(is.na(completed_imputed_data)) # hi ha 1 encara

# li trec perquè no és una variable rellevant, és la mateixa que CD4_0 (O BASAL)
# decisió clínica
completed_imputed_data<-completed_imputed_data%>%
  select(-c(mITT,Cd4w0))

#intentem diversos mètodes per reduir variables

# mètode 1:selecció de la formula per correlacions ----

#seleccionem les variables numèriques
data_variables_numeriques<-completed_imputed_data %>%
  select(where(is.numeric))

#creem la matriu de correlacions
matriu_correlacions_data_variables_numeriques <- cor(data_variables_numeriques, use = "complete.obs", method = "pearson")

#seleccionem els noms de les variables correlacionades (llindar 0.8) entre elles
correlacions_majors_a_0.8<-which(abs(matriu_correlacions_data_variables_numeriques)>0.8 , arr.ind=TRUE)
correlacions_majors_a_0.8<-correlacions_majors_a_0.8[correlacions_majors_a_0.8[,1]!=correlacions_majors_a_0.8[,2], ]
variables_correlacionades<-rownames(correlacions_majors_a_0.8)

#seleccionem aquelles variables que no estan correlacionades entre elles
variables_numeriques_no_correlacionades<-data_variables_numeriques%>%
  select(-all_of(variables_correlacionades)) %>%
  names()

#generem un model de regressió logística per les no correlacionades
variables_per_glm<-completed_imputed_data %>%
  select(where(is.factor), all_of(variables_numeriques_no_correlacionades)) %>%
  names()

formula_glm_variables_no_correlacionades<- as.formula(
  paste(
    variables_per_glm[1], 
    "~", 
    paste(variables_per_glm[-1], 
          collapse="+")))

glm(formula=formula_glm_variables_no_correlacionades,
    data=completed_imputed_data,
    family="binomial")

# mal model, mala formula

# mètode 2: selecció de la fórmula per stepwise ----

# creem el model buit
glm_buida<-glm(formula = CMVD_rev~1, completed_imputed_data, family='binomial')

# seleccionem les variables numèriques
variables_numeriques_glm<-completed_imputed_data %>%
  select(where(is.numeric)) %>%
  names()

# creem el model complet
formula_per_glm_completa<-paste("CMVD_rev ~", paste(variables_numeriques_glm, collapse = "+"))
glm_completa<-glm(CMVD_rev~., data=completed_imputed_data, family='binomial')

# apliquem stepwise combinat
model_stepwise<-MASS::stepAIC(glm_buida, 
                     scope=list(lower=glm_buida, upper=glm_completa),
                     direction="both")

summary(model_stepwise)
# mal model -> model massa restrictiu

# mètode 3: selecció de la formula per model lasso ----
library(tidymodels)

# creem la recepta amb la formula i preprocessament de factors
recipe_busqueda_de_formula<-recipes::recipe(completed_imputed_data, 
                                            formula="CMVD_rev~.") %>%
  step_dummy(all_factor_predictors())

# especifiquem el tipus de model
modelo_busqueda_formula<-parsnip::logistic_reg(
  mode="classification", 
  engine="glmnet", 
  mixture=0.25, 
  penalty=0.01)

# sintetitzem
workflow_busqueda_de_formula<-workflows::workflow()%>%
  add_recipe(recipe_busqueda_de_formula) %>%
  add_model(modelo_busqueda_formula)

# generem el model amb la recepta i model especificats
modelo_lasso_busqueda_de_formula<-parsnip::fit(workflow_busqueda_de_formula, completed_imputed_data) 

# extreiem el model
modelo_lasso_busqueda_de_formula %>%
  pull_workflow_fit() %>%
  tidy() %>%
  print(n=150)

# seleccionem variables significatives, fent també selecció clínica
completed_seleccionat<-completed_imputed_data %>%
  as_tibble() %>%
  select(c(CMVD_rev,CMV0Cat4,Edad, BMI, CD_01_02_05_12, CD_01_04_02_30, CD_01_04_02_31, CD_01_04_03_32, CD_01_04_03_33, CD_01_04_04_35, CD_02_05_01_02, CD_03_06_01_02, Leucytes, modo_infeccion, EventoDefiSIDA))

skimr::skim(completed_seleccionat)

#gaurdem la base de dades
#write_rds(completed_seleccionat, file="programacio/dades/1_arreglada/dataset_complet.rds")
