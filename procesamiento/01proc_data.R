# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                       Análisis multinivel 2025
#
#                Cahuil Ortiz, Juan Prado y Angela Valeria
#                     Profesor: Juan Carlos Castillo
#                    Apoyo docente: Kevin Carrasco
#                  Fecha de entrega: 28 de mayo de 2025
#
#                        Plantilla procesamiento
#
# ******************************************************************************


## Carga Librerías --------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, # para sintaxis
               ggplot2,  
               rempsyc, # Reporte
               kableExtra, # Tablas
               broom,
               Publish,
               dplyr,
               car,
               lme4,
               summarytools,
               corrplot,
               stargazer,
               reghelper,
               texreg)

options(scipen = 999) # para desactivar notación científica
rm(list = ls())       # para limpiar el entorno de trabajo

## Carga de datos ------------------------------------------

# Carga datos *SÓLO 1ERA VEZ* (para poder crear las otras, no tocar después
# de crear la base reducida)

load("input/data/Latinobarometro_2023_Esp_Rdata_v1_0.rdata")

# Carga datos base limpia, con el índice y las variables que lo componen
# no tocar después de hacer las recodificaciones

latbar2023 <- readRDS("output/latbar2023.RDS")

## Selección variables de interés ----------------------------------

latbar2023 <- Latinobarometro_2023_Esp_v1_0 %>%
  select(S2, P16ST, P61ST, P32INN, 
         idenpa, P41ST.A, P41ST.B, P41ST.C,
         P41ST.D, P41ST.E, P41ST.F, P41ST.G, 
         P41ST.H, P41ST.I, P41ST.J, P41ST.K,
         P41ST.L, P41ST.M) %>%
  as.data.frame()

# Eliminar datos perdidos------------------------------------------------------

latbar2023 <- latbar2023 %>%
  mutate(across(everything(), ~ replace(., . %in% c(-5, -2, -3, -1, 97), NA)))

latbar2023=na.omit(latbar2023) #Sacar missing data

names(latbar2023)
summary(latbar2023)

# Recodificación valores para índice y construcción del índice

latbar2023 <- mutate(latbar2023, P41ST.A = car::recode(latbar2023$P41ST.A, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.B = car::recode(latbar2023$P41ST.B, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.C = car::recode(latbar2023$P41ST.C, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.D = car::recode(latbar2023$P41ST.D, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.E = car::recode(latbar2023$P41ST.E, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.F = car::recode(latbar2023$P41ST.F, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.G = car::recode(latbar2023$P41ST.G, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.H = car::recode(latbar2023$P41ST.H, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.I = car::recode(latbar2023$P41ST.I, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.J = car::recode(latbar2023$P41ST.J, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.K = car::recode(latbar2023$P41ST.K, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.L = car::recode(latbar2023$P41ST.L, 
                                                       "1=3; 2=2; 3=1; 4=0"))
latbar2023 <- mutate(latbar2023, P41ST.M = car::recode(latbar2023$P41ST.M, 
                                                       "1=3; 2=2; 3=1; 4=0"))

latbar2023 <- mutate(latbar2023, garantias_pais = (P41ST.A + P41ST.B + P41ST.C + 
                                                   P41ST.D + P41ST.E + P41ST.F + 
                                                   P41ST.G + P41ST.H + P41ST.I + 
                                                   P41ST.J + P41ST.K + P41ST.L + 
                                                   P41ST.M)/13)

# Otras recodificaciones

latbar2023 <- mutate(latbar2023, S2 = car::recode(latbar2023$S2, "1=4; 2=3; 3=2;
                                                                  4=1; 5=0"))

latbar2023 <- mutate(latbar2023, P61ST = car::recode(latbar2023$P61ST,
                           "1=0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=6; 8=7; 9=8; 10=9"))

latbar2023 <- mutate(latbar2023, P32INN = car::recode(latbar2023$P32INN,
                                                      "1=2; 2=0; 3=1"))

# Creación base con el índice, excluyendo las variables que lo componen

latbar2023_final <- latbar2023 %>% select(clase_scl = S2, 
                                          orientacion_politica = P16ST,
                                          perc_desigualdad = P61ST, 
                                          perc_liber_pol = P41ST.A, 
                                          perc_migracion = P32INN,
                                          pais = idenpa, 
                                          garantias_pais = garantias_pais)

# Etiquetar valores

latbar2023_final$pais <- factor(latbar2023_final$pais, levels = 
          c(32,68,76,170,188,152,218,222,320,340,484,591,600,604,858,862,214), 
          labels = c("Argentina","Bolivia", "Brasil", "Colombia", "Costa Rica", 
                      "Chile", "Ecuador", "El Salvador", "Guatemala", 
                      "Honduras", "México", "Panamá", "Paraguay", "Perú", 
                      "Uruguay", "Venezuela", "República Dominicana"))

# Creación base agregada por país

agg_latbar2023_final=latbar2023_final %>% group_by(pais) %>% 
  summarise_all(funs(mean)) %>% as.data.frame()

## Guardar datos --------------------------------------------------------------

save(data,file="output/data.RData")
saveRDS(latbar2023, file = "output/latbar2023.RDS")
saveRDS(latbar2023_final, file = "output/latbar2023_final.RDS")
saveRDS(agg_latbar2023_final, file = "output/agg_latbar2023_final.RDS")


