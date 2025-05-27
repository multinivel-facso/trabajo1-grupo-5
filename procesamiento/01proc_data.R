# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                       Análisis multinivel 2025
#
#                Cahuil Ortiz, Juan Prado y Ángela Valeria
#                     Profesor: Juan Carlos Castillo
#                         Apoyo docente: Kevin
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
            
options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpiar el entorno de trabajo

## Carga de datos ------------------------------------------

# Carga datos SÓLO 1ERA VEZ (para subir la base original al github)

load("input/data/Latinobarometro_2023_Esp_Rdata_v1_0.rdata")

# Carga datos base limpia, con los índices y las variables que lo componen 

latbar2023 <- readRDS("output/latbar2023.RDS")

# Carga datos base limpia, sólo con los índices, no con los que lo componen****

latbar2023_final <- readRDS("output/latbar2023_final.RDS")

# Carga datos base agregada****

latbar2023_final_agg <- readRDS("output/latbar2023_final_agg.RDS")

# Eliminar datos perdidos------------------------------------------------------

latbar2023 <- latbar2023 %>%
  mutate(across(everything(), ~ replace(., . %in% c(-5, -2, -3, -1, 97), NA)))

latbar2023_final=na.omit(latbar2023) #Sacar missing data
names(latbar2023_final)
summary(latbar2023_final)
