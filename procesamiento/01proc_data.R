# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                       Análisis multinivel 2025
#
#                         Plantilla procesamiento
#
# ******************************************************************************


# Carga Librerías --------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, # para sintaxis
               ggplot2,  
               rempsyc, # Reporte
               kableExtra, # Tablas
               broom,
               Publish) # Varios
options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpiar el entorno de trabajo


# Carga datos ------------------------------------------------------------------

load(url("https://raw.githubusercontent.com/multinivel-facso/trabajo1-grupo-5/main/input/data/Latinobarometro_2023.rdata"))


# Limpieza de datos ------------------------------------------------------------


## Filtrar y seleccionar -------------------------------------------------------
LatinBase = Latinobarometro_2023_Esp_v1_0 %>% 
  select(idenpa, P16ST, S2, sexo) %>%
  as.data.frame()


## Remover NA's ----------------------------------------------------------------



## Recodificación variables --------------------------------------------------------

LatinBase$idenpa <- factor(LatinBase$idenpa, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), labels = c("Argentina","Bolivia", "Brasil", "Colombia", "Costa Rica", "Chile", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Perú", "Uruguay", "Venezuela", "España", "República Dominicana"))


# Guardar datos ----------------------------------------------------------------
save(data,file="output/LatinBase.RData")


#########################################################










