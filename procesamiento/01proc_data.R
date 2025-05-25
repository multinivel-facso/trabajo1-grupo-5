# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                       Análisis multinivel 2025
#
#                         Plantilla procesamiento
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
               haven) # Varios
options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpiar el entorno de trabajo


## Carga datos SÓLO 1ERA VEZ (para subir la base original al github)------------------------------------------------------------------

Latinobarometro_2023 <- read_sav("~/Downloads/bases/latbar2023/Latinobarometro_2023_Esp_Spss_v1_0.sav")
saveRDS(Latinobarometro_2023, file = "input/Latinobarometro_2023.RDS")
## Carga de datos para trabajar---------------------------------------------

# Base original (sólo antes de las modifcaciones)
Latinobarometro_2023 <- readRDS("input/Latinobarometro_2023.RDS")

# Base con modificaciones (en adelante)
latbar2023 <- readRDS("output/latbar2023.RDS")

## Limpieza de datos ------------------------------------------------------------


# BORRAR ESTO CUANDO ANOTEMOS EL CÓDIGO --> para la limpieza de datos en adelante, llamémosla latbar2023
# para que la Latinobarometro_2023 sea la base original, sin filtrar ni nada

latbar2023

## Filtrar y seleccionar -------------------------------------------------------
latbar2023 = latbar2023 %>% 
  select(idenpa, P16ST, S2, sexo) %>%
  as.data.frame()


## Remover NA's ----------------------------------------------------------------



## Recodificación variables --------------------------------------------------------

latbar2023$idenpa <- factor(latbar2023$idenpa, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), labels = c("Argentina","Bolivia", "Brasil", "Colombia", "Costa Rica", "Chile", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Perú", "Uruguay", "Venezuela", "España", "República Dominicana"))


## Estadísticos descriptivos ----------------------------------------------------


## Correlación intraclase, efectos aleatorios, modelos preliminares --------------------



# Guardar datos ----------------------------------------------------------------

save(data,file="output/data.RData")
saveRDS(latbar2023, file = "output/latbar2023.RDS")
#########################################################










