# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                    Estadística Correlacional 2023
#
#             Plantilla procesamiento trabajo final curso
#
# ******************************************************************************


# Carga Librerías --------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse,   # manipulacion datos
               sjPlot,      # tablas
               confintr,    # IC
               gginference, # visualizacion 
               rempsyc,     # reporte
               broom,       # varios
               sjmisc,      # para descriptivos
               knitr)       # para       

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo


# Carga datos ------------------------------------------------------------------

load("input/data/ELSOC_Long.RData")


# Limpieza de datos ------------------------------------------------------------


## Filtrar y seleccionar -------------------------------------------------------
data <- elsoc_long_2016_2022 %>% 
  filter(ola==1) %>%
  select(sexo=m0_sexo,edad=m0_edad,nedu=m01,
         s11_01,s11_02,s11_03,s11_04,s11_05,s11_06,s11_07,s11_08,s11_09)


## Remover NA's ----------------------------------------------------------------
data <- data %>% 
  set_na(., na = c(-888, -999)) %>% 
  na.omit()


## Crear variable nueva --------------------------------------------------------
data <- data %>% 
  rowwise() %>%
  mutate(sint_depresivos = mean(c(s11_01,s11_02,s11_03,s11_04,s11_05,s11_06,s11_07,s11_08,s11_09))) %>% 
  ungroup()


# Guardar datos ----------------------------------------------------------------
save(data,file="output/data.RData")


#########################################################


library(pacman)
pacman::p_load(tidyverse, # para sintaxis
               ggplot2,  
               rempsyc, # Reporte
               kableExtra, # Tablas
               broom,
               Publish) # Varios
options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpar el entonrno de trabajo


load(url("https://raw.githubusercontent.com/multinivel-facso/trabajo1-grupo-5/main/input/data/Latinobarometro_2023.rdata"))


LatinBase = Latinobarometro_2023_Esp_v1_0 %>% 
  select(idenpa, P16ST, S2, sexo) %>%
  as.data.frame()