# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                       Análisis multinivel 2025
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
               haven,
               lme4,
               corrplot,
               stargazer,
               texreg)
            
options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpiar el entorno de trabajo


## Carga datos SÓLO 1ERA VEZ (para subir la base original al github)------------------------------------------------------------------

load("input/data/Latinobarometro_2023_Esp_Rdata_v1_0.rdata")

## Filtrar y recodificaciones --------------------------------------------------

latbar2023 <- Latinobarometro_2023_Esp_v1_0 %>%
  select(sexo, S2, edad, P16ST,
         P11STGBS.B, P61ST, P41ST.A, P32INN, idenpa) %>%
  as.data.frame()

latbar2023 <- latbar2023 %>% select(clase_scl = S2, tend_politica = P16ST, perc_economia = P11STGBS.B,
              perc_desigualdad = P61ST, perc_libertad = P41ST.A, perc_migracion = P32INN,
              pais = idenpa, edad = edad, sexo = sexo)


## Limpieza de datos ------------------------------------------------------------

# Remover NA

latbar2023 <- latbar2023 %>%
  filter(if_all(everything(), ~ . != -5))

# Etiquetar valores

latbar2023$pais <- factor(latbar2023$pais, levels = c(32,68,76,170,188,152,218,222,320,340,484,591,600,604,858,862,214), labels = c("Argentina","Bolivia", "Brasil", "Colombia", "Costa Rica", "Chile", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Panamá", "Paraguay", "Perú", "Uruguay", "Venezuela", "República Dominicana"))


## Descriptivos generales ----------------------------------------------------

stargazer(latbar2023, title = "Descriptivos generales", type='text')

## Estimación correlación intraclase

# Null model

results_0 = lmer(tend_politica ~ 1 + (1 | pais), data = latbar2023)
summary(results_0)

screenreg(results_0) # de library texreg

# Los componentes del cálculo son: 

30.5/(30.5+582.9)

# Resultado -> 0.04972286 correlación intra clase de 5%

# Guardar base de datos limpia en el output

# se me olvido qué codigo se usaba para eso
