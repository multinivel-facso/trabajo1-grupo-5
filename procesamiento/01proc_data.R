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
               car,
               lme4,
               summarytools,
               corrplot,
               stargazer,
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

## Filtrar y recodificaciones --------------------------------------------------

# Filtrar base original

latbar2023 <- Latinobarometro_2023_Esp_v1_0 %>%
  select(sexo, S2, edad, P16ST,
         P11STGBS.B, P61ST, P32INN, idenpa, P41ST.A, P41ST.B, P41ST.C,
         P41ST.D, P41ST.E, P41ST.F, P41ST.G, P41ST.H, P41ST.I, P41ST.J, P41ST.K,
         P41ST.L, P41ST.M) %>%
  as.data.frame()

# Remover NA

latbar2023 <- latbar2023 %>%
  filter(if_all(everything(), ~ . != -5))

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
                                     P41ST.D + P41ST.E + P41ST.F + P41ST.G + 
                                     P41ST.H + P41ST.I + P41ST.J + P41ST.K + 
                                     P41ST.L + P41ST.M)/13)

# Otras recodificaciones

latbar2023 <- mutate(latbar2023, sexo = car::recode(latbar2023$sexo, 
                                                    "1=0; 2=1"))

latbar2023 <- mutate(latbar2023, S2 = car::recode(latbar2023$S2, "1=4; 2=3; 3=2;
                                                  4=1; 5=0; -1=4; -2=3"))

latbar2023 <- mutate(latbar2023, P16ST = car::recode(latbar2023$P16ST, "97=NA; 
                                                     -1=1; -2=2"))

latbar2023 <- mutate(latbar2023, P11STGBS.B = car::recode(latbar2023$P11STGBS.B,
                                            "1=3; 2=2; 3=1; 4=0; -1=3; -2=2"))

latbar2023 <- mutate(latbar2023, P61ST = car::recode(latbar2023$P61ST,
              "1=0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=6; 8=7; 9=8; 10=9; -1=0; -2=1"))

latbar2023 <- mutate(latbar2023, P32INN = car::recode(latbar2023$P32INN,
                                                     "1=2; 2=0; 3=1"))

# Creación base con el índice,
# excluyendo las variables que lo componen

latbar2023_final <- latbar2023 %>% select(clase_scl = S2, tend_politica = P16ST,
              perc_economia = P11STGBS.B, perc_desigualdad = P61ST, 
              perc_libertad = P41ST.A, perc_migracion = P32INN,
              pais = idenpa, edad = edad, sexo = sexo, 
              garantias_pais = garantias_pais)

# Etiquetar valores

latbar2023_final$pais <- factor(latbar2023_final$pais, levels = 
          c(32,68,76,170,188,152,218,222,320,340,484,591,600,604,858,862,214), 
          labels = c("Argentina","Bolivia", "Brasil", "Colombia", "Costa Rica", 
                     "Chile", "Ecuador", "El Salvador", "Guatemala", "Honduras",
                     "México", "Panamá", "Paraguay", "Perú", "Uruguay", 
                     "Venezuela", "República Dominicana"))

# Creacion variable promedio de garantías país

latbar2023_final_agg=latbar2023_final %>% group_by(pais) %>% 
  summarise_all(funs(mean)) %>% as.data.frame()

stargazer(latbar2023_final_agg, type = "text")

## Descriptivos generales ----------------------------------------------------

stargazer(latbar2023_final, title = "Descriptivos generales", type='text')

# Revisión estadísticos descriptivos cada variable

freq(latbar2023_final$sexo)

freq(latbar2023_final$clase_scl)

descr(latbar2023_final$edad, stats = 
        c("min", "med", "max", "mean", "sd"), transpose = T)
freq(latbar2023_final$edad)

descr(latbar2023_final$tend_politica, stats = 
        c("min", "q1", "med", "q3", "max", "mean", "sd"), transpose = T)
freq(latbar2023_final$tend_politica)

descr(latbar2023_final$perc_economia, stats = 
        c("min", "med", "max", "mean", "sd"), transpose = T)
freq(latbar2023_final$perc_economia)

descr(latbar2023_final$perc_desigualdad, stats = 
        c("min", "med", "max", "mean", "sd"), transpose = T)
freq(latbar2023_final$perc_desigualdad)

freq(latbar2023_final$perc_migracion)

freq(latbar2023_final$pais)

descr(latbar2023_final$perc_libertad, stats = 
        c("min", "med", "max", "mean", "sd"), transpose = T)
freq(latbar2023_final$perc_libertad)

descr(latbar2023_final$garantias_pais, stats = 
        c("min", "med", "max", "mean", "sd"), transpose = T)

## Estimación correlación intraclase---------------------------------------

# Null model

results_0 = lmer(tend_politica ~ 1 + (1 | pais), data = latbar2023)
summary(results_0)

screenreg(results_0) # de library texreg

# Los componentes del cálculo son: 

30.5/(30.5+582.9)

# Resultado -> 0.04972286 correlación intra clase de 5%


## Guardar datos --------------------------------------------------------------

save(data,file="output/data.RData")
saveRDS(latbar2023, file = "output/latbar2023.RDS")
saveRDS(latbar2023_final, file = "output/latbar2023_final.RDS")
saveRDS(latbar2023_final_agg, file = "output/latbar2023_final_agg.RDS")
