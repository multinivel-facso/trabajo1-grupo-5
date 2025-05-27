
## Exploración y descripción--------------------------------------------------

dim(latbar2023) # dimensiones de base de datos

names(latbar2023) # Muestra los nombres de las variables en la base

# Selección variables de interés

latbar2023 <- Latinobarometro_2023_Esp_v1_0 %>%
  select(S2, edad, P16ST,
         P11STGBS.B, P61ST, P32INN, idenpa,P41ST.A, P41ST.B, P41ST.C,
         P41ST.D, P41ST.E, P41ST.F, P41ST.G, P41ST.H, P41ST.I, P41ST.J, P41ST.K,
         P41ST.L, P41ST.M) %>%
  as.data.frame()

dim(latbar2023) # dimensiones

head(latbar2023) # primeros 10 casos cada variable seleccionada

summary(latbar2023) # Descriptivos generales (evaluación de datos perdidos)

# Tabla descriptiva con stargazer

stargazer(latbar2023, title = "Descriptivos generales", type='html')

# Recodificación valores para índice y construcción del índice

latbar2023_final <- mutate(latbar2023_final, P41ST.A = car::recode(latbar2023_final$P41ST.A, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.B = car::recode(latbar2023_final$P41ST.B, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.C = car::recode(latbar2023_final$P41ST.C, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.D = car::recode(latbar2023_final$P41ST.D, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.E = car::recode(latbar2023_final$P41ST.E, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.F = car::recode(latbar2023_final$P41ST.F, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.G = car::recode(latbar2023_final$P41ST.G, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.H = car::recode(latbar2023_final$P41ST.H, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.I = car::recode(latbar2023_final$P41ST.I, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.J = car::recode(latbar2023_final$P41ST.J, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.K = car::recode(latbar2023_final$P41ST.K, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.L = car::recode(latbar2023_final$P41ST.L, 
                                                                   "1=3; 2=2; 3=1; 4=0"))
latbar2023_final <- mutate(latbar2023_final, P41ST.M = car::recode(latbar2023_final$P41ST.M, 
                                                                   "1=3; 2=2; 3=1; 4=0"))

latbar2023_final <- mutate(latbar2023_final, garantias_pais = (P41ST.A + P41ST.B + P41ST.C + 
                                                                 P41ST.D + P41ST.E + P41ST.F + P41ST.G + 
                                                                 P41ST.H + P41ST.I + P41ST.J + P41ST.K + 
                                                                 P41ST.L + P41ST.M)/13)

# Otras recodificaciones

latbar2023_final <- mutate(latbar2023_final, S2 = car::recode(latbar2023_final$S2, "1=4; 2=3; 3=2;
                                                  4=1; 5=0; -1=4; -2=3"))

latbar2023_final <- mutate(latbar2023_final, P16ST = car::recode(latbar2023_final$P16ST, "-1=1; -2=2"))

latbar2023_final <- mutate(latbar2023_final, P11STGBS.B = car::recode(latbar2023_final$P11STGBS.B,
                                                                      "1=3; 2=2; 3=1; 4=0; -1=3; -2=2"))

latbar2023_final <- mutate(latbar2023_final, P61ST = car::recode(latbar2023_final$P61ST,
                                                                 "1=0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=6; 8=7; 9=8; 10=9; -1=0; -2=1"))

latbar2023_final <- mutate(latbar2023_final, P32INN = car::recode(latbar2023_final$P32INN,
                                                                  "1=2; 2=0; 3=1"))

# Creación base con el índice,
# excluyendo las variables que lo componen

latbar2023_final <- latbar2023_final %>% select(clase_scl = S2, tend_politica = P16ST,
                                                perc_economia = P11STGBS.B, perc_desigualdad = P61ST, 
                                                perc_libertad = P41ST.A, perc_migracion = P32INN,
                                                pais = idenpa, edad = edad, 
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

## Descriptivos generales nueva base ----------------------------------------------------

stargazer(latbar2023_final, title = "Descriptivos generales", type='html')

# Revisión estadísticos descriptivos cada variable

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

freq(latbar2023_final$pais)

descr(latbar2023_final$perc_libertad, stats = 
        c("min", "med", "max", "mean", "sd"), transpose = T)
freq(latbar2023_final$perc_libertad)

descr(latbar2023_final$garantias_pais, stats = 
        c("min", "med", "max", "mean", "sd"), transpose = T)

## Estimación correlación intraclase---------------------------------------

# Null model

resultados_0 = lmer(tend_politica ~ 1 + (1 | pais), data = latbar2023_final)
summary(resultados_0)

screenreg(resultados_0) # de library texreg

reghelper::ICC(resulresultados_0)

# Los componentes del cálculo son: 

0.31/(0.31+8.88)

# Resultado -> 0.03373232 correlación intra clase de 0.34%

## Modelos multinivel-----------------------------------------------------------

# Modelos de nivel 1

# Edad en relación a tendencia política

resultados_1 = lmer(tend_politica ~ 1 + edad + (1 | pais), data = latbar2023_final)
screenreg(resultados_1, naive=TRUE)

screenreg(resultados_1)

0.31/(0.31+9.30) = 0.03225806

# Clase social en relación a tendencia política

resultados_2 = lmer(tend_politica ~ 1 + clase_scl + (1 | pais), data = latbar2023_final)
screenreg(resultados_2, naive=TRUE)

screenreg(resultados_2)

0.32/(0.31+9.34) = 0.03316062

# Modelos de nivel 2

# Generar promedio clase social

promedio_clase <- mean(latbar2023_final$clase_scl, na.rm = TRUE)
latbar2023_final$promedio_clase <- promedio_clase

# Clase social con percepción economía y relación con tendencia política

resultados_3 = lmer(tend_politica ~ 1 + promedio_clase + perc_economia + (1 | pais), data = latbar2023_final)
screenreg(resultados_3)

screenreg(resultados_3)

0.31/(0.31+9.30) = 0.03225806

# Libertad individual con percepción desigualdad y relación con tendencia política

resultados_4 = lmer(tend_politica ~ 1 + perc_libertad + perc_desigualdad + (1 | pais), data = latbar2023_final)
screenreg(resultados_4)

screenreg(resultados_4)

0.21/(0.21+9.03) = 0.02272727

## Comparación individual, agregado y multinivel--------------------------------

# Regersión comparación

reg_ind=lm(tend_politica ~ edad + clase_scl + perc_economia + perc_libertad + perc_desigualdad, data=latbar2023_final)
agg_latbar2023_final=latbar2023_final %>% group_by(pais) %>% summarise_all(funs(mean))

reg_agg=lm(tend_politica ~ edad + clase_scl + perc_economia + perc_libertad + perc_desigualdad, data=agg_latbar2023_final)

# Qué sucede cuando se comparan

screenreg(list(reg_ind, reg_agg, resultados_3))

screenreg(list(reg_ind, reg_agg, resultados_4))

# Para HTML

htmlreg(list(reg_ind, reg_agg, resultados_3), 
        custom.model.names = c("Individual","Agregado","Multinivel"),    
        custom.coef.names = c("Log Likelihood", "$promedio_clase_{ij}$", "$perc_libertad_{ij}$", "$perc_desigualdad_{ij}$", "$edad_{ij}$", "$clase_scl_{ij}$"), 
        custom.gof.names=c("AIC",
                           "BIC",
                           "Log-verosimilitud",
                           "N observaciones",
                           "N países",
                           "Varianza país ($\\tau_{00}$)", 
                           "Varianza residual ($\\sigma^2$)", 
                           "Var:id ($\\tau_{00}$)",
                           "Var: Residual ($\\sigma^2$)"),
        custom.note = "%stars. Errores estándar en paréntesis",
        caption="Primera comparación de modelos Individual, Agregado y Multinivel",
        caption.above=TRUE,
        doctype = FALSE)


## Guardar datos --------------------------------------------------------------

save(data,file="output/data.RData")
saveRDS(latbar2023, file = "output/latbar2023.RDS")
saveRDS(latbar2023_final, file = "output/latbar2023_final.RDS")
saveRDS(latbar2023_final_agg, file = "output/latbar2023_final_agg.RDS")

