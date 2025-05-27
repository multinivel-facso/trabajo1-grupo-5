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
#                        Plantilla análisis
#
# ******************************************************************************


## Carga Librerías --------------------------------------------------------------


## Carga de datos ------------------------------------------

# Base limpia, sólo con el índice, no con las variables que lo componen

latbar2023_final <- readRDS("output/latbar2023_final.RDS")

# Carga datos base anterior agregada

agg_latbar2023_final <- readRDS("output/agg_latbar2023_final.RDS")

## Exploración y descripción--------------------------------------------------

names(latbar2023_final) # Muestra los nombres de las variables en la base

summary(latbar2023_final) # Descriptivos generales (evaluación de datos perdidos)

# Tabla descriptiva con stargazer

stargazer(latbar2023_final, title = "Descriptivos generales", type='text')

stargazer(agg_latbar2023_final, type = "text")

# Revisión estadísticos descriptivos cada variable

freq(latbar2023_final$clase_scl)

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

reghelper::ICC(resultados_0)

# Los componentes del cálculo son: 

0.31/(0.31+8.88)

# Resultado -> 0.03373232 correlación intra clase de 0.34%

## Modelos multinivel-----------------------------------------------------------

# Modelos de nivel 1

# Clase social en relación a tendencia política

resultados_1 = lmer(tend_politica ~ 1 + clase_scl + (1 | pais), data = latbar2023_final)
screenreg(resultados_2, naive=TRUE)

0.32/(0.31+9.34)


#------
#Modelo 1 con todas las variables tipo 1:

#propuesta
resultados_2 = lmer(tend_politica ~ 1 + clase_scl + perc_economia + 
                      perc_desigualdad + perc_libertad + perc_migracion + (1 | pais), data = latbar2023_final)
screenreg(resultados_2)



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


#----------
#Modelo 2 con todas las variables tipo 2

#propuesta
resultados_5 = lmer(tend_politica ~ 1 + pais + garantias_pais + (1 | pais), data = latbar2023_final)
screenreg(resultados_5)



#Modelo 3 (individual con grupal)

resultados_6 = lmer(tend_politica ~ 1 + clase_scl + perc_economia + 
                   perc_desigualdad + perc_libertad + perc_migracion + garantias_pais + (1 | pais), data = latbar2023_final)
screenreg(resultados_6)


## Comparación individual, agregado y multinivel--------------------------------

# Regresión comparación

reg_ind=lm(tend_politica ~ clase_scl + perc_economia + perc_libertad + perc_desigualdad, data=latbar2023_final)

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

#Nuevooo
#Comparación de regresiones

reg_ind=lm(tend_politica ~ clase_scl + perc_economia + perc_libertad + perc_desigualdad + perc_migracion, data=latbar2023_final)
reg_agg=lm(tend_politica ~ clase_scl + perc_economia + perc_libertad + perc_desigualdad + garantias_pais, data=agg_latbar2023_final)

#Tres modelos juntos
screenreg(list(reg_ind, reg_agg, resultados_6))
