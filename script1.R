#----Script trabajo 1 grupo 5----

library(haven) # creo que pueden ignorar este, no sé si vamos a usarlo de nuevo

#esto de abajo no lo corran, era para subir la base al repositorio
latbar2023 <- read_sav("~/Downloads/bases/latbar2023/Latinobarometro_2023_Esp_Spss_v1_0.sav")
saveRDS(latbar2023, "latbar2023.RDS")

#esto sí córranlo
latbar2023 <- readRDS("latbar2023.RDS")
