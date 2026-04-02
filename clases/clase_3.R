# Clase 3
# Introducción al análisis de datos con R para ciencias sociales
# 01-04-2026
# Bastián Olea Herrera - https://bastianolea.rbind.io - bastianolea@gmail.com

# cargar paquetes ----
# recuerda: install.packages() solo la primera vez; library() cada vez que abres R

# install.packages("dplyr")
library(dplyr) # para manipular datos

# install.packages("readxl")
library(readxl) # para cargar archivos Excel

datos <- read_excel("estimaciones_pobreza.xlsx")

datos |> 
  select(region, comuna, personas) |> 
  mutate(cantidad = ifelse(personas > 4000, "muchas", "pocas"))

datos |> 
  count(region)

# crear la variable nivel
datos_recod <- datos |> 
  select(region, comuna, personas, porcentaje) |> 
  mutate(nivel = case_when(porcentaje < 0.1 ~ "bajo",
                           porcentaje < 0.3 ~ "medio",
                           porcentaje < 0.4 ~ "alto",
                           porcentaje >= 0.4 ~ "crítico"))

# datos |> 
#   count(nivel)

names(datos)

names(datos_recod)

datos_recod |> 
  count(nivel)

datos_recod |>
  mutate(nivel = factor(nivel,
                        c("bajo", "medio", "alto", "crítico")
                        )) |> 
  count(nivel)

datos_recod |> 
  count(region, nivel)

datos_recod |> 
  count(region)

datos_recod |> 
  distinct(region)

datos |> 
  summarize(mean(personas))

datos |> 
  summarize(max(personas))

datos |> 
  distinct(region) |> 
  arrange(region)

datos |> 
  summarize(total = sum(personas))

datos |> 
  group_by(region) |> 
  summarize(total = sum(personas)) |> 
  arrange(desc(total))

datos_recod |> 
  group_by(nivel) |> 
  summarize(total = sum(personas)) |> 
  arrange(desc(total))


educacion <- read_xlsx("educacion.xlsx")

glimpse(educacion)

educacion |> 
  count(region)

educacion |> 
  filter(region != "País")

educacion |> 
  filter_out(region == "País")


4 != 5
4 != 4

4 == 4
4 == 8

educacion |> 
  count(sexo)

educacion_filtrado <- educacion |> 
  filter_out(region == "País") |> 
  filter(sexo == "Mujer")

educacion_filtrado |> 
  group_by(region) |> 
  summarize(promedio_escolaridad = mean(escolaridad),
            promedio_escolaridad_may = mean(escolaridad_mayores_edad)
            )

educacion |> 
  mutate(sexo = case_when(sexo == "Total Comuna" ~ "Mujer",
                          .default = sexo)) |> 
  count(sexo)

educacion |> 
  count(sexo)

educacion |> 
  mutate(sexo = recode_values(sexo,
                              "Total Comuna" ~ "Mujer",
                              default = sexo)) |> 
  count(sexo)


datos

# cargar
clasificacion <- read_xlsx("clasificacion.xlsx")

clasificacion

datos

# limpiar
clasificacion_limpio <- clasificacion |> 
  select(codigo, clasificacion, tot_pers_res_2024)

clasificacion_limpio

# limpiar
datos_limpio <- datos |> 
  mutate(codigo = as.numeric(codigo)) |> 
  select(codigo, region, comuna, personas, porcentaje)

datos_limpio

# unimos
datos_2 <- datos_limpio |> 
  left_join(clasificacion_limpio, by = "codigo")


datos_2 |> 
  group_by(clasificacion) |> 
  summarise(personas = sum(personas))

# install.packages("tidyr")
library(tidyr)


poblacion <- read_xlsx("estimaciones_poblacion.xlsx")


glimpse(poblacion)

View(poblacion)


poblacion |> 
  select(1:6) |> 
  print(n=Inf)

poblacion_long <- poblacion |> 
  pivot_longer(cols = where(is.numeric),
               names_to = "año",
               values_to = "poblacion")


poblacion_long |> 
  filter(año == 2050)

poblacion_long |> 
  filter(año == 2050,
         edad == 33,
         sexo == "Mujeres")

poblacion_long |> 
  group_by(año) |> 
  summarise(poblacion = sum(poblacion)) |> 
  print(n=Inf)

poblacion_sexo <- poblacion_long |> 
  group_by(año, sexo) |> 
  summarise(poblacion = sum(poblacion)) |> 
  print(n=Inf)

poblacion_sexo |> 
  pivot_wider(names_from = sexo,
              values_from = poblacion) |> 
  mutate(Total = Hombres + Mujeres)
