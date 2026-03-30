# https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion

library(readxl)
library(dplyr)

datos_original <- read_xlsx("datos/estimaciones/estimaciones-y-proyecciones-base.xlsx")

library(janitor)

datos <- datos_original |> 
  clean_names()
  
datos |> 
  distinct(nivel)

datos |> 
  distinct(sexo)

datos |> 
  distinct(fecha)


library(lubridate)

datos_2 <- datos |> 
  mutate(fecha = dmy(fecha)) |> 
  mutate(año = year(fecha))



datos_2 |> 
  filter(año == 2026) |> 
  print(n=Inf)
  summarize(poblacion = sum(poblacion))
  
# cada año tiene dos mediciones
datos_2 |> 
  group_by(año) |> 
  summarize(n_distinct(fecha)) |> 
  print(n=Inf)

pob_año <- datos_2 |> 
  group_by(año) |> 
  summarize(poblacion = sum(poblacion))

pob_año |> 
  filter(año >= 2026)

datos |> 
  filter(fecha == "1/1/2026") |> 
  summarize(poblacion = sum(poblacion))

# gráfico ----
library(ggplot2)

datos_2

datos_2 |> 
  group_by(fecha, sexo) |>
  summarize(poblacion = sum(poblacion)) |>
  ggplot() +
  aes(x=fecha, y=poblacion/1e6,
      color = sexo) +
  geom_line() +
  geom_vline(xintercept = as.Date("2026-01-29")) +
  labs(
    x = "Año",
    y = "Población (millones)",
    title = "Proyección de la población de Chile",
    subtitle = "Fuente: INE - Proyecciones de Población 1992-2070"
  ) +
  theme_minimal()


datos_2 |> 
  filter(edad <= 10) |> 
  filter(año > 2000,
         año < 2030) |> 
  # group_by(fecha, sexo) |>
  # summarize(poblacion = sum(poblacion)) |>
  ggplot() +
  aes(x=fecha, y=poblacion/1e6,
      color = as.factor(edad)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2026-01-29")) +
  labs(
    x = "Año",
    y = "Población (millones)",
    title = "Proyección de la población de Chile",
    subtitle = "Fuente: INE - Proyecciones de Población 1992-2070"
  ) +
  theme_minimal() +
  facet_wrap(~sexo)



migracion <- read_xlsx("datos/estimaciones/estimaciones-y-proyecciones-tabulados.xlsx",
                       sheet = 4)


migracion |> 
  slice(1)

library(tidyr)

migracion |> 
  select(-1) |> 
  row_to_names(1) |> 
  rename(variables = 1) |> 
  filter(variables == "Saldo Migratorio") |> 
  pivot_longer(cols = where(is.numeric),
               names_to = "año", 
               values_to = "migrantes")



poblacion <- read_xlsx("datos/estimaciones/estimaciones-y-proyecciones-tabulados.xlsx",
                       sheet = 3)

poblacion_2 <- poblacion |> 
  slice(-c(1:5)) |> 
  row_to_names(1) |> 
  rename(variables = 1) |> 
  print(n=Inf)

# pob_total <- poblacion_2 |> 
#   slice(2:103) |> 
#   mutate(sexo = "Total") |> 
#   print(n=Inf)
# 
# pob_hombres <- poblacion_2 |> 
#   slice(106:207) |> 
#   mutate(sexo = "Hombres") |> 
#   print(n=Inf)
# 
# pob_mujeres <- poblacion_2 |> 
#   slice(210:311) |> 
#   mutate(sexo = "Mujeres") |> 
#   print(n=Inf)

poblacion_3 <- poblacion_2 |> 
  mutate(sexo = case_when(
    row_number() >= 2 & row_number() <= 103 ~ "Ambos",
    row_number() >= 106 & row_number() <= 207 ~ "Hombres",
    row_number() >= 210 & row_number() <= 311 ~ "Mujeres",
    TRUE ~ NA_character_
  )) |> 
  relocate(sexo, .after = 1) |> 
  filter(!is.na(`1992`)) |> 
  filter(variables != "Total") |> 
  filter(sexo != "Ambos")

poblacion_3 |> 
  distinct(variables) |> print(n=Inf)

poblacion_3 |> 
  distinct(sexo) |> print(n=Inf)


poblacion_3 |> 
  distinct(sexo, variables) |> 
  print(n=Inf)

poblacion_3 |> 
  filter(sexo == "Mujeres")

poblacion_3 |> names()


writexl::write_xlsx(poblacion_3, "datos/estimaciones/estimaciones_poblacion.xlsx")
