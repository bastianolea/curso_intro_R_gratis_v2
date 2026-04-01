# https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion

poblacion <- read_xlsx("datos/estimaciones/estimaciones-y-proyecciones-tabulados.xlsx",
                       sheet = 3)

poblacion_2 <- poblacion |> 
  slice(-c(1:5)) |> 
  row_to_names(1) |> 
  rename(variables = 1) |> 
  print(n = Inf)

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
  filter(sexo != "Ambos") |> 
  rename(edad = variables)

poblacion_4 <- poblacion_3 |> 
  mutate(across(`1992`:`2070`, as.numeric))

# poblacion_3 |> 
#   distinct(variables) |> print(n=Inf)
# 
# poblacion_3 |> 
#   distinct(sexo) |> print(n=Inf)
# 
# 
# poblacion_3 |> 
#   distinct(sexo, variables) |> 
#   print(n=Inf)
# 
# poblacion_3 |> 
#   filter(sexo == "Mujeres")
# 
# poblacion_3 |> names()


writexl::write_xlsx(poblacion_4, "datos/estimaciones/estimaciones_poblacion.xlsx")
