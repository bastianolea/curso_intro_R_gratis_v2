library(dplyr)
library(janitor)
library(readxl)

educacion <- read_xlsx("datos/censo/P7_Educacion.xlsx",
                    sheet = 5)

educacion_2 <- educacion |> 
  row_to_names(3) |> 
  clean_names() |> 
  select(-codigo_provincia, -provincia) |> 
  rename(escolaridad = anos_de_escolaridad_promedio,
         escolaridad_mayores_edad = anos_de_escolaridad_promedio_para_la_poblacion_de_18_anos_o_mas)

educacion_3 <- educacion_2 |> 
  mutate(codigo_region = as.numeric(codigo_region),
         codigo_comuna = as.numeric(codigo_comuna),
         escolaridad = as.numeric(escolaridad),
         escolaridad_mayores_edad = as.numeric(escolaridad_mayores_edad)) |> 
  filter_out(is.na(codigo_region))

educacion_3

writexl::write_xlsx(educacion_3, "datos/censo/educacion.xlsx")
