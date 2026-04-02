# Clase 3
# Introducción al análisis de datos con R para ciencias sociales
# 2026-04-01
# Bastián Olea Herrera - https://bastianolea.rbind.io - bastianolea@gmail.com

# en esta clase veremos:
# 1) crear variables nuevas con case_when()
# 2) contar categorías con count()
# 3) resumir datos con summarize() y group_by()
# 4) unir tablas con left_join()
# 5) pasar datos de formato ancho a largo (pivot_longer) y ancho (pivot_wider)


# cargar paquetes ----
# recuerda: install.packages() solo la primera vez; library() cada vez que abres R

# install.packages("dplyr")
library(dplyr) # para manipular datos

# install.packages("readxl")
library(readxl) # para cargar archivos Excel (.xlsx)


# cargar datos de pobreza ----

# cargamos el archivo Excel a un objeto llamado "datos"
datos <- read_excel("estimaciones_pobreza.xlsx")

# exploramos rápidamente su estructura (columnas, tipos y primeras filas)
datos |> glimpse()


# crear variables ----

## variables dicotómicas ----
# ifelse(condición, valor_si_se_cumple, valor_si_no_se_cumple)

# ejemplo: crear una categoría simple según la cantidad de personas
datos |> 
  select(region, comuna, personas) |> 
  mutate(cantidad = ifelse(personas > 4000, "muchas", "pocas"))
# esto crea la columna "cantidad" con dos valores posibles: "muchas" o "pocas"


# contar categorías ----
# count() agrupa y cuenta en una sola línea

# contar cuántas filas hay por región
datos |> 
  count(region)

datos |> 
  count(comuna)
# el conteo es 1, porque hay una sola fila por comuna

# distinct() entrega valores únicos (sin repetir)
datos_recod |> 
  distinct(region)

# crear variables con varias categorías (case_when) ----
# case_when() sirve para crear categorías más complejas (más de 2 grupos)
# se evalúa de arriba hacia abajo: la primera condición que se cumple gana

# creamos el objeto "datos_recod" con menos columnas y una variable nueva "nivel"
datos_recod <- datos |> 
  select(region, comuna, personas, porcentaje) |> 
  mutate(
    nivel = case_when(
      porcentaje < 0.1 ~ "bajo",
      porcentaje < 0.3 ~ "medio",
      porcentaje < 0.4 ~ "alto",
      porcentaje >= 0.4 ~ "crítico"
    )
  )

# ver resultado
datos_recod

# contar cuántas comunas quedaron en cada nivel
datos_recod |> 
  count(nivel)

# convertir "nivel" a factor para ordenar sus categorías
# creando así una variable ordinal
# esto es útil para tablas y gráficos
datos_recod |>
  mutate(
    nivel = factor(nivel,
                   c("bajo", "medio", "alto", "crítico")) # orden que queremos
  ) |> 
  count(nivel)


# cruces de variables: contar por dos categorías ----
# count(region, nivel) cuenta combinaciones de región + nivel

datos_recod |> 
  count(region, nivel)


# resumir datos ----
# summarize() reduce un dataset a resultados agregados (promedios, máximos, sumas, etc.)
# pasa de muchas filas a una sola fila con el resultado que da la función que usemos

# promedio de "personas"
datos |> 
  summarize(promedio_personas = mean(personas))

# máximo de "personas"
datos |> 
  summarize(max_personas = max(personas))

# suma total de personas (agregado)
datos |> 
  summarize(total = sum(personas))


## agrupar y resumir ----
# group_by() separa el cálculo por grupos (por ejemplo por región)
# lo que nos permite hacer resúmenes de datos por grupo

# total de personas por región y ordenar de mayor a menor
datos |> 
  group_by(region) |> 
  summarize(total = sum(personas)) |> 
  arrange(desc(total))

# total de personas por nivel (usando la tabla recodificada)
datos_recod |> 
  group_by(nivel) |> 
  summarize(total = sum(personas)) |> 
  arrange(desc(total))


# cargar datos de educación ----
# ahora trabajaremos con otra base, para practicar filtros y resúmenes

educacion <- read_xlsx("educacion.xlsx")

# explorar
educacion |> glimpse()

# contar filas por región
educacion |> 
  count(region)

# filtrar para sacar filas que no queremos
educacion |> 
  filter(region != "País") 
# deja las filas *que no son* "País"; es decir,
# excluye las filas que tienen el valor "País" en la variable "region"


# operadores lógicos (repaso rápido) ----
4 != 5  # TRUE: 4 es distinto de 5
4 != 4  # FALSE: 4 no es distinto de 4

4 == 4  # TRUE: 4 es igual a 4
4 == 8  # FALSE: 4 no es igual a 8


# filtrar y resumir educación ----

# contar por sexo
educacion |> 
  count(sexo)

# crear una tabla filtrada
educacion_filtrado <- educacion |> 
  filter(region != "País") |> 
  filter(sexo == "Mujer")
# sin "País" y solo mujeres

# promedios por región
educacion_filtrado |> 
  group_by(region) |> 
  summarize(
    promedio_escolaridad = mean(escolaridad),
    promedio_escolaridad_may = mean(escolaridad_mayores_edad)
  )


# recodificar valores  ----

# caso hipotético: hay una categoría en sexo que no debería estar ("Total Comuna")
# podemos arreglarla antes de analizar

# usamos case_when() para corregir los valores que coincidan con la condición que demos
educacion |> 
  mutate(sexo = case_when(sexo == "Total Comuna" ~ "Mujer", # los que dicen "Total Comuna" pasan a ser "Mujer"
                          .default = sexo) # todos los demás datos que queden como estaban
  ) |> 
  count(sexo) # revisar el resultado


# unir tablas ----
# ahora combinaremos la base de pobreza con una clasificación externa

# ver el objeto original
datos

# cargar clasificación
clasificacion <- read_xlsx("clasificacion.xlsx")

clasificacion

# limpiar clasificación: quedarnos con las columnas necesarias
clasificacion_limpio <- clasificacion |> 
  select(codigo, clasificacion, tot_pers_res_2024)

clasificacion_limpio

# limpiar datos de pobreza:
# - convertir codigo a numérico (para que coincida con la otra tabla)
# - quedarnos con columnas útiles
datos_limpio <- datos |> 
  mutate(codigo = as.numeric(codigo)) |> 
  select(codigo, region, comuna, personas, porcentaje)

datos_limpio

# unir (left_join):
# left_join mantiene todas las filas de la tabla de la izquierda (datos_limpio)
# y pega columnas de la derecha (clasificacion_limpio) donde coincida "codigo"
datos_2 <- datos_limpio |> 
  left_join(clasificacion_limpio, by = "codigo")

# revisar resultado
datos_2
# ahora "datos" tiene lascolumna "clasificacion" y "tot_pers_res_2024" pegada desde la tabla de clasificación

datos_2 |> glimpse()

# ahora podemos contar personas por tipo de clasificación
datos_2 |> 
  group_by(clasificacion) |> 
  summarise(personas = sum(personas))


# transformar estructura de datos ----
# install.packages("tidyr")
library(tidyr)

# cargamos estimaciones de población
poblacion <- read_xlsx("estimaciones_poblacion.xlsx")

# explorar
poblacion |> glimpse()
# este conjunto de datos viene en "ancho": una columna por año
# esto suele ser cómodo para leer, pero no para procesar datos

# ver tabla en visor (RStudio)
View(poblacion)

# imprimir muchas filas (n=Inf imprime todas; ojo: puede ser demasiado)
poblacion |> 
  select(1:6) |> 
  print(n = Inf)

# convertir de formato ancho a largo:
# - cols = where(is.numeric): toma todas las columnas numéricas (por ejemplo años 2020, 2021, 2022...)
# - names_to: el nombre de la nueva columna con los nombres originales (año)
# - values_to: el nombre de la columna con los valores (poblacion)
poblacion_long <- poblacion |> 
  pivot_longer(
    cols = where(is.numeric), # también podría ser cols = 3:last_col() que sería desde la tercera a la última
    names_to = "año",
    values_to = "poblacion")

# mirar el resultado
poblacion_long

poblacion_long |> glimpse()

# filtrar solo año 2050
poblacion_long |> 
  filter(año == 2050)

# con los datos en formato largo podemos filtrar por año, edad y sexo
poblacion_long |> 
  filter(año == 2050,
    edad == 33,
    sexo == "Mujeres")

# también podemos sumar población total por año
poblacion_long |> 
  group_by(año) |> 
  summarise(poblacion = sum(poblacion)) |> 
  print(n = Inf)

# o sumar población por año y sexo
poblacion_sexo <- poblacion_long |> 
  group_by(año, sexo) |> 
  summarise(poblacion = sum(poblacion)) |> 
  print(n = Inf)

# volver de largo a ancho:
# ahora queremos que los datos que están clasificados por sexo aparezcan en dos columnas
# pivot_wider crea una columna por cada valor de "sexo"
poblacion_sexo |> 
  pivot_wider(names_from = sexo,
    values_from = poblacion) |> 
  # y podemos hacer cosas como la siguiente:
  mutate(Total = Hombres + Mujeres)
