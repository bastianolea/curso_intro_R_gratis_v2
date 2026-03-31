# Clase 2
# Introducción al análisis de datos con R para ciencias sociales
# 25-03-2026
# Bastián Olea Herrera - https://bastianolea.rbind.io - bastianolea@gmail.com

# diapositivas: https://bastianolea.github.io/curso_intro_R_gratis_v2/

# en esta clase aprenderemos a explorar, ordenar, filtrar y transformar datos
# usando el paquete {dplyr}, que vimos brevemente al final de la clase anterior


# cargar paquetes ----
# recuerda: install.packages() solo la primera vez; library() cada vez que abres R

# install.packages("dplyr")
library(dplyr) # para manipular datos

# install.packages("readxl")
library(readxl) # para cargar archivos Excel


# cargar datos ----

# creamos el objeto "datos" cargando el archivo Excel
datos <- read_excel("estimaciones_pobreza.xlsx")

# explorar el objeto: ver todas las columnas, sus tipos y los primeros valores
datos |> glimpse()

# ver los datos completos en la consola
datos


# seleccionar columnas ----
# select() nos permite quedarnos solo con las columnas que nos interesan

# seleccionar dos columnas por nombre
datos |> select(region, porcentaje)

# también podemos excluir columnas usando el signo -
datos |> select(-codigo)

# seleccionar columnas por posición (número de columna)
datos |> select(1, 2, 3, 5)

# seleccionar un rango de columnas
datos |> select(1:6)

# seleccionar columnas cuyos nombres comienzan con cierto texto
datos |> select(starts_with("limite"))

# seleccionar solo columnas numéricas (y la columna "comuna")
datos |> select(comuna, where(is.numeric))


# ordenar filas ----
# arrange() ordena las filas según los valores de una columna

# ordenar de menor a mayor según número de personas en pobreza
datos |> arrange(personas)

# ordenar de mayor a menor usando el signo - delante de la variable
datos |> arrange(-(personas))

# ordenar alfabéticamente por nombre de comuna
datos |> arrange(comuna)

# podemos combinar select() y arrange() con el pipe |>
# esto significa: "a 'datos', selecciono dos columnas y luego ordeno por personas"
datos |> 
  arrange(personas) |> 
  select(region, personas)

# guardar el resultado en un nuevo objeto
datos_filtrados_ordenados <- datos |> 
  arrange(personas) |> 
  select(region, personas)

datos_filtrados_ordenados


# operadores lógicos ----
# antes de filtrar, necesitamos entender cómo R evalúa condiciones verdaderas o falsas

# mayor que
5 > 3

# también podemos aplicarlos a variables
edad <- 33
edad > 50 # ¿es mayor que 50? → FALSE
edad > 18 # ¿es mayor que 18? → TRUE

# igual a (se escribe con dos signos ==, no uno solo)
6 == 4 # FALSE
6 == 6 # TRUE

# distinto de (!=  significa "no igual a")
1 != 1 # FALSE, porque sí son iguales
1 != 10 # TRUE, porque son distintos


# filtrar filas ----
# filter() nos permite quedarnos solo con las filas que cumplan una condición

# comunas con más de 500.000 personas proyectadas
datos |> 
  filter(personas_proy > 500000)

# comunas con más de 300.000 personas Y con más del 15% de pobreza
# podemos encadenar varios filter() para combinar condiciones
datos |> 
  filter(personas_proy > 300000) |> 
  filter(porcentaje > 0.15) |> 
  select(comuna, personas_proy, porcentaje)

# filtrar una comuna específica por nombre exacto (usamos == para texto también)
datos |> 
  filter(comuna == "Quilicura") |> 
  glimpse()

# guardar en un objeto las comunas con alta pobreza (más del 30%)
comunas_alta_pobreza <- datos |> 
  filter(porcentaje > 0.3) |> 
  select(region, comuna, porcentaje)

# ver el resultado ordenado de mayor a menor pobreza
# desc() dentro de arrange() invierte el orden (mayor a menor)
comunas_alta_pobreza |> 
  arrange(desc(porcentaje))


# crear y transformar columnas ----
# mutate() nos permite crear nuevas columnas o modificar columnas existentes

# crear una columna nueva con un valor fijo (igual para todas las filas)
datos |> 
  select(1, 2, 3, 5) |> 
  mutate(numero = 9) |>   # columna numérica constante
  mutate(animal = "gato") # columna de texto constante

# modificar una columna existente: convertir porcentaje de proporción a porcentaje real
# (multiplicamos por 100 para que 0.15 se muestre como 15)
datos |> 
  select(1:6) |> 
  mutate(porcentaje = porcentaje * 100) 

# crear varias columnas nuevas y guardar el resultado en un nuevo objeto "datos_2"
# también eliminamos las columnas originales que ya reemplazamos
datos_2 <- datos |> 
  select(1:6) |> 
  # porcentaje en escala 0-100
  mutate(porcentaje = porcentaje * 100) |>          
  # personas en miles
  mutate(personas_miles = personas / 1000) |>       
  # población proyectada en miles
  mutate(poblacion_miles = personas_proy / 1000) |> 
  # luego de crear nuevas columnas, eliminamos las columnas originales
  select(-personas_proy, -personas)                 

datos_2


# crear rankings ----
# row_number() asigna un número de fila según el orden actual del objeto
# si primero ordenamos con arrange(), nos da un ranking real

# ranking de comunas con mayor pobreza y mayor población a nivel nacional
datos_2 |> 
  select(region, comuna, porcentaje, personas_miles) |> 
  arrange(desc(porcentaje), desc(personas_miles)) |>  # ordenar por dos criterios
  mutate(ranking = row_number())                      # crear columna de ranking

# el mismo ranking pero solo para la región de Valparaíso
datos_2 |> 
  filter(region == "Valparaíso") |> 
  select(region, comuna, porcentaje, personas_miles) |> 
  arrange(desc(porcentaje), desc(personas_miles)) |> 
  mutate(ranking = row_number())

# ranking para la región del Biobío, guardado en un objeto
datos_biobio <- datos_2 |> 
  filter(region == "Biobío") |> 
  select(region, comuna, porcentaje, personas_miles) |> 
  arrange(desc(porcentaje), desc(personas_miles)) |> 
  mutate(ranking = row_number())

datos_biobio


# filtrar usando un vector de valores ----
# %in% permite filtrar filas cuyo valor esté dentro de una lista de valores

# extraer las 10 comunas con más pobreza del Biobío como un vector de nombres
# pull() extrae una columna como un vector simple (no como tabla)
comunas_top10_biobio <- datos_biobio |> 
  filter(ranking <= 10) |> 
  pull(comuna)

# ver el vector de nombres de comunas
comunas_top10_biobio

# filtrar la base de datos original usando ese vector
# %in% significa "cuyo valor esté incluido en..."
datos |> 
  filter(comuna %in% comunas_top10_biobio)


# buscar texto dentro de columnas ----
# para buscar texto necesitamos el paquete {stringr}
library(stringr)

# str_detect() detecta si una columna de texto contiene cierto patrón
# útil cuando no sabemos el nombre exacto o hay variantes (ej: "Alto Hospicio", "Alto Biobío")
datos_2 |> 
  filter(str_detect(comuna, "Alto")) |> 
  select(region, comuna, porcentaje, personas_miles) |> 
  arrange(desc(porcentaje), desc(personas_miles)) |> 
  mutate(ranking = row_number())


# redondear números ----
# round() redondea un número al número de decimales que indiquemos

round(3434.5656, 2)           # redondear un número a 2 decimales
round(3434.5656, digits = 2)  # es lo mismo, pero escribiendo el nombre del argumento
# escribir el nombre del argumento (digits =) hace el código más legible


# crear columnas condicionales con ifelse() ----
# ifelse() crea una columna nueva según si una condición se cumple o no:
# ifelse(condición, valor_si_cumple, valor_si_no_cumple)

# clasificar comunas según si su población proyectada supera los 200.000 habitantes
datos_2 |> 
  mutate(tipo_poblacion = ifelse(
    poblacion_miles > 200,  # condición
    "alta",                 # valor si se cumple
    "baja"                  # valor si no se cumple
  ))

# es exactamente lo mismo, pero escribiendo los nombres de los argumentos (yes= y no=)
# esto es una buena práctica: hace el código más fácil de leer
datos_2 |> 
  mutate(tipo_poblacion = ifelse(
    poblacion_miles > 200,
    yes = "alta",
    no  = "baja"
  ))
