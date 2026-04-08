# Clase 4: visualización de datos
# Introducción al análisis de datos con R para ciencias sociales
# 2026-04-07
# Bastián Olea Herrera - https://bastianolea.rbind.io - bastianolea@gmail.com


# cargar paquetes ----
# recuerda: install.packages() solo la primera vez; library() cada vez que abres R

# install.packages("dplyr")
library(dplyr) # para manipular datos

# install.packages("readxl")
library(readxl) # para cargar archivos Excel


# cargar datos ----

# cargamos el archivo Excel a un objeto llamado "datos"
datos <- read_excel("estimaciones_pobreza.xlsx")


# introducción a ggplot2 ----
# ggplot2 es un paquete para hacer gráficos en R
# los gráficos se construyen sumando capas con el operador +

# install.packages("ggplot2")
library(ggplot2)

# un ggplot() vacío, sin datos ni capas, produce un lienzo en blanco
ggplot()


# gráficos de barras ----
# geom_col() hace barras donde la altura representa el valor de una variable numérica
# aes() define qué variables van en cada eje

# gráfico de barras horizontales: personas en situación de pobreza por comuna en Tarapacá
datos |>
  filter(region == "Tarapacá") |>
  ggplot() +
  aes(x = personas, y = comuna) + # x = largo de la barra, y = etiqueta
  geom_col()

# el mismo gráfico pero con los ejes invertidos (barras verticales)
datos |>
  filter(region == "Tarapacá") |>
  ggplot() +
  aes(x = comuna, y = personas) +
  geom_col()

# podemos cambiar el color de las barras con el argumento fill
datos |>
  filter(region == "Tarapacá") |>
  # gráfico
  ggplot() +
  aes(x = personas, y = comuna) +
  geom_col(fill = "purple3") # fill define el color de relleno de las barras


# color según variable ----
# cuando fill va dentro de aes(), se mapea la variable qeu elijamos al relleno,
# y el color se asigna según los valores de una variable

datos |>
  filter(region == "Tarapacá") |>
  # gráfico
  ggplot() +
  aes(x = personas, y = comuna, fill = comuna) + # una barra de color distinto por comuna
  geom_col()

# mapeamos en aes() una variable contínua o numérica al relleno de color
datos |>
  filter(region == "Arica y Parinacota") |>
  ggplot() +
  aes(x = porcentaje, y = comuna, fill = personas) + # color según número de personas
  geom_col()


# ordenar variables de texto ----
# las variables de texto se ordenan por orden alfabético de forma predeterminada
datos |>
  filter(region == "Arica y Parinacota") |>
  select(comuna, porcentaje) |>
  arrange(comuna) # confirma que el orden es alfabético


# reordenar factores con forcats ----
# para cambiar el orden en que aparecen las categorías en un gráfico,
# necesitamos convertir la variable de texto a factor y asignarle un orden

# install.packages("forcats")
library(forcats)

# fct_reorder() reordena los niveles de un factor según los valores de otra variable
# esto cambia el orden en que aparecen las barras en el gráfico
datos |>
  filter(region == "Arica y Parinacota") |>
  select(comuna, porcentaje) |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |> # ordenar comunas según porcentaje
  arrange(comuna)
# lo confirmamos primero pidiendo que ordene por comuna, y vemos que ahora el orden no es alfabético

# gráfico con comunas ordenadas de mayor a menor ----
# .desc = TRUE invierte el orden del factor (de mayor a menor)
datos |>
  # ordenar
  filter(region == "Arica y Parinacota") |>
  mutate(comuna = fct_reorder(comuna, porcentaje, .desc = TRUE)) |>
  # gráfico
  ggplot() +
  aes(x = porcentaje, y = comuna, fill = personas) +
  geom_col()

# alternativa: usar fct_reorder() + fct_rev() para invertir el orden
# fct_rev() invierte los niveles de un factor
datos |>
  filter(region == "Tarapacá") |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |>
  mutate(comuna = fct_rev(comuna)) |> # invertir el orden del factor
  # gráfico
  ggplot() +
  aes(x = porcentaje, y = comuna) +
  geom_col()

# lo mismo pero coloreando cada barra según la comuna
datos |>
  filter(region == "Tarapacá") |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |>
  mutate(comuna = fct_rev(comuna)) |>
  ggplot() +
  aes(x = porcentaje, y = comuna, fill = comuna) +
  geom_col() +
  theme(legend.position = "none") # eliminar leyenda (porque el color ya se entiende con las etiquetas de las barras)


# crear variables nuevas para el gráfico ----
# podemos crear una variable antes de graficar para usarla como color

datos |>
  # ordenar
  filter(region == "Tarapacá") |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |>
  mutate(comuna = fct_rev(comuna)) |>
  select(2:6) |>
  # crear una nueva variable que clasifica si el porcentaje es alto o bajo
  mutate(nivel = ifelse(porcentaje > 0.32, "alto", "bajo")) |>
  # gráfico
  ggplot() +
  aes(x = porcentaje, y = comuna,
      fill = nivel) + # aplicar la variable nueva como color de relleno
  geom_col()

# escala de colores manual ----
# scale_fill_manual() permite asignar colores específicos a cada categoría
datos |>
  # ordenar
  filter(region == "Tarapacá") |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |>
  mutate(comuna = fct_rev(comuna)) |>
  select(2:6) |>
  mutate(nivel = ifelse(porcentaje > 0.32, "alto", "bajo")) |>
  # gráfico
  ggplot() +
  aes(x = porcentaje, y = comuna,
      fill = nivel) +
  geom_col() +
  # asignar un color específico a cada valor de la variable "nivel"
  scale_fill_manual(values = c("alto" = "#344e41", "bajo" = "#a3b18a"))


# unir datos de pobreza y educación ----

# cargar datos de educación
educacion <- read_xlsx("educacion.xlsx")

# limpieza: quedarnos solo con las filas de totales comunales y las columnas de escolaridad
educacion_comuna <- educacion |>
  filter(sexo == "Total Comuna") |>             # solo totales por comuna (sin desglose por sexo)
  select(codigo_comuna, starts_with("escolaridad")) |> # código de comuna y columnas de escolaridad
  rename(codigo = codigo_comuna)                # renombrar para que coincida con la otra tabla

# cruce: unir datos de pobreza con datos de educación por código de comuna
pobreza_educ <- datos |>
  mutate(codigo = as.numeric(codigo)) |>        # convertir código a numérico para que coincida
  select(codigo:porcentaje) |>                  # quedarnos con las columnas relevantes
  left_join(educacion_comuna, by = "codigo")    # pegar columnas de educación donde coincida la variable código

pobreza_educ

# gráficos de dispersión (geom_point) ----
# geom_point() muestra puntos; útil para ver la relación entre dos variables numéricas

# personas en pobreza vs. años de escolaridad (un punto por comuna)
pobreza_educ |>
  ggplot() +
  aes(x = personas, y = escolaridad) +
  geom_point(alpha = 0.3) # alpha controla la transparencia (0 = invisible, 1 = opaco)

# el mismo gráfico con los ejes invertidos
pobreza_educ |>
  ggplot() +
  aes(y = personas, x = escolaridad) +
  geom_point(alpha = 0.3)

# agregar una tercera variable con el tamaño de los puntos
pobreza_educ |>
  ggplot() +
  aes(x = personas, y = escolaridad, size = personas_proy) + # size = tamaño según población proyectada
  geom_point(alpha = 0.3)

# usar porcentaje de pobreza en el eje x y aplicar un tema
pobreza_educ |>
  ggplot() +
  aes(x = porcentaje, y = escolaridad, size = personas_proy) +
  geom_point(color = "#5a189a", alpha = .5) +
  theme_minimal() # theme_minimal() elimina el fondo gris y simplifica el gráfico


# formatear etiquetas de ejes con {scales} ----
# el paquete {scales} permite formatear números en los ejes (miles, porcentajes, etc.)

# install.packages("scales")
library(scales)

# guardar el gráfico en un objeto para ir agregándole capas paso a paso
grafico <- pobreza_educ |>
  ggplot() +
  aes(x = porcentaje, y = escolaridad, size = personas_proy) +
  geom_point(color = "#22223b", alpha = .5) +
  theme_minimal() +
  scale_size_continuous(labels = label_comma()) # formatear leyenda con separador de miles

grafico

# agregar colores y tipografía al tema del gráfico
grafico_2 <- grafico +
  theme_minimal(
    paper = "#f2e9e4",    # color de fondo
    ink = "#4a4e69",      # color del texto
    accent = "#22223b",   # color de acento
    base_family = "Arial" # tipografía base
  )

grafico_2

# agregar títulos y etiquetas con labs()
# labs() permite definir el título, subtítulo, nombre de los ejes y fuente
grafico_3 <- grafico_2 +
  labs(title = "Gráfico de dispersión",
       subtitle = "Relación entre escolaridad y pobreza a nivel comunal",
       x = "Porcentaje de población en situación de pobreza",
       y = "Años promedio de escolaridad",
       size = "Población",
       caption = "Fuente: Casen 2022, Censo 2024")

grafico_3

# personalizar elementos del tema con theme()
# theme() permite ajustar cualquier elemento visual del gráfico individualmente
grafico_4 <- grafico_3 +
  theme(
    plot.title = element_text(face = "bold", size = 18, family = "Futura"), # título en negrita
    plot.subtitle = element_text(face = "italic") # subtítulo en cursiva
  )

grafico_4

# guardar el gráfico como imagen
ggsave("pobreza_educacion.png", width = 8, height = 5, dpi = 300)
# esta función guarda el último gráfico que hayamos visto


# destacar una región en el gráfico ----

# contar comunas por región para explorar los datos
pobreza_educ |>
  count(region)

# crear una variable que distingue la Región Metropolitana del resto del país
datos_grafico <- pobreza_educ |>
  mutate(destacar = ifelse(region == "Metropolitana", "Metropolitana", "Resto del país"))

# gráfico final con todas las capas integradas
datos_grafico |>
  ggplot() +
  aes(x = porcentaje,
      y = escolaridad,
      size = personas_proy,
      color = destacar) +   # color según si es RM o no
  # geometrías
  geom_point(alpha = .5) +
  # escalas
  scale_size_continuous(labels = label_comma()) +
  scale_color_manual(values = c("#22223b", "#9a8c98")) + # colores para cada grupo
  # tema
  theme_minimal(
    paper = "#f2e9e4",
    ink = "#4a4e69",
    accent = "#22223b",
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, family = "Futura"),
    plot.subtitle = element_text(face = "italic")
  ) +
  # textos
  labs(
    title = "Gráfico de dispersión",
    subtitle = "Relación entre escolaridad y pobreza a nivel comunal",
    x = "Porcentaje de población en situación de pobreza",
    y = "Años promedio de escolaridad",
    size = "Población",
    color = "Región",
    caption = "Fuente: Casen 2022, Censo 2024"
  )

# guardar el gráfico
ggsave("pobreza_educacion_region.png", width = 8, height = 5, dpi = 300)


# proyecciones de población ----

# cargar datos de estimaciones de población
poblacion <- read_xlsx("estimaciones_poblacion.xlsx")

# cargar tidyr para transformar la estructura de los datos
library(tidyr)

# los datos vienen en formato ancho (una columna por año)
# pivot_longer() los transforma a formato largo (una fila por año)
# esto facilita filtrar, agrupar y graficar por año
poblacion_long <- poblacion |>
  pivot_longer(cols = where(is.numeric), # toma todas las columnas numéricas (los años)
               names_to = "año",         # los nombres de columna pasan a ser valores de "año"
               values_to = "poblacion")  # los valores pasan a la columna "poblacion"


# gráfico de puntos: evolución de la población total ----
# primero agrupamos por año y sumamos la población total
poblacion_long |>
  group_by(año) |>
  summarise(poblacion = sum(poblacion)) |>
  mutate(año = as.numeric(año)) |>   # convertir "año" de texto a número (para el eje)
  filter(año > 1990 & año < 2030) |> # filtrar el rango de años que queremos mostrar
  ggplot() +
  aes(x = año, y = poblacion) +
  geom_point() +
  theme_minimal()

# agregar una línea que conecte los puntos con geom_line()
poblacion_long |>
  group_by(año) |>
  summarise(poblacion = sum(poblacion)) |>
  mutate(año = as.numeric(año)) |>
  filter(año > 1990 & año < 2030) |>
  ggplot() +
  aes(x = año, y = poblacion) +
  geom_line() +  # línea que conecta los puntos
  geom_point() +
  theme_minimal()

# gráfico completo con todas las capas ----
# geom_vline() agrega una línea vertical para marcar el año del censo
poblacion_long |>
  group_by(año) |>
  summarise(poblacion = sum(poblacion)) |>
  mutate(año = as.numeric(año)) |>
  filter(año > 1990 & año < 2030) |>
  ggplot() +
  aes(x = año, y = poblacion) +
  geom_vline(xintercept = 2024, linetype = "dashed") + # línea vertical en el año del censo
  geom_line(linewidth = 1.2, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  # formatear eje Y en millones para facilitar la lectura
  scale_y_continuous(labels = label_number(scale = 0.000001, suffix = " millones"),
                     limits = c(0, NA)) +  # el eje parte desde 0
  theme_minimal(
    paper = "#f2e9e4",
    ink = "#4a4e69",
    accent = "#22223b",
    base_family = "Arial") +
  labs(y = "Proyección de población",
       title = "Proyecciones de población",
       subtitle = "Censo 2024, Chile",
       color = "Sexo",
       x = NULL) +  # x = NULL elimina la etiqueta del eje X
  theme(
    plot.title = element_text(face = "bold", size = 18, family = "Futura"),
    plot.subtitle = element_text(face = "italic"),
    axis.text.x = element_text(face = "bold", size = 10)
  )

# guardar el gráfico
ggsave("proyeccion_poblacion.png", width = 8, height = 5, dpi = 300)


# desglosar proyecciones por sexo ----
# exactamente el mismo código que antes, pero agregamos sexo al group_by() para obtener una línea por sexo
poblacion_long |>
  group_by(año, sexo) |>  # agrupar por año y sexo
  summarise(poblacion = sum(poblacion)) |>
  mutate(año = as.numeric(año)) |>
  filter(año > 1990 & año < 2030) |>
  ggplot() +
  aes(x = año, y = poblacion, color = sexo) + # color distinto por sexo
  geom_vline(xintercept = 2024, linetype = "dashed") +
  geom_line(linewidth = 1.2, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  scale_y_continuous(labels = label_number(scale = 0.000001, suffix = " millones"),
                     limits = c(0, NA)) +
  theme_minimal(
    paper = "#f2e9e4",
    ink = "#4a4e69",
    accent = "#22223b",
    base_family = "Arial") +
  scale_color_manual(values = c("Mujeres" = "#22223b", "Hombres" = "#9a8c98")) +
  guides(color = guide_legend(position = "top", override.aes = list(size = 4))) + # aumentar tamaño de puntos en leyenda
  labs(y = "Proyección de población",
       title = "Proyecciones de población",
       subtitle = "Censo 2024, Chile",
       color = "Sexo",
       x = NULL) +
  theme(
    plot.title = element_text(face = "bold", size = 18, family = "Futura"),
    plot.subtitle = element_text(face = "italic"),
    axis.text.x = element_text(face = "bold", size = 10)
  )

# guardar el gráfico
ggsave("proyeccion_poblacion_sexo.png", width = 8, height = 5, dpi = 300)