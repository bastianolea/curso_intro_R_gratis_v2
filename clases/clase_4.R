



library(dplyr) # para manipular datos
library(readxl) # para cargar archivos Excel

datos <- read_excel("estimaciones_pobreza.xlsx")

# install.packages("ggplot2")
library(ggplot2)

# gráfico vacío
ggplot()

datos |>
  filter(region == "Tarapacá") |>
  ggplot() +
  aes(x = personas, y = comuna) +
  geom_col()

datos |>
  filter(region == "Tarapacá") |>
  ggplot() +
  aes(x = comuna, y = personas) +
  geom_col()

datos |>
  filter(region == "Tarapacá") |>
  # gráfico
  ggplot() +
  aes(x = personas, y = comuna) +
  geom_col(fill = "purple")


datos |>
  filter(region == "Tarapacá") |>
  # gráfico
  ggplot() +
  aes(x = personas, y = comuna, fill = comuna) +
  geom_col()


datos |>
  filter(region == "Arica y Parinacota") |>
  ggplot() +
  aes(x = porcentaje, y = comuna, fill = personas) +
  geom_col()

# las variables de texto se ordenan por orden alfabético
datos |>
  filter(region == "Arica y Parinacota") |>
  select(comuna, porcentaje) |>
  arrange(comuna)


# install.packages("forcats")
library(forcats)


# cambiar el orden de la variable comuna con fct_reorder()
# para que se ordene según la variable  porcentaje
datos |>
  filter(region == "Arica y Parinacota") |>
  select(comuna, porcentaje) |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |>
  arrange(comuna)


datos |>
  # ordenar
  filter(region == "Arica y Parinacota") |>
  mutate(comuna = fct_reorder(comuna, porcentaje, .desc = TRUE)) |>
  # gráfico
  ggplot() +
  aes(x = porcentaje, y = comuna, fill = personas) +
  geom_col()

datos |>
  filter(region == "Tarapacá") |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |>
  mutate(comuna = fct_rev(comuna)) |>
  # gráfico
  ggplot() +
  aes(x = porcentaje, y = comuna) +
  geom_col()

datos |>
  filter(region == "Tarapacá") |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |>
  mutate(comuna = fct_rev(comuna)) |>
  ggplot() +
  aes(x = porcentaje, y = comuna, fill = comuna) +
  geom_col()


datos |>
  # ordenar
  filter(region == "Tarapacá") |>
  mutate(comuna = fct_reorder(comuna, porcentaje)) |>
  mutate(comuna = fct_rev(comuna)) |>
  select(2:6) |>
  # crear una nueva variable
  mutate(nivel = ifelse(porcentaje > 0.32, "alto", "bajo")) |>
  # gráfico
  ggplot() +
  aes(x = porcentaje, y = comuna, 
      fill = nivel) + # aplicar la variable nueva
  geom_col()

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
  # escala de colores para la variable
  scale_fill_manual(values = c("alto" = "#344e41", "bajo" = "#a3b18a"))



# cargar
educacion <- read_xlsx("educacion.xlsx")

# limpieza
educacion_comuna <- educacion |>
  filter(sexo == "Total Comuna") |>
  select(codigo_comuna, starts_with("escolaridad")) |>
  rename(codigo = codigo_comuna)

# cruce
pobreza_educ <- datos |>
  mutate(codigo = as.numeric(codigo)) |>
  select(codigo:porcentaje) |>
  left_join(educacion_comuna, by = "codigo")


pobreza_educ |>
  ggplot() +
  aes(x = personas, y = escolaridad) +
  geom_point(alpha = 0.3)

pobreza_educ |>
  ggplot() +
  aes(y = personas, x = escolaridad) +
  geom_point(alpha = 0.3)

pobreza_educ |>
  ggplot() +
  aes(x = personas, y = escolaridad, size = personas_proy) +
  geom_point(alpha = 0.3)

pobreza_educ |>
  ggplot() +
  aes(x = porcentaje, y = escolaridad, size = personas_proy) +
  geom_point(color = "#5a189a", alpha = .5) +
  theme_minimal()

# install.packages("scales")
library(scales)

grafico <- pobreza_educ |>
  ggplot() +
  aes(x = porcentaje, y = escolaridad, size = personas_proy) +
  geom_point(color = "#22223b", alpha = .5) +
  theme_minimal() +
  scale_size_continuous(labels = label_comma())

grafico

grafico_2 <- grafico +
  theme_minimal(
    paper = "#f2e9e4",
    ink = "#4a4e69",
    accent = "#22223b",
    base_family = "Arial")

grafico_2

grafico_3 <- grafico_2 +
  labs(title = "Gráfico de dispersión",
       subtitle = "Relación entre escolaridad y pobreza a nivel comunal",
       x = "Porcentaje de población en situación de pobreza",
       y = "Años promedio de escolaridad",
       size = "Población",
       caption = "Fuente: Casen 2022, Censo 2024")

grafico_3

grafico_4 <- grafico_3 +
  theme(plot.title = element_text(face = "bold", size = 18, family = "Futura"),
        plot.subtitle = element_text(face = "italic"))

grafico_4

ggsave("pobreza_educacion.png", width = 8, height = 5, dpi = 300)



pobreza_educ |>
  count(region)

datos_grafico <- pobreza_educ |>
  mutate(destacar = ifelse(region == "Metropolitana", "Metropolitana", "Resto del país"))

datos_grafico |>
  ggplot() +
  aes(x = porcentaje,
      y = escolaridad,
      size = personas_proy,
      color = destacar) +
  # geometrías
  geom_point(alpha = .5) +
  # escalas
  scale_size_continuous(labels = label_comma()) +
  scale_color_manual(values = c("#22223b", "#9a8c98")) +
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


ggsave("pobreza_educacion_region.png", width = 8, height = 5, dpi = 300)




poblacion <- read_xlsx("estimaciones_poblacion.xlsx")

library(tidyr)

poblacion_long <- poblacion |>
  pivot_longer(cols = where(is.numeric),
               names_to = "año",
               values_to = "poblacion")

poblacion_long |>
  group_by(año) |>
  summarise(poblacion = sum(poblacion)) |>
  mutate(año = as.numeric(año)) |>
  filter(año > 1990 & año < 2030) |>
  ggplot() +
  aes(x = año, y = poblacion) +
  geom_point() +
  theme_minimal()

poblacion_long |>
  group_by(año) |>
  summarise(poblacion = sum(poblacion)) |>
  mutate(año = as.numeric(año)) |>
  filter(año > 1990 & año < 2030) |>
  ggplot() +
  aes(x = año, y = poblacion) +
  geom_line() +
  geom_point() +
  theme_minimal()

poblacion_long |>
  group_by(año) |>
  summarise(poblacion = sum(poblacion)) |>
  mutate(año = as.numeric(año)) |>
  filter(año > 1990 & año < 2030) |>
  ggplot() +
  aes(x = año, y = poblacion) +
  geom_vline(xintercept = 2024, linetype = "dashed") +
  geom_line(linewidth = 1.2, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  scale_y_continuous(labels = label_number(scale = 0.000001, suffix = " millones"),
                     limits = c(0, NA)) +
  theme_minimal(
    paper = "#f2e9e4",
    ink = "#4a4e69",
    accent = "#22223b",
    base_family = "Arial") +
  labs(y = "Proyección de población", title = "Proyecciones de población",
       subtitle = "Censo 2024, Chile",
       color = "Sexo", x = NULL) +
  theme(plot.title = element_text(face = "bold", size = 18, family = "Futura"),
    plot.subtitle = element_text(face = "italic"),
    axis.text.x = element_text(face = "bold", size = 10))

ggsave("proyeccion_poblacion.png", width = 8, height = 5, dpi = 300)

poblacion_long |>
  group_by(año, sexo) |>
  summarise(poblacion = sum(poblacion)) |>
  mutate(año = as.numeric(año)) |>
  filter(año > 1990 & año < 2030) |>
  ggplot() +
  aes(x = año, y = poblacion, color = sexo) +
  geom_vline(xintercept = 2024, linetype = "dashed") +
  geom_line(linewidth = 1.2, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  scale_y_continuous(labels = label_number(scale = 0.000001, suffix = " millones"),
                     limits = c(0, NA)) +
  theme_minimal(
    paper = "#f2e9e4",
    ink = "#4a4e69",
    accent = "#22223b",
    base_family = "Arial") +
  scale_color_manual(values = c("Mujeres" = "#22223b", "Hombres" = "#9a8c98")) +
  guides(color = guide_legend(position = "top", override.aes = list(size = 4))) + # aumentar tamaño de puntos en leyenda
  labs(y = "Proyección de población", title = "Proyecciones de población",
       subtitle = "Censo 2024, Chile",
       color = "Sexo", x = NULL) +
  theme(plot.title = element_text(face = "bold", size = 18, family = "Futura"),
        plot.subtitle = element_text(face = "italic"),
        axis.text.x = element_text(face = "bold", size = 10))

ggsave("proyeccion_poblacion_sexo.png", width = 8, height = 5, dpi = 300)
