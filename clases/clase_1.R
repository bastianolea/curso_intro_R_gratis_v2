# Clase 1
# Introducción al análisis de datos con R para ciencias sociales
# 25-03-2026
# Bastián Olea Herrera - https://bastianolea.rbind.io - bastianolea@gmail.com

# diapositivas: https://bastianolea.github.io/curso_intro_R_gratis_v2/

# en el script podemos ejecutar las lineas con control + enter
1+2
6-5 # ejemplo de resta

# operaciones matemáticas ----

# sumar dos números
1+1

# sumar muchos números en una sola línea
4+4+5+5+5+5+5+5+5+5 

# multiplicar números grandes
89889*65546

# podemos comentar líneas de código para que no se ejecuten
# 34343-65656+3232*4343


## trabajar con texto ----

# en R, el texto se escribe entre comillas
"gato"

# no se pueden sumar textos como si fueran números
# "gato" + "perro" # esto genera un error

# tampoco se pueden sumar números con texto!
# 5 + "5"




# asignación de variables ----
# guardar un valor en una variable usando <-

# guardar el año actual en una variable
año <- 2026
# notemos que al crear un objeto éste aparece en el panel de entorno (Environment)

# ver el contenido de la variable
año

# guardar un texto en una variable
nombre <- "basti"


# reasignar valores ----

# podemos cambiar el valor de una variable existente
año <- 1993


## operaciones con variables ----

# crear dos variables con números
edad <- 32
año <- 2026

# restar variables (el orden importa)
edad - año
año - edad


# ejemplo de una operación un poco más compleja con objetos
presupuesto = 100000
pizza = 15000
presupuesto - (pizza * 4)


# instalación de paquetes ----
# los paquetes nos permiten extender R con funcionalidades nuevas

# la instalación sólo es necesaria de hacer una vez, ya que es como bajar una app

# instalamos paquete para cargar datos desde excel
install.packages("readxl")

# luego cargamos el paquete, que es como abrir una app
library(readxl) 

# ahora usamos una función del paquete para cargar datos
# las funciones son como pequeños programas que realizan acciones
# en este caso, la función "read_excel" carga un archivo si le das la ruta al mismo

datos <- read_excel("estimaciones_pobreza.xlsx")
# creamos un objeto "datos" que contiene los datos del archivo

datos


# ahora instalamos {dplyr}, paquete para trabajar con datos
install.packages("dplyr")

# cargamos el paquete
library(dplyr)

# nos entrega funciones para explorar datos
glimpse(datos)

# seleccionar columnas
datos |> select(comuna, region)
# esto significa: "al objeto 'datos' luego le selecciono las columnas comuna y region"

# seleccionar y ordenar columnas
datos |> 
  select(region, comuna, personas) |> 
  arrange(personas)
# "al objeto 'datos' luego le selecciono 3 columnas y ordeno los datos según la variable personas"