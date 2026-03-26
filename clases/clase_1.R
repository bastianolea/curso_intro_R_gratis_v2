# clase 1
# 2026-03-25

# en el script puedo ejecutar las lineas con control enter
1+2
6-5 # ejemplo de resta

# esto es una resta
5-9

# sigamos

1+1
"gato"+"perro"
5+"5"




# crear objetos
año <- 2026

año


edad <- 37
año <- 2026

año - edad

gatos <- 3
gatos


presupuesto = 100000
pizza = 15000
presupuesto - (pizza * 4)

# instalacion, 1 vez no más
# install.packages("readxl")

library(readxl) # para cargar archivos de excel

datos <- read_excel("estimaciones_pobreza.xlsx")

datos

# datos <- read_excel("~/Documents/Basty/Clases/perritos.xlsx")



install.packages("dplyr")

library(dplyr)

glimpse(datos)

select(datos, comuna, region)


datos |> 
  select(region, comuna, personas) |> 
  arrange(personas)
