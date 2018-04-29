library(tidyverse)

## Lectura de datos ------------------------------------

trecetones <- read_file("data/trecetones_redux.txt",
                        locale = locale(encoding = "UTF-8"))

cat(trecetones)

## Iniciales -----------------------------------

iniciales <- trecetones %>% 
  str_split("\r\n") %>% 
  flatten_chr() %>% 
  str_replace("^ ", "") %>% 
  str_sub(1L, 1L) %>% 
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
# factorial(length(iniciales))

## Pruebas ----------------------------------------------

# Longitudes de palabras

# 3:10 %>% 
#   map(function(n){
#     sample(iniciales, size = n, replace = FALSE)
#   })

## Algoritmo genético ---------------------------------------------

longitud <- 6
corte <- floor(longitud/2)

set.seed(31818)

# Progenitores
palabra1 <- iniciales %>% 
  sample(longitud, FALSE) %>% 
  paste0(collapse = "")

palabra2 <- iniciales %>%
  # setdiff(palabra1) %>% 
  sample(longitud, FALSE) %>% 
  paste0(collapse = "")

# Descendencia
palabra3 <- paste0(
  str_sub(palabra1, end = corte), 
  str_sub(palabra2, start = corte + 1)
)

palabra4 <- paste0(
  str_sub(palabra2, end = corte), 
  str_sub(palabra1, start = corte + 1)
)

palabra3

