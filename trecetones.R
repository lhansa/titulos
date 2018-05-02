library(tidyverse)

## Lectura de datos ------------------------------------

trecetones <- read_file("data/trecetones_redux.txt",
                        locale = locale(encoding = "UTF-8"))

cat(trecetones)

diccionario <- read_csv("data/Ratings_Warriner_et_al_Spanish.csv")

palabras <- iconv(str_to_lower(diccionario$Palabra), from = "UTF-8", to = "ASCII//TRANSLIT")

## Iniciales -----------------------------------

iniciales <- trecetones %>% 
  str_split("\n") %>% 
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

pop_size <- 100

posibilidades <- palabras[which(str_length(palabras) == longitud)]

set.seed(31818)
poblacion <- map(1:pop_size, function(i) sample(iniciales, longitud, FALSE)) %>%
  unlist() %>% 
  matrix(ncol = 6, byrow = TRUE)

## Cruces de progenitores --------------------------------------------------------

poblacion_nueva <- c()

for(i in 1:pop_size/2){
  
  # Progenitores
  progenitores <- sample.int(pop_size, 2)
  
  progenitor1 <- paste0(poblacion[progenitores[1], ], collapse = "")
  progenitor2 <- paste0(poblacion[progenitores[2], ], collapse = "")
  
  # Descendencia
  descendencia1 <- paste0(
    str_sub(progenitor1, end = corte), 
    str_sub(progenitor2, start = corte + 1)
  )
  
  descendencia2 <- paste0(
    str_sub(progenitor2, end = corte), 
    str_sub(progenitor1, start = corte + 1)
  )
  
  poblacion_nueva <- c(poblacion_nueva, descendencia1, descendencia2)
  
}

#' Ahora tengo que ver si la descendencia tiene letras en común 
#' con las palabras posibles y cuantificar esa similitud. 

check_common_letters <- function(palabra_hija){
  
  comunes <- map2(str_split(posibilidades, ""), 
                  str_split(palabra_hija, ""), 
                  `==`) %>% 
    map_dbl(sum)
  
  return(c(simil = max(comunes), ind = which.max(comunes)))
}

comunes3 <- check_common_letters(descendencia1)
comunes4 <- check_common_letters(descendencia2)


