library(tidyverse)

## Pendiente usar stringdist

## Funciones --------------------------------------------------

check_common_letters <- function(palabra_hija, con_ind = FALSE){
  
  comunes <- map2(str_split(posibilidades, ""), 
                  str_split(palabra_hija, ""), 
                  `==`) %>% 
    map_dbl(sum)
  
  if(con_ind){
    return(c(simil = max(comunes), ind = which.max(comunes)))  
  } else {
    return(max(comunes))
  }
  
}

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

## Parámetros ---------------------------------------------

longitud <- 6

pop_size <- 200

maxiter <- 1000
prob_cruce <- 0.25
prob_mutacion <- 0.1

## Inicialización del algoritmo -----------------------------------------

posibilidades <- palabras[which(str_length(palabras) == longitud)]

set.seed(31818)
poblacion <- map(1:pop_size, function(i) sample(iniciales, longitud, FALSE)) %>%
  unlist() %>% 
  matrix(ncol = 6, byrow = TRUE) %>% 
  apply(1, function(x) paste0(x, collapse = ""))

comunes <- map_dbl(poblacion, check_common_letters)

iter <- 1

## Algoritmo genético ---------------------------------------------------

while(iter <= maxiter){
  
  poblacion_nueva <- c()
  
  for(i in 1:(pop_size/2)){
    
    # Progenitores
    progenitores <- sample.int(pop_size, 2, prob = comunes / sum(comunes))
    
    # progenitor1 <- paste0(poblacion[progenitores[1], ], collapse = "")
    # progenitor2 <- paste0(poblacion[progenitores[2], ], collapse = "")
    
    progenitor1 <- poblacion[progenitores[1]]
    progenitor2 <- poblacion[progenitores[2]]
    
    # Descendencia
    
    if(runif(1) > prob_cruce){ # tienen descendencia
      
      corte <- sample.int(1 : (longitud-1)) # Punto de corte de la palabra
      
      descendencia1 <- paste0(
        str_sub(progenitor1, end = corte), 
        str_sub(progenitor2, start = corte + 1)
      )
      
      descendencia2 <- paste0(
        str_sub(progenitor2, end = corte), 
        str_sub(progenitor1, start = corte + 1)
      )
      
    } else { # no la tienen
      descendencia1 <- progenitor1
      descendencia2 <- progenitor2
    }
    
    # Mutación (pendiente)
    muta <- function(palabra){
      for(p in 1:length(palabra)){
        if(runif(1) > prob_mutacion) str_sub(palabra, p, p) <- sample(letters, 1)    
      }
      return(palabra)
    }
    
    descendencia1 <- muta(descendencia1)
    descendencia2 <- muta(descendencia2)

    # Actualizamos población nueva
    poblacion_nueva <- c(poblacion_nueva, descendencia1, descendencia2)
    
  }
  
  #' Ahora tengo que ver si la descendencia tiene letras en común 
  #' con las palabras posibles y cuantificar esa similitud. 
  
  # comunes3 <- check_common_letters(descendencia1)
  # comunes4 <- check_common_letters(descendencia2)
  
  comunes <- map_dbl(poblacion_nueva, check_common_letters)
  
  max_common <- max(comunes)
  mejor <- poblacion_nueva[which.max(comunes)]
  
  message(paste0("Iteración: ",iter, ". Mejor palabra (", max_common, "): ", mejor))
  
  # Actualizamos la población global
  poblacion <- poblacion_nueva
  
  iter <- iter + 1
  
}




