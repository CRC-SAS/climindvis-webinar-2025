# Limpiar todos los objetos preexistentes y liberar memoria 
rm(list = ls());
gc();

# Cargar librerias necesarias
library(ClimIndVis)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# Leer los metadatos de las estaciones. Usamos la función "read_delim" del paquete
# "readr". Más adelante se explicará porqué.
metadatos <- readr::read_delim('data/Estaciones.csv', delim = "\t")
head(metadatos)

# Leer los registros de datos. Conviene usar las funciones del paquete "readr"
# porque devuelven un tibble, el cual es un objeto similar a un data.frame, pero 
# que además permite ver el tipo de dato de cada columna. 

# Por ejemplo, acá leemos los datos con read.delim y luego mostramos las 6 primeras filas con "head"
# pero no tenemos información del tipo de dato.
buenos_aires <- read.delim('data/Registros_87585_Buenos_Aires.csv', sep = "\t") %>%
  dplyr::select(omm_id, fecha, tmax, tmin, tmed, prcp)
head(buenos_aires)

# Si ahora hacemos lo mismo con read_delim de readr, nos muestra que las variables 
# tmax y tmed son de tipo character. Esto se debe a que tienen el valor "\N" para indicar
# que hay un faltante para una fecha determinada.
buenos_aires <- readr::read_delim('data/Registros_87585_Buenos_Aires.csv', delim = "\t") %>%
  dplyr::select(omm_id, fecha, tmax, tmin, tmed, prcp)
head(buenos_aires)

# Ahora leemos de nuevo indicando que tome como NAs los valores \N.
# Como se observa, todos los valores son de tipo numérico.
buenos_aires <- readr::read_delim('data/Registros_87585_Buenos_Aires.csv', delim = "\t", na = c("\\N")) %>%
  dplyr::select(omm_id, fecha, tmax, tmin, tmed, prcp)
head(buenos_aires)

# Lo mismo hacemos para Posadas, Tartagel y Bariloche
posadas_aero <- readr::read_delim('data/Registros_87178_Posadas_Aero.csv', delim = "\t", na = c("\\N")) %>%
  dplyr::select(omm_id, fecha, tmax, tmin, tmed, prcp)
head(posadas_aero)

tartagal <- readr::read_delim('data/Registros_87022_Tartagal_Aero.csv', delim = "\t", na = c("\\N")) %>%
  dplyr::select(omm_id, fecha, tmax, tmin, tmed, prcp)
head(tartagal)

bariloche <- readr::read_delim('data/Registros_87765_Bariloche_Aero.csv', delim = "\t", na = c("\\N")) %>%
  dplyr::select(omm_id, fecha, tmax, tmin, tmed, prcp)
head(bariloche)

# Ahora procedemos a consolidar los datos en un solo tibble.
datos <- dplyr::bind_rows(buenos_aires, posadas_aero, tartagal, bariloche)

# Una vez que se tienen los datos consolidados, se necesita definir una única serie
# de fechas para ambas estaciones. Es posible que los rangos no sean los mismos o que haya fechas faltantes.
# Para que no haya inconsistencias, es necesario que la serie de fechas sea única.
# Para ellos, generamos una secuencia de fechas entre la mímina y máxima del tibble consolidado.
fechas <- seq(from = min(datos$fecha), to = max(datos$fecha), by = "day")

# Una vez generada la serie de tiempo final, reconstruimos el tibble consolidado.
# Mediante la función left_join, forzamos a que cada serie de datos tenga exactamente las fechas requeridas.
fechas_df <- tibble::tibble(fecha = fechas)
datos <- dplyr::bind_rows(
  dplyr::left_join(fechas_df, tartagal, by = "fecha"),
  dplyr::left_join(fechas_df, posadas_aero, by = "fecha"),
  dplyr::left_join(fechas_df, buenos_aires, by = "fecha"),
  dplyr::left_join(fechas_df, bariloche, by = "fecha")
)

# Ahora vamos a generar una matriz de datos para cada variable (tmax, tmin, tmed y prcp). 
# Cada matriz debe tener 2 dimensiones (N, M), donde N es la cantidad de estaciones o puntos
# y M es la cantidad de fechas.

# Primero obtendo los ID de las estaciones ordenados
omm_ids <- metadatos %>%
  dplyr::arrange(omm_id) %>%
  dplyr::pull(omm_id)
  
# Ahora armo la matriz para cada variable
variables <- c('tmax', 'tmin', 'tmed', 'prcp')
datos_variables <- purrr::map(
  .x = variables,
  .f = function(variable) {
    # Creo una matriz vacía del tamaño requerido
    matriz_variable <- array(data = NA, dim = c(length(omm_ids), length(fechas)))
    
    for (i in seq_along(omm_ids)) {
      estacion_id <- omm_ids[i]
      matriz_variable[i, ] <- datos %>%
        # Me quedo con los datos de esa estación
        dplyr::filter(omm_id == estacion_id) %>%
        # Y ahora ordeno por fecha para asegurarme que la serie temporal vaya de pasado a futuro
        dplyr::arrange(fecha) %>%
        # Finalmente, selecciono la variable
        dplyr::pull(!! variable)
    }
    
    # Devuelvo la matriz completa
    return(matriz_variable)
  }
)

# Ahora les pongo nombres a cada elemento de la lista (variable correspondiente)
names(datos_variables) <- variables

# También es necesario definir latitudes y longitudes en caso que se necesiten generar mapas.
# Las latitudes y longitudes son vectores, donde cada elemento corresponde a una estación.
# El elemento "k" de cada vector corresponde a la latitud y longitud de la estación "k" en
# el vector omm_ids. La información la extraemos igual que los IDs.
latitudes <- metadatos %>%
  dplyr::arrange(omm_id) %>%
  dplyr::pull(latitude)
longitudes <- metadatos %>%
  dplyr::arrange(omm_id) %>%
  dplyr::pull(longitude)

# Hacemos lo mismo con los nombres
nombres <- metadatos %>%
  dplyr::arrange(omm_id) %>%
  dplyr::pull(name)

# Para poder crear el objeto ClimIndVis, necesitamos definir la lista que le vamos a pasar
# como parámetro "data_info"
data_info <- list(
  type = "p", # Indicamos que son datos puntuales
  date_format = "t1d", # Indicamos que es una serie temporal de 1 dimensión (para hindcasts y forecasts esto es distinto)
  data_name = "Datos puntuales de Tartagal, Posadas, Buenos Aires y Bariloche", # Nombre de nuestro dataset
  pnames = nombres # Nombre de cada punto (estaciones)
)

# Finalmente, creamos el objeto ClimIndVis
climindvis_st <- make_object(
  # Datos de variables tmin, tmax, tmed y prcp
  tmin = datos_variables$tmin,
  tmax = datos_variables$tmax, 
  tavg = datos_variables$tmed, 
  prec = datos_variables$prcp,
  # Serie de fechas (es siempre la misma para cada variable)
  dates_tmin = fechas,
  dates_tmax = fechas, 
  dates_tavg = fechas, 
  dates_prec = fechas,
  # Latitudes y longitudes
  lon = longitudes,
  lat = latitudes,
  # Información adicional
  data_info = data_info
)

# Guardamos los metadatos, los registros y el objeto para utilizarlos más adelante.
save(metadatos, datos, climindvis_st, file = "data/ClimIndVis_Stations.RData")
