# Limpiar todos los objetos preexistentes y liberar memoria 
rm(list = ls());
gc();

# Cargar librerias necesarias
library(ClimIndVis)
library(dplyr)
library(tidyr)
library(ncdf4)
library(purrr)

# Abrir archivo NetCDF
nc_file <- ncdf4::nc_open("data/PP_flux_Salta_1991-2023.nc")

# Revisar metadatos del NetCDF.
# Vamos a ver que variables y dimensiones tiene
print(names(nc_file$var))
print(names(nc_file$dim))

# Tiene 2 variables: Precipitación (Precipitation_flux) y CRS (esta no nos interesa)
# Tiene 3 dimensiones, 2 espaciales (latitude y longitude) y 1 temporal (time)
# Vamos a analizar un poco más en detalle la precipitación. Nos interesa principalmente
# saber si son datos de PP diaria o acumulada y también saber las unidades.
# El importante saber que ClimIndVis trabaja con datos de PP diaria no acumulada y valores en mm.
print(nc_file$var$Precipitation_flux)

# Como esta información no está, o bien podemos consultar el sitio desde el cual fue descargado
# o analizar un poco más la información. Para ello, obtenemos todos los valores de PP.
prec <- ncdf4::ncvar_get(nc_file, varid = "Precipitation_flux")

# Veamos las dimensiones que tiene.
print(names(nc_file$dim))
dim(prec)

# Tiene 70 longitudes, 60 latitudes y 12053 fechas. Analicemos los datos de PP
# para la primer celda de la grilla, mediante una serie temporal. Sumemos también algunas
# medidas de posición.
pp_analisis <- prec[30,25,]
plot(pp_analisis)
summary(pp_analisis)

# De lo observado se concluye que son valores de PP diaria y en mm. Seguimos adelante.
# Obtenemos las fechas, latitudes y longitudes. Para trabajar con ClimIndVis se requiere que
# todas las dimensiones estén ordenadas de forma ascendente.

# Las fechas son días desde el 1 de Enero de 1970, así que están correctamente ordenadas.
# Obtenemos los datos y los pasamos a formato de fecha. Efectivamente, la serie comienza en 1991,
# tal como lo indica el archivo y finaliza el 31 de diciembre de 2023.
ncdf4_time <- ncvar_get(nc_file, varid = "time")
fechas <- as.Date(ncdf4_time, origin = "1970-01-01", tz = "UTC")
head(fechas)
tail(fechas)

# Seguimos con las longitudes y latitudes.
longitude <- ncvar_get(nc_file, varid = "longitude")
head(longitude)
latitude <- ncvar_get(nc_file, varid = "latitude")
head(latitude)

# Haciendo una inspección, se observa que las latitudes no están ordenadas
# de modo ascendente. Por tal motivo, hay que ordenar las latitudes. A raíz de esto, también
# hay que reordenar los valores de PP
latitude <- latitude[length(latitude):1]
head(latitude)
prec <- prec[,length(latitude):1,]

# Anteúltimo paso: definir la lista correspondiente al parámetro "data_info"
# que es necesario para crear el objeto ClimIndVis.
data_info <- list(
  data_name = "Datos grillados de precipitación de Salta", # Nombre del conjunto de datos
  info = "Datos de reanalisis de precipitación",  # Descripción
  type = "grid", # Tipo de datos: "grid" para datos grillados
  date_format = "t1d",  # Formato de fecha
  pnames = c("Salta")
)

# Creación de objeto ClimIndVis
climindvis_grid <- ClimIndVis::make_object(
  prec = prec,
  dates_prec = fechas,
  lon = longitude,
  lat = latitude,
  data_info = data_info
)

# Guardamos el objeto para utilizarlos más adelante.
save(climindvis_grid, file = "data/ClimIndVis_Grid.RData")
