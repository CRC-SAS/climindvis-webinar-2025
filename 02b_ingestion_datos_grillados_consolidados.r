# Limpiar todos los objetos preexistentes y liberar memoria 
rm(list = ls());
gc();

# Cargar librerias necesarias
library(ClimIndVis)
library(dplyr)
library(tidyr)
library(ncdf4)
library(purrr)

# Abrir archivos NetCDF
nc_file_tmax <- ncdf4::nc_open("data/tmax_AgERA5_puna-salta.nc")
nc_file_tmin <- ncdf4::nc_open("data/tmin_AgERA5_puna-salta.nc")

# Revisar metadatos de los NetCDF. Vamos a ver que variables y dimensiones tienen.
print(names(nc_file_tmax$var))
print(names(nc_file_tmax$dim))
print(names(nc_file_tmin$var))
print(names(nc_file_tmin$dim))

# Tienen 2 variables: una de temperatura (mínima o máxima) y CRS (esta no nos interesa)
# Tienen 3 dimensiones, 2 espaciales (latitude y longitude) y 1 temporal (time)
# Vamos a analizar un poco más en detalle la temperatura Nos interesa principalmente
# saber si están en grados Celsius o en otra unidad.
print(nc_file_tmax$var$tmax)
print(nc_file_tmin$var$tmin)

# Como esta información no está, o bien podemos consultar el sitio donde fue descargado
# o analizar un poco más la información. Para ello, obtenemos todos los valores de tmax y tmin
tmax <- ncdf4::ncvar_get(nc_file_tmax, varid = "tmax")
tmin <- ncdf4::ncvar_get(nc_file_tmin, varid = "tmin")

# Veamos las dimensiones que tiene.
print(names(nc_file_tmax$dim))
print(names(nc_file_tmin$dim))
dim(tmax)
dim(tmin)

# Tiene 70 longitudes, 60 latitudes y 5113 fechas. Analicemos los datos de PP
# para una celda cualquiera de la grilla, mediante una serie temporal. Sumemos también algunas
# medidas de posición.
tmax_analisis <- tmax[30,25,]
plot(tmax_analisis)
summary(tmax_analisis)
tmin_analisis <- tmin[30,25,]
plot(tmin_analisis)
summary(tmin_analisis)

# De lo observado se concluye que son valores de temperatura en grados Kelvin. 
# Para convertirlas a grados Celsius, restamos 273.15 y seguimos adelante.
tmax <- tmax - 273.15
tmin <- tmin - 273.15

# Obtenemos las fechas, latitudes y longitudes. Para trabajar con ClimIndVis se requiere que
# todas las dimensiones estén ordenadas de forma ascendente.

# Las fechas son días desde el 1 de Enero de 1970, así que están correctamente ordenadas.
# Obtenemos los datos y los pasamos a formato de fecha.
ncdf4_time_tmax <- ncvar_get(nc_file_tmax, varid = "time")
ncdf4_time_tmin <- ncvar_get(nc_file_tmin, varid = "time")
fechas_tmax <- as.Date(ncdf4_time_tmax, origin = "1970-01-01", tz = "UTC")
fechas_tmin <- as.Date(ncdf4_time_tmax, origin = "1970-01-01", tz = "UTC")
range(fechas_tmax)
range(fechas_tmin)

# En ambos casos, la serie comienza el 1 de Enero de 2010 y termina el 31 de Diciembre de 2023.
# Seguimos con las longitudes y latitudes.
longitude_tmax <- ncvar_get(nc_file_tmax, varid = "longitude")
longitude_tmin <- ncvar_get(nc_file_tmin, varid = "longitude")
head(longitude_tmax)
head(longitude_tmin)
range(longitude_tmax)
range(longitude_tmin)
latitude_tmax <- ncvar_get(nc_file_tmax, varid = "latitude")
latitude_tmin <- ncvar_get(nc_file_tmin, varid = "latitude")
head(latitude_tmax)
head(latitude_tmin)
range(latitude_tmax)
range(latitude_tmin)

# Haciendo una inspección, se observa que ambos NetCDF cubren el mismo bounding box.
# sin embargo, las latitudes no están ordenadas de modo ascendente. 
# Por tal motivo, hay que ordenar las latitudes. A raíz de esto, también
# hay que reordenar los valores de tmax y tmin. Vamos a unificar latitudes y longitudes
# ya que son las mismas para ambos casos
latitude <- latitude_tmax
longitude <- longitude_tmax

# Reordenamos la latitud y las variables de tmax y tmin
latitude <- latitude[length(latitude):1]
head(latitude)
tmax <- tmax[,length(latitude):1,]
tmin <- tmin[,length(latitude):1,]

# Como los datos de PP también cubren el mismo bounding box (lo pueden verificar),
# entonces tambiém vamos a leer nuevamente el NetCDF de PP para integrarlo todo en el
# mismo objeto. La diferencia es que la serie temporal de PP es más larga,
# pero eso no reporta ningún problema para ClimIndVis, ya que permite series temporales
# de distinto largo.
nc_file_pp <- ncdf4::nc_open("data/PP_flux_Salta_1991-2023.nc")
prec <- ncdf4::ncvar_get(nc_file_pp, varid = "Precipitation_flux")
ncdf4_time_pp <- ncvar_get(nc_file_pp, varid = "time")
fechas_pp <- as.Date(ncdf4_time_pp, origin = "1970-01-01", tz = "UTC")

# También hay que reordenar los datos de PP en relación a las latitudes
prec <- prec[,length(latitude):1,]

# Anteúltimo paso: definir la lista correspondiente al parámetro "data_info"
# que es necesario para crear el objeto ClimIndVis.
data_info <- list(
  data_name = "Datos grillados de temperatura y precipitación de Salta", # Nombre del conjunto de datos
  info = "Datos de temperatura y precipitación",  # Descripción
  type = "grid", # Tipo de datos: "grid" para datos grillados
  date_format = "t1d",  # Formato de fecha
  pnames = c("Salta")
)

# Creación de objeto ClimIndVis
climindvis_grid <- ClimIndVis::make_object(
  # Variables
  tmax = tmax,
  tmin = tmin,
  prec = prec,
  # Fechas
  dates_tmax = fechas_tmax,
  dates_tmin = fechas_tmin,
  dates_prec = fechas_pp,
  # Latitudes y longitudes
  lon = longitude,
  lat = latitude,
  # Información adicional
  data_info = data_info
)

# Guardamos el objeto para utilizarlos más adelante.
save(climindvis_grid, file = "data/ClimIndVis_Grid.RData")
