### This script is intended to read hindcasts from Climate Data Store into a 
### ClimIndVis object and make different plots using autoplot_* functions

# Para obtener una guía paso a paso sobre cómo descargar los datos de previsión del Climate Data Store (CDS), 
# consulte el manual proporcionado en el curso de Moodle 

# Cargar librerías
library(ncdf4)
library(ClimIndVis)
library(abind)

# Función para crear un objeto ClimIndVis a partir de hindcasts/forecasts del CDS 
# Este ejemplo es sólo para datos de precipitación diaria 

# data_info debe tener el siguiente formato: list(type="", date_format="", data_name="",fmon="")
# Para más información, consulte aquí: https://rdrr.io/github/Climandes/ClimIndVis/man/make_object.html

process_nc_file_precip_CDS <- function(nc_file_path, data_info, factor = 1, diff = FALSE) {
  # Abrir el archivo NetCDF
  nc_file <- nc_open(nc_file_path)
  
  # Extraer la precipitación y el tiempo
  realizations <- ncvar_get(nc_file, varid = "number")
  precip <- ncvar_get(nc_file, varid = "tp")
  valid_time <- ncvar_get(nc_file, varid = "valid_time")
  
  # Convertir valid_time en fechas normalizadas
  valid_time_normal <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC")
  valid_time_normal <- as.Date(valid_time_normal, format = "%Y-%m-%d %Z")
  valid_time_normal <- split(valid_time_normal, format(valid_time_normal, "%Y"))
  
  # Ordenación de la latitud
  if (!all(diff(nc_file$dim$latitude$vals) >= 0)) {
    lat <- nc_file$dim$latitude$vals[length(nc_file$dim$latitude$vals):1]
    #precip <- precip[, length(nc_file$dim$latitude$vals):1, , , ] #,
    precip <- if (length(dim(precip)) == 5) precip[,length(nc_file$dim$latitude$vals):1, , , ] else precip[,length(nc_file$dim$latitude$vals):1, , ]
  } else {
    lat <- nc_file$dim$latitude$vals
  }
  
  # Ordenación de la longitud
  if (!all(diff(nc_file$dim$longitude$vals) >= 0)) {
    lon <- nc_file$dim$longitude$vals[length(nc_file$dim$longitude$vals):1]
    #precip <- precip[length(nc_file$dim$longitude$vals):1, , , , ]
    precip <- if (length(dim(precip)) == 5) precip[,length(nc_file$dim$longitude$vals):1, , ,] else precip[,length(nc_file$dim$longitude$vals):1, , ]
  } else {
    lon <- nc_file$dim$longitude$vals
  }
  
  # LON LAT ENSEMBLE MEMBERS LEADTIME YEAR
  # Ajustar las dimensiones de la precipitación
  if (length(dim(precip)) == 5){
    precip <- aperm(precip, c(1, 2, 5, 3, 4)) 
  } else if ((length(dim(precip)) == 4) && (length(realizations) > 1)) {
    precip <- aperm(precip, c(1, 2, 4, 3)) 
  }
  
  # Añada una quinta dimensión si los datos sólo contienen un año de inicio
  # Si tienen solamente una realizacion, agregarla pero en tercer lugar
  if (length(dim(precip)) != 5) {
    precip <- abind(precip, along = 5)
    if (length(realizations) == 1) {
      precip <- aperm(precip, c(1, 2, 5, 3, 4))
    }
  }
  
  # Aplicar factor si corresponde
  if (factor != 1) {
    precip <- precip * factor
  }
  
  # Aplicar diferencia si corresponde
  if (diff) {
    dims <- dim(precip)
    for(x in seq_len(dims[1])) {
      for(y in seq_len(dims[2])) {
        for(n in seq_len(dims[3])) {
          for (t in seq_len(dims[5])) {
            precip[x, y, n, , t] <- c(precip[x, y, n, 1, t], diff(precip[x, y, n, , t], lag=1))
          }
        }
      }
    }
  }
  
  # Crear objeto ClimIndVis
  climindvis_grid <- make_object(
    prec = precip,
    dates_prec = valid_time_normal,
    lon = lon,
    lat = lat,
    data_info = data_info
  )
  
  # Cerrar el archivo NetCDF
  nc_close(nc_file)
  
  # Mostrar el objeto ClimIndVis
  return(climindvis_grid)
}

process_nc_file_max_min_temp_CDS <- function(nc_file_path, data_info) {
  # Abrir el archivo NetCDF
  nc_file <- nc_open(nc_file_path)
  
  # Extraer la temperatura maxima, miminma y el tiempo
  realizations <- ncvar_get(nc_file, varid = "number")
  maxtemp <- ncvar_get(nc_file, varid = "mx2t24")
  mintemp <- ncvar_get(nc_file, varid = "mn2t24")
  valid_time <- ncvar_get(nc_file, varid = "valid_time")
  
  # Obtener la unidad de las variables
  maxtemp_units <- ncatt_get(nc_file, "mx2t24", "units")$value
  mintemp_units <- ncatt_get(nc_file, "mn2t24", "units")$value
  
  # Convert from Kelvin to Celsius if needed
  if (maxtemp_units == "K") {
    maxtemp <- maxtemp - 273.15
  }
  
  if (mintemp_units == "K") {
    mintemp <- mintemp - 273.15
  }
  
  # Convertir valid_time en fechas normalizadas
  valid_time_normal <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC")
  valid_time_normal <- as.Date(valid_time_normal, format = "%Y-%m-%d %Z")
  valid_time_normal <- split(valid_time_normal, format(valid_time_normal, "%Y"))
  
  # Ordenación de la latitud
  if (!all(diff(nc_file$dim$latitude$vals) >= 0)) {
    lat <- nc_file$dim$latitude$vals[length(nc_file$dim$latitude$vals):1]
    #precip <- precip[, length(nc_file$dim$latitude$vals):1, , , ] #,
    maxtemp <- if (length(dim(maxtemp)) == 5) maxtemp[,length(nc_file$dim$latitude$vals):1, , , ] else maxtemp[,length(nc_file$dim$latitude$vals):1, , ]
    mintemp <- if (length(dim(mintemp)) == 5) mintemp[,length(nc_file$dim$latitude$vals):1, , , ] else mintemp[,length(nc_file$dim$latitude$vals):1, , ]
  } else {
    lat <- nc_file$dim$latitude$vals
  }
  
  # Ordenación de la longitud
  if (!all(diff(nc_file$dim$longitude$vals) >= 0)) {
    lon <- nc_file$dim$longitude$vals[length(nc_file$dim$longitude$vals):1]
    #precip <- precip[length(nc_file$dim$longitude$vals):1, , , , ]
    maxtemp <- if (length(dim(maxtemp)) == 5) maxtemp[,length(nc_file$dim$longitude$vals):1, , ,] else maxtemp[,length(nc_file$dim$longitude$vals):1, , ]
    mintemp <- if (length(dim(mintemp)) == 5) mintemp[,length(nc_file$dim$longitude$vals):1, , ,] else mintemp[,length(nc_file$dim$longitude$vals):1, , ]
  } else {
    lon <- nc_file$dim$longitude$vals
  }
  
  # LON LAT ENSEMBLE MEMBERS LEADTIME YEAR
  # Ajustar las dimensiones de la precipitación
  if (length(dim(maxtemp)) == 5){
    maxtemp <- aperm(maxtemp, c(1, 2, 5, 3, 4))
    mintemp <- aperm(mintemp, c(1, 2, 5, 3, 4))
  } else if (length(dim(maxtemp)) == 4){
    maxtemp <- aperm(maxtemp, c(1, 2, 4, 3))
    mintemp <- aperm(mintemp, c(1, 2, 4, 3)) 
  }
  
  
  # Añada una quinta dimensión si los datos sólo contienen un año de inicio
  if (length(dim(maxtemp)) != 5) {
    maxtemp <- abind(maxtemp, along = 5)
    mintemp <- abind(mintemp, along = 5)
  }
  # Añada una quinta dimensión si los datos sólo contienen un año de inicio
  # Si tienen solamente una realizacion, agregarla pero en tercer lugar
  if (length(dim(precip)) != 5) {
    maxtemp <- abind(maxtemp, along = 5)
    mintemp <- abind(mintemp, along = 5)
    if (length(realizations) == 1){
      maxtemp <- aperm(maxtemp, c(1, 2, 5, 3, 4))
      mintemp <- aperm(mintemp, c(1, 2, 5, 3, 4))
    }
  }
  
  # Crear objeto ClimIndVis
  climindvis_grid <- make_object(
    tmin = mintemp,
    tmax = maxtemp,
    dates_tmin = valid_time_normal,
    dates_tmax = valid_time_normal,
    lon = lon,
    lat = lat,
    data_info = data_info
  )
  
  # Cerrar el archivo NetCDF
  nc_close(nc_file)
  
  # Mostrar el objeto ClimIndVis
  return(climindvis_grid)
}


### Crear Objeto ClimIndVis Hindcast ###

# Definir data_info
# Asegúrese de que "type" incluye "hc" o "fc", ya que las funciones de autoplot buscan por eso
data_info <- list(type="grid_hc", date_format="t2d", data_name="ECMWF hc",fmon="01")


climindvis_grid_prcp_hindcast <- process_nc_file_precip_CDS(nc_file_path = "../ENANDES_CLIMA/Hindcast Data/hindcasts_ecuador_precip_1981_1990_janfebmar.nc",
                                                            data_info = data_info)

climindvis_grid_tmax_tmin_hindcast <- process_nc_file_max_min_temp_CDS(nc_file_path = "Hindcast Data/hindcast_max_min_temp_Argentina_jan_1981_1990.nc",
                                                                       data_info = data_info)

### Crear Objeto ClimIndVis Forecast ### 

# Definir data_info
# Asegúrese de que "type" incluye "hc" o "fc", ya que las funciones de autoplot buscan por eso
data_info <- list(type="grid_fc", date_format="t2d", data_name="ECMWF fc",fmon="01")

climindvis_grid_prcp_forecast <- process_nc_file_precip_CDS(nc_file_path = "Hindcast Data/forecast_ecuador_janfebmar_2024.nc",
                                                            data_info = data_info)

climindvis_grid_tmax_tmin_forecast <- process_nc_file_max_min_temp_CDS(nc_file_path = "Hindcast Data/forecast_max_min_temp_Argentina_jan_2024.nc", 
                                                                       data_info = data_info)




# Usar la función autoplot 
#autoplot_forecast_map(fc_grid = climindvis_grid_forecast, hc_grid = climindvis_grid_hindcast, index ="dd", index_args = list(aggt = "seasonal"), 
                      #selyears = 1981:1990, 
                      #plotdir = "/home/zue/users/wer/R/ENANDES_CLIMA/", output = "png")

autoplot_forecast_map(fc_grid = climindvis_grid_prcp_forecast, hc_grid = climindvis_grid_prcp_hindcast, index ="dd", index_args = list(aggt = "other", aggmons = c(4:7)),
                      plotdir = "/home/zue/users/wer/R/ENANDES_CLIMA/Workshop2/", output = "png")
autoplot_forecast_map(fc_grid = climindvis_grid_prcp_forecast, hc_grid = climindvis_grid_prcp_hindcast, index ="dd", index_args = list(aggt = "seasonal"),
                      plotdir = "/home/zue/users/wer/R/ENANDES_CLIMA/Workshop2/", output = "png")

#### temperature indices ###
# coldest daily minimum temperature 
autoplot_forecast_map(fc_grid = climindvis_grid_tmax_tmin_forecast, hc_grid = climindvis_grid_tmax_tmin_hindcast, index ="tnn", index_args = list(aggt = "other",aggmons = c(4:7)),
                      plotdir = "/home/zue/users/wer/R/ENANDES_CLIMA/Workshop2/", output = "png")
# coldest daily maximum temperature 
autoplot_forecast_map(fc_grid = climindvis_grid_tmax_tmin_forecast, hc_grid = climindvis_grid_tmax_tmin_hindcast, index ="txn", index_args = list(aggt = "other",aggmons = c(4:7)))
# hottest daily maximum temperature 
autoplot_forecast_map(fc_grid = climindvis_grid_tmax_tmin_forecast, hc_grid = climindvis_grid_tmax_tmin_hindcast, index ="txx", index_args = list(aggt = "other",aggmons = c(4:7)))


autoplot_forecast_map(fc_grid = climindvis_grid_tmax_tmin_forecast, hc_grid = climindvis_grid_tmax_tmin_hindcast, index ="th", index_args = list(aggt = "other",aggmons = c(4:7), th = 5, thvar = "tmin", op = ">"))

autoplot_forecast_map(fc_grid = climindvis_grid_tmax_tmin_forecast, hc_grid = climindvis_grid_tmax_tmin_hindcast, index ="tx90p", index_args = list(aggt = "other",aggmons = c(4:7)))

autoplot_forecast_map(fc_grid = climindvis_grid_tmax_tmin_forecast, hc_grid = climindvis_grid_tmax_tmin_hindcast, index ="mean", index_args = list(aggt = "other",aggmons = c(4:7), var="tmax"))

autoplot_forecast_map(fc_grid = climindvis_grid_tmax_tmin_forecast, hc_grid = climindvis_grid_tmax_tmin_hindcast, index ="fd", index_args = list(aggt = "seasonal"))






