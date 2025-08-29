### Este script está diseñado para leer Forecasts/Hindcasts del Climate Data Store y crear  
### objetos de ClimIndVis y crear diferentes gráficos utilizando las funciones autoplot_*

# Para obtener una guía paso a paso sobre cómo descargar los datos de previsión del Climate Data Store (CDS), 
# consulte el manual proporcionado en el curso de Moodle 

# Cargar librerías
library(ncdf4)
library(ClimIndVis)
library(abind)
library(dplyr)

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
  valid_time_normal <- as.Date(valid_time_normal, format = "%Y-%m-%d %Z") - lubridate::days(1)
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
  
  # Obtener la lista de variables del archivo
  var_names <- names(nc_file$var)
  
  # Comprueba qué variables de temperatura están presentes
  has_tmax <- "mx2t24" %in% var_names
  has_tmin <- "mn2t24" %in% var_names
  
  # Inicializar variables como NULL
  maxtemp <- NULL
  mintemp <- NULL
  maxtemp_units <- NULL
  mintemp_units <- NULL
  
  # Extraiga las realizaciones y valid_time (estas siempre deben estar presentes)
  realizations <- ncvar_get(nc_file, varid = "number")
  valid_time <- ncvar_get(nc_file, varid = "valid_time")
  
  # Extraer las variables de temperatura si existen
  if (has_tmax) {
    maxtemp <- ncvar_get(nc_file, varid = "mx2t24")
    maxtemp_units <- ncatt_get(nc_file, "mx2t24", "units")$value
    
    # Convierta de Kelvin a Celsius si es necesario
    if (maxtemp_units == "K") {
      maxtemp <- maxtemp - 273.15
    }
  }
  
  if (has_tmin) {
    mintemp <- ncvar_get(nc_file, varid = "mn2t24")
    mintemp_units <- ncatt_get(nc_file, "mn2t24", "units")$value
    
    # Convierta de Kelvin a Celsius si es necesario.
    if (mintemp_units == "K") {
      mintemp <- mintemp - 273.15
    }
  }
  
  # Comprueba si existe al menos una variable de temperatura
  if (!has_tmax && !has_tmin) {
    nc_close(nc_file)
    stop("Ni mx2t24 ni mn2t24 se encuentran en el archivo NetCDF.")
  }
  
  # Convertir valid_time en fechas normalizadas
  valid_time_normal <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC")
  valid_time_normal <- as.Date(valid_time_normal, format = "%Y-%m-%d %Z") - lubridate::days(1)
  valid_time_normal <- split(valid_time_normal, format(valid_time_normal, "%Y"))
  
  # Utilice cualquier matriz de temperatura existente para la comprobación de dimensiones
  temp_for_dims <- if (!is.null(maxtemp)) maxtemp else mintemp
  
  # Ordenación de la latitud
  if (!all(diff(nc_file$dim$latitude$vals) >= 0)) {
    lat <- nc_file$dim$latitude$vals[length(nc_file$dim$latitude$vals):1]
    if (!is.null(maxtemp)) {
      maxtemp <- if (length(dim(maxtemp)) == 5) {
        maxtemp[,length(nc_file$dim$latitude$vals):1, , , ]
      } else {
        maxtemp[,length(nc_file$dim$latitude$vals):1, , ]
      }
    }
    if (!is.null(mintemp)) {
      mintemp <- if (length(dim(mintemp)) == 5) {
        mintemp[,length(nc_file$dim$latitude$vals):1, , , ]
      } else {
        mintemp[,length(nc_file$dim$latitude$vals):1, , ]
      }
    }
  } else {
    lat <- nc_file$dim$latitude$vals
  }
  
  # Ordenación de la longitud
  if (!all(diff(nc_file$dim$longitude$vals) >= 0)) {
    lon <- nc_file$dim$longitude$vals[length(nc_file$dim$longitude$vals):1]
    if (!is.null(maxtemp)) {
      maxtemp <- if (length(dim(maxtemp)) == 5) {
        maxtemp[,length(nc_file$dim$longitude$vals):1, , ,]
      } else {
        maxtemp[,length(nc_file$dim$longitude$vals):1, , ]
      }
    }
    if (!is.null(mintemp)) {
      mintemp <- if (length(dim(mintemp)) == 5) {
        mintemp[,length(nc_file$dim$longitude$vals):1, , ,]
      } else {
        mintemp[,length(nc_file$dim$longitude$vals):1, , ]
      }
    }
  } else {
    lon <- nc_file$dim$longitude$vals
  }
  
  # LON LAT ENSEMBLE MEMBERS LEADTIME YEAR
  # Ajustar las dimensiones
  if (!is.null(maxtemp)) {
    if (length(dim(maxtemp)) == 5) {
      maxtemp <- aperm(maxtemp, c(1, 2, 5, 3, 4))
    } else if (length(dim(maxtemp)) == 4) {
      maxtemp <- aperm(maxtemp, c(1, 2, 4, 3))
    }
    
    # Añada una quinta dimensión si los datos sólo contienen un año de inicio
    if (length(dim(maxtemp)) != 5) {
      maxtemp <- abind(maxtemp, along = 5)
      if (length(realizations) == 1) {
        maxtemp <- aperm(maxtemp, c(1, 2, 5, 3, 4))
      }
    }
  }
  
  if (!is.null(mintemp)) {
    if (length(dim(mintemp)) == 5) {
      mintemp <- aperm(mintemp, c(1, 2, 5, 3, 4))
    } else if (length(dim(mintemp)) == 4) {
      mintemp <- aperm(mintemp, c(1, 2, 4, 3))
    }
    
    # Añada una quinta dimensión si los datos sólo contienen un año de inicio
    if (length(dim(mintemp)) != 5) {
      mintemp <- abind(mintemp, along = 5)
      if (length(realizations) == 1) {
        mintemp <- aperm(mintemp, c(1, 2, 5, 3, 4))
      }
    }
  }
  
  # Crear objeto ClimIndVis
  climindvis_grid <- make_object(
    tmin = mintemp,
    tmax = maxtemp,
    dates_tmin = if (!is.null(mintemp)) valid_time_normal else NULL,
    dates_tmax = if (!is.null(maxtemp)) valid_time_normal else NULL,
    lon = lon,
    lat = lat,
    data_info = data_info
  )
  
  # Cerrar el archivo NetCDF
  nc_close(nc_file)
  
  # Mostrar el objeto ClimIndVis
  return(climindvis_grid)
}


process_nc_file_precip_CDS_to_points <- function(nc_file_path, data_info, metadatos, 
                                                 factor = 1, diff = FALSE) {
  # Abrir el archivo NetCDF
  nc_file <- nc_open(nc_file_path)
  
  # Extraer la precipitación y el tiempo
  realizations <- ncvar_get(nc_file, varid = "number")
  precip <- ncvar_get(nc_file, varid = "tp")
  valid_time <- ncvar_get(nc_file, varid = "valid_time")
  
  # Convertir valid_time en fechas normalizadas
  valid_time_normal <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC")
  valid_time_normal <- as.Date(valid_time_normal, format = "%Y-%m-%d %Z") - lubridate::days(1)
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
  
  # 1. Get station name, longitude and latitude.
  station_names <- metadatos %>%
    dplyr::pull(name)
  longitudes <- metadatos %>%
    dplyr::pull(longitude)
  latitudes <- metadatos %>%
    dplyr::pull(latitude)
  
  # 2. Find cell for each station. I assume that latitude and longitude values
  #    correspond to the center of each cell.
  delta_x <- as.numeric(unique(diff(lon))/2)
  delta_y <- as.numeric(unique(diff(lat))/2)
  breaks_longitud <- c(lon - delta_x, max(lon) + delta_x)
  breaks_latitud <- c(lat - delta_y, max(lat) + delta_y)
  celdas <- purrr::pmap(
    .l = list(longitudes, latitudes),
    .f = function(longitud, latitud) {
      x <- cut(x = longitud, breaks = breaks_longitud, labels = FALSE, right = FALSE)
      y <- cut(x = latitud, breaks = breaks_latitud, labels = FALSE, right = FALSE)
      if (! is.na(x) && ! is.na(y)) {
        return (c(x, y))
      } else {
        return (NULL)
      }
    }
  )
  
  # 3. filter out cell outside the bounding box. Redefine list of names, longitudes and latitudes.
  celdas_interiores <- which(! unlist(purrr::map(celdas, is.null)))
  station_names     <- station_names[celdas_interiores]
  longitudes        <- longitudes[celdas_interiores]
  latitudes         <- latitudes[celdas_interiores]
  celdas            <- celdas[celdas_interiores]
  
  # 4. Generar matriz con dimensiones [stations] x [ensemble members] x [forecast days] x [forecast years]
  dimensiones_originales <- dim(precip)
  dimensiones <- c(length(celdas), dimensiones_originales[3:5])
  matriz <- array(data = NA, dim = dimensiones)
  for (i in seq_along(celdas)) {
    celda <- celdas[[i]]
    matriz[i,,,] <- precip[celda[1], celda[2], , , ]
  }
  
  # 4. Generar objeto ClimIndVis
  data_info_st <- list(
    type = "p_fc",
    date_format = data_info$date_format,
    data_name = data_info$data_name,
    fmon = data_info$fmon, 
    pnames = station_names
  )
  climindvis_st <- make_object(
    prec = matriz,
    dates_prec = valid_time_normal,
    lon = longitudes,
    lat = latitudes,
    data_info = data_info_st
  )
  
  # Cerrar el archivo NetCDF
  nc_close(nc_file)
  
  # Mostrar el objeto ClimIndVis
  return(climindvis_st)
}

process_nc_file_max_min_temp_CDS_points <- function(nc_file_path, data_info, metadatos) {
  # Validar la entrada de metadatos
  if (!all(c("longitude", "latitude") %in% names(metadatos))) {
    stop("Los metadatos deben contener las columnas «longitude» y «latitude».")
  }
  
  # Abrir el archivo NetCDF
  nc_file <- nc_open(nc_file_path)
  
  # Obtener la lista de variables del archivo
  var_names <- names(nc_file$var)
  
  # Comprueba qué variables de temperatura están presentes
  has_tmax <- "mx2t24" %in% var_names
  has_tmin <- "mn2t24" %in% var_names
  
  # Inicializar variables
  maxtemp <- NULL
  mintemp <- NULL
  maxtemp_units <- NULL
  mintemp_units <- NULL
  
  # Extraiga las realizaciones y valid_time
  realizations <- ncvar_get(nc_file, varid = "number")
  valid_time <- ncvar_get(nc_file, varid = "valid_time")
  
  # Extraer Tmax
  if (has_tmax) {
    maxtemp <- ncvar_get(nc_file, varid = "mx2t24")
    maxtemp_units <- ncatt_get(nc_file, "mx2t24", "units")$value
    if (maxtemp_units == "K") {
      maxtemp <- maxtemp - 273.15
    }
  }
  
  # Extraer Tmin
  if (has_tmin) {
    mintemp <- ncvar_get(nc_file, varid = "mn2t24")
    mintemp_units <- ncatt_get(nc_file, "mn2t24", "units")$value
    if (mintemp_units == "K") {
      mintemp <- mintemp - 273.15
    }
  }
  
  # Comprobar que haya al menos una variable
  if (!has_tmax && !has_tmin) {
    nc_close(nc_file)
    stop("Ni mx2t24 ni mn2t24 se encuentran en el archivo NetCDF.")
  }
  
  # Convertir valid_time en fechas normalizadas
  valid_time_normal <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC")
  valid_time_normal <- as.Date(valid_time_normal, format = "%Y-%m-%d %Z")
  valid_time_normal <- split(valid_time_normal, format(valid_time_normal, "%Y"))
  
  # Obtener lat/lon originales
  lat_orig <- nc_file$dim$latitude$vals
  lon_orig <- nc_file$dim$longitude$vals
  
  # Ordenación de latitudes
  if (!all(diff(lat_orig) >= 0)) {
    lat <- lat_orig[length(lat_orig):1]
    if (!is.null(maxtemp)) {
      maxtemp <- if (length(dim(maxtemp)) == 5) maxtemp[,length(lat_orig):1, , , ] else maxtemp[,length(lat_orig):1, , ]
    }
    if (!is.null(mintemp)) {
      mintemp <- if (length(dim(mintemp)) == 5) mintemp[,length(lat_orig):1, , , ] else mintemp[,length(lat_orig):1, , ]
    }
  } else {
    lat <- lat_orig
  }
  
  # Ordenación de longitudes
  if (!all(diff(lon_orig) >= 0)) {
    lon <- lon_orig[length(lon_orig):1]
    if (!is.null(maxtemp)) {
      maxtemp <- if (length(dim(maxtemp)) == 5) maxtemp[,length(lon_orig):1, , ,] else maxtemp[,length(lon_orig):1, , ]
    }
    if (!is.null(mintemp)) {
      mintemp <- if (length(dim(mintemp)) == 5) mintemp[,length(lon_orig):1, , ,] else mintemp[,length(lon_orig):1, , ]
    }
  } else {
    lon <- lon_orig
  }
  
  # Ajuste de dimensiones
  adjust_dims <- function(var, realizations) {
    if (is.null(var)) return(NULL)
    if (length(dim(var)) == 5) {
      var <- aperm(var, c(1, 2, 5, 3, 4))
    } else if (length(dim(var)) == 4) {
      var <- aperm(var, c(1, 2, 4, 3))
    }
    if (length(dim(var)) != 5) {
      var <- abind(var, along = 5)
      if (length(realizations) == 1) {
        var <- aperm(var, c(1, 2, 5, 3, 4))
      }
    }
    return(var)
  }
  
  maxtemp <- adjust_dims(maxtemp, realizations)
  mintemp <- adjust_dims(mintemp, realizations)
  
  # Extraer datos en puntos específicos
  n_points <- nrow(metadatos)
  nearest_indices <- matrix(NA, nrow = n_points, ncol = 2)
  colnames(nearest_indices) <- c("lon_idx", "lat_idx")
  
  grid_coords <- data.frame(grid_lon = numeric(n_points), grid_lat = numeric(n_points))
  
  for (i in 1:n_points) {
    # Longitud más cercana
    lon_diff <- abs(lon - metadatos$longitude[i])
    nearest_indices[i, "lon_idx"] <- which.min(lon_diff)
    grid_coords$grid_lon[i] <- lon[nearest_indices[i, "lon_idx"]]
    
    # Latitud más cercana
    lat_diff <- abs(lat - metadatos$latitude[i])
    nearest_indices[i, "lat_idx"] <- which.min(lat_diff)
    grid_coords$grid_lat[i] <- lat[nearest_indices[i, "lat_idx"]]
  }
  
  # Extraer datos en cada punto
  dims_original <- if (!is.null(maxtemp)) dim(maxtemp) else dim(mintemp)
  
  maxtemp_points <- if (!is.null(maxtemp)) {
    array(NA, dim = c(n_points, dims_original[3], dims_original[4], dims_original[5]))
  } else NULL
  
  mintemp_points <- if (!is.null(mintemp)) {
    array(NA, dim = c(n_points, dims_original[3], dims_original[4], dims_original[5]))
  } else NULL
  
  for (i in 1:n_points) {
    if (!is.null(maxtemp)) {
      maxtemp_points[i, , , ] <- maxtemp[nearest_indices[i, "lon_idx"],
                                         nearest_indices[i, "lat_idx"], , , ]
    }
    if (!is.null(mintemp)) {
      mintemp_points[i, , , ] <- mintemp[nearest_indices[i, "lon_idx"],
                                         nearest_indices[i, "lat_idx"], , , ]
    }
  }
  
  # Usar coordenadas reales de la estación
  point_lons <- metadatos$longitude
  point_lats <- metadatos$latitude
  
  # Crear objeto ClimIndVis (puntos)
  climindvis_points <- make_object(
    tmin = mintemp_points,
    tmax = maxtemp_points,
    dates_tmin = if (!is.null(mintemp_points)) valid_time_normal else NULL,
    dates_tmax = if (!is.null(maxtemp_points)) valid_time_normal else NULL,
    lon = point_lons,
    lat = point_lats,
    data_info = data_info
  )
  
  # Cerrar NetCDF
  nc_close(nc_file)
  
  return(climindvis_points)
}




### Ejemplo SPI Forecast usando ClimIndVis_Stations.RData ###

# Datos del 01_ingestion_datos_puntuales.r
load("data/ClimIndVis_Stations.RData")

# Prueba rápida del anomaly time series del SPI
autoplot_anomaly_ts(climindvis_st, index = "spi",
                    index_args = list(aggt = "monthly"),
                    ts_type = "single_ts", pcols = "royalblue")

# Datos del forecast
# Definir data_info
data_info <- list(type="grid_fc", date_format="t2d", data_name="ECMWF fc",fmon="01")
climindvis_grid_prcp_forecast <- process_nc_file_precip_CDS(nc_file_path = "data/forecasts/seasonal_forecast_ECMWF_jan2025_ARG.nc",
                                                            data_info = data_info)

nombres <- metadatos %>%
  dplyr::arrange(omm_id) %>%
  dplyr::pull(name)

data_info <- list(type="p_fc", date_format="t2d", data_name="ECMWF fc stations",fmon="01",
                  pnames=nombres)
climindvis_point_prcp_forecast <- process_nc_file_precip_CDS_to_points(nc_file_path = "data/forecasts/seasonal_forecast_ECMWF_jan2025_ARG.nc", data_info, 
                                                                    metadatos, factor = 1, diff = FALSE)


# SPI Forecast 
autoplot_forecast_spi(obs_p = climindvis_st, fc_p = climindvis_point_prcp_forecast, index = "spi_forecast", index_args = list(aggt="monthly", timescale=6),
                      plotstart = 2015, output = "png", plotdir = "TUDIRECTORIO",plotname = "FINALTEST")


### Ejemplo Forecast Map ###

# Forecast Precipitación Argentina Enero 2025 con max lead time
data_info <- list(type="grid_fc", date_format="t2d", data_name="ECMWF fc",fmon="01")
climindvis_grid_prcp_forecast <- process_nc_file_precip_CDS(nc_file_path = "data/forecasts/seasonal_forecast_ECMWF_jan2025_ARG.nc",
                                                            data_info = data_info)

# Hindcast Precipitación Argentina Enero 1981-2010 con max lead time
data_info <- list(type="grid_hc", date_format="t2d", data_name="ECMWF hc",fmon="01")
climindvis_grid_prcp_hindcast <- process_nc_file_precip_CDS(nc_file_path = "data/hindcasts/hindcast_ECWMF_jan1981_2010_ARG_precip.nc",
                                                            data_info = data_info)

# Ejemplo días secos (dd)
autoplot_forecast_map(fc_grid = climindvis_grid_prcp_forecast, hc_grid = climindvis_grid_prcp_hindcast, index ="dd", 
                      index_args = list(aggt = "seasonal", selagg = "MAM"))
autoplot_forecast_map(fc_grid = climindvis_grid_prcp_forecast, hc_grid = climindvis_grid_prcp_hindcast, index ="dd", 
                      index_args = list(aggt = "other", aggmons = c(1:3)))


# Forecast Temperatura (tmax,tmin) Argentina Enero 2025
data_info <- list(type="grid_fc", date_format="t2d", data_name="ECMWF fc",fmon="01")
climindvis_grid_temp_forecast <- process_nc_file_max_min_temp_CDS(nc_file_path = "data/forecasts/seasonal_forecast_ECMWF_jan2025_ARG.nc",
                                                                  data_info = data_info)

# Hindcast Temperatura (solamente tmax) Argentina Enero 1981-2010
data_info <- list(type="grid_hc", date_format="t2d", data_name="ECMWF hc",fmon="01")
climindvis_grid_tmax_hindcast <- process_nc_file_max_min_temp_CDS(nc_file_path = "data/hindcasts/hindcast_ECMWF_jan1981_2010_ARG_tmax.nc",
                                                              data_info = data_info)

# Ejemplo duración de períodos cálídos (wsdi) 
autoplot_forecast_map(fc_grid = climindvis_grid_temp_forecast, hc_grid = climindvis_grid_tmax_hindcast, index ="wsdi", 
                      index_args = list(aggt = "seasonal", selagg = "MAM"))


### Ejemplo Forecast Station ###

# Datos del 01_ingestion_datos_puntuales.r
load("data/ClimIndVis_Stations.RData")

nombres <- metadatos %>%
  dplyr::arrange(omm_id) %>%
  dplyr::pull(name)

# Objeto ClimIndVis point Forecast Precipitación
data_info <- list(type="p_fc", date_format="t2d", data_name="ECMWF fc stations",fmon="01",
                  pnames=nombres)
climindvis_point_prcp_forecast <- process_nc_file_precip_CDS_to_points(nc_file_path = "data/forecasts/seasonal_forecast_ECMWF_jan2025_ARG.nc", data_info, 
                                                                    metadatos, factor = 1, diff = FALSE)

# Objeto ClimIndVis point Hindcast Precipitación
data_info <- list(type="p_hc", date_format="t2d", data_name="ECMWF hc stations",fmon="01",
                  pnames=nombres)
climindvis_point_prcp_hindcast <- process_nc_file_precip_CDS_to_points(nc_file_path = "data/hindcasts/hindcast_ECWMF_jan1981_2010_ARG_precip.nc", data_info, 
                                                                    metadatos, factor = 1, diff = FALSE)

# Ejemplo días secos (dd)
autoplot_forecast_stations(
  fc_p = climindvis_point_prcp_forecast, hc_p = climindvis_point_prcp_hindcast, obs_p = climindvis_st,
  index = "dd",  index_args = list(aggt = "other", aggmons = c(1:3)),
  verify = FALSE)


# Ejemplo Temparatura 

# ClimIndVis objeto point Forecast Temperatura (tmax,tmin) Argentina Enero 2025
data_info <- list(type="p_fc", date_format="t2d", data_name="ECMWF fc stations",fmon="01",
                  pnames=nombres)
climindvis_point_temp_forecast <- process_nc_file_max_min_temp_CDS_points(nc_file_path = "data/forecasts/seasonal_forecast_ECMWF_jan2025_ARG.nc", data_info, 
                                                                    metadatos)

# ClimIndVis objeto point Hindcast Temperatura (solamente tmax) Argentina Enero 2025
data_info <- list(type="p_hc", date_format="t2d", data_name="ECMWF hc stations",fmon="01",
                  pnames=nombres)
climindvis_point_tmax_hindcast <- process_nc_file_max_min_temp_CDS_points(nc_file_path = "data/hindcasts/hindcast_ECMWF_jan1981_2010_ARG_tmax.nc", data_info, 
                                                                    metadatos)

# Ejemplo duración de períodos cálídos (wsdi) 
autoplot_forecast_stations(
  fc_p = climindvis_point_temp_forecast, hc_p = climindvis_point_tmax_hindcast, obs_p = climindvis_st,
  index = "wsdi",  index_args = list(aggt = "seasonal", selagg = "MAM"),
  verify = TRUE, plot_climatology = TRUE)

# Ejemplo con verificación
autoplot_forecast_stations(
  fc_p = climindvis_point_temp_forecast, hc_p = climindvis_point_tmax_hindcast, obs_p = climindvis_st,
  index = "wsdi",  index_args = list(aggt = "seasonal", selagg = "MAM"),
  verify = TRUE, veri_metric = "Ens2AFC", plot_climatology = TRUE)




