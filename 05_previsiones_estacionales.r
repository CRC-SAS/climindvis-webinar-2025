### This script is intended to read hindcasts from Climate Data Store into a 
### ClimIndVis object and make different plots using autoplot_* functions

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
  valid_time_normal <- as.Date(valid_time_normal, format = "%Y-%m-%d %Z")
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
  
  # Crear objeto ClimIndVis with available data
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


process_nc_file_precip_CDS_points <- function(nc_file_path, data_info, metadatos, factor = 1, diff = FALSE) {
  # Validate metadatos input
  if (!all(c("longitude", "latitude") %in% names(metadatos))) {
    stop("metadatos must contain 'longitude' and 'latitude' columns")
  }
  
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
  
  # Get original lat/lon from file
  lat_orig <- nc_file$dim$latitude$vals
  lon_orig <- nc_file$dim$longitude$vals
  
  # Ordenación de la latitud
  if (!all(diff(lat_orig) >= 0)) {
    lat <- lat_orig[length(lat_orig):1]
    precip <- if (length(dim(precip)) == 5) precip[,length(lat_orig):1, , , ] else precip[,length(lat_orig):1, , ]
  } else {
    lat <- lat_orig
  }
  
  # Ordenación de la longitud
  if (!all(diff(lon_orig) >= 0)) {
    lon <- lon_orig[length(lon_orig):1]
    precip <- if (length(dim(precip)) == 5) precip[,length(lon_orig):1, , ,] else precip[,length(lon_orig):1, , ]
  } else {
    lon <- lon_orig
  }
  
  # LON LAT ENSEMBLE MEMBERS LEADTIME YEAR
  # Ajustar las dimensiones de la precipitación
  if (length(dim(precip)) == 5){
    precip <- aperm(precip, c(1, 2, 5, 3, 4))
  } else if ((length(dim(precip)) == 4) && (length(realizations) > 1)) {
    precip <- aperm(precip, c(1, 2, 4, 3))
  }
  
  # Añada una quinta dimensión si los datos sólo contienen un año de inicio
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
  
  # Extract data at specific points
  n_points <- nrow(metadatos)
  
  # Find nearest grid points for each station
  nearest_indices <- matrix(NA, nrow = n_points, ncol = 2)
  colnames(nearest_indices) <- c("lon_idx", "lat_idx")
  
  # Store actual grid coordinates for printing
  grid_coords <- data.frame(
    grid_lon = numeric(n_points),
    grid_lat = numeric(n_points)
  )
  
  for (i in 1:n_points) {
    # Find nearest longitude index
    lon_diff <- abs(lon - metadatos$longitude[i])
    nearest_indices[i, "lon_idx"] <- which.min(lon_diff)
    grid_coords$grid_lon[i] <- lon[nearest_indices[i, "lon_idx"]]
    
    # Find nearest latitude index
    lat_diff <- abs(lat - metadatos$latitude[i])
    nearest_indices[i, "lat_idx"] <- which.min(lat_diff)
    grid_coords$grid_lat[i] <- lat[nearest_indices[i, "lat_idx"]]
  }
  
  # Print coordinate information
  cat("Coordinate mapping:\n")
  cat("==================\n")
  for (i in 1:n_points) {
    cat(sprintf("Point %d:\n", i))
    cat(sprintf("  Requested (station): lon = %.4f, lat = %.4f\n", 
                metadatos$longitude[i], metadatos$latitude[i]))
    cat(sprintf("  Nearest grid point:  lon = %.4f, lat = %.4f\anantml", 
                grid_coords$grid_lon[i], grid_coords$grid_lat[i]))
    cat(sprintf("  Distance: %.4f degrees\n", 
                sqrt((metadatos$longitude[i] - grid_coords$grid_lon[i])^2 + 
                       (metadatos$latitude[i] - grid_coords$grid_lat[i])^2)))
    cat("\n")
  }
  
  # Extract precipitation data for the selected points
  # New dimensions: POINTS x ENSEMBLE x LEADTIME x YEAR
  dims_original <- dim(precip)
  precip_points <- array(NA, dim = c(n_points, dims_original[3], dims_original[4], dims_original[5]))
  
  for (i in 1:n_points) {
    precip_points[i, , , ] <- precip[nearest_indices[i, "lon_idx"], 
                                     nearest_indices[i, "lat_idx"], , , ]
  }
  
  # Replace coordinates with exact station coordinates
  # This ensures autoplot functions work correctly
  point_lons <- metadatos$longitude
  point_lats <- metadatos$latitude
  
  # Create point ClimIndVis object
  climindvis_points <- make_object(
    prec = precip_points,
    dates_prec = valid_time_normal,
    lon = point_lons,
    lat = point_lats,
    data_info = data_info
  )
  
  # Cerrar el archivo NetCDF
  nc_close(nc_file)
  
  # Return the point ClimIndVis object
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
climindvis_grid_prcp_forecast <- process_nc_file_precip_CDS(nc_file_path = "data/seasonal_forecast_ECMWF_jan2025_ARG.nc",
                                                            data_info = data_info)

# Convertir climindvis grid en un climindvis point objeto 

nombres <- metadatos %>%
  dplyr::arrange(omm_id) %>%
  dplyr::pull(name)

data_info <- list(type="p_fc", date_format="t2d", data_name="ECMWF fc stations",fmon="01",
                  pnames=nombres)
climindvis_point_prcp_forecast <- process_nc_file_precip_CDS_points(nc_file_path = "data/seasonal_forecast_ECMWF_jan2025_ARG.nc", data_info, metadatos, factor = 1, diff = FALSE)


# SPI Forecast
autoplot_forecast_spi(obs_p = climindvis_st, fc_p = climindvis_point_prcp_forecast, index = "spi_forecast", index_args = list(aggt="monthly", timescale=3))



### Más ejemplos  ###

# Forecast Precipitación Argentina Enero 2025 con max lead time
data_info <- list(type="grid_fc", date_format="t2d", data_name="ECMWF fc",fmon="01")
climindvis_grid_prcp_forecast <- process_nc_file_precip_CDS(nc_file_path = "data/seasonal_forecast_ECMWF_jan2025_ARG.nc",
                                                            data_info = data_info)

# Hindcast Precipitación Argentina Enero 1981-2010 con max lead time
data_info <- list(type="grid_hc", date_format="t2d", data_name="ECMWF hc",fmon="01")
climindvis_grid_prcp_hindcast <- process_nc_file_precip_CDS(nc_file_path = "data/hindcast_ECWMF_jan1981_2010_ARG_precip.nc",
                                                            data_info = data_info)

# Ejemplos
autoplot_forecast_map(fc_grid = climindvis_grid_prcp_forecast, hc_grid = climindvis_grid_prcp_hindcast, index ="dd", 
                      index_args = list(aggt = "seasonal", selagg = "MAM"))
autoplot_forecast_map(fc_grid = climindvis_grid_prcp_forecast, hc_grid = climindvis_grid_prcp_hindcast, index ="dd", 
                      index_args = list(aggt = "other", aggmons = c(1:3)))


# Forecast Temperatura (tmax,tmin) Argentina Enero 2025
data_info <- list(type="grid_fc", date_format="t2d", data_name="ECMWF fc",fmon="01")
climindvis_grid_temp_forecast1 <- process_nc_file_max_min_temp_CDS(nc_file_path = "data/seasonal_forecast_ECMWF_jan2025_ARG.nc",
                                                                  data_info = data_info)

# Hindcast Temperatura (solamente tmax) Argentina Enero 1981-2010
data_info <- list(type="grid_hc", date_format="t2d", data_name="ECMWF hc",fmon="01")
climindvis_grid_tmax_hindcast <- process_nc_file_max_min_temp_CDS(nc_file_path = "data/hindcast_ECMWF_jan1981_2010_ARG_tmax.nc",
                                                              data_info = data_info)

# Forecast Map WSDI 
autoplot_forecast_map(fc_grid = climindvis_grid_temp_forecast, hc_grid = climindvis_grid_tmax_hindcast, index ="wsdi", 
                      index_args = list(aggt = "seasonal", selagg = "MAM"),
                      output = "png", plotdir = "../ENANDES_CLIMA/Workshop2/") # Ajusta esto si quieres guardar el gráfico con tu directorio

