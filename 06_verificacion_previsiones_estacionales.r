rm(list = ls())
cat("\014")
graphics.off()

library(dplyr)
library(lubridate)
library(verification)
library(easyVerification)
library(ncdf4)
library(ClimIndVis)
library(abind)
require(here)

# Vamos a usar las funciones: 
# "process_nc_file_max_min_temp_CDS", 
# "process_nc_file_max_min_temp_CDS_points", 

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

### Ejemplos de verificación en el período hindcast: 

# Datos observados:
load("data/ClimIndVis_Stations.RData")

nombres <- metadatos %>%
  dplyr::arrange(omm_id) %>%
  dplyr::pull(name)
print(nombres)

# Ejemplo con temperatura: 

# ClimIndVis objeto point Forecast Temperatura (tmax,tmin) Argentina Enero 2025
data_info <- list(type = "p_fc", date_format = "t2d", 
                  data_name = "ECMWF fc stations", 
                  fmon = "01",
                  pnames = nombres)
climindvis_point_temp_forecast <- process_nc_file_max_min_temp_CDS_points(nc_file_path = "data/forecasts/seasonal_forecast_ECMWF_jan2025_ARG.nc", 
                                                                          data_info, 
                                                                          metadatos)

# ClimIndVis objeto point Hindcast Temperatura (solamente tmax) Argentina Enero 2025
data_info <- list(type = "p_hc", date_format = "t2d", 
                  data_name = "ECMWF hc stations",
                  fmon = "01",
                  pnames = nombres)
climindvis_point_tmax_hindcast <- process_nc_file_max_min_temp_CDS_points(nc_file_path = "data/hindcasts/hindcast_ECMWF_jan1981_2010_ARG_tmax.nc", 
                                                                          data_info, 
                                                                          metadatos)
graphics.off()


# VERIFICACIÓN DE LOS PRONÓSTICOS PROBABILÍSTICOS CON CLIMINDVIS ----------

# Verificación de pronósticos probabilísticos:
autoplot_verification_map(obs_p = climindvis_st, 
                          hc_p = climindvis_point_tmax_hindcast, 
                          index = "tx90p", 
                          index_args = list(aggt = "seasonal", selagg = "MAM"),
                          veri_metrics = c("EnsRocss"), 
                          plot_args = list(xlims = c(-77, -50),
                                           ylims = c(-45, -20),
                                           p_breaks = seq(-1,1, 0.1), 
                                           p_cex = 1.5))
graphics.off()

# VERIFICACIÓN DE LOS PRONÓSTICOS DETERMINÍSTICOS CON CLIMINDVIS ----------

autoplot_verification_map(obs_p = climindvis_st, 
                          hc_p = climindvis_point_tmax_hindcast, 
                          index = "tx90p", 
                          index_args = list(aggt = "seasonal", selagg = "MAM"),
                          veri_metrics = c("EnsCorr"), 
                          plot_args = list(xlims = c(-77, -50),
                                           ylims = c(-45, -20),
                                           p_breaks = seq(-1,1, 0.2), 
                                           p_cex = 1.5, 
                                           p_nlev = 40))
graphics.off()

# También podemos usar los paquetes "EasyVerification" y "verification":

# Calculamos el índice TX90:
# Observaciones: 
tx90_st <- calc_index(climindvis = climindvis_st, 
                      index = "tx90p", 
                      aggt = "seasonal",
                      selagg = "MAM")
# Hindcast: 
tx90_hindcast <- calc_index(climindvis = climindvis_point_tmax_hindcast, 
                            index = "tx90p", 
                            aggt = "seasonal",
                            selagg = "MAM")

# Analizamos la estación Buenos Aires:
# Observaciones: 
dim(tx90_st[["index"]]) 
tx90_st_bsas <- cbind.data.frame(
  anios = tx90_st[["index_info"]][["years"]], 
  tx90 = tx90_st[["index"]][3,1,]
)

# Hindcast: 
dim(tx90_hindcast[["index"]])
tx90_hindcast_bsas <- cbind.data.frame(anios = tx90_hindcast[["index_info"]][["years"]], 
                                       t(tx90_hindcast[["index"]][3,,1,]))

# Recortamos a "tx90_st_bsas" para quedarnos con los anios comunes (1981-2010): 
anios_comunes <- tx90_st_bsas$anios %in% tx90_hindcast_bsas$anios
tx90_st_bsas <- tx90_st_bsas[anios_comunes, ]

# VERIFICACIÓN DE LOS PRONÓSTICOS PROBABILÍSTICOS -------------------------

# Reordenamos los datos del pronóstico para la verificación probabilística:
# - Matriz con probabilidades pronosticadas.
# - Matriz en formato binario (1 para la categoría con mayor probabilidad pronosticada, 
# 0 en caso contrario).
# - Vector con categorías (1, 2 o 3 según la categoría con mayor probabilidad pronosticada).

# - Matriz con probabilidades pronosticadas:
hindcast_prob <- easyVerification::convert2prob(as.matrix(tx90_hindcast_bsas[,-1]), prob = 1:2 / 3)
View(hindcast_prob)
hindcast_prob <- hindcast_prob/rowSums(hindcast_prob)
colnames(hindcast_prob) <- c("cat1", "cat2", "cat3")
# - Matriz en formato binario:
hindcast_bin <- matrix(0, nrow(hindcast_prob), ncol(hindcast_prob))
idx <- max.col(hindcast_prob, ties.method = "first")
hindcast_bin[cbind(1:nrow(hindcast_prob), idx)] <- 1
dimnames(hindcast_bin) <- dimnames(hindcast_prob)
# - Vector con categorías: 
hindcast_cat <- max.col(hindcast_bin, ties.method = "first")

# Reordenamos los datos observados para la verificación probabilística:
# - Matriz en formato binario (1 para la categoría observada, 
# 0 en caso contrario).
# - Vector con categorías (1, 2 o 3 según la categoría observada).

# - Matriz en formato binario:
obs_bin <- easyVerification::convert2prob(as.matrix(tx90_st_bsas[,-1]), prob = 1:2 / 3)
colnames(obs_bin) <- c("cat1", "cat2", "cat3")
# - Vector con categorías:
obs_cat <- max.col(obs_bin, ties.method = "first")

## ROC SCORE + DIAGRAMA ROC: 

# Para la categoría 3 ("superior a lo normal"):
roc_score_cat3 <- roc.area(obs = obs_bin[,3], pred = hindcast_prob[,3])$A
# Me creo un "verify object" que me va a permitir graficar más fácilmente
# el diagrama ROC:  
V_cat3 <- verification::verify(obs = obs_bin[,1], 
                               pred = hindcast_prob[,1], 
                               obs.type = "binary", 
                               frcst.type = "prob")

# jpeg(here(paste0("diagrama_roc", "_", "categoria3", "_", "bsas", ".jpg")))
roc.plot(V_cat3, show.thres = FALSE,
         main = paste("Diagrama ROC", "-", "Categoria", "superior a lo normal"),
         xlab = "Tasa de falsa alarma", ylab = "Tasa de aciertos")
leg_txt <- paste("Roc Score:", round(roc_score_cat3, 2))
legend( 0.6, 0.4, leg_txt, bty = "n")
graphics.off()

# DIAGRAMA DE CONFIABILIDAD:

# jpeg(here(paste0("diagrama_confiabilidad", "_", categoria3, "_", nombre_fig, ".jpg")))
reliability.plot(V_cat3, 
                 titl = paste("Diagrama de confiabilidad", "-", "Categoria", "Superior a lo normal"),
                  show.thres = FALSE, 
                  # xlab = "Frecuencias relativas observadas", # La función no admite xlab. 
                  # ylab = "Probabilidad pronosticada", # La función no admite ylab.
                  legend.names = "") 
graphics.off()
  
# BRIER SCORE:   
brier_score_cat3 <- brier(obs = obs_bin[,3], 
                          pred = hindcast_prob[,3])$bs
brier_score_cat3

# PROBABILIDAD DE DETECCIÓN: 
very_cat3 <- verify(obs = obs_bin[,3], pred = hindcast_bin[,3], 
                    obs.type = "binary", frcst.type = "binary")
pod_cat3 <- very_cat3$POD
pod_cat3

# RANKED PROBABILITY SCORE: 
rps_cats <- rps(obs = obs_cat, pred = hindcast_prob)$rps
rps_cats

# VERIFICACIÓN DE LOS PRONÓSTICOS DETERMINÍSTICOS -------------------------

# Reordenamos los datos del pronóstico para la verificación determinística:
# - Media del ensemble: 
hindcast_media_ens <- rowMeans(tx90_hindcast_bsas[,-1])
# Reordenamos los datos observados para la verificación determinística:
obs_det <- tx90_st_bsas$tx90

ens_correlation <- cor(as.vector(hindcast_media_ens), as.vector(obs_det))

# Otra forma: 
ens_correlation <- easyVerification::EnsCorr(as.matrix(tx90_hindcast_bsas[,-1]), 
                                             tx90_st_bsas$tx90)

ens_mae <- easyVerification::EnsMae(as.matrix(tx90_hindcast_bsas[,-1]), 
                                             tx90_st_bsas$tx90)
