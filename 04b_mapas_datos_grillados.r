# Limpiar todos los objetos preexistentes y liberar memoria 
rm(list = ls());
gc();

# Cargar librerias necesarias
library(ClimIndVis)
library(glue)

# Carga el archivo ClimIndVis generado a partir de datos grillados
load("data/ClimIndVis_Grid.RData")

# Carga del archivo ClimIndVis generado a partir de datos puntuales
load("data/ClimIndVis_Stations.RData")

### 1. MAPAS DE CLIMATOLOGIA

# 1a. Generación de mapa de media te temperatura máxima
index_args <- list(aggt = "seasonal", var = "tmax")
plot_args  <- list(p_cex = 5, plwidth = 10, NA_col = "grey50", zlim = c(-30, 30))
ClimIndVis::autoplot_climatology_map(
  dat_grid = climindvis_grid, index = "mean", index_args = index_args,
  selyears = c(2010:2019), plot_args = plot_args, title = "Promedio de temperatura máxima",
  output = "png", plotdir = "data/", plotname = "mapa_climatologia_estacional"
)

# 1b. Generación de mapa de precipitación acumulada
index_args <- list(aggt = "seasonal", var = "prec")
plot_args  <- list(p_cex = 5, plwidth = 10, NA_col = "grey50", zlim = c(0, 500))
ClimIndVis::autoplot_climatology_map(
  dat_grid = climindvis_grid, index = "sum", index_args = index_args,
  selyears = c(2010:2019), plot_args = plot_args, title = "Precipitación acumulada",
  output = "png", plotdir = "data/", plotname = "mapa_climatologia_prec"
)


### 2. MAPAS DE ANOMALIAS

# 2a. Generación de mapa de anomalías de días secos
index_args <- list(aggt = "seasonal", selagg = c("SON"), iformat = "perc")
plot_args  <- list(p_cex = 5, plwidth = 10, NA_col = "grey50")
ClimIndVis::autoplot_anomaly_map(
  dat_grid = climindvis_grid, index = "dd", index_args = index_args,
  refyears = c(1991:2000), anomyears = 2008, 
  plot_args = plot_args, title = "Anomalías de días secos",
  output = "png", plotdir = "data/", plotname = "mapa_anomalias_dd"
)

# 2b. Generación de mapa de anomalías de días secos con datos punduales
ClimIndVis::autoplot_anomaly_map(
  dat_grid = climindvis_grid, dat_p = climindvis_st, 
  index = "dd", index_args = index_args,
  refyears = c(1991:2000), anomyears = 2008, 
  plot_args = plot_args, title = "Anomalías de días secos + datos puntuales",
  output = "png", plotdir = "data/", plotname = "mapa_anomalias_dd_puntuales"
)

# 2c. Generación de mapa de anomalías de SPI-3
index_args <- list(timescale = 3, ref = c(1991, 2020), aggt = "monthly")
plot_args  <- list(p_cex = 5, plwidth = 10, NA_col = "grey50")
ClimIndVis::autoplot_anomaly_map(
  dat_grid = climindvis_grid, index = "spi", index_args = index_args,
  refyears = c(1991:2020), anomyears = 2023, 
  plot_args = plot_args, title = glue("Anomalías de SPI-3"),
  output = "png", plotdir = "data/", plotname = "mapa_anomalias_spi_3"
)

### 3. MAPAS DE TENDENCIAS