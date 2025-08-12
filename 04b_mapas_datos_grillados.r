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

# 1. Generación de mapa de anomalías de días secos
index_args <- list(aggt = "seasonal", selagg = c("SON"), iformat = "perc")
plot_args  <- list(p_cex = 5, plwidth = 10, NA_col = "grey50")
ClimIndVis::autoplot_anomaly_map(
  dat_grid = climindvis_grid, index = "dd", index_args = index_args,
  refyears = c(1991:2000), anomyears = 2008, 
  plot_args = plot_args, title = "Anomalías de días secos",
  output = "png", plotdir = "data/", plotname = "mapa_anomalias_dd"
)

# 2. Generación de mapa de anomalías de SPI-3
index_args <- list(timescale = 3, ref = c(1991, 2020), aggt = "monthly")
plot_args  <- list(p_cex = 5, plwidth = 10, NA_col = "grey50")
ClimIndVis::autoplot_anomaly_map(
  dat_grid = climindvis_grid, index = "spi", index_args = index_args,
  refyears = c(1991:2020), anomyears = 2023, 
  plot_args = plot_args, title = glue("Anomalías de SPI-3"),
  output = "png", plotdir = "data/", plotname = "mapa_anomalias_spi_3"
)

