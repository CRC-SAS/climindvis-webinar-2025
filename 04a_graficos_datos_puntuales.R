# Limpiar todos los objetos preexistentes y liberar memoria 
rm(list = ls());
  gc();

# Cargar librerias necesarias
library(ClimIndVis)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)


# Carga el archivo .RData
load("data/ClimIndVis_Stations.RData")


################################################################################

# GRAFICOS

################################################################################

# autoplot_ts_stations

# Función contenedora para calcular y trazar series de tiempo de índice 
# a partir de datos de la estación

autoplot_ts_stations(
  dat_p = climindvis_st,
  index = "tn10p",
  ts_type = "single_ts",
  trendplots = TRUE,
  index_args = list(aggt = "annual", iformat = "perc", baseperiod = c(1971,2000), NAmaxAgg = 20)
)


# si desea comparar valores anuales y estacionales se usa "multi_agg"

autoplot_ts_stations(
  dat_p = climindvis_st,
  index = "tn10p",
  ts_type = "multi_agg",
  trendplots = FALSE,
  index_args = list(list(aggt="annual",iformat = "days", baseperiod = c(1971,2010)),list(aggt="seasonal",iformat = "days",baseperiod = c(1971,2010)))
)
  

# vemos que no tiene sentido unir anual y estacional con días, entonces dejamos "perc"

autoplot_ts_stations(
  dat_p = climindvis_st,
  index = "tn10p",
  ts_type = "multi_agg",
  trendplots = FALSE,
  index_args = list(list(aggt="annual",iformat = "perc", baseperiod = c(1971,2010)),list(aggt="seasonal",iformat = "perc",baseperiod = c(1971,2010))),
  cex = 1,   #tamaño del punto
  text_cex = 1 #tamaño de la fuente
  
)


# ahora quiero comparar los datos de todas las estaciones, uso "multi_point"

autoplot_ts_stations(
  dat_p = climindvis_st,
  index = "tn10p",
  ts_type = "multi_point",
  trendplots = FALSE,
  index_args = list(aggt = "annual", iformat = "days",  NAmaxAgg = 20))



# para ver varios indices al mismo tiempo uso "multi_ind"

index = c("txn", "txx","dd")
index_args = list(
  txn = list(aggt = "other", aggmons = c(6:8)),
  txx = list(aggt = "other", aggmons = c(6:8)),
  dd = list(aggt = "other", aggmons = c(6:8)))
autoplot_ts_stations(
  dat_p = climindvis_st,
  index = index, 
  index_args = index_args,
  trend = TRUE,
  ts_type = "multi_ind"
) 


# para ver varios indices al mismo tiempo uso "multi_ind"

index = c("tx90p", "tn90p","txx")
index_args = list(
  tx90p = list(aggt = "other", aggmons = c(1:3), baseperiod = c(1971,2010)),
  tn90p = list(aggt = "other", aggmons = c(1:3), baseperiod = c(1971,2010)),
  txx = list(aggt = "other", aggmons = c(1:3)))
autoplot_ts_stations(
  dat_p = climindvis_st,
  index = index, 
  index_args = index_args,
  ts_type = "multi_ind"
) 



#Para hacer gráficos de SPI

## ts_type = "spi_barplot"
autoplot_ts_stations(
  dat_p = climindvis_st, index = "spi", index_args=list(aggt="monthly",timescale = 6,ref = c(1961,2010), limit = 3),
  ts_type = "spi_barplot", plot_title = TRUE)



################################################################################

# Autoplot_overview_stations

################################################################################

# Función contenedora para mostrar diagramas de caja y valores de índice de años 
# seleccionados para diferentes índices para cada estación.

index <- c("dd","fd","tn10p","tx90p", "th", "sdii", "mean", "qval")

index_args <- list(
  dd = list(aggt="annual",iformat = "days"),
  fd = list(aggt="annual",iformat = "perc"),
  tn10p = list(aggt="annual",iformat = "perc", baseperiod = c(1991,2020)),
  tx90p = list(aggt="annual",iformat = "perc", baseperiod = c(1991,2020)),
  th =  list(aggt="annual",iformat = "perc", threshold = 0.5, operator = ">=",thvar = "prec"),
  sdii = list(aggt="annual"),
  mean = list(aggt="annual",  var = "tmin"),
  qval = list(aggt="annual", var = "tmax",  percentile = 75)
)

autoplot_overview_stations(
  dat_p = climindvis_st,
  indices = index,
  indices_args = index_args,
  selpoints = NULL,   #Matriz entera opcional de estaciones para seleccionar para trazar. Default = NULL (todas las estaciones están trazadas).
  addyears = c(1974,1997),
  yearcols = c("blue", "red"),  #Matriz opcional de la misma longitud que addyears con colores para marcar los años seleccionados. Las entradas deben ser argumentos válidos para col2rgb. Predeterminado=1:longitud(addyears)
  yearpchs = 18,  #Matriz opcional de formas para cada año. Predeterminado= 18 (rombo relleno)
  yearcex = 2,   #tamaño de los puntos agregados
  NAmaxClim = 20
)






index_args <- list(
  sdii = list(aggt="annual",iformat = "days")
)

autoplot_overview_stations(
  dat_p = climindvis_st,
  indices = "sdii",
  indices_args = index_args,
  selpoints = NULL,   #Matriz entera opcional de estaciones para seleccionar para trazar. Default = NULL (todas las estaciones están trazadas).
  addyears = c(1974,1997),
 yearcols = c("blue", "red"),  #Matriz opcional de la misma longitud que addyears con colores para marcar los años seleccionados. Las entradas deben ser argumentos válidos para col2rgb. Predeterminado=1:longitud(addyears)
  
  yearpchs = 18,  #Matriz opcional de pch para cada año. Predeterminado= 18
  yearcex = 2,   #tamaño de los puntos agregados
  NAmaxClim = 20
)


index <- c("dd")
index_args <- list(
  dd = list(aggt="seasonal",sealag = "SON",iformat = "days")
)
autoplot_overview_stations(
  dat_p = climindvis_st,
  indices = index,
  indices_args = index_args,
  selpoints = NULL,   #Matriz entera opcional de estaciones para seleccionar para trazar. Default = NULL (todas las estaciones están trazadas).
  addyears = c(1973,1975,1988,1998,2010,1965,1982,1997,2015,2023),
  yearcols = c("blue","blue","blue","blue","blue","red","red","red","red","red"),  #Matriz opcional de la misma longitud que addyears con colores para marcar los años seleccionados. Las entradas deben ser argumentos válidos para col2rgb. Predeterminado=1:longitud(addyears)
 # yearcols = c(rep("blue", 5), rep("red", 5)),
  yearpchs = 18,  #Matriz opcional de pch para cada año. Predeterminado= 18
  yearcex = 2,   #tamaño de los puntos agregados
  NAmaxClim = 20

)


################################################################################

#AUTOPLOT_ANOMALY_TS

################################################################################

# se usa exactemente igual que autoplot_ts_Stations, solo se llama a otra funcion

autoplot_anomaly_ts(
  dat_p = climindvis_st,
  index = "tn10p",
  ts_type = "single_ts",
  trendplots = TRUE,
  index_args = list(aggt = "annual", iformat = "perc", baseperiod = c(1971,2000), NAmaxAgg = 20)
)
