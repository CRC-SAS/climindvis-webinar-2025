
# Limpiar TODOS los objetos preexistentes y liberar memoria 

rm(list = ls()); gc();

library(ClimIndVis)
library(dplyr)
library(tidyr)

# Cargar datos 
df1 <- read.csv('data/Registros_87585_Buenos_Aires.csv', sep = "\t")
df2 <- read.csv('data/Registros_87178_Posadas_Aero.csv', sep = "\t")

# Las coordenadas son necesarias para cualquier objeto ClimIndVis y se añaden manualmente para este conjunto de datos
lat1 <- -34.59
lon1 <- -58.48
lat2 <- -27.4
lon2 <- -56.0

df1$lon <- lon1
df1$lat <- lat1
df2$lon <- lon2
df2$lat <- lat2

df <- rbind(df1, df2)

# Seleccionar las columnas necesarias
df <- df %>% select(omm_id, fecha, tmax, tmin, tmed, prcp, lon, lat)


# Convertir en valores numéricos (existen NA)
df$tmax <- as.numeric(df$tmax)
df$tmin <- as.numeric(df$tmin)
df$tmed <- as.numeric(df$tmed)
df$prcp <- as.numeric(df$prcp)

# Convertir fecha
df$fecha <- as.Date(df$fecha)

# Renombrar columnas
df <- df %>% rename(pnames = omm_id)
df <- df %>% rename(time = fecha)

# Crear lista objeto en el formato necesario para la función make_object
data_list <- list()

# Definir variables
variables <- c("prcp", "tmax", "tmed", "tmin")

# Crear variable temporal
data_list$time <- unique(df$time)

# Crear matrices para cada variable
for (var in variables) {
  mat <- df %>%
    select(time, pnames, all_of(var)) %>%
    pivot_wider(names_from = pnames, values_from = all_of(var)) %>%
    arrange(time) %>%
    select(-time) %>%
    as.matrix()
  
  data_list[[var]] <- t(mat)
}




# Añadir lat y lon como vectores numéricos
data_list$lat <- unique(df$lat)
data_list$lon <- unique(df$lon)

# Nombres de las estaciones
pnames <- unique(df$pnames)

# Definir  data_info
data_info = list(type="p",date_format="t1d", data_name="test data station", pnames = pnames)


# Crear objeto ClimIndVis
climindvis_st <- make_object(
  tmin = data_list$tmin, tmax = data_list$tmax, tavg = data_list$tmed, prec = data_list$prcp,
  dates_tmin = data_list$time, dates_tmax = data_list$time, dates_tavg = data_list$time, dates_prec = data_list$time,
  lon = data_list$lon, lat = data_list$lat,  data_info = data_info)




#grafico del indice
autoplot_ts_stations(climindvis_st, index ="tn10p", trendplots = TRUE, index_args = list(aggt = "annual"))

#grafico de la anomalia del indice
autoplot_anomaly_ts(climindvis_st, index ="tn10p", index_args = list(aggt = "annual"), 
                    ts_type = "single_ts", pcols = "royalblue")



autoplot_trend_map(dat_p = climindvis_st, index = "tn10p", index_args = list(aggt = "annual"))


index_tn10p <- calc_index(climindvis_st, index = "tn10p", aggt = "annual", trend = "Mannkendall", NAmaxTrend = 20)


#Aqui estan todos los datos del indice, tendencias lineal y loess, metricas de la tendencia, etc
index_tn10p

#Santi podrias hacer un solo grafico con el indice y las tendencias (fit y loess)
# y por otro lado toda la 1er parte es la preparacion de los datos para convertirlos en objeto climindvis, 
#asi lo hizo Rahel para el primer curso, tenes alguna forma mas amena de hacerlo?

#otra cosa, no se como hacer para que calcule la tendencua con otros metodos (solo calcula con MannKendall - method 3)
index_tn10p <- calc_index(climindvis_st, index = "tn10p", iformat = "perc", aggt = "annual", trend = "Logistic Regression", NAmaxTrend = 20)

#en el excel calculé la tendencia lineal y puse la formula en el grafico,
#y ahi se ve la pendiente por año (que da como el abs.trend (salvando que este es por decada (x10) )
# queria reproducir esa tendencia con el climindvis, por mas que vea que abs.trend da lo mismo, 
#veo que las tendencias no se ven igual en el grafico, y eso me marea...







index = c("txn", "txx","dd")
index_args = list(
  txn = list(aggt = "other", aggmons = c(10:12)),
  txx = list(aggt = "other", aggmons = c(10:12)),
  tnx = list(aggt = "other", aggmons = c(10:12)))
autoplot_anomaly_ts(
  dat_p = climindvis_st, index = index, index_args = index_args,
  ts_type = "multi_ind")




indices = c("dd", "fd", "th", "tx90p", "cdd", "sdii")

aggt = "seasonal"

indices_args = list(dd = list(aggt = "other", aggmons = c(10:12)), 
                    fd = list(aggt = "other", aggmons = c(10:12)),
                    th = list(aggt = "other", aggmons = c(10:12), th = 5, thvar = "prec", op = ">"),
                    tx90p = list(aggt = "other", aggmons = c(10:12)),
                    cdd = list(aggt = "other", aggmons = c(10:12)),
                    sdii = list(aggt = "other", aggmons = c(10:12))
)


indices_args = list(dd = list(aggt = aggt), 
                    fd = list(aggt = aggt),
                    th = list(aggt = aggt, th = 5, thvar = "prec", op = ">"),
                    tx90p = list(aggt = aggt),
                    cdd = list(aggt = aggt),
                    sdii = list(aggt = aggt)
)





autoplot_overview_stations(dat_p = climindvis_st, indices = indices, indices_args = indices_args,
                           addyears = c(1963,1965,1972,1982,1986,1987,1991,1994,1997,2002,2009,2015,2023,1973,1975,1983,1988,1995,1998,1999,2007,2010,2011,2020), 
                           yearcols = c("tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue"), 
                           selpoints = 2, yearcex = 2)





index_dd <- calc_index(climindvis_st, index = "dd", iformat = "days", aggt = "annual", trend = "Mannkendall", NAmaxTrend = 20)


index_dd$trend_info





indices_args = list(dd = list(aggt = "other", aggmons = c(10:12)), 
                    fd = list(aggt = "other", aggmons = c(10:12)),
                    th = list(aggt = "other", aggmons = c(10:12), th = 5, thvar = "prec", op = ">"),
                    tx90p = list(aggt = "other", aggmons = c(10:12)),
                    cdd = list(aggt = "other", aggmons = c(10:12)),
                    sdii = list(aggt = "other", aggmons = c(10:12))
)
autoplot_overview_stations(dat_p = climindvis_st, indices = indices, indices_args = indices_args,
                           addyears = c(1973,1975,1983,1988,1995,1998,1999,2007,2010,2011,2020), yearcols = c("blue") , selpoints = 2, yearcex = 2)



autoplot_overview_stations(dat_p = climindvis_st, indices = indices, indices_args = indices_args,
                           addyears = c(1963,1965,1972,1982,1986,1987,1991,1994,1997,2002,2009,2015,2023), 
                           yearcols = c("tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato"), 
                           selpoints = 2, yearcex = 2)




autoplot_overview_stations(dat_p = climindvis_st, indices = indices, indices_args = indices_args,
                           addyears = c(1963,1965,1972,1982,1986,1987,1991,1994,1997,2002,2009,2015,2023,1973,1975,1983,1988,1995,1998,1999,2007,2010,2011,2020), 
                           yearcols = c("tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","tomato","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue"), 
                           selpoints = 1, yearcex = 2,  plot_title = TRUE)

library(Rtools)




library(raster)
library(ncdf4)
library(ClimIndVis)
library(abind)

### Script para convertir datos de previsión del Climate Data Store (CDS) en un objeto ClimIndVis  ###

# Para obtener una guía paso a paso sobre cómo descargar los datos de previsión del CDS, 
# consulte el manual proporcionado en el curso de Moodle 

# Elija el directorio donde se almacenan los datos descargados
dir <- "C:/Users/nherrera/Desktop/NATI/CURSOS/ClimIndVis-Oct2024"

nc_file <- nc_open(paste0(dir,"/data2024nov.nc")) #ejemplo

#nc_file <- nc_open(paste0(dir,"/global.nc")) #ejemplo


maxtemp <- ncvar_get(nc_file, varid = "mx2t24")
valid_time <- ncvar_get(nc_file, varid = "valid_time")
valid_time_normal <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC")
valid_time_normal <- as.Date(valid_time_normal, format = "%Y-%m-%d %Z")
valid_time_normal <- split(valid_time_normal, format(valid_time_normal, "%Y")) 

# Las latitudes y longitudes deben estar en orden ascendente
# Si hay que adaptar las coordenadas, hay que hacer la misma adaptación con las variables para que cada variable se ajuste a la coordenada correcta 
if (!all(diff(nc_file$dim$latitude$vals) >= 0)) {
  lat <- nc_file$dim$latitude$vals[length(nc_file$dim$latitude$vals):1]
  maxtemp<-maxtemp[,length(nc_file$dim$latitude$vals):1,,] #saque una coma pórque es un año en este caso
  
} else {
  lat <- nc_file$dim$latitude$vals
}

if (!all(diff(nc_file$dim$longitude$vals) >= 0)) {
  lon <- nc_file$dim$longitude$vals[length(nc_file$dim$longitude$vals):1]
  maxtemp<-maxtemp[length(nc_file$dim$longitude$vals):1,,,]
  
} else {
  lon <- nc_file$dim$longitude$vals
}

# las funciones sólo pueden trabajar con la siguiente dimensión: 
# LON LAT ENSEMBLE MEMBERS LEADTIME YEAR
maxtemp <- aperm(maxtemp, c(1,2,5,3,4)) #adáptelo si la previsión es sólo de un año (tendrá una dimensión menos) 
maxtemp <- aperm(maxtemp, c(1,2,4,3)) #para 1 año 


# Añadir una quinta dimensión si los datos sólo contienen un año y falta la dimensión año (de lo contrario, la función make_object no funciona)
if (length(dim(maxtemp)) != 5){
  maxtemp <-abind(maxtemp, along = 5)
}


# La función make_object sólo puede trabajar con datos de temperatura en grados Celsius 
if (all(maxtemp > 200)) {
  # Convertir de Kelvin a Celsius
  maxtemp <- maxtemp - 273.15
}



# tiempo de previsión en horas
forecast_time <- nc_file$dim$forecast_period$vals

# Definir entrada data_info
data_info <- list(type="grid_hc", date_format="t2d", data_name="ECMWF hc",fmon="11") #cambiar fmon
#PROBAR DESCARGA ENERO
# Crear el objeto ClimIndVis
climindvis_grid <- make_object(tmax=maxtemp,dates_tmax = valid_time_normal,
                               lon=lon ,lat=lat,
                               data_info=data_info)
