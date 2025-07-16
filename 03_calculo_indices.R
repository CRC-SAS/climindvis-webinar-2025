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



#exploración rápida de los datos diarios

st = 4
fechas <- climindvis_st$time         ## Fechas
prec <- climindvis_st$data$prec[st,] ## Precip. Station 4
name <- climindvis_st$data_info$pnames[st] # Nombre
plot(fechas,prec,type="l",main=name)
grid()


################################################################################

# INDICES UMBRALES

################################################################################

# Días secos (dd) (Prec < 1mm)

dd <- calc_index(
  climindvis = climindvis_st,
  index = "dd",
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "annual",        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
  dd_threshold = 1        # 1 (default), definido por el usuario    
)

# podemos hacer un gráfico para un indice calculado a nivel anual para ver valores del indice
library(ggplot2)

st <- 2
años <- dd$index_info$years           #Fechas
value <- dd$index[st, ]               #valor del índice de la estación 2
name <- dd$data_info$pnames[st]       #Nombre de la estación
name_ind <- dd$index_info$iname       #Nombre del índice 

df <- data.frame(
  años = as.numeric(años),
  value = as.numeric(value)
)

ggplot(df, aes(x = años, y = value)) +
  geom_line() +
  geom_point() +
  labs(
    title = name,
    y = name_ind,
    x = "Años"
  ) +
  theme_bw()  


# Ejemplo de cómo se pueden guardar los datos anuales (1 por año) en un formato más amigable:

#si los datos son de a 1 por año:
datos_anual <- purrr::imap_dfr(
  .x = dplyr::pull(metadatos, omm_id),
  .f = function(omm_id, i) {
    indice <- dd$index
    indice_estacion <- indice[i,]
    return (tibble::tibble(
      omm_id = omm_id,
      year = names(indice_estacion),
      value = unname(indice_estacion)
    ))
  }
)
write.table(datos_anual, "dd_anual.txt", sep="\t", row.names = TRUE)


# si ahora queremos calcular a nivel mensual:

dd <- calc_index(
  climindvis = climindvis_st,
  index = "dd",
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly",         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
 # aggmons = c(1,4),
  dd_threshold = 1        # 1 (default), definido por el usuario    
)


# Ejemplo de cómo se pueden guardar los datos mensuales (12 por año) en un formato más amigable:

datos_mensual <- purrr::imap_dfr(
  .x = dplyr::pull(metadatos, omm_id),
  .f = function(omm_id, i) {
    indice_dd <- dd$index          # aqui modificar con el nombre del indice
    dd_estacion <- indice_dd[i,,]
    datos_dd_estacion <- purrr::map_dfr(
      .x = seq(from = 1, to = 12),
      .f = function(mes) {
        datos_dd_estacion_mes <- dd_estacion[mes,]
        return (tibble::tibble(
          omm_id = omm_id,
          year = names(datos_dd_estacion_mes),
          month = mes,
          value = unname(datos_dd_estacion_mes)
        ))
      }
    )
    
    return (datos_dd_estacion)
  }
) %>% dplyr::arrange(omm_id, year, month)


write.table(datos_mensual, "dd_mensual.txt", sep="\t", row.names = TRUE)



# otra opcion es definir los datos entre dias especificos con "dates":

dd_anual_dates <- calc_index(
  climindvis = climindvis_st,
  index = "dd",
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "dates",         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
  start_days = "0000-01-15",
  end_days = "0000-05-14",
  dd_threshold = 1        # 1 (default), definido por el usuario    
)



################################################################################
# Días con heladas (fd) (Tmin < 0)

fd <- calc_index(
  climindvis = climindvis_st,
  index = "fd",
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# Días con Tmin bajo un umbral (th_tmin) (Tmin < umbral)

th_tmin <- calc_index(
  climindvis = climindvis_st,
  index = "th_tmin",
  threshold = 5,          # umbral en °C definido por el usuario
  iformat = "days",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# Días con Tmax sobre un umbral (th_tmax) (Tmax > umbral)

th_tmax <- calc_index(
  climindvis = climindvis_st,
  index = "th_tmax",
  threshold = 30,        # umbral en °C definido por el usuario
  iformat = "days",      # "perc"(default) / "days" 
  NAmaxAgg = 20,         # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "seasonal",     # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
  sealagg = c("JJA")
)


################################################################################
# Días con Tmax sobre un umbral (th_topt) (umbral1 <= Tavg <= umbral2).

th_topt <- calc_index(
  climindvis = climindvis_st,
  index = "th_topt",
  threshold = 15,         # umbral en °C definido por el usuario
  threshold2 = 30,        # umbral en °C definido por el usuario
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "annual"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################
# Días bajo/sobre un umbral (th) (Variable >/>=/</<= umbral)

th <- calc_index(
  climindvis = climindvis_st,
  index = "th",
  threshold = 50,         # umbral definido por el usuario (en °C para temperatura y en mm para precip)
  operator = ">=",        # ">" / ">=" / "<" / "<="
  thvar = "prec",         # prec / tmin / tmax / tavg
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################
# Días entre dos umbrales (th_range) (umbral1 >/>=/</<= variable >/>=/</<= umbral2)

th_range <- calc_index(
  climindvis = climindvis_st,
  index = "th_range",
  threshold = 40,         # umbral definido por el usuario (en °C para temperatura y en mm para precip)
  operator = ">=",        # ">" / ">=" / "<" / "<="
  threshold2 = 100,       # umbral2 definido por el usuario (en °C para temperatura y en mm para precip)
  operator2 = "<",        # ">" / ">=" / "<" / "<="
  thvar = "prec",         # prec / tmin / tmax / tavg
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# INDICES MAXIMO-MINIMO

################################################################################

# Valor mínimo de temperatura mínima diaria (tnn)

tnn <- calc_index(
  climindvis = climindvis_st,
  index = "tnn",
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# Valor máximo de temperatura mínima diaria (tnx)

tnx <- calc_index(
  climindvis = climindvis_st,
  index = "tnx",
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# Valor mínimo de temperatura máxima diaria (txn)

txn <- calc_index(
  climindvis = climindvis_st,
  index = "txn",
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "annual"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# Valor máximo de temperatura máxima diaria (txx)

txx <- calc_index(
  climindvis = climindvis_st,
  index = "txx",
  NAmaxAgg = 20,         # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "annual"        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# Valor mínimo de la variable definida por el usuario (varmin)

varmin <- calc_index(
  climindvis = climindvis_st,
  index = "varmin",        
  var = "tavg",            # prec / tmin / tmax / tavg
  NAmaxAgg = 20,           # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# Valor máximo de la variable definida por el usuario (varmax)

varmax <- calc_index(
  climindvis = climindvis_st,
  index = "varmax",        
  var = "prec",            # prec / tmin / tmax / tavg
  NAmaxAgg = 20,           # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "annual"          # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# Máximo de precipitación en x días (rx)

rx <- calc_index(
  climindvis = climindvis_st,
  index = "rx",        
  rx = 3,                  # dias a eleccion
  NAmaxAgg = 20,           # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# Máximo/mínimo de la variable definida por el usuario en x días (minmax_xdays)

minmax_xdays <- calc_index(
  climindvis = climindvis_st,
  index = "minmax_xdays",        
  var = "tmin",            #prec / tmin / tmax / tavg
  rx = 3,                  # dias a eleccion
  fun = "max",             # max (default)/ min
  NAmaxAgg = 20,           # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# INDICES DE DURACIÓN DE RACHAS

################################################################################

################################################################################

# Días húmedos consecutivos (cwd)

cwd <- calc_index(
  climindvis = climindvis_st,
  index = "cwd",        
  NAmaxAgg = 20,           # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  dd_threshold = 1,        # 1 (default)(0.1mm/0mm/…)
  spells_span_agg = TRUE,  # FALSE (default) ¿Deberían considerarse los períodos que comienzan antes del período de agregación elegido?
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# Días secos consecutivos (cdd)

cdd <- calc_index(
  climindvis = climindvis_st,
  index = "cdd",        
  NAmaxAgg = 20,           # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  dd_threshold = 1,        # 1 (default)(0.1mm/0mm/…)
  spells_span_agg = TRUE,  # FALSE (default) ¿Deberían considerarse los períodos que comienzan antes del período de agregación elegido?
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# Días consecutivos (cxd)

cxd <- calc_index(
  climindvis = climindvis_st,
  index = "cxd",        
  NAmaxAgg = 20,           # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  thvar = "prec",          # prec / tmin / tmax / tavg
  threshold = 10,          # umbral definido por el usuario (en °C para temperatura y en mm para precip)
  operator = ">",          # ">" / ">=" / "<" / "<="
  spells_span_agg = TRUE,  # FALSE (default) ¿Deberían considerarse los períodos que comienzan antes del período de agregación elegido?
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# duración de períodos fríos (csdi) 

csdi <- calc_index(
  climindvis = climindvis_st,
  index = "csdi",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  qthreshold = 10,          # 10 (default) umbral de cuantil de ola de frío
  n = 5,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  min_length = 3,           # 6 (default) longitud mínima de duración de ola de frío 
  spells_span_agg = TRUE,   # FALSE (default) ¿Deberían considerarse las períodos que comienzan antes del período de agregación elegido?
  baseperiod = c(1961,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "monthly"          # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# duración de períodos cálídos (wsdi)

wsdi <- calc_index(
  climindvis = climindvis_st,
  index = "wsdi",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  qthreshold = 90,          # 90 (default) umbral de cuantil de ola de calor
  n = 5,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  min_length = 3,           # 6 (default) longitud mínima de duración de ola de calor 
  spells_span_agg = TRUE,   # FALSE (default) ¿Deberían considerarse las períodos que comienzan antes del período de agregación elegido?
  baseperiod = c(1961,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

# wsdi$index_info$th_quantiles da los valores de los P90 para cada dia del año
# write.table(wsdi$index_info$th_quantiles,"p90.txt",sep="\t", row.names = FALSE)
# Ejemplo: En Buenos Aires da 26 el año 2023, fueron 2 periodos en donde el la tmax diaria fue superior a su P90 diario, y cada periodo fue como mínimo de 6 dias, 
# un periodo fue de 6 días desde el 09/02/2023 hasta el 14/02/2023, y el otro fue de 20 días desde el 28/02/2023
# hasta el 19/03/2023. Se destaca que estos períodos cálidos pueden ocurrir en cualuier momento del año.  


################################################################################

# INDICES BASADOS EN CUANTILES

################################################################################

# Noches frías (tn10p)

tn10p <- calc_index(
  climindvis = climindvis_st,
  index = "tn10p",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  iformat = "days",         # "perc"(default) / "days" 
  n = 1,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  baseperiod = c(1961,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
   min_base_fraction = 0.1, # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual",          # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons 
)


################################################################################

# Noches cálidas (tn90p)

tn90p <- calc_index(
  climindvis = climindvis_st,
  index = "tn90p",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  iformat = "perc",         # "perc"(default) / "days" 
  n = 5,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "monthly"          # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# días fríos (tx10p)

tx10p <- calc_index(
  climindvis = climindvis_st,
  index = "tX10p",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  iformat = "perc",         # "perc"(default) / "days" 
  n = 5,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# días cálidos (tx90p)

tx90p <- calc_index(
  climindvis = climindvis_st,
  index = "tx90p",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  iformat = "perc",         # "perc"(default) / "days" 
  n = 5,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# días sobre/bajo un umbral (qth)

qth <- calc_index(
  climindvis = climindvis_st,
  index = "qth",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  thvar = "tmax",           # prec / tmin / tmax / tavg  
  iformat = "days",         # "perc"(default) / "days" 
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  q_threshold = 90,         # Umbral percentil (numérico). Valor entre 0 y 100.
  n = 5,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  operator = ">",           # ">" / ">=" / "<" / "<="
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 30,              # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# días húmedos (rXptot)

rXptot <- calc_index(
  climindvis = climindvis_st,
  index = "rXptot",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  q_threshold = 90,         # 95 (default) Umbral percentil (numérico).
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  operator = ">",
  dd_threshold = 1,
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 30,              # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################

# percentil (qval)

qval <- calc_index(
  climindvis = climindvis_st,
  index = "qval",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  percentile = 75,          # valor de percentil (entre 0 y 100)
  var = "tmax",             # prec / tmin / tmax / tavg  
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

# calcula entre todos los datos del periodo de agregacion el percentil indicado por el usuario. 

################################################################################

# rango percentil (qrange)

qrange <- calc_index(
  climindvis = climindvis_st,
  index = "qrange",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  percentile1 = 25,         # valor de percentil inferior (entre 0 y 100)
  percentile2 = 75,         # valor de percentil superior (entre 0 y 100)
  var = "tmax",             # prec / tmin / tmax / tavg  
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

# calcula la diferencia entre los dos valores percentiles para cada agregación y año por separado, p.e. en el caso de los percentiles 25 y 75 se calcula el rango intercuantil.


################################################################################

# MÁS INDICES 

################################################################################

# Media (mean)

mean <- calc_index(
  climindvis = climindvis_st,
  index = "mean",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  var = "tmin",             # prec / tmin / tmax / tavg  
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# Suma (sum)

sum <- calc_index(
  climindvis = climindvis_st,
  index = "sum",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  var = "tmin",             # prec / tmin / tmax / tavg  
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# (spi) indice de precipitación estandarizado

spi <- calc_index(
  climindvis = climindvis_st,
  index = "spi",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  timescale = 6,            # 6 (default) escala de tiempo para el cálculo de SPI mensual.
  ref = c(1971,2010),       # Vector con año de inicio y fin del período de referencia. Si no se especifica ningún período de referencia, se utiliza el período completo como período de referencia.    
  distribution = "gamma",   # gamma (default)
  limit = 3,                # 4 (default) Trunque los valores de SPI que sean mayores que un umbral determinado, ya que los valores mayores que 4 no son razonables. El truncamiento se puede desactivar estableciendo límite = Inf. Si limit = 3, el valor del spi estará siempre entre -3 y 3.
  aggt = "monthly"          # tiene que estar seteado en monthly
  )

 
################################################################################

# Índice simple de intensidad diaria (sdii) 

sdii <- calc_index(
  climindvis = climindvis_st,
  index = "sdii",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  dd_threshold = 1,         # 1 (default) Umbral de precipitación para días secos en mm
  aggt = "monthly"
)


################################################################################

# Precipitación total en los días húmedos (prcptot) 

prcptot <- calc_index(
  climindvis = climindvis_st,
  index = "prcptot",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  dd_threshold = 1,         # 1 (default) Umbral de precipitación para días secos en mm
  aggt = "monthly"
)


################################################################################

# Inicio de la temporada de lluvias (rainy_season_start) 
#Metodo gurgiser

rainy_season_start <- calc_index(
  climindvis = climindvis_st,
  index = "rainy_season_start",        
  aggt = "dates",
  start_days = "0000-02-01",
  end_days = "0000-10-01",  
  #NAmaxAgg = 20, 
  rs_method="gurgiser")



#Metodo consec_th 

rainy_season_start <- calc_index(
  climindvis = climindvis_st,
  index = "rainy_season_start",        
  days =  4,                    # Número de días consecutivos de los cuales la suma de precipitación es superior al umbral <<th>>
  th = 30,                      # Umbral para la suma de precipitaciones de <<días>> días consecutivos en mm
  dd_th = 1,                    # Umbral de día seco
  nval = "/",                   # Valor que se devuelve si no se cumplen los criterios para el inicio de la temporada de lluvias, predeterminado=NA. Se pueden establecer otros valores para distinguir entre años en los que los datos son NA y años en los que no se cumplen los criterios del índice.
  mdays = 10,                   # Número de días para comprobar <<mcdd>>
  mcdd = 7,                     # Máximo de días secos consecutivos en los próximos <<mdays>> días
  aggt = "dates",
  start_days = "0000-06-01",
  end_days = "0000-03-01",
  rs_method="consec_th")


################################################################################

# Fin de la temporada de lluvias (rainy_season_end) 

#Metodo gurgiser

rainy_season_end <- calc_index(
  climindvis = climindvis_st,
  index = "rainy_season_end",        
  aggt = "dates",
  start_days = "0000-08-01",  #tener conocimiento de la climatologia del lugar. Si llueve de abr a oct, poner fecha inicio en el maximo de la estacion lluviosa y el fin muchos meses despues que termine
  end_days = "0000-01-01",  
  NAmaxAgg = 20, 
  rs_method="gurgiser")






