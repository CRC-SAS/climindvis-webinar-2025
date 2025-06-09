# Limpiar todos los objetos preexistentes y liberar memoria 
rm(list = ls());
gc();

# Cargar librerias necesarias
library(ClimIndVis)
library(dplyr)


# Carga el archivo .RData
load("data/ClimIndVis_Stations.RData")


################################################################################

# INDICES UMBRALES

################################################################################

# Días secos (dd) (Prec < 1mm). index_arguments.dd

dd <- calc_index(
  climindvis = climindvis_st,
  index = "dd",
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "other",         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
  aggmons = c(1,4),
  dd_threshold = 1        # 1 (default), definido por el usuario    
)

write.table(dd$index, "dd_perc.txt", sep="\t", row.names = TRUE)

################################################################################
# Días con heladas (fd) (Tmin < 0). index_arguments.fd

fd <- calc_index(
  climindvis = climindvis_st,
  index = "fd",
  iformat = "days",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "annual"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################
# Días con Tmin bajo un umbral (th_tmin) (Tmin < umbral)

th_tmin <- calc_index(
  climindvis = climindvis_st,
  index = "th_tmin",
  threshold = 5,          # umbral en °C definido por el usuario
  iformat = "days",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "annual"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################
# Días con Tmax sobre un umbral (th_tmax) (Tmax > umbral)

th_tmax <- calc_index(
  climindvis = climindvis_st,
  index = "th_tmax",
  threshold = 30,          # umbral en °C definido por el usuario
  iformat = "days",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "annual"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
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
# Días bajo/sobre un umbrall (th) (Variable >/>=/</<= umbral)

th <- calc_index(
  climindvis = climindvis_st,
  index = "th",
  threshold = 50,         # umbral definido por el usuario (en °C para temperatura y en mm para precip)
  operator = ">=",        # ">" / ">=" / "<" / "<="
  thvar = "prec",         # prec / tmin / tmax / tavg
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################
# Días bajo/sobre un umbrall (th) (Variable >/>=/</<= umbral)

th_range <- calc_index(
  climindvis = climindvis_st,
  index = "th_range",
  threshold = 50,         # umbral definido por el usuario (en °C para temperatura y en mm para precip)
  operator = ">=",        # ">" / ">=" / "<" / "<="
  threshold2 = 100,       # umbral2 definido por el usuario (en °C para temperatura y en mm para precip)
  operator2 = "<",        # ">" / ">=" / "<" / "<="
  thvar = "prec",         # prec / tmin / tmax / tavg
  iformat = "perc",       # "perc"(default) / "days" 
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################

# INDICES MAXIMO-MINIMO

################################################################################
# Valor mínimo de temperatura mínima diaria (tnn)

tnn <- calc_index(
  climindvis = climindvis_st,
  index = "tnn",
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################
# Valor máximo de temperatura mínima diaria (tnx)

tnx <- calc_index(
  climindvis = climindvis_st,
  index = "tnx",
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################
# Valor mínimo de temperatura máxima diaria (txn)

txn <- calc_index(
  climindvis = climindvis_st,
  index = "txn",
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"         # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################
# Valor máximo de temperatura máxima diaria (txx)

txx <- calc_index(
  climindvis = climindvis_st,
  index = "txx",
  NAmaxAgg = 20,          # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  aggt = "monthly"        # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################
# Valor mínimo de la variable definida por el usuario (varmin)

varmin <- calc_index(
  climindvis = climindvis_st,
  index = "varmin",        
  var = "tmax",            # prec / tmin / tmax / tavg
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
  aggt = "annual"          # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
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
  aggt = "annual"          # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
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
  min_length = 6,           # 6 (default) longitud mínima de duración de ola de frío 
  spells_span_agg = TRUE,   # FALSE (default) ¿Deberían considerarse las períodos que comienzan antes del período de agregación elegido?
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################
# duración de períodos cálídos (wsdi)

wsdi <- calc_index(
  climindvis = climindvis_st,
  index = "wsdi",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  qthreshold = 90,          # 90 (default) umbral de cuantil de ola de calor
  n = 5,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  min_length = 6,           # 6 (default) longitud mínima de duración de ola de calor 
  spells_span_agg = TRUE,   # FALSE (default) ¿Deberían considerarse las períodos que comienzan antes del período de agregación elegido?
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  inbase = FALSE,           # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

# wsdi$index_info$th_quantiles da los valores de los cP90 para cada dia del año
# write.table(wsdi$index_info$th_quantiles,"p90.txt",sep="\t", row.names = FALSE)
# Ejemplo: En Buenos Aires da 26 el año 2023, fueron 2 periodos en donde el ña tmax diaria fue superior a su P90 diario, y cada periodo fue como mínimo de 6 dias, 
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
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
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
  thvar = "prec",           # prec / tmin / tmax / tavg  
  iformat = "perc",         # "perc"(default) / "days" 
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  q_threshold = 10,         # Umbral percentil (numérico). Valor entre 0 y 100.
  n = 5,                    # 5 (default) tamaño de ventana (en días) para ejecutar la ventana en el cálculo del cuantil de temperatura. 
  inbase = TRUE,            # TRUE (default) Para los cuantiles de temperatura, calcule los cuantiles dentro y fuera del período base siguiendo el método de impulso de Zhang 2005. Solo se aplica cuando se selecciona el período base
  min_base_fraction = 0.1,  # 0.1 (default). Fracción mínima de los datos base que deben estar presentes para que el cuantil se calcule para un día en particular (solo se aplica a los cuantiles de temperatura)
  operator = ">",           # ">" / ">=" / "<" / "<="
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)


################################################################################
# días húmedos (rXptot)

rXptot <- calc_index(
  climindvis = climindvis_st,
  index = "rXptot",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  q_threshold = 95,         # 95 (default) Umbral percentil (numérico).
  baseperiod = c(1981,2010),# Vector de año de inicio y fin para el cálculo de cuantiles. Si no se proporciona, se utilizará todo el rango de años del conjunto de datos para el cálculo de los cuantiles
  th_object = NULL,         # Si se calcula un índice cuantil para datos de pronóstico, se debe proporcionar un objeto climindvis_index de datos retrospectivo para las mismas dimensiones espaciales y meses de pronóstico. Los valores de umbral se tomarán del objeto hindcast (objeto$index_info$quantiles). En este caso, todos los demás argumentos (q, período base, umbral,...) se ignoran y se toman del objeto climindvis_index. En el caso de funciones de trazado automático, estos valores se entregan automáticamente y th_object debe dejarse en el valor predeterminado de NULL
  NAmaxQ = 365,             # 365 (default) Número mínimo de días necesarios para el cálculo de cuantiles
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

################################################################################
# percentil (qval)

qval <- calc_index(
  climindvis = climindvis_st,
  index = "qval",        
  NAmaxAgg = 20,            # 20 (default) porcentaje de datos diarios aceptados en el periodo de agregación seleccionado (entre 0 y 100)
  percentile = 100,         # valor de percentil (entre 0 y 100)
  var = "prec",             # prec / tmin / tmax / tavg  
  aggt = "annual"           # annual / seasonal / monthly / other. Si definimos "other" hay que agregar el argumento aggmons
)

# calcula entre todos los datos del periodo de agregacion el percentil indicado por el usuario. 

################################################################################
#  rango percentil (qrange)

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










#si queremos un gráfico con la serie temporal usamos autoplot_ts_stations

autoplot_ts_stations(
  dat_p = climindvis_st,
  index = "dd",
  index_args = list(iformat = "days",list(aggt = "annual"),     #"perc"
  
  #  index_args = list(list(aggt="annual"),list(aggt="seasonal")),
                              # "annual", "seasonal", selagg(1:4)
  ts_type = "multi_agg")
)
  
autoplot_ts_stations(
  dat_p = climindvis_st,
  index = "dd", 
  index_args = list(iformat = "days",list(aggt="annual"),list(aggt="seasonal")), #no se como poner en dias
  # "annual", "seasonal", selagg(1:4)
  ts_type = "multi_agg",
  selyears = c(1981:1990),    #(default es periodo comppleto)
  title="días secos", #no funciona 
  plot_title = TRUE,
  plot_legend = TRUE,
#  ylims = c(10:90)   no funciona
  cex = 1,
  text_cex = 1
  
)



autoplot_ts_stations(
  dat_p = climindvis_st,
  index = "dd", 
  aggt="annual",
  # "annual", "seasonal", selagg(1:4)
  ts_type = "multi_agg",
  selyears = c(1961:2024),    #(default es periodo comppleto)
  title="días secos", #no funciona 
  plot_title = TRUE,
  plot_legend = TRUE,
  #  ylims = c(10:90)   no funciona
  cex = 1,
  text_cex = 1
  
)



