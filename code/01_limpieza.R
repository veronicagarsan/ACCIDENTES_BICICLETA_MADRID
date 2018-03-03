#################################################################################
# El objetivo de este script es leer y limpiar los datos de la tabla de dt_accidentes
################################################################################




# Tabla acciedentes -------------------------------------------------------


# 1) Leemos los datos:

# Leemos el csv de accidentes:
dt_accidentes <- fread(paste0(datapath,"AccidentesBicicletas_2017.csv"))



# 2) Limpieza de datos:

# Inspeccionamos dataset:
str(dt_accidentes)

# 2.1) Cambiamos el nombre variables, de forma que sean una única palabra:
setnames(dt_accidentes,
         new = toupper(gsub(" ", "_", names(dt_accidentes))),
         old = names(dt_accidentes))

# 2.2) Transformación a formato fechas:
dt_accidentes[,FECHA := as.Date(dt_accidentes$FECHA, "%d/%m/%Y")]


# 2.3) Tramo horario como factor indicando el orden:
dt_accidentes$TRAMO_HORARIO<-factor(dt_accidentes$TRAMO_HORARIO, 
                                   levels =c("DE 00:00 A 00:59",
                                             "DE 1:00 A 1:59",
                                             "DE 2:00 A 2:59",
                                             "DE 3:00 A 3:59",
                                             "DE 4:00 A 4:59",
                                             "DE 5:00 A 5:59",
                                             "DE 6:00 A 6:59",
                                             "DE 7:00 A 7:59",
                                             "DE 8:00 A 8:59",
                                             "DE 9:00 A 9:59",
                                             "DE 10:00 A 10:59",
                                             "DE 11:00 A 11:59",
                                             "DE 12:00 A 12:59",
                                             "DE 13:00 A 13:59",
                                             "DE 14:00 A 14:59",
                                             "DE 15:00 A 15:59",
                                             "DE 16:00 A 16:59",
                                             "DE 17:00 A 17:59",
                                             "DE 18:00 A 18:59",
                                             "DE 19:00 A 19:59",
                                             "DE 20:00 A 20:59",
                                             "DE 21:00 A 21:59",
                                             "DE 22:00 A 22:59",
                                             "DE 23:00 A 23:59",
                                             "DE 24:00 A 24:59"))
# Análisis de los niveles:
levels(dt_accidentes$TRAMO_HORARIO)

# 2.4) Transformamos variables a factor:

cols = c("DISTRITO",
                "TIPO_ACCIDENTE",
                "TIPO_VEHICULO")

dt_accidentes[, (cols) := lapply(.SD, factor), .SDcols=cols]
rm(cols)

# 2.5) Creamos nueva variable con calle y número unido:
dt_accidentes[, CALLE_NUM := ifelse(is.na(NUMERO), 
                                    LUGAR,
                                    paste0(LUGAR,"_",NUMERO))]
dt_accidentes[,CALLE_NUM := gsub("-", 
                                 " \n", 
                                 dt_accidentes$CALLE_NUM,
                                 perl = TRUE)]



# Archivo shp (mapas) -----------------------------------------------------

# 1) Distritos:
distritos <- readOGR(dsn = "data/BARRIOS", 
                     layer = "DISTRITOS",
                     encoding = "UTF-8")
str(distritos@data)

# Creamos variable con nombre en mayusc
distritos@data$NOMBRE<-toupper(distritos@data$NOMBRE)
distritos@data$NOMBRE<-factor(distritos@data$NOMBRE)

# Quitamos acentos:
distritos@data$NOMBRE <- stri_trans_general(distritos@data$NOMBRE,"Latin-ASCII")

# 2) Barrios:
barrios <- readOGR(dsn = "data/BARRIOS", 
                     layer = "Barrios",
                     encoding = "UTF-8")

str(barrios, max.level = 2)
str(barrios@data)

# 2.1) Transfomamos a mayúsc el nombre de los distritos, para que crucen con las otras tablas y tb los barrios, ya que el resto está en mayusc
cols = c("NOMDIS",
         "NOMBRE")

setDT(barrios@data)
barrios@data[, (cols) := lapply(.SD, toupper), .SDcols=cols]
barrios@data[, (cols) := lapply(.SD, factor), .SDcols=cols]
rm(cols)



# Callejero: --------------------------------------------------------------

dt_callejero_cruces <- fread("data/CALLEJERO_VIGENTE_CRUCES_201802.csv")
dt_callejero_numeraciones <- fread("data/CALLEJERO_VIGENTE_NUMERACIONES_201802.csv")

# 1) Cambiamos nombre de las variables: 
cambio_nombre <- function(dt){
setnames(dt,
         new = toupper(gsub(" ", "_", names(dt))),
         old = names(dt))
}

lapply(list(dt_callejero_cruces, dt_callejero_numeraciones), FUN = cambio_nombre)


