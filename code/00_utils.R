if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggmap,
               sp,
               rgdal,
               rgeos,
               maptools,
               dplyr,
               tidyr,
               tmap,
               leaflet,
               spbabel,
               spbabel,
               geosphere,
               htmlwidgets,
               stringi,
               stringdist, 
               raster)

options(scipen=999, digits = 2)

# Establecemos directorios:

# 1) Working directory
setwd("~/R/ANALISIS/ACCIDENTES_BICICLETA_MADRID")

# 2) Código:
codepath <- "~/R/ANALISIS/ACCIDENTES_BICICLETA_MADRID/code/"

# 3) Datos:
datapath <- "~/R/ANALISIS/ACCIDENTES_BICICLETA_MADRID/data/"

theme(axis.text.y = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank())



