


# Análisis:

# Cargamos scripts:
source(paste0(codepath,'00_utils.R'))
source(paste0(codepath,'01_limpieza.R'))

# Número de accidentes:
NROW(dt_accidentes)

# Número de accidentes por tipología:
table(dt_accidentes$TIPO_ACCIDENTE)
accidentes <- dt_accidentes[,.(.N), by = TIPO_ACCIDENTE][,PERC := N/sum(N)][,TABLA := 'Accidentes']

# Número de víctimas:
sum(dt_accidentes$NM_TOT_VICTIMAS)


# Número de víctimas por tipo de accidentes:
victimas <- dt_accidentes[, .(N = sum(NM_TOT_VICTIMAS)), by = TIPO_ACCIDENTE][,PERC := N/sum(N)][,TABLA := 'Víctimas']

# Número de víctimas versus número de accidentes:
funion(accidentes,
       victimas, 
       all = FALSE) %>% 
  ggplot(aes(x = TIPO_ACCIDENTE, y = N, group = TABLA, fill = TABLA)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_brewer(palette = 'Pastel2')+
  geom_text(aes(x = TIPO_ACCIDENTE, 
                y = N, 
                label = N),
            position = position_dodge(width = 1),
            vjust = -0.5) +
  labs(x = 'Tipo de accidente',
       title = 'Número de víctimas vs número de accidentes', 
       fill = NULL)+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank())

rm(accidentes, victimas) 
    
# Accidentes por franja horaria            

dt_accidentes %>% 
  group_by(TRAMO_HORARIO) %>% 
  count() %>%
  ggplot(aes(TRAMO_HORARIO,y= n))+
  geom_col(aes(fill = n))+
  coord_polar()+
  theme_bw()

# Accidentes por franja horaria y distrito:
dt_accidentes %>% 
    group_by(TRAMO_HORARIO,DISTRITO) %>% 
    count() %>%
    ggplot(aes(TRAMO_HORARIO, n, group=DISTRITO, color=DISTRITO))+
    geom_polygon(aes(fill= DISTRITO),  alpha=0.3)+
    coord_polar()+
    theme_bw()+
  labs(x = "Tramo horario", 
       y = "Número de accidentes", 
       color = "Distrito", 
       fill = "Distrito")


# Accidentes por mes:
dt_accidentes %>% 
  mutate(MES = as.Date(as.yearmon(FECHA))) %>% 
  group_by(MES) %>% 
  count() %>% 
  ggplot(aes(x = MES, y = n))+
  geom_col(fill = 'darkgreen', alpha = 0.3)+
  geom_line(aes(y=mean(n)),color =  "orange")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B")+
  geom_text(aes(x = MES, 
                y = n, 
                label = n),
            position = position_dodge(width = 1),
            vjust = -0.5) +
  labs(x = 'Accidentes',
       title = 'Número accidentes por mes', 
       fill = NULL)+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank())


# Número de accidentes por distrito:

acc_dist <-dt_accidentes[,.(NUM_ACC = .N), by = DISTRITO]
setorder(acc_dist, -NUM_ACC)
top_10 <- head(acc_dist, 10)
rm(acc_dist)


# Accidentes por distrito:

# Distritos coincidentes:
table(levels(dt_accidentes$DISTRITO) %in% distritos@data$NOMBRE)

# Hacer que coincidan:
dt_accidentes[DISTRITO == 'FUENCARRAL-EL PARDO']$DISTRITO <- 'FUENCARRAL - EL PARDO'
dt_accidentes[DISTRITO == 'MONCLOA-ARAVACA']$DISTRITO <- 'MONCLOA - ARAVACA'
dt_accidentes[DISTRITO == 'SAN BLAS']$DISTRITO <- 'SAN BLAS - CANILLEJAS'

# Volvemos a crear factor:
droplevels(dt_accidentes$DISTRITO)
dt_accidentes$DISTRITO <- factor(dt_accidentes$DISTRITO)


# Visualización accidentes por distrito
a<- dt_accidentes[,.(.N), by = DISTRITO]
a<- dt_accidentes[,.(ATROPELLOS = .N), by = .(DISTRITO, TIPO_ACCIDENTE)][TIPO_ACCIDENTE == 'ATROPELLO']


distritos@data <-distritos@data %>% 
  left_join(a, by = c('NOMBRE' = 'DISTRITO' ))

tmap_leaflet(tm_shape(distritos) +
               tm_fill("ATROPELLOS")+
               tm_text(text = "NOMBRE", size = 1)+
               tm_borders()+
               tm_style_gray())

callejero <- dt_callejero_numeraciones[, .(CODIGO_DE_BARRIO,
                                          NOMBRE_DEL_BARRIO,
                                          CODIGO_DE_DISTRITO,
                                          NOMBRE_DEL_DISTRITO,
                                          NOMBRE_DE_LA_VÍA)]

# Pasamos a factor:
cols <- names(callejero)
callejero[, (cols) := lapply(.SD, factor), .SDcols=cols]
callejero[, (cols) := lapply(.SD, str_trim), .SDcols=cols]
rm(cols)

str(callejero)

# Transformo nombre de calles:
dt_accidentes$LUGAR <- gsub(pattern="CALLE ",
     replacement="",
     dt_accidentes$LUGAR)

dt_accidentes$LUGAR <- gsub(pattern="DE ",
     replacement="",
     dt_accidentes$LUGAR)

dt_accidentes$LUGAR <- gsub(pattern="LA ",
                            replacement="",
                            dt_accidentes$LUGAR)6



























































-
dt_accidentes$LUGAR <- str_trim(dt_accidentes$LUGAR)

table(dt_accidentes$LUGAR  %in% callejero$NOMBRE_DE_LA_VÍA)

dt_accidentes[LUGAR  %in% callejero$NOMBRE_DE_LA_VÍA,]
dt_accidentes[!(LUGAR  %in% callejero$NOMBRE_DE_LA_VÍA),]



# Concentración de accidentes por calles:
accidentes %>% 
  group_by(DISTRITO, Lugar) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  top_n(n=15) %>% 
  ggplot(aes(reorder(Lugar,-n), n, group=DISTRITO))+
  geom_col(aes(fill = DISTRITO))+
  theme_bw()+
  theme(axis.text.x =element_text(angle = 90),
        axis.title = element_blank() )









table(dt_accidentes$TIPO_ACCIDENTE)
dt_accidentes %>% 
  group_by(DISTRITO, TIPO_ACCIDENTE) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(DISTRITO) %>% 
  mutate(n_distrito = sum(n)) %>% 
  mutate(perc = n/n_distrito) %>% 
  arrange(desc(n)) %>% 
  filter(DISTRITO %in% c("CENTRO", "CHAMBERI", "RETIRO")) %>% 
  ggplot(aes(reorder(TIPO_ACCIDENTE, -perc), perc, group = DISTRITO)) +
  geom_bar(aes(fill=DISTRITO), stat = 'identity', position = 'dodge')+
  scale_y_continuous(labels = scales::percent)


dt_accidentes %>% 
  filter(TIPO_ACCIDENTE == 'COLISIÓN DOBLE') %>% 
  group_by(DISTRITO) %>% 
  count() %>% 
  arrange(desc(n))

dt_accidentes %>% 
  mutate(MES = as.Date(as.yearmon(FECHA))) %>% 
  group_by(MES) %>% 
  count() %>% 
  ggplot(aes(x = MES, y = n))+
  geom_col(color = 'dimgrey', alpha = 0.3)+
  geom_line(aes(y=mean(n)),color =  "red")+
  theme_classic()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B")