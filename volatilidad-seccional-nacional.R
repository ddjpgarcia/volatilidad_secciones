setwd("~/volatildiad-diputados")
rm(list=ls(all=TRUE))
options(scipen = 999)
###----------------
library(pacman)
p_load(purrr, dplyr,tidyverse, haven, tibble, sjlabelled,janitor, readr, ggplot2, readxl, openxlsx, foreign, esaps)
#---------Vamos a identificar las coaliciones en cada elección 
coaliciones_diputados <- read.xlsx("coaliciones diputados 2018-2021.xlsx")
names(coaliciones_diputados)
table(coaliciones_diputados$ESTADO)


#----------
coalicion_pri_2018 <- read.xlsx("F:/bases diputados 2012-2021/volatilidad electoral/calculo volatilidad/volatildiad-diputados/otros/coalicion todos por méxico.xlsx")
names(coalicion_pri_2018)
table(coalicion_pri_2018$ESTADO)
table(coalicion_pri_2018$TXM_2018)

coaliciones_diputados <- left_join(coaliciones_diputados, coalicion_pri_2018, by = c("ESTADO","ID_DISTRITO"))
#-------
coalicion_pan_2018 <- read.xlsx("F:/bases diputados 2012-2021/volatilidad electoral/calculo volatilidad/volatildiad-diputados/otros/mexico_al_frente.xlsx")
names(coalicion_pan_2018)
table(coalicion_pan_2018$ESTADO)
table(coalicion_pan_2018$MXF_2018)
coalicion_pan_2018$ESTADO[coalicion_pan_2018$ESTADO=="MÉXICO"]<-"EDOMEX"


coaliciones_diputados <- left_join(coaliciones_diputados, coalicion_pan_2018, by = c("ESTADO","ID_DISTRITO"))
table(coaliciones_diputados$ESTADO)
#--------------------
coalicion_morena_2018 <- read.xlsx("F:/bases diputados 2012-2021/volatilidad electoral/calculo volatilidad/volatildiad-diputados/otros/juntos_haremos_historia.xlsx")
names(coalicion_morena_2018)
table(coalicion_morena_2018$ESTADO)
table(coalicion_morena_2018$JHH_2018)


coaliciones_diputados <- left_join(coaliciones_diputados, coalicion_morena_2018, by = c("ESTADO","ID_DISTRITO"))
#----------------
coaliciones_diputados[is.na(coaliciones_diputados)] <- 0
table(coaliciones_diputados$JHH_2018)
table(coaliciones_diputados$TXM_2018)
table(coaliciones_diputados$MXF_2018)
#-----------
#------------Cargamos todos las bases de los resultados 2012-2021
xlsx <- list() # creates a list
list_xlsx <- dir(pattern = "*.xlsx")
for (k in 1:length(list_xlsx)){
  xlsx[[k]] <- read_xlsx(list_xlsx[k])
}

#Asignamos dataframes por año
#diputados_secc_2012 <- (xlsx[[1]])
#diputados_secc_2015 <- (xlsx[[2]])
diputados_secc_2018 <- (xlsx[[3]])
diputados_secc_2021 <- (xlsx[[4]])

#--
excluir <- c("CIRCUNSCRIPCION","ID_ESTADO","CABECERA_DISTRITAL","ID_MUNICIPIO","MUNICIPIO","CASILLAS", "LISTA_NOMINAL")

filtro_columnas <- function(dataframe){
  
  dataframe <- dataframe|>select(!excluir)
  dataframe <- dataframe|>mutate(TOTAL_VOTO_EFECTIVO=TOTAL_VOTOS-(NUM_VOTOS_CAN_NREG+NUM_VOTOS_NULOS))  
  ###-----------------
  dataframe <- dataframe|>select(!starts_with("NUM_"))
  ##--------------------
  return(dataframe)
}

#-------
#diputados_secc_2012 <- filtro_columnas(diputados_secc_2012)
#diputados_secc_2015 <- filtro_columnas(diputados_secc_2015)
diputados_secc_2018 <- filtro_columnas(diputados_secc_2018)
diputados_secc_2021 <- filtro_columnas(diputados_secc_2021)

#---Resultados secciones electorales 2018
diputados_secc_2018 <- diputados_secc_2018|>mutate(CAND_IND=CAND_IND1+CAND_IND2+CAND_IND3+CAND_IND4+CAND_IND5
                                                   +CAND_IND6+CAND_IND7+CAND_IND8+CAND_IND9+CAND_IND10+CAND_IND11+
                                                     CAND_IND12+CAND_IND13+CAND_IND14+CAND_IND15+CAND_IND16+CAND_IND17+CAND_IND18+
                                                     CAND_IND19+CAND_IND20+CAND_IND21+CAND_IND22+CAND_IND23+CAND_IND24+CAND_IND25+
                                                     CAND_IND26+CAND_IND27+CAND_IND28+CAND_IND29+CAND_IND30+CAND_IND31+CAND_IND32+
                                                     CAND_IND33+CAND_IND34+CAND_IND35+CAND_IND36+CAND_IND37+CAND_IND38)

#--
diputados_secc_2018 <- diputados_secc_2018|>select(1:24,63:65)
names(diputados_secc_2018)
#----
diputados_secc_2018 <- diputados_secc_2018|>
  mutate(C_TXM=PRI+PVEM+NVA_ALIANZA+PRI_NVA_ALIANZA+PRI_PVEM+PRI_NVA_ALIANZA+PVEM_NVA_ALIANZA,
         C_FXM=PAN+PRD+MC+PAN_PRD_MC+PAN_PRD+PAN_MC+PRD_MC,
         C_JHH=MORENA+PT+ES+PT_MORENA_ES+PT_MORENA+PT_ES+MORENA_ES)

#diputados_secc_2018 <- diputados_secc_2018|>select(1:12,27:30,TOTAL_VOTOS,TOTAL_VOTO_EFECTIVO)
#------
names(diputados_secc_2018)
table(diputados_secc_2018$NOMBRE_ESTADO)

diputados_secc_2018 <- diputados_secc_2018|>rename(ESTADO="NOMBRE_ESTADO")
diputados_secc_2018$ESTADO[diputados_secc_2018$ESTADO=="MÉXICO"]<-"EDOMEX"

diputados_secc_2018 <- left_join(diputados_secc_2018, coaliciones_diputados,by=c("ESTADO","ID_DISTRITO"))
diputados_secc_2018$COALICION_JHH_2021 <- NULL
diputados_secc_2018$COALICION_VXM_2021 <- NULL
#--------Resultados secciones 2021
names(diputados_secc_2021) 
diputados_secc_2021 <- diputados_secc_2021|>mutate(CAND_IND=CAND_IND1+CAND_IND2+CAND_IND3)

#--
diputados_secc_2021 <- diputados_secc_2021|>select(1:21,25:27)
diputados_secc_2021 <- diputados_secc_2021|>
  mutate(C_VXM=PAN+PRD+PRI+PAN_PRI_PRD+PAN_PRI+PAN_PRD+PRI_PRD,
         C_JHH=MORENA+PT+PVEM+PVEM_PT_MORENA+PVEM_PT+PVEM_MORENA+PT_MORENA)

#diputados_secc_2021 <- diputados_secc_2021|>select(1:13,24:26,TOTAL_VOTOS,TOTAL_VOTO_EFECTIVO)
#------
table(diputados_secc_2021$NOMBRE_ESTADO)
diputados_secc_2021 <- diputados_secc_2021|>rename(ESTADO="NOMBRE_ESTADO")
diputados_secc_2021$ESTADO[diputados_secc_2021$ESTADO=="MÉXICO"]<-"EDOMEX"

diputados_secc_2021 <- left_join(diputados_secc_2021, coaliciones_diputados,by=c("ESTADO","ID_DISTRITO"))
diputados_secc_2021$TXM_2018 <- NULL
diputados_secc_2021$MXF_2018 <- NULL
diputados_secc_2021$JHH_2018 <- NULL
#-------------------------------------
#write.xlsx(diputados_secc_2018, "secciones-diputados_2018.xlsx")
#write.xlsx(diputados_secc_2021, "secciones-diputados_2021.xlsx")
#-----
#Ahora vamos a crear una función para asignar los votos a partidos en coalición según la legislación electoral 
#--------
voto_coalicion <- function(df, col1,col2 ,col3,col4,col5, col6,col7, col8){ 
  
  #Para cada sección dentro de la base de datos cuando la exista alianza en el distrito 
  for (i in 1:nrow(df)) {
    if (df[i,col1] == 1) {
      
      #-Calculo los votos enteros y el resto de cada división para las diferentes combinaciones de la coalición  
      #-Votos para el partido 1
      votos_1 <- df[i,col5]%/%3 + df[i,col6]%/%2 +df[i,col7]%/%2
      resto_1 <- df[i,col5]%%3 + df[i,col6]%%2 +df[i,col7]%%2
      #-Votos para el partido 2
      votos_2 <- df[i,col5]%/%3 + df[i,col6]%/%2  + df[i,col8]%/%2
      resto_2 <- df[i,col5]%%3 + df[i,col6]%%2  + df[i,col8]%%2
      #-Votos para el partido 3
      votos_3 <- df[i,col5]%/%3  +df[i,col7]%/%2 + df[i,col8]%/%2
      resto_3 <- df[i,col5]%%3  +df[i,col7]%%2 + df[i,col8]%%2

      #Asigno los votos al partido 1 según los posibles supuestos, si hay empate en votos entre los tres al partido 1 le asigno el resto
      df[i,col2] <- case_when(
        df[i,col2]>df[i,col3]&df[i,col2]>df[i,col4]~df[i,col2]+votos_1+resto_1,
        df[i,col2]<df[i,col3]|df[i,col2]<df[i,col4]~df[i,col2]+votos_1,
        df[i,col2]==df[i,col3]&df[i,col2]==df[i,col4]&df[i,col3]==df[i,col4]~df[i,col2]+votos_1+resto_1,
        TRUE~NA)
      #Asigno los votos al partido 2 según los posibles supuestos
      df[i,col3] <- case_when(
        df[i,col3]>df[i,col2]&df[i,col3]>df[i,col4]~df[i,col3]+votos_2+resto_2,
        df[i,col3]<df[i,col2]|df[i,col3]<df[i,col4]~df[i,col3]+votos_2,
        TRUE~NA)
      #Asigno los votos al partido 3 según los posibles supuestos
      df[i,col4] <- case_when(
        df[i,col4]>df[i,col2]&df[i,col4]>df[i,col3]~df[i,col4]+votos_3+resto_3,
        df[i,col4]<df[i,col2]|df[i,col4]<df[i,col3]~df[i,col4]+votos_3,
        TRUE~NA)
      
    }
  }
  
  return(df)  
}


#---------------------------- 
diputados_secc_2018
diputados_secc_2018 <- voto_coalicion(diputados_secc_2018,"MXF_2018","PAN","PRD","MC","PAN_PRD_MC","PAN_PRD","PAN_MC","PRD_MC")#Coalición Al Frente por México
diputados_secc_2018 <- voto_coalicion(diputados_secc_2018,"TXM_2018","PRI","PVEM","NVA_ALIANZA","PRI_PVEM_NA_ALIANZA","PRI_PVEM","PRI_NVA_ALIANZA","PVEM_NVA_ALIANZA")#Coalición Todos por México
diputados_secc_2018 <- voto_coalicion(diputados_secc_2018,"JHH_2018","MORENA","PT","ES","PT_MORENA_ES","PT_MORENA","MORENA_ES","PT_ES")#Coalición Juntos Haremos Historia
#------------

voto_coalicion_2 <- function(df, col1,col2 ,col3,col4,col5, col6,col7){ 
  
  #Para cada sección dentro de la base de datos cuando la exista alianza en el distrito 
  for (i in 1:nrow(df)) {
    if (df[i,col1] == 1) {
      
      #-Calculo los votos enteros y el resto de cada división para las diferentes combinaciones de la coalición  
      #-Votos para el partido 1
      votos_1 <- df[i,col5]%/%3 + df[i,col6]%/%2
      resto_1 <- df[i,col5]%%3 + df[i,col6]%%2
      #-Votos para el partido 2
      votos_2 <- df[i,col5]%/%3 + df[i,col7]%/%2
      resto_2 <- df[i,col5]%%3 + df[i,col7]%%2
      #-Votos para el partido 3
      votos_3 <- df[i,col5]%/%3  +df[i,col6]%/%2 + df[i,col7]%/%2
      resto_3 <- df[i,col5]%%3  +df[i,col6]%%2 + df[i,col7]%%2
      
      #Asigno los votos al partido 1 según los posibles supuestos, si hay empate en votos entre los tres al partido 1 le asigno el resto
      df[i,col2] <- case_when(
        df[i,col2]>df[i,col3]&df[i,col2]>df[i,col4]~df[i,col2]+votos_1+resto_1,
        df[i,col2]<df[i,col3]|df[i,col2]<df[i,col4]~df[i,col2]+votos_1,
        df[i,col2]==df[i,col3]&df[i,col2]==df[i,col4]&df[i,col3]==df[i,col4]~df[i,col2]+votos_1+resto_1,
        TRUE~NA)
      #Asigno los votos al partido 2 según los posibles supuestos
      df[i,col3] <- case_when(
        df[i,col3]>df[i,col2]&df[i,col3]>df[i,col4]~df[i,col3]+votos_2+resto_2,
        df[i,col3]<df[i,col2]|df[i,col3]<df[i,col4]~df[i,col3]+votos_2,
        TRUE~NA)
      #Asigno los votos al partido 3 según los posibles supuestos
      df[i,col4] <- case_when(
        df[i,col4]>df[i,col2]&df[i,col4]>df[i,col3]~df[i,col4]+votos_3+resto_3,
        df[i,col4]<df[i,col2]|df[i,col4]<df[i,col3]~df[i,col4]+votos_3,
        TRUE~NA)
      
    }
  }
  
  return(df)  
}

#----------
diputados_secc_2021 <- voto_coalicion_2(diputados_secc_2021,"COALICION_VXM_2021","PAN","PRI","PRD","PAN_PRI_PRD","PAN_PRD","PRI_PRD")#Coalición Al Frente por México
diputados_secc_2021 <- voto_coalicion_2(diputados_secc_2021,"COALICION_JHH_2021","MORENA","PVEM","PT","PVEM_PT_MORENA","PT_MORENA","PVEM_PT")#Coalición Juntos Haremos Historia
#---------
colnames(diputados_secc_2018)
diputados_secc_2018 <- diputados_secc_2018|>select(31,32,1:12,27:30,25,26,33:35)

colnames(diputados_secc_2021)
diputados_secc_2021 <- diputados_secc_2021|>select(27,28,1:13,24:26,22,23,29,30)
#-------------
write.xlsx(diputados_secc_2018, "diputados-2018-voto sin coalicion.xlsx")
write.xlsx(diputados_secc_2021, "diputados-2021-voto sin coalicion.xlsx")

#------Calculo del porcentaje de voto
porcentaje_voto <- function(dataframe, col1){
  
  dataframe[6:18] <- dataframe[6:18]*100/dataframe[[col1]]
  return(dataframe)
  
}

#hist(rowSums(porcentaje_voto(diputados_secc_2021, "TOTAL_VOTO_EFECTIVO")[6:15], na.rm = T))
diputados_secc_2018_porc <-  porcentaje_voto(diputados_secc_2018, "TOTAL_VOTO_EFECTIVO")
diputados_secc_2021_porc <-  porcentaje_voto(diputados_secc_2021, "TOTAL_VOTO_EFECTIVO")

#------------------
colnames(diputados_secc_2018_porc)
diputados_secc_2018_porc <- diputados_secc_2018_porc|>rename(PES="ES")

############------Ahora calculamos la volatilidad tipo A : partidos nuevos y partidos que desaparecieron 
exit <- setdiff(names(diputados_secc_2018_porc[6:15]),names(diputados_secc_2021_porc[6:16]))
enter <- setdiff(names(diputados_secc_2021_porc[6:15]),names(diputados_secc_2018_porc[6:16]))

#---
colnames(diputados_secc_2021_porc)
volatilidad_A <-  diputados_secc_2021_porc|>select(3:5,all_of(enter))
volatilidad_A <- left_join(volatilidad_A,select(diputados_secc_2018_porc,3:5,all_of(exit)),by = c("ESTADO","ID_DISTRITO","SECCION"))
volatilidad_A[is.na(volatilidad_A)] <- 0
colnames(volatilidad_A)
volatilidad_A$VOLATILIDAD_A <- apply(volatilidad_A[4:6],1,sum)
volatilidad_A$VOLATILIDAD_A <- volatilidad_A$VOLATILIDAD_A/2

hist(volatilidad_A$VOLATILIDAD_A)
mean(volatilidad_A$VOLATILIDAD_A)
#------Vaoltilidad B: partidos estables
###Identificacmos a los partidos que no  cambiaron entre eleciones
stable <- intersect(names(diputados_secc_2018_porc),names(diputados_secc_2021_porc))
stable

#---Elección tiempo 0
a <- diputados_secc_2018_porc|>select(all_of(stable)&!starts_with("TOTAL_"))
a <- a|>select(3:14)
colnames(a)
a <- a|>gather(PARTIDO,VOTO_2018,4:12)
a
#---Elección tiempo 1
b <- diputados_secc_2021_porc|>select(all_of(stable)&!starts_with("TOTAL_"))
b <- b|>select(3:14)
colnames(b)
b <- b|>gather(PARTIDO,VOTO_2021,4:12)
b
#-------
volatilidad_B <- left_join(b, a, by = c("ESTADO","ID_DISTRITO","SECCION","PARTIDO"))
volatilidad_B[is.na(volatilidad_B)] <- 0
volatilidad_B <- volatilidad_B|>mutate(VOLATILIDAD_B=VOTO_2021-VOTO_2018)

colnames(volatilidad_B)
volatilidad_B <- volatilidad_B|>select(!starts_with("VOTO"))
volatilidad_B <- volatilidad_B|>spread(PARTIDO,VOLATILIDAD_B)
#write.xlsx(volatilidad_B, "volatilidad_b_secciones.xlsx")
volatilidad_B[4:12] <- apply(volatilidad_B[4:12],2,abs)
volatilidad_B$VOLATILIDAD_B <- apply(volatilidad_B[4:12],1,sum)
volatilidad_B$VOLATILIDAD_B <- volatilidad_B$VOLATILIDAD_B/2

hist(volatilidad_B$VOLATILIDAD_B)
mean(volatilidad_B$VOLATILIDAD_B)
rm(a,b)
#-----------------------------------
colnames(volatilidad_total)
volatilidad_total <- inner_join(volatilidad_A,volatilidad_B,by=c("ESTADO","ID_DISTRITO","SECCION"))
volatilidad_total[is.na(volatilidad_total)]<-0

volatilidad_total <- volatilidad_total|>mutate(VOLATILIDAD_TOTAL=VOLATILIDAD_A+VOLATILIDAD_B)

hist(volatilidad_total$VOLATILIDAD_TOTAL)
mean(volatilidad_total$VOLATILIDAD_TOTAL)
#----------------------------------
write.xlsx(volatilidad_total, "volatilidad_total_secciones.xlsx")
#-------
##----Cargando shapefiles INE
p_load(MCMCpack,coda,sf, patchwork, cowplot, viridis, broom, viridis)
###
seccion <- read_sf("F:/encuestas-edomex/analisis/SECCION.geojson")
head(seccion)
dim(seccion)
colnames(seccion)

seccion <- seccion|>rename(ID_ESTADO="ENTIDAD",ID_MUNICIPIO="MUNICIPIO", ID_DISTRITO="DISTRITO_F")
seccion
#edomex_seccion <- seccion|>filter(ESTADO==15)
#edomex_seccion
###
estados <- read_sf("F:/encuestas-edomex/analisis/ENTIDAD.geojson")
head(estados)
colnames(estados)

estados <- estados[order(estados$entidad), ]
estados <- estados|>rename(ID_ESTADO="entidad",ESTADO="nombre")

id_estados <- dplyr::select(estados,ID_ESTADO, ESTADO)
id_estados$geometry <- NULL
#write.xlsx(id_estados,"id_estados.xlsx")
#edomex_shp <- estados|>filter(CVE_ESTADO==15)
#rm(estados)
#--------------------------------------------
#municipios <- read_sf("MUNICIPIO.geojson")
#head(municipios)
#dim(municipios)
#municipios <- municipios|>rename(ESTADO="entidad",ID_MUNICIPIO="municipio",MUNICIPIO="nombre")
#edomex_munip <- municipios|>filter(ESTADO==15)
#----------------------------
id_estados <- read.xlsx("id_estados.xlsx")
id_estados

mapa_ine <- left_join(seccion, id_estados, by =  c("ID_ESTADO"))
mapa_ine <- inner_join(volatilidad_total, mapa_ine, by = c("ESTADO","ID_DISTRITO","SECCION"))
glimpse(mapa_ine)
#####---------------
extrafont::loadfonts()
#------------------------------------------
p_load(stratification)
cv <- sd(mapa_ine$VOLATILIDAD_TOTAL) / mean(mapa_ine$VOLATILIDAD_TOTAL) * 100
set.seed(123)
dalenius_secciones <-  strata.cumrootf(mapa_ine$VOLATILIDAD_TOTAL, CV=cv, Ls=5)
mapa_ine$estrato <-  dalenius_secciones$stratumID


table(mapa_ine$estrato)
mapa_ine <- mapa_ine|>mutate(VOLATILIDAD_NIVEL=case_when(
  estrato==1~"Muy baja",
  estrato==2~"Baja",
  estrato==3~"Media",
  estrato==4~"Alta",
  estrato==5~"Muy alta",
  TRUE~NA))

table(mapa_ine$VOLATILIDAD_NIVEL)
mapa_ine$VOLATILIDAD_NIVEL <- factor(mapa_ine$VOLATILIDAD_NIVEL, levels = c("Muy baja","Baja","Media","Alta","Muy alta"))
#---------------------------
#------Mapa de volatilidad : sinaloa
mapa_ine|>
  filter(ID_ESTADO==25)%>%
  ggplot()+
  scale_alpha(name = "",na.value = "grey80",
              range = c(0.6, 0),
              guide = "none")+
  geom_sf(aes(geometry=geometry, fill=VOLATILIDAD_NIVEL),
          size = 0.2,
          inherit.aes = FALSE)+
  scale_fill_manual(
    values = c('#016450','#02818a','#fdbe85','#e6550d',"#a63603"),
    # Redefine the fill colour for NA values
    na.value = "grey80",
    # Set the scale limits to be 0 and 10%
    #limits = c(0, 0.1),
    # Set the out-of-bounds ("oob") rule to squish out-of-bounds values to the nearest limit
    #oob = scales::squish,
    # Format labels as percentages
    #labels = scales::percent,
    # Give the scale a title
    guide = guide_legend(
      keyheight = unit(4, units = "mm"),
      title.position = "top",title = "Sinaloa\nVolatilidad Electoral\npor secciones (2018-2021, diputados)",
      reverse = T ))+
  geom_sf(
    data =filter(mapa_ine, ID_ESTADO==25),aes(geometry=geometry),
    fill = "transparent",
    color = "black",
    size = 0.001,
    inherit.aes = FALSE)+
  geom_sf(
    data =filter(estados, ID_ESTADO==25),aes(geometry=geometry),
    fill = "transparent",
    color = "black",
    size = 1,
    inherit.aes = FALSE)+
  #theme_map()+
  theme(
    legend.justification = c(0, 1),
    legend.position = c(0, .2)
  ) +
  theme(
    text = element_text(family = "JetBrains MOno", face="bold"),
    legend.title = element_text(family = "JetBrains Mono", size = 14, face="bold"),
    legend.text = element_text(family = "JetBrains Mono", size = 12)) -> volatilidad_sinaloa

volatilidad_sinaloa
dev.off()
ggsave(volatilidad_sinaloa, filename = "volatilidad_sinaloa.png",width = 20, height = 10, dpi = 100)
##--------------
mapa_ine_sinaloa <- mapa_ine|>filter(ID_ESTADO==25)
table(mapa_ine_sinaloa$VOLATILIDAD_NIVEL)

cor(mapa_ine_sinaloa$VOLATILIDAD_TOTAL, mapa_ine_sinaloa$MORENA)
cor(mapa_ine_sinaloa$VOLATILIDAD_B, mapa_ine_sinaloa$MORENA)
cor(mapa_ine_sinaloa$VOLATILIDAD_A, mapa_ine_sinaloa$MORENA)

write.xlsx(dplyr::select(mapa_ine_sinaloa, 1:24,29,30), "volatilidad_sinaloa.xlsx")
#-----
# Create data
secciones_volatilies_sinaloa <- data.frame(
  name=c("Muy baja","Baja","Media","Alta","Muy alta") ,  
  value=c(1284,925,587,469,465))

secciones_volatilies_sinaloa$name <- factor(secciones_volatilies_sinaloa$name, levels = c("Muy baja","Baja","Media","Alta","Muy alta"))

# Barplot
secciones_volatilies_sinaloa|>
  ggplot(aes(name,value, fill=name))+
  geom_bar(stat = "identity")+
  geom_text(aes(y=150,label=value, group=1),position=position_dodge(0.9),vjust=0.99,hjust=0.3,size=5, color="white",show.legend = F,fontface="bold" ,family="JetBrains Mono")+
  scale_fill_manual(
    values = c('#016450','#02818a','#fdbe85','#e6550d',"#a63603"),
    guide = guide_legend(
      keyheight = unit(4, units = "mm"),
      title.position = "top",title = "",
      reverse = T ))+
  labs(title = stringr::str_wrap("Sinaloa - Volatilidad por secciones electorales", width = 200),
       subtitle = stringr::str_wrap("Elecciones 2018-2021 diputados", width = 200),
       x =" ", y =" ",
       caption = "Muy baja=0% – 15.5% Baja=15.5% – 19.9% Media=19.9% – 24.6% Alta=24.6% – 32.6% Muy alta=32.6% – 100% ")+
  theme(legend.box ="vertical",
        legend.position="none",
        #legend.position = c(0.10, 0.88),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.title = element_text(hjust =0.1,  
                                    size = 12, face = "bold", color="black"),
        legend.text= element_text(size = 20, face = "bold", color = "black",family="JetBrains Mono"),
        axis.title.y = element_text(size = 14, face="bold", colour = "black"),
        axis.title.x = element_text(size = 14, face="bold", colour = "black"),
        axis.ticks  =  element_blank(),
        axis.text.x = element_text(size = 14, color = "black",face="bold",family="JetBrains Mono"),
        #axis.text.y = element_text(size = 19, color = "black",face="bold",family="JetBrains Mono"),
        axis.text.y = element_blank(),
        #panel.grid.major.x  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
        #panel.grid.minor.x  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
        panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        legend.box.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(face = "bold", size = 18,margin = margin(10,0,20,0), hjust =0, family="JetBrains Mono",colour = "black"),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 17.6, colour = "black", family = "JetBrains Mono"),
        plot.caption = element_text(hjust = 0,face = "bold", colour = "black", family="JetBrains Mono",size = 14),
        axis.line.y  = element_blank(), 
        legend.background = element_rect(color="black",fill = "white"),
        strip.background = element_rect(color="black",fill = "white"),
        strip.text = element_text(hjust = 0.5, face = "bold", size = 12, color = "black")) -> sinaloa_resumen


sinaloa_resumen
dev.off()
ggsave(sinaloa_resumen, filename = "sinaloa-resumen.png",width = 20, height = 10, dpi = 100)



