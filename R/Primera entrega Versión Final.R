# Instalación de paquetes ########################
library(leaflet)
library(GADMTools)
library(ggplot2)
library(reshape2)
library(shiny)
library(markdown)
library(mapview)
library(readxl)
library(tidyverse)
library(tidyr)
library(plotly)
library(readr)
library(plyr)
library(sqldf)
suppressPackageStartupMessages(library(mapview))
# Preliminares del Shiny  ###########

ui <-navbarPage("Trabajo Demografía",
                
                tabPanel("Descripcion",
                         verbatimTextOutput("Descripcion"),
                         
                         
                         
                ),
                
                

###############################################################################
# ****************************************************************************#
###############################################################################
                                
###################################################
# Paneles de los mapas de Fecundidad
################################################### 
tabPanel("Estructura Poblacional",
         sidebarLayout(
             sidebarPanel(
                 selectInput("D1",
                             "Seleccione que desea analizar",
                             choices = c(
                                         "Población por municipio",
                                         "Población por genero",
                                         "Población por genero en cada municipio",
                                         "Poblacion por edad ",
                                         "Trancisión demografica"
                                         )),
              
                 
             ),
             
             
             
             mainPanel = plotOutput("D1")
             
             
             
             
         )),


###################################################
# Paneles de las tasas de Fecundidad
################################################### 

tabPanel("Fecundidad",
         sidebarLayout(
           sidebarPanel(
             selectInput("D2",
                         "Seleccione que desea analizar",
                         choices = c("Tasa de Crecimiento poblacional",
                                     "Diferencias absolutas",
                                     "Diferencias porcentuales",
                                     "TBN Ajustado",
                                     "TFGen Ajustado",
                                     "Tasa especifica de fecundidad por edad Ajustado",
                                     "Edad media de fecundidad Ajustado",
                                     "TGF Ajustado",
                                     "TGF por municipio")),
             
             
           ),
           
           
           
           mainPanel = plotOutput("D2")
           
           
           
           
         )),



###################################################
# Paneles de los mapas de Mortalidad
################################################### 
tabPanel("Mapas Mortalidad",
         sidebarLayout(
           sidebarPanel(
             selectInput("D4",
                         "Seleccione que desea analizar",
                         choices = c(
                           "Porcentaje de muertes por accidentes de tránsito en cada municipio",
                           "Porcentaje de muertos por homicidios en cada municipio",
                           "Población por genero en cada municipio",
                           "Poblacion por edad ",
                           "Trancisión demografica"
                         )),
             
             
           ),
           
           
           
           mainPanel = plotOutput("D4")
           
           
           
           
         )),



###################################################
# Paneles de los tasas de mortalidad
################################################### 

tabPanel("Mortalidad",
         sidebarLayout(
             sidebarPanel(
                 selectInput("D3",
                             "Seleccione que desea analizar",
                             choices = c("Tasa Bruta de Mortalidad",
                                         "Mortalidad por rango etario",
                                         "Tasa específica de Mortalidad",
                                         "Defunciones causas Externas Hombre",
                                         "Defunciones causas Externas Mujer",
                                         "Defunciones causas Externas Hombre y Mujer",
                                         "Defunciones causas Externas Accidentes de tránsito",
                                         "Defunciones causas Externas Homicidios",
                                         "Defunciones causas Externas Suicidios",
                                         "Defunciones causas Naturales Hombre",
                                         "Defunciones causas Naturales Mujer",
                                         "Defunciones causas Naturales Hombre y Mujer",
                                         "Defunciones causas IIJ",
                                         "Defunciones causas NPOe y Mujer",
                                         "Defunciones causas NOPÒe y Mujer",
                                         "Defunciones causas UImbre y Mujer")),
                 
                 
             ),
             mainPanel(
                 plotOutput("D3")
         )
         )
                )

)




###############################################################################
# ****************************************************************************#
###############################################################################

###################################################
#  * * * Creación del mapa central 
##################################################

# Bases de datos
Nombres <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Defunciones/Nombres.xlsx")

# Parámetros de Colombia y mapa de colombia
COL <- gadm_sf_loadCountries(c("COL"), level=0, basefile="./")
COL; gadm_plot(COL)

# Parámetros de Colombia por departamentos
DEPTOS <- gadm_sf_loadCountries(c("COL"), level = 1, basefile = "./")
gadm_plot(DEPTOS)

# Parámetros de Colombia por municipios
MUNICIPIOS <- gadm_sf_loadCountries(c("COL"), level = 2, basefile = "./")
gadm_plot(MUNICIPIOS)

# Parámetro de Boyacá y mapa de Boyaca
Boyaca <- gadm_subset(DEPTOS, level=1, regions="Boyacá")
gadm_plot(Boyaca) %>% gadm_showNorth("tl") %>% gadm_showScale("bl")

# Parámetro de Boyacá y mapa de Boyaca por municipios
muncB <- gadm_subset(MUNICIPIOS, 1, "Boyacá", usevar = NULL)
gadm_plot(muncB)

# Creación de municipalidades
Municpalidades<-MUNICIPIOS$sf
Boyaca<-subset(Municpalidades, NAME_1=="Boyacá")
names(Boyaca)[4]<-"Municipios"
for (i in 1:123) {
    Boyaca$Municipios[i]<-Nombres$Nombres[i]
}
Boyaca %>% mapview(zcol = "ISO", legend = T, col.regions = sf.colors)






###############################################################################
# ******************** Estructura poblacional ******************************#
###############################################################################

###### Base de datos Censo ####
Censo_2018 <- read_csv("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Censo/Censo-2018.CSV")
Censo_2018$P_SEXO<-factor(Censo_2018$P_SEXO,levels = c("1","2"),labels = c("Hombre","Mujer"))
Censo_2018$P_EDADR<-factor(Censo_2018$P_EDADR,
                         levels = c("1","2","3","4","5","6","7","8","9","10", "11","12","13","14",
                                    "15","16","17","18","19","20","21"),
                         labels = c(" 0 A 4 Años"," 5 A 9 Años"," 10 A 14 Años"," 15 A 19 Años",
                                    " 20 A 24 Años"," 25 A 29 Años"," 30 A 34 Años"," 35 A 39 Años",
                                    " 40 A 44 Años"," 45 A 49 Años"," 50 A 54 Años"," 55 A 59 Años",
                                    " 60 A 64 Años"," 65 A 69 Años"," 70 A 74 Años"," 75 A 79 Años",
                                    " 80 A 84 Años"," 85 A 89 Años"," 90 A 94 Años"," 95 A 99 Años",
                                    " 100 o mas Años"))

###### Gráfico 1. Número de personas por Género####

G1<-ggplot(Censo_2018,mapping = aes(x=P_SEXO))+
    geom_bar(fill=cm.colors(2))+
    annotate("text", x=c(1,2),y=500000,label=c("49.23%","50,77%"))+
    ylab("Número de personas")+
    xlab("Genero")+
    ggtitle("Número de personas por genero en Boyacá Año 2018" )
G1<-ggplotly(G1)


###### Mapa 1. Poblacion por genero según el municipio ####
B2<-Censo_2018 %>% filter(P_SEXO=='Hombre') %>% 
    dplyr::select(U_MPIO) %>% 
    dplyr::group_by(U_MPIO) %>% 
      dplyr::summarise(h=n())
Total<-Censo_2018 %>% 
    dplyr::select(U_MPIO) %>% 
    dplyr::group_by(U_MPIO) %>% 
    dplyr::summarise(t=n())
b2<-inner_join(B2,Total)    
Municipios_dane <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Municipios_dane.xlsx")
b2[,4]<-b2$h/b2$t*100

b2[,1]<-Municipios_dane$Nombres
names(b2)[4]<-"Porcentaje de hombres"
names(b2)[1]<-"Municipios"

b2<-b2 %>% select('Municipios',`Porcentaje de hombres`)
b2<-left_join(Boyaca,b2)

G2<-b2 %>% mapview(zcol = "Porcentaje de hombres", legend = T, col.regions = sf.colors)

###### Mapa 2. Poblacion por municipios ####
B3<-Censo_2018 %>% 
  dplyr::select(U_MPIO) %>% 
  dplyr::group_by(U_MPIO) %>% 
  dplyr::summarise(t=n())


B3[,1]<-Municipios_dane$Nombres
B3[,2]<-round( B3[,2]/nrow(Censo_2018)*100,5)
names(B3)[2]<-"Porcentaje por municipio"

names(B3)[1]<-"Municipios"



B3<-left_join(Boyaca,B3)

G3<-B3 %>% mapview(zcol = "Porcentaje por municipio", legend = T, col.regions = sf.colors)




###### Gráfico no mostrado 1. Poblacion del departamento desde el 2018 ############
B14 <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Totales poblaacionales proyectados/T_P.xlsx")
B14<- B14 %>%dplyr::select(Año,Total)
B14<-B14[c(-1,-2-3),]

G14<-ggplotly(B14 %>% ggplot(mapping = aes(x=Año,y=Total))+
                geom_line(col="red")+
                geom_point()+
                ggtitle("Población en el departameto desde el 2008."))


###### Gráfico no mostrado 2. Población por genero desde el 2008 ###########
B13 <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Totales poblaacionales proyectados/H_total.xlsx")
B13<-B13 %>% dplyr::select(Año, Hombres) %>%
  dplyr::filter(Año>2007)

B132 <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Totales poblaacionales proyectados/M_T.xlsx")
B132<-B132 %>% dplyr::select(Año, Total) %>%
  dplyr::filter(Año>2007) 
names(B132)[2]<-"Mujeres"
B13<-inner_join(B13,B132)

B13<-B13 %>% reshape2::melt(value.name = "Total",
                            variable.name = 'Sexo',
                            id.vars = 'Año'
)
G13<-B13 %>% ggplot(mapping = aes(x=Año,y=Total,fill=Sexo,col=Sexo))+
  geom_line()+
  geom_point()+
  ggtitle("Población por genero desde el 2008")
G13<-ggplotly(G13)





###############################################################################
# ************************* Pirámide poblacional *****************************#
###############################################################################

###### Pirámide 1. Poblacion por edades año 2018 ####
datos2<-Censo_2018 %>%dplyr::filter(P_SEXO=="Mujer") %>% 
  dplyr::select(P_EDADR) %>% 
  dplyr::group_by(P_EDADR) %>% 
  dplyr::summarise(Mujeres=n())
datos3<-Censo_2018 %>% filter(P_SEXO=="Hombre") %>% 
  dplyr::select(P_EDADR) %>% 
  dplyr::group_by(P_EDADR) %>% 
  dplyr::summarise(Hombres=n())  

Datos<-inner_join(datos2,datos3)  
Datos[,4]<-round(Datos$Mujeres/nrow(Censo_2018)*100,3)
Datos[,5]<-round(Datos$Hombres/nrow(Censo_2018)*100,3)
Datos<-Datos[,-2]
Datos<-Datos[,-2]
names(Datos)[2:3]<-c("Mujeres","Hombres")
Datos$Hombres<--1*Datos$Hombres
datos.melt<-melt(Datos,
                 value.name = 'Poblacion',
                 variable.name = 'Sexo',
                 id.vars = 'P_EDADR')

G4<-ggplotly( ggplot(datos.melt, aes(x=P_EDADR,y=Poblacion, fill=Sexo))+
            geom_bar(subset=.(Sexo=="Mujeres"),stat ="identity" )+
            geom_bar(subset=.(Sexo=="Hombre"),stat ="identity" )+
            coord_flip()+
              ggtitle("Piramide poblacional año 2018")+
              xlab("Quinquenios "))

###### Piramide 2. Piramide superpuesta ####
Superpuesta <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Pirámide poblacional/Superpuesta.xlsx")
B5<-Superpuesta[,1:5]
B5[,2:5]<-round(100*B5[,2:5],4)
B5$`Hombres 2005`<--1*B5$`Hombres 2005`
B5$`Hombres 2010`<--1*B5$`Hombres 2010`

B5<-B5[,-1]
B5<-cbind(Datos,B5)
names(B5)[2:3]<-c("Mujeres 2018","Hombres 2018 ")
datos.melt2<-melt(B5,
                 value.name = 'Poblacion',
                 variable.name = 'Sexo',
                 id.vars = 'P_EDADR')

G5<-ggplotly( ggplot(datos.melt2, aes(x=P_EDADR,y=Poblacion, fill=Sexo))+
                
                geom_point(subset=.(Sexo=="Hombres 2005"),stat ="identity",size=3 )+
                geom_point(subset=.(Sexo=="Mujeres 2010"),stat ="identity",size=3 )+
                geom_point(subset=.(Sexo=="Hombres 2010"),stat ="identity" ,size=3)+
                geom_point(subset=.(Sexo=="Mujeres 2018"),stat ="identity",size=3 )+
                geom_point(subset=.(Sexo=="Hombres 2018"),stat ="identity" ,size=3)+
                coord_flip()+
                ggtitle("Piramide poblacional")+
                xlab("Quinquenios "))




###############################################################################
# *********************** Tasas de Natalidad *********************************#
###############################################################################

###### Gráfico 2. Tasa de Crecimiento poblacional ####
Crecimiento_poblacional<- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Totales poblaacionales proyectados/CRECIMIENTO_POBLACIONAL.xlsx")
Crecimiento<-Crecimiento_poblacional[c(1,3:5)]
Crecimiento<-melt(Crecimiento,
                  value.name = 'Total',
                  id.vars = 'Año', 
                  variable.name = 'Promedio')

Grafico_40<-ggplotly(ggplot(data=Crecimiento, 
                            mapping = aes(x=Año, y=Total,group=Promedio,colour=Promedio))+
                       geom_line( )+
                       geom_point()+
                       ggtitle(label = "Tasa de Crecimiento Poblacional"))


###### Gráfico 3  Diferencias con los datos ajustados ################
Diferencias_absolutas <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Diferencias/Diferencias_absolutas.xlsx")
Diferencias_absolutas<-Diferencias_absolutas[1:2,]

Diferencias_absolutas<-melt(Diferencias_absolutas,
                            value.name = 'Total',
                            variable.name = 'Edad',
                            id.vars = 'Diferencia')


G11<-ggplotly(ggplot(data=Diferencias_absolutas, mapping = aes(x=Edad, y=Total,group=Diferencia,fill = Diferencia,colour=Diferencia))+
                geom_bar(stat="identity",na.rm=TRUE) + 
                ggtitle(label = "Diferencia Dato ajustado - Dato Censo (Total->81.678, H->40209, M->41469)")
)


Diferencias_porcentuales <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Diferencias/Diferencias_porcentuales.xlsx")
Diferencias_porcentuales<-Diferencias_porcentuales[1:2,]

Diferencias_porcentuales<-melt(Diferencias_porcentuales,
                               value.name = 'Total',
                               variable.name = 'Edad',
                               id.vars = 'Diferencia')

G11B<-ggplotly(ggplot(data=Diferencias_porcentuales, mapping = aes(x=Edad, y=Total,group=Diferencia,fill = Diferencia,colour=Diferencia))+
                 geom_bar(stat="identity",na.rm=TRUE) + 
                 ggtitle(label = "Diferencia porcentual (M->50.77%, H->49.22)"))


###### Gráfico 4. Tasa bruta de natalidad Ajustado segundo corte ####
c7 <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Nacimientos/C7-Nacimientos por grupo de edad de la madre, según departamento y municipio de residencia de la madre 2008-2018/c7.xlsx")
names(c7)[1]<-"Municipios"
B6 <- c7 %>% filter(Municipios=='Total') %>% 
  select(Municipios,Total,año)

T_P <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Totales poblaacionales proyectados/T_P.xlsx")
names(B6)[2]="Total nacimientos"
B6[,4]<-T_P[4:14,23]
B6[,5]<-round( B6[,2]/B6[,4]*1000,2)
names(B6)[5]<-"TBN"
BB6<-B6
BB6[12,]<-BB6[11,]
BB6[12,4]<-1217376
BB6[12,5]<-13801/1217376*1000
TBN<-rbind(BB6[1:10,],BB6[12,],BB6[11,])
G6<-ggplotly(ggplot(data=TBN, mapping = aes(x=año, y=TBN))+
           geom_line(col="blue")+
           geom_point(size=1,col="black")+
           scale_x_continuous(limit=c(2007,2018))+
           ggtitle(label = "TBN (por 1.000 habitantes) desde 2008 a 2018 (Con Correción)"))




###### Gráfico 5. Tasa de Fecundidad Ajustado segundo corte #### 
M_T <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Totales poblaacionales proyectados/M_T.xlsx")

B7<-M_T %>% select(Año,T_muj54)
B7<-B7[c(-1,-2,-3),]
B7[,3]<-B6$`Total nacimientos`
B7[,4]<-round(B7[,3]/B7[,2]*1000,2)
names(B7)[4]<-"TFGen"

BB7<-B7
BB7[12,]<-BB7[11,]
BB7[12,2]<-388168
BB7[12,4]<-13801/388168*1000

Tfgem<-rbind(BB7[1:10,],BB7[12,],BB7[11,])


G7<-ggplotly(ggplot(data=Tfgem, mapping = aes(x=Año, y=TFGen))+
               geom_line(col="Red")+
               geom_point(size=1,col="black")+
               scale_x_continuous(limit=c(2007,2018))+
               ggtitle(label = "TFGen (por 1.000 mujeres entre 10 y 50 años ) desde 2008 a 2018 ajustado"))


###### Gráfico 6. Tasas específicas de fecundidad en la serie histórica Ajustado segundo corte #### 
M_T1 <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Totales poblaacionales proyectados/MUJER.xlsx")
B8<-M_T1[c(-1,-2,-3),c(1,4:11)]


B8<-melt(B8,
         value.name = 'Total',
         variable.name = 'Edad',
         id.vars = 'Año')
B81 <- c7 %>% filter(Municipios=='Total')

B81<-B81[,c(3:10,13)]  
B81[12,]<-B81[11,]
B81[,9]<-as.character(B81[,9])
B81[12,9]<-"2018AD"

B81<-melt(B81,
          value.name = 'Nacimientos',
          variable.name = 'Edad',
          id.vars = 'año')
B8[,4]<-B81$Nacimientos
B8[,5]<-round(B8[,4]/B8[,3]*1000,2)
names(B8)[5]<-"TEF"
B8$Año<-factor(B8$Año)
BPLOT<-subset(x = B8,subset = c(Año=="2008" | Año=="2018AJ"| Año=="2018"))



G8<-ggplotly(ggplot(data=BPLOT, mapping = aes(x=Edad, y=TEF,group=Año,colour=Año))+
               geom_line( )+
               geom_point()+
               ggtitle(label = "TEF (por 1.000 habitantes) desde 2008 a 2018")
)


###### Gráfico 7. Edad media de fecundidad Ajustado segundo corte ######
B88<-subset(x = B8,subset = c(Año=="2008" | Año=="2009"| Año=="2010"| Año=="2011"
                              | Año=="2012"
                              | Año=="2013"
                              | Año=="2014"
                              | Año=="2015"
                              | Año=="2016"
                              | Año=="2017"
                              | Año=="2018"))
B9<-B88 %>% dplyr::arrange(Año)
B9[,6]<-B9[,5]/1000
B9[,7]<-c(rep(c(12,17,22,27,32,37,42,47),11))
B9[,8]<-B9[,6]*B9[,7]
B9 <- B9 %>% dplyr::select(Año,V6,V8) %>% 
  dplyr::group_by(Año) %>% 
  dplyr::summarise(Edad_Media=sum(V8)/sum(V6))
B9$Año<-c(2008:2018)


PRUIEBA2<-subset(x = B8,subset = c(Año=="2018AJ"))                  
PRUIEBA2[,6]<-PRUIEBA2[,5]/1000
PRUIEBA2[,7]<-c(rep(c(12,17,22,27,32,37,42,47),1))
PRUIEBA2[,8]<-PRUIEBA2[,6]*PRUIEBA2[,7]
PRUIEBA2 <- PRUIEBA2 %>% dplyr::select(Año,V6,V8) %>% 
  dplyr::group_by(Año) %>% 
  dplyr::summarise(Edad_Media=sum(V8)/sum(V6))

B9[12,]<-B9[11,]
B9[11,2]<-PRUIEBA2[1,2]


G9<-ggplotly(B9 %>% ggplot(aes(x=Año,y=Edad_Media))+
               geom_line(col="Red")+
               geom_point(size=1,col="green")+
               ggtitle("Edad media de la fecundad en la serie histórica"))


###### Gráfico 8. Tasa de gobal de fecundidad Ajustado segundo corte ####
B10<-B88 %>% dplyr::arrange(Año)
B10[,6]<-B10[,5]/1000
B10 <- B10 %>% dplyr::select(Año,V6) %>% 
  dplyr::group_by(Año) %>% 
  dplyr::summarise(TGF=5*sum(V6))
B10$Año<-c(2008:2018)

PRUIEBA3<-subset(x = B8,subset = c(Año=="2018AJ"))                  
PRUIEBA3[,6]<-PRUIEBA3[,5]/1000
PRUIEBA3 <- PRUIEBA3 %>% dplyr::select(Año,V6) %>% 
  dplyr::group_by(Año) %>% 
  dplyr::summarise(TFG=5*sum(V6))

B10[12,]<-B10[11,]
B10[11,2]<-PRUIEBA3[1,2]

G10<-ggplotly(B10 %>% ggplot(aes(x=Año,y=TGF))+
               geom_line(col="red")+
               geom_point()+
               ggtitle("Tasa global fecundad en la serie histórica"))






###### Mapa 3. Tasa global de fecundidad por municipo #####
B12<-Censo_2018 %>% dplyr::select(U_MPIO,P_EDADR,P_SEXO) %>% 
  dplyr::filter(P_SEXO=='Mujer') %>% 
  dplyr::group_by(U_MPIO,P_EDADR) %>% 
  dplyr::summarise(total=n())
B12<-sqldf("select * from B12 where P_EDADR in (' 15 A 19 Años',' 20 A 24 Años',' 25 A 29 Años',' 30 A 34 Años',' 35 A 39 Años',' 40 A 44 Años',' 45 A 49 Años') order by U_MPIO")

nom<-Municipios_dane$Nombres
for(i in 1:123){
  for (j in 1:7) {
    B12[7*(i-1)+j,1]<-nom[i]
    
  }
}
B12<-sqldf("Select * from B12 order by U_MPIO  ")


c7_2018 <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Nacimientos/C7-Nacimientos por grupo de edad de la madre, según departamento y municipio de residencia de la madre 2008-2018/c7_2018.xlsx")
c7_2018<-melt(c7_2018)
c7_2018<-sqldf("select * from c7_2018 order by MINICIPIO")

B12[,4]<-c7_2018$value
names(B12)[4]<-"nacimientos"
B12[,5]<-B12[,4]/B12[,3]
names(B12)[5]<-"TEF"

B12 <- B12 %>% dplyr::select(U_MPIO,TEF) %>% 
  dplyr::group_by(U_MPIO) %>% 
  dplyr::summarise(TGF=5*sum(TEF))
B122<-cbind(Boyaca,B12$TGF)
names(B122)[7]<-"TGF"

G12<-B122 %>% mapview




###############################################################################
# ************************* Tasas de Mortalidad  *****************************#
###############################################################################

###### Gráfico 9. Tasa bruta de mortalidad ####
Defunciones_x_ano_x_sexo <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Defunciones_x_ano_x_sexo.xls")
Defunc.anual<-subset(x = Defunciones_x_ano_x_sexo,subset = Dpto =="TOTAL_1",select = c(4,49))
TotalPoblación<-T_P[4:14,23]
TotalPoblación[11,]<-1217376
Defunc.anual[,3]<-TotalPoblación
Defunc.anual[,4]<-round( Defunc.anual[,1]/Defunc.anual[,3]*1000,2)
names(Defunc.anual)<-c("T.defunciones","Año","T.Poblacional","TBM")
T.Bruta.Mortalidad<-ggplotly(ggplot(data=Defunc.anual, mapping = aes(x=Año, y=TBM))+
               geom_line(col="blue")+
               geom_point(size=1,col="black")+
               scale_x_continuous(limit=c(2007,2018))+
               ggtitle(label = "Tasa Bruta de Mortalidad (por 1.000 habitantes) desde 2008 a 2018"))

###### Gráfico 10.Mortalidad por rango etario####
Defunciones_absolutas<- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Especificas_Absolutas.xlsx")

Defun.Absolutas.HM.Ano<-ggplotly(ggplot(data=Defunciones_absolutas, mapping = aes(x=Años, y=`No. De muertes`,group=Sexo,colour=Sexo))+
                                   geom_line( )+
                                   geom_point()+
                                   ggtitle(label = "Tasa Bruta Mortalidad (por 1.000 habitantes) desde 2008 a 2018"))


###### Gráfico 11. Tasas específicas de mortalidad ####
Defunciones_especificas<- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Especificas_tasas.xlsx")

Defun.Espec.HM.Ano<-ggplotly(ggplot(data=Defunciones_especificas, mapping = aes(x=Años, y=Tasa,group=Sexo,colour=Sexo))+
               geom_line( )+
               geom_point()+
               ggtitle(label = "Tasa Bruta Mortalidad (por 1.000 habitantes) desde 2008 a 2018"))

###### Gráfico 12. Estandarización de tasas de mortalidad por año ####
Defunciones_estandarizadas <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Tasaestandarizada.xlsx")

Hombres_muertos<-subset(x=Defunciones_estandarizadas,subset = Sexo=="Hombres")
Hombres_muertosCol<-colSums(Hombres_muertos[,3:13])
Hombres<-Hombres_muertosCol/sum(Hombres_muertos$Poblacion)*1000

Mujeres_muertas<-subset(x=Defunciones_estandarizadas,subset = Sexo=="Mujeres")
Mujeres_muertascol<-colSums(Mujeres_muertas[,3:13])
Mujeres<-Mujeres_muertascol/sum(Mujeres_muertas$Poblacion)*1000

Defunciones_agregadas<-cbind(Mujeres,Hombres)

Defunciones_agregadas<-melt(Defunciones_agregadas,
                            value.name = 'Año',
                            variable.name = 'Año',
                            id.vars = c(Mujeres,Hombres))

names(Defunciones_agregadas)<-c("Año","Sexo","Tasa")

Defun.Ano.estandar<-ggplotly(ggplot(data=Defunciones_agregadas, mapping = aes(x=Año, y=Tasa,group="Sexo",colour=Sexo,shape="Sexo"))+
                               geom_line( )+
                               geom_point(size=2)+
                               scale_x_continuous(limit=c(2007,2018))+
                               theme(legend.key = element_blank(), 
                                     legend.background = element_rect(colour = 'black', fill = 'white'), 
                                     legend.position = "top", legend.title = element_blank()) + 
                               ggtitle(label = "Estandarización de tasa de Mortalidad por Sexo (por 1.000 habitantes) desde 2008 a 2018"))



###### Gráfico 13. Defunciones causas Externas Hombre y Mujer ####
Defunciones_por_causas_externas_no_fetales <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Defunciones por causas externas no fetales.xlsx")
Defunc_externasHM<-Defunciones_por_causas_externas_no_fetales[,c(1,3,4,96)]

Defunc_externasHMa<-melt(Defunc_externasHM,
                            value.name = 'No',
                            variable.name = 'Sexo',
                            id.vars = c("Causa",'Ano_tabla'))

names(Defunc_externasHMa)<-c("Causa", "Año", "Sexo", "Total")
  
Defunc_externasH<-subset(Defunc_externasHMa,subset = Sexo  =="TotalHombres")
Defunc_externasM<-subset(Defunc_externasHMa,subset = Sexo  =="TotalMujeres")


GDefunc_externasM<-ggplotly(ggplot(data=Defunc_externasM, mapping = aes(x=Año, y=Total,group="Causa",colour=Causa))+
                               geom_line()+
                               geom_point()+
                               scale_x_continuous(limit=c(2007,2018))+
                               theme(legend.key = element_blank(), 
                                     legend.background = element_rect(colour = 'blue', fill = 'white'), 
                                     legend.position = "top", legend.title = element_blank()) + 
                               ggtitle(label = "Defunciones por causas externas desde 2008 a 2018 por Sexo= Mujer"))


GDefunc_externasH<-ggplotly(ggplot(data=Defunc_externasH, mapping = aes(x=Año, y=Total,group="Causa",colour=Causa))+
                              geom_line()+
                              geom_point()+
                              scale_x_continuous(limit=c(2007,2018))+
                              theme(legend.key = element_blank(), 
                                    legend.background = element_rect(colour = 'blue', fill = 'white'), 
                                    legend.position = "top", legend.title = element_blank()) + 
                              ggtitle(label = "Defunciones por causas externas desde 2008 a 2018 por Sexo= Hombre"))

GDefunc_externasHM<-ggplotly(ggplot(data=Defunc_externasH, mapping = aes(x=Año, y=Total,group="Causa",colour=Causa))+
                               geom_line()+
                               geom_point(size=3)+
                               
                               geom_line(data = Defunc_externasM, mapping= aes(x=Año, y=Total,group="Causa",colour=Causa))+
                               
                               scale_x_continuous(limit=c(2007,2018))+
                               theme(legend.key = element_blank(), 
                                     legend.background = element_rect(colour = 'blue', fill = 'white'), 
                                     legend.position = "top", legend.title = element_blank()) + 
                               ggtitle(label = "Defunciones por causas externas desde 2008 a 2018"))





###### Gráfico 14. Defunciones causas Externas Accidentes de tránsito ####
Defunc_externas_accidentes <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Accidentes.xlsx")

Defun.accidentes<-ggplotly(ggplot(data=Defunc_externas_accidentes, mapping = aes(x=Años, y=`No. De muertes`,group=Sexo,colour=Sexo))+
                               geom_line( )+
                               geom_point()+
                               ggtitle(label = "Comparación 2008 - 2018 causa de muerte: Accidente de tránsito"))

###### Gráfico 15. Defunciones causas Externas Homicidios ####
Defunc_externas_homi <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Homicidios_rango.xlsx")

Defun.homi<-ggplotly(ggplot(data=Defunc_externas_homi, mapping = aes(x=Años, y=`No. De muertes`,group=Sexo,colour=Sexo))+
                             geom_line( )+
                             geom_point()+
                             ggtitle(label = "Comparación 2008 - 2018 causa de muerte: Homicidio"))



###### Gráfico 16. Defunciones causas Externas Suicicidios ####
Defunc_externas_suicidios <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Suicidios_rango.xlsx")

Defun.suicidios<-ggplotly(ggplot(data=Defunc_externas_suicidios, mapping = aes(x=Años, y=`No. De muertes`,group=Sexo,colour=Sexo))+
                             geom_line( )+
                             geom_point()+
                             ggtitle(label = "Tasa Bruta Mortalidad (por 1.000 habitantes) desde 2008 a 2018"))




###### Gráfico 17. Defunciones causas Naturales Hombre y Mujer ####
Defunciones_Naturales <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Defunciones_naturales.xlsx")
Defunciones_NaturalesHM<-Defunciones_Naturales[,c(2,4,5,20)]

Defunc_NaturalesHMa<-melt(Defunciones_NaturalesHM,
                         value.name = 'No',
                         variable.name = 'Sexo',
                         id.vars = c("Causas",'Año'))

names(Defunc_NaturalesHMa)<-c("Causa", "Año", "Sexo", "Total")

Defunc_NaturalesH<-subset(Defunc_NaturalesHMa,subset = Sexo  =="Total_Hombres")
Defunc_NaturalesM<-subset(Defunc_NaturalesHMa,subset = Sexo  =="Total_Mujeres")


GDefunc_NaturalesH<-ggplotly(ggplot(data=Defunc_NaturalesH, mapping = aes(x=Año, y=Total,group="Causa",colour=Causa))+
                              geom_line()+
                              geom_point()+
                              scale_x_continuous(limit=c(2007,2018))+
                              theme(legend.key = element_blank(), 
                                    legend.background = element_rect(colour = 'blue', fill = 'white'), 
                                    legend.position = "top", legend.title = element_blank()) + 
                              ggtitle(label = "Defunciones por causas naturales desde 2008 a 2018 por Sexo= Hombre"))


GDefunc_NaturalesM<-ggplotly(ggplot(data=Defunc_NaturalesM, mapping = aes(x=Año, y=Total,group="Causa",colour=Causa))+
                              geom_line()+
                              geom_point()+
                              scale_x_continuous(limit=c(2007,2018))+
                              theme(legend.key = element_blank(), 
                                    legend.background = element_rect(colour = 'blue', fill = 'white'), 
                                    legend.position = "top", legend.title = element_blank()) + 
                              ggtitle(label = "Defunciones por causas naturales desde 2008 a 2018 por Sexo= Mujer"))

GDefunc_NaturalesHM<-ggplotly(ggplot(data=Defunc_NaturalesH, mapping = aes(x=Año, y=Total,group="Causa",colour=Causa))+
                               geom_line()+
                               geom_point(size=3)+
                               
                               geom_line(data = Defunc_NaturalesM, mapping= aes(x=Año, y=Total,group="Causa",colour=Causa))+
                               
                               scale_x_continuous(limit=c(2007,2018))+
                               theme(legend.key = element_blank(), 
                                     legend.background = element_rect(colour = 'blue', fill = 'white'), 
                                     legend.position = "top", legend.title = element_blank()) + 
                               ggtitle(label = "Defunciones por causas naturales desde 2008 a 2018"))




###### Gráfico 15. Defunciones causas Naturales Homicidios ####
Defunc_externas_homi <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Homicidios_rango.xlsx")

Defun.homi<-ggplotly(ggplot(data=Defunc_externas_homi, mapping = aes(x=Años, y=`No. De muertes`,group=Sexo,colour=Sexo))+
                       geom_line( )+
                       geom_point()+
                       ggtitle(label = "Comparación 2008 - 2018 causa de muerte: Homicidio"))





###### Gráfico 16. Defunciones causas Externas Suicicidios ####
Defunc_externas_suicidios <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Suicidios_rango.xlsx")

Defun.suicidios<-ggplotly(ggplot(data=Defunc_externas_suicidios, mapping = aes(x=Años, y=`No. De muertes`,group=Sexo,colour=Sexo))+
                            geom_line( )+
                            geom_point()+
                            ggtitle(label = "Tasa Bruta Mortalidad (por 1.000 habitantes) desde 2008 a 2018"))









###### Mapa 4. Accidentes de tránsito 2018 ####
Accidentes_transito <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Accidentes_transito.xlsx")
names(Accidentes_transito)[1]<-'Municipios'
Accidentes_transito<-left_join(Boyaca,Accidentes_transito)
mapAccidentes_transito<-Accidentes_transito %>% mapview(zcol = 'Porcentaje Accidentes de Transito', legend = T, col.regions = sf.colors)

###### Mapa 5. Homicidios en 2018 ####
Homicidios <- read_excel("G:/Mi unidad/PREGRADO ESTADISTICA/Sexto semestre/Demografìa/Bases de datos finales/Total Defunciones/Homicidios.xlsx")
names(Homicidios)[1]<-'Municipios'
Homicidios<-left_join(Boyaca,Homicidios)
mapAHomicidios<-Homicidios %>% mapview(zcol = 'Homicidios porcentaje', legend = T, col.regions = sf.colors)































###################################################
#  * * * Salidas del Shiny
#####################################################

server <- function(input, output, session) {
  
 output$D1<-renderPlot({
   
   if(input$D1=="Población por genero"){p=G1}
   if(input$D1=="Población por genero en cada municipio"){p=G2}
   if(input$D1== "Población por municipio"){p=G3}
   if(input$D1== "Poblacion por edad "){p=G4}
   if(input$D1== "Trancisión demografica"){p=G5}

   
   p

    })
 
 output$D2<-renderPlot({
  
   if(input$D2=="TBN Ajustado"){p=G6}
   if(input$D2=="TFGen Ajustado"){p=G7}
   if(input$D2=="Tasa especifica de fecundidad por edad Ajustado"){p=G8}
   if(input$D2=="Tasa de Crecimiento poblacional"){p=Grafico_40}
   if(input$D2=="Edad media de fecundidad Ajustado"){p=G9}
   if(input$D2=="TGF Ajustado"){p=G10}
   if(input$D2=="Diferencias absolutas"){p=G11}
   if(input$D2=="Diferencias porcentuales"){p=G11B}
   if(input$D2=="TGF por municipio"){p=G12}
   
   
   p
   

 })
 
 output$D3<-renderPlot({
   
   if(input$D3=="Tasa Bruta de Mortalidad"){p=T.Bruta.Mortalidad}
   if(input$D3=="Tasa específica de Mortalidad"){p=Defun.Espec.HM.Ano}
   if(input$D3== "Mortalidad por rango etario"){p=Defun.Absolutas.HM.Ano}
   if(input$D3== "Defunciones causas Externas Hombre"){p=GDefunc_externasH}
   if(input$D3== "Defunciones causas Externas Mujer"){p=GDefunc_externasM}
   if(input$D3== "Defunciones causas Externas Hombre y Mujer"){p=GDefunc_externasHM}
   if(input$D3== "Defunciones causas Externas Accidentes de tránsito"){p=Defun.accidentes}
   if(input$D3== "Defunciones causas Externas Homicidios"){p=Defun.homi}
   if(input$D3== "Defunciones causas Externas Suicidios"){p=Defun.suicidios}
   if(input$D3== "Defunciones causas Naturales Hombre"){p=GDefunc_NaturalesH}
   if(input$D3== "Defunciones causas Naturales Mujer"){p=GDefunc_NaturalesM}
   if(input$D3== "Defunciones causas Naturales Hombre y Mujer"){p=GDefunc_NaturalesHM}

   p
   
 })
 
 output$D4<-renderPlot({
   
   if(input$D4=="Porcentaje de muertes por accidentes de tránsito en cada municipio"){p=mapAccidentes_transito}
   if(input$D4=="Porcentaje de muertos por homicidios en cada municipio"){p=mapAHomicidios}
   if(input$D4== "Poblacion por edad "){p=todavíano}
   if(input$D4== "Trancisión demografica"){p=todavíano}
   
   
   p
   
 })
 
######################################################
# Descripción del trabajo##
#####################################################
 output$Descripcion<-renderText({
   "El siguiente trabajo fuer realizado 
   por Diego Fernando Vargas Poveda y 
   William Camilo López Vega. En el marco 
   de la cátedra de Demografía del pregrado 
   en estadítica de la Universidad Santo Tomás 
   en el año 2020-2.
   
   
   Todos los gráficos acá presentados 
   fueron diseñados por los autores 
   anteriormente mencionados.
   
   
   
   Cualquier inquietud o sugerencia 
   pueden escribirnos al correo 
   williamlopezv@usantotomas.edu.co
   diegovargasp@usantotomas.edu.co"
 })
    
}

#####################################################
#  * * * Correr la aplicación
#####################################################
shinyApp(ui = ui, server = server)

