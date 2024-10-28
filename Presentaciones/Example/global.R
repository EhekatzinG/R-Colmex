library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(data.table)
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library("shinyWidgets")
library(mxmaps)
library(rgdal)
library(readr)
library(tidyr)
#QUANTILE REGRESSION
quantil <- as.data.frame(read_dta("www/coef_cuant.dta"))
quantil$fecha <-   fromStataTime(quantil$date, '%tq')

ols <- as.data.frame(read_dta("www/coef.dta"))
ols$fecha <-   fromStataTime(ols$date, '%tq')

formal_mujeres <- as.data.frame(read_dta("www/formal.dta"))
formal_mujeres$fecha <-   fromStataTime(formal_mujeres$date, '%tq')

formal <- as.data.frame(read_dta("www/2formal.dta"))
formal$fecha <-   fromStataTime(formal_mujeres$date, '%tq')

#Esto es para la muestra de SOLO mujeres
part_mujeres <- as.data.frame(read_dta("www/trabajadorv.dta"))
part_mujeres$fecha <-   fromStataTime(part_mujeres$date, '%tq')

part <- as.data.frame(read_dta("www/2trabajadorv.dta"))
part$fecha <-   fromStataTime(part$date, '%tq')

#Formal para entidades, muestra completa
formal_ent <- as.data.frame(read_dta("www/2formal_ent.dta"))
formal_ent$fecha <-   fromStataTime(formal_ent$date, '%tq')

#Formal para entidades, muestra mujeres
formal_entmujer <- as.data.frame(read_dta("www/formal_ent.dta"))
formal_entmujer$fecha <-   fromStataTime(formal_entmujer$date, '%tq')

part_ent <- as.data.frame(read_dta("www/2trabajadorv_ent.dta"))
part_ent$fecha <-   fromStataTime(part_ent$date, '%tq')

part_entmujer <- as.data.frame(read_dta("www/trabajadorv_ent.dta"))
part_entmujer$fecha <-   fromStataTime(part_entmujer$date, '%tq')

quant_region <- as.data.frame(read_dta("www/b_q_reg_region_marcus.dta"))
quant_region$fecha <-   fromStataTime(quant_region$date, '%tq')

#OAXACA BLINDER
df <- as.data.frame(read_dta("www/oaxaca.dta"))
df2 <- as.data.frame(read_dta("www/oax_anual.dta"))

oax <- as.data.frame(cbind(df$date, df$diff, df$unexplained, df$explained))
oax100 <- as.data.frame(cbind(df$date, df$diff_100, df$unexplained_100, df$explained_100))
oax_y <- as.data.frame(cbind(df2$y, df2$diff, df2$unexplained, df2$explained))

colnames(oax) <- c("Periodo", "Brecha", "Unexplained","Explained")
oax$Fecha <-   fromStataTime(oax$Periodo, '%tq')

colnames(oax_y) <- c("Periodo", "Brecha", "Unexplained","Explained")
colnames(oax100) <- c("Periodo", "Brecha", "Unexplained","Explained")
oax100$Fecha <-  fromStataTime(oax100$Periodo, '%tq')


oax2 <- as.data.frame(cbind(df$date,df$wm, df$wf))
colnames(oax2) <- c("Periodo", "Hombres","Mujeres")
oax2$Fecha <-  fromStataTime(oax2$Periodo, '%tq')

oax2_y <- as.data.frame(cbind(df2$y,df2$wm, df2$wf))


colnames(oax2_y) <- c("Periodo", "Hombres","Mujeres")


df <- as.data.frame(read_dta("www/oax_ent.dta"))
oax_ent <- as.data.frame(cbind(df$year, df$cve_ent ,df$diff, df$unexplained, df$explained, df$av_ent, df$diff_100, df$unexplained_100, df$explained_100))
colnames(oax_ent) <- c("Year", "Clave","Brecha", "Unexplained","Explained", "Estado", "diff","unexplained", "explained")

#Mapas
data<-df_mxstate
data$Clave <- data$region
data$ent <- data$region

#PARTICIPACION (RESPECTO HOMBRES SOLTEROS)
#MUJERES CON PAREJA
data$ent<-df_mxstate$region

mappart <- merge(data, part_ent, by=c('ent'), all.y = TRUE)
mappart$value <- as.numeric((exp(mappart$fem1_b)-1)*100)
#MUJERES SIN PAREJA
mappart2 <- merge(data, part_ent, by=c('ent'), all.y = TRUE)
mappart2$value <- as.numeric((exp(mappart2$fem0_b)-1)*100)
#HOMBRES CON PAREJA
mappart3 <- merge(data, part_ent, by=c('ent'), all.y = TRUE)
mappart3$value <- as.numeric((exp(mappart3$mal1_b)-1)*100)

#PARTICIPACION (RESPECTO MUJERES SOLTERAS O SIN HIJOS)
mappart22 <- merge(data, part_entmujer, by=c('ent'), all.y = TRUE)
mappart22$value <- as.numeric((exp(mappart22$par_b)-1)*100)
#PARTICIPACION (RESPECTO MUJERES SOLTERAS O SIN HIJOS)
mappart222 <- merge(data, part_entmujer, by=c('ent'), all.y = TRUE)
mappart222$value <- as.numeric((exp(mappart222$hijo_b)-1)*100)

#PARTICIPACION (RESPECTO HOMBRES SOLTEROS)
#Mujeres con pareje
mapfor <- merge(data, formal_ent, by=c('ent'), all.y = TRUE)
mapfor$value <- as.numeric((exp(mapfor$fem1_b)-1)*100)
#Mujeres sin pareje
mapfor2 <- merge(data, formal_ent, by=c('ent'), all.y = TRUE)
mapfor2$value <- as.numeric((exp(mapfor2$fem1_b)-1)*100)
#Hombres con pareje
mapfor3 <- merge(data, formal_ent, by=c('ent'), all.y = TRUE)
mapfor3$value <- as.numeric((exp(mapfor2$mal1_b)-1)*100)

#Formalidad (RESPECTO MUJERES SOLTERAS O SIN HIJOS)
mapfor22 <- merge(data, formal_entmujer, by=c('ent'), all.y = TRUE)
mapfor22$value <- as.numeric((exp(mapfor22$par_b)-1)*100)
#Formalidad (RESPECTO MUJERES SOLTERAS O SIN HIJOS)
mapfor222 <- merge(data, formal_entmujer, by=c('ent'), all.y = TRUE)
mapfor222$value <- as.numeric((exp(mapfor222$hijo_b)-1)*100)

row.has.na <- apply(mapfor22, 1, function(x){any(is.na(x))})
mapfor22 <- mapfor22[!row.has.na,]

row.has.na <- apply(mapfor222, 1, function(x){any(is.na(x))})
mapfor222 <- mapfor222[!row.has.na,]


#OAXACA-BLINDER

#Discriminación
mapox<-merge(data, oax_ent, by=c('Clave'), all.y = TRUE)
mapox$value <- as.numeric(mapox$Unexplained)
mapox$value <- as.numeric(mapox$value*100)

#Habilidades
mapox2<-merge(data, oax_ent, by=c('Clave'), all.y = TRUE)
mapox2$value <- as.numeric(mapox2$Explained)
mapox2$value <- as.numeric(mapox2$value*100)

#Brecha
mapox3<-merge(data, oax_ent, by=c('Clave'), all.y = TRUE)
mapox3$value <- as.numeric(mapox3$Brecha)
mapox3$value <- as.numeric(mapox3$value*100)

