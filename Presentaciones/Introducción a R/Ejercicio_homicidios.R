#Ejercicio

homicidios<- readRDS("C:/Users/user/OneDrive - El Colegio de México A.C/Programación Científica en R/Presentaciones/2_Objetos_Modos/homicidios.rds")


#2. Obtengan el promedio de homicidios por año.

homicidios<- readRDS("D:/OneDrive - El Colegio de México A.C/Programación Científica en R/Presentaciones/2_Objetos_Modos/homicidios.rds")
Fyear <- as.factor(homicidios$Year)
tapply(homicidios$Total, Fyear, mean)

#3. El número de homicidios por estado durante cada periodo


Festado <- as.factor(homicidios$Estado)
tapply(homicidios$Total, Festado, sum)


#4. El número de homicidios total durante este sexenio. 

sum(homicidios$Total[homicidios$Year>=2018])


#5. La tasa de crecimiento del número de homicidios de mujeres en este sexenio (año 2018 vs año 2023) y la tasa de crecimiento de los homicidios de mujeres en el sexenio pasado (año 2012 vs 2017).

100*sum(homicidios$Mujer[homicidios$Year == 2023]) / sum(homicidios$Mujer[homicidios$Year == 2018])-100

100*sum(homicidios$Mujer[homicidios$Year == 2017]) / sum(homicidios$Mujer[homicidios$Year == 2012])-100