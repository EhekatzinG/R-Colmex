#En esta carpeta esta la base
"C:/Users/eheka/Downloads/enigh2022_ns_concentradohogar_dta (1)/"
###################################################################
library(tidyverse)
library(haven)

#1. Abrir la base de datos
setwd("C:/Users/eheka/Downloads/enigh2022_ns_concentradohogar_dta (1)/")
concentrado <- read_dta("concentradohogar.dta")

#2. 
concentrado <- concentrado %>% 
  filter(ing_cor>0) %>% 
  mutate(decil = ntile(ing_cor, 10))

#3. bene_gob
ben_ps<- concentrado %>% 
  mutate(d=bene_gob>0) %>% 
  group_by(decil) %>%
  summarise(f=mean(d))  

plot(ben_ps$decil, ben_ps$f)

####################
#4.
boxplot(concentrado$ing_cor[concentrado$sexo_jefe==1],
        concentrado$ing_cor[concentrado$sexo_jefe==2])
class(concentrado$educa_jefe)
concentrado$educa_jefe <- as.integer(concentrado$educa_jefe)

boxplot(concentrado$ing_cor[concentrado$educa_jefe<=5],
        concentrado$ing_cor[concentrado$educa_jefe>5],
        names = c("Sin secundaria", "Con secundaria"))
############
#5.

resumen<-concentrado %>% 
  group_by(decil) %>% 
  summarise(promc=mean(ing_cor),
         promt=mean(ingtrab),
         promr=mean(rentas))

plot(resumen$decil,resumen$promc,col="blue",type="l",ylim=c(0,200000))
lines(resumen$decil,resumen$promt,col="red",type="l")
lines(resumen$decil,resumen$promr,col="gray",type="l")

help(plot)

filal <- concentrado %>% 
  group_by(decil) %>% 
  summarise(corr = sum(ing_cor), trab = sum(ingtrab)) %>% 
  mutate(acumulado = cumsum(corr), Total = sum(corr),
         por = cumsum(corr)/sum(corr))

plot(filal$decil,filal$por, type ="o")
lines(filal$decil, filal$decil/10)

