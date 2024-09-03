setwd("D:/OneDrive - El Colegio de México A.C/Programación Científica en R/Presentaciones/5_Tidyverse")
#------------------------------------------------------------------------------#
library('haven')
library('tidyverse')
df<-read_dta("ENOE_SDEMT124.dta")
#------------------------------------------------------------------------------#
#1. Vamos a crear un nuevo data frame con las siguientes características
  #a. Nos quedaremos con las variables ingocup, clase2, emp_ppal, sex y
  #   eda, r_def, c_res, tipo y fac_tri
  #b. Nos quedaremos solamente con individuos ocupados
  #c. Nos quedamos con todos los individuos que tienen ingresos laborales
  #   positivos.
  #d. Remplazaremos los valores del ingreso laboral por NA si el igreso
  #   laboral es mayor a 999998. 
  #e. Solo nos quedamos con las obsservaciones donde r_def == 0,
  #   c_res == 1 | c_res == 3, eda >= 15 y tipo == 1
  #f. Creamos una nueva variable, el logaritmo del salario.

enoe <- df %>%
  select(ingocup, clase2, emp_ppal, sex, eda, r_def, c_res, tipo, fac_tri,
         anios_esc) %>%
  filter(clase2 == 1, ingocup >0, eda>=15, tipo == 1, r_def==0, 
         c_res == 1  |  c_res ==3 ) %>% 
  mutate(ingocup = na_if(ingocup, 999998), lwage = log(ingocup))


#2. Crearemos un data frame con las siguientes estadísticas
#a. Media, mediana, desv. estándar, mínimo y máximo de el ingreso (log)
#b. Mismas estadísticas pero para formales e informales.
#c. Mismas estadísticas pero para hombres y mujeres.


b <- enoe %>% 
    group_by(emp_ppal) %>% 
    summarise(mean = mean(lwage), sd = sd(lwage))

c <- enoe %>% 
  group_by(sex) %>% 
  summarise(mean = mean(lwage), sd = sd(lwage))

#3. ¿Qué porcentaje de ocupados informales son mujeres?
#   ¿Qué porcentaje de trabajadores son informales?

per_mujeres <- 100*mean(enoe$sex==2)
per_informal <- 100*mean(enoe$emp_ppal==2)

#----------------------------------------------------------------------

#4. 
#a. Crearemos una variable adicional que nos indique en qué percentil
#   del ingreso se encuentra cada uno de los individuos (quitando Na)
#b. Grafiquemos el ingreso promedio por decil (log). 
#c. Grafiquemos el coef. de correlación entre lwage y sex por decil.

corr <- enoe %>%
  mutate(sex = sex-1 , q = ntile(lwage, 10)) %>% 
  group_by(q) %>% 
  summarise(cor = cor(lwage, sex), mean = mean(lwage))

plot(corr$q, corr$mean, type = "o")
plot(corr$q, corr$cor, type = "o")

enoe <- mutate(enoe, sex = sex-1 , q = ntile(lwage, 10) )

#5. Grafiquemos la diferencia en el ingreso de hombres y mujeres por decil

ing_h <- enoe %>% 
  filter(sex == 0) %>% 
  group_by(q) %>% 
  summarise(mean_h = mean(lwage))

ing_m <- enoe %>% 
  filter(sex == 1) %>% 
  group_by(q) %>% 
  summarise(mean_m = mean(lwage))

ing <- data.frame(cbind(ing_h, ing_m)) %>% 
  mutate(dif = mean_h - mean_m)

plot(ing$q, ing$dif, type = "o")

# Otro método

ing <- enoe %>% 
  mutate(ing_h = case_when(sex==0 ~ lwage),
         ing_m = case_when(sex==1 ~ lwage)) %>% 
  group_by(q) %>% 
  summarise(ing_h = mean(ing_h, na.rm = T),
            ing_m = mean(ing_m, na.rm = T)) %>% 
  mutate(diff = ing_h-ing_m)


plot(ing$q, ing$diff, type = "o")

#6. Ahora utilizaremos la variable anios_esc 
#   Primero hay que cambiar anios_esc == 99 por na
  #a. Grafiquemos la variable anios_esc y el logaritmo del ingreso
  #b. Calculemos la correlacion entre las dos variables por decil y graficar
  #c. Ahora hagamoslo por hombres y mujeres.

enoe <- mutate(enoe, anios_esc = na_if(anios_esc,99))
plot(enoe$anios_esc, enoe$lwage)

educor <- enoe %>% 
  group_by(q) %>% 
  mutate(ing_h = case_when(sex==0 ~ lwage),
         ing_m = case_when(sex==1 ~ lwage))%>% 
  group_by(q) %>% 
  summarise(ing_h = cor(ing_h, anios_esc, use = "na.or.complete"),
            ing_m = cor(ing_m, anios_esc, use = "na.or.complete"),
            Cor = cor(lwage, anios_esc, use = "na.or.complete"))

plot(educor$q, educor$ing_h, type ="o", col="blue")
lines(educor$q, educor$ing_m, type ="o", col = "red")

#-----------------------------------------------------------------------------#
  