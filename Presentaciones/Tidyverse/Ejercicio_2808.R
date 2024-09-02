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
