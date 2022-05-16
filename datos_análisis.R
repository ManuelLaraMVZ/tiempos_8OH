library("pacman")

p_load("vroom",
       "ggplot2",
       "tidyverse",
       "rstatix",
       "ggpubr",
       "dplyr")

crudos8OH <- vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/tiempos_8OH/main/Datos%20crudos%20separados.csv")
crudos8OH <- vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/tiempos_8OH/main/Datos%20crudos%20juntos.csv")
head(crudos8OH)

names(crudos8OH)


#########################################################
#Cargamos paquetes

crudos2 <- crudos8OH
crudos2$Hora <- factor(crudos2$Hora, 
                         levels = c("3h", "6h", "12h", "24h"))
crudos2$Tratamiento <- factor(crudos2$Tratamiento, 
                             levels = c("Control","DMSO", "31.25 然", "62.50 然", "125.00 然", "250.00 然",
                                        "500.00 然", "750.00 然", "1000.00 然"))

head(crudos2)
#obtenemos el resumen de la media y e.e.m.

resumen8OH <- crudos2 %>% 
    group_by(Hora, Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen8OH

#visualizamos la distribuci鏮
caja1 <- crudos2 %>% 
  ggboxplot(x="Tratamiento",
            y="Datos",
            color="Hora",
            palette = "jco")
caja1

#supuestos

supuesto1 <- crudos2 %>% 
  group_by(Hora, Tratamiento) %>% 
  identify_outliers(Datos)  #buscamos outliers

supuesto1

modelo <- lm(Datos~Hora*Tratamiento, data = crudos2)
ggqqplot(residuals(modelo))

#prueba de normalidad de Shapiro

shapiro_test(residuals(modelo))

#No pasa la prueba de normalidad
resumen8OH
crudos2 %>% 
  group_by(Hora,Tratamiento) %>% 
  shapiro_test(Datos)

ggqqplot(crudos2, "Datos", ggtheme = theme_bw())+
  facet_grid(Hora~Tratamiento)
rlang::last_error()

crudos2 %>% 
  select("3h")
