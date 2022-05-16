library("pacman")
p_load("vroom", #llama datos
       "ggplot2",#gráfica
       "dplyr", #magriter
       "agricolae", #para poder agrupar el Tukey
       "ggpubr") #gráficas simplificadas


###########################
#datos

crudos8OH <- vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/tiempos_8OH/main/Datos%20crudos%20juntos.csv")
head(crudos8OH)

names(crudos8OH)

##############################
#convertimos a factores para que los pueda considerar anova y sacamos resumen

crudos2 <- crudos8OH
crudos2$Hora <- factor(crudos2$Hora, 
                       levels = c("3h", "6h", "12h", "24h"))
crudos2$Tratamiento <- factor(crudos2$Tratamiento, 
                              levels = c("Control","DMSO", "31.25 µM", "62.50 µM", "125.00 µM", "250.00 µM",
                                         "500.00 µM", "750.00 µM", "1000.00 µM"))
head(crudos2)

#obtenemos el resumen de la media y e.e.m.

resumen8OH <- crudos2 %>% 
  group_by(Hora, Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen8OH

#####################################
#Prueba ANOVA todos los datos

#visualizamos la distribución
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

#No pasa la prueba de normalidad con p<0.05

######################################################
#Se decide hacer análisis de los tiempos por separados

#primero hay que extraer los datos por separados 

######################################################

######################################################
# para 3h

crudo3h <- crudos2 %>% 
  filter(Hora=="3h") %>% 
  select(Tratamiento,Datos)

crudo3h

resumen3h <- crudo3h %>% 
  group_by(Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen3h

#gráfica
grafica3h <- resumen3h %>% 
  ggplot(mapping = aes(x=Tratamiento,
                       y=mean))+
  geom_point()

grafica3h

supuesto3h <- crudo3h %>% 
  group_by(Tratamiento) %>% 
  identify_outliers(Datos)  #buscamos outliers

supuesto3h

modelo <- lm(Datos~Tratamiento, data = crudo3h)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudo3h$Datos)
ks.test(crudo3h$Datos, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(Datos~Tratamiento, data = crudo3h)
fligner.test(Datos~Tratamiento, data = crudo3h)
bartlett.test(Datos~Tratamiento, data = crudo3h)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVA3h <- aov( Datos~Tratamiento,data = crudo3h) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVA3h
#obtenemos la tabla e anova
summary.aov(ANOVA3h)

#Existe diferencia entre los tratamientos

#Prueba estadística
posthoc3h <- (ANOVA3h)

posthoc3h

#obtenemos las similitudes

agrupados3h <- HSD.test(ANOVA3h,"Tratamiento", group=T, console=T,
                        main = "células con tratamiento por 3h con 8-OH")

agrupados3h

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","a","b","c","cd","d","e", "f","g") #se construye con los resultados de las interacciones

resumen3h_ANOVA <- resumen3h %>% 
  select(Tratamiento, n, mean, sd) %>% 
  mutate(similitud)

resumen3h_ANOVA

write.csv(resumen3h_ANOVA,
          file = "ANOVA_3h.csv",
          row.names = F)

################################################################################
################################################################################
# para 6h

crudo6h <- crudos2 %>% 
  filter(Hora=="6h") %>% 
  select(Tratamiento,Datos)

crudo6h

resumen6h <- crudo6h %>% 
  group_by(Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen6h

#gráfica
grafica6h <- resumen6h %>% 
  ggplot(mapping = aes(x=Tratamiento,
                       y=mean))+
  geom_point()

grafica6h

supuesto6h <- crudo6h %>% 
  group_by(Tratamiento) %>% 
  identify_outliers(Datos)  #buscamos outliers

supuesto6h

modelo <- lm(Datos~Tratamiento, data = crudo6h)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudo6h$Datos)
ks.test(crudo6h$Datos, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(Datos~Tratamiento, data = crudo6h)
fligner.test(Datos~Tratamiento, data = crudo6h)
bartlett.test(Datos~Tratamiento, data = crudo6h)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVA6h <- aov( Datos~Tratamiento,data = crudo6h) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVA6h
#obtenemos la tabla e anova
summary.aov(ANOVA6h)

#Existe diferencia entre los tratamientos

#Prueba estadística
posthoc6h <- TukeyHSD(ANOVA6h)

posthoc6h

#obtenemos las similitudes

agrupados6h <- HSD.test(ANOVA6h,"Tratamiento", group=T, console=T,
                        main = "células con tratamiento por 6h con 8-OH")

agrupados6h

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","a","b","c","cd","d","e", "f","g") #se construye con los resultados de las interacciones

resumen6h_ANOVA <- resumen6h %>% 
  select(Tratamiento, n, mean, sd) %>% 
  mutate(similitud)

resumen6h_ANOVA

write.csv(resumen6h_ANOVA,
          file = "ANOVA_6h.csv",
          row.names = F)
################################################################################
################################################################################
# para 12h

crudo12h <- crudos2 %>% 
  filter(Hora=="12h") %>% 
  select(Tratamiento,Datos)

crudo12h

resumen12h <- crudo12h %>% 
  group_by(Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen12h

#gráfica
grafica12h <- resumen12h %>% 
  ggplot(mapping = aes(x=Tratamiento,
                       y=mean))+
  geom_point()

grafica12h

supuesto12h <- crudo12h %>% 
  group_by(Tratamiento) %>% 
  identify_outliers(Datos)  #buscamos outliers

supuesto12h

modelo <- lm(Datos~Tratamiento, data = crudo12h)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudo12h$Datos~crudo12h$Tratamiento)
ks.test(crudo12h$Datos, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(Datos~Tratamiento, data = crudo12h)
fligner.test(Datos~Tratamiento, data = crudo12h)
bartlett.test(Datos~Tratamiento, data = crudo12h)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVA12h <- aov( Datos~Tratamiento,data = crudo12h) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVA12h
#obtenemos la tabla e anova
summary.aov(ANOVA12h)

#Existe diferencia entre los tratamientos

#Prueba estadística
posthoc12h <- TukeyHSD(ANOVA12h)

posthoc12h

#obtenemos las similitudes

agrupados12h <- HSD.test(ANOVA12h,"Tratamiento", group=T, console=T,
                         main = "células con tratamiento por 12h con 8-OH")

agrupados12h

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","b","c","d","d","d","e", "f","g") #se construye con los resultados de las interacciones

resumen12h_ANOVA <- resumen12h %>% 
  select(Tratamiento, n, mean, sd) %>% 
  mutate(similitud)

resumen12h_ANOVA

write.csv(resumen12h_ANOVA,
          file = "ANOVA_12h.csv",
          row.names = F)

##########################################

################################################################################
################################################################################
# para 24h

crudo24h <- crudos2 %>% 
  filter(Hora=="24h") %>% 
  select(Tratamiento,Datos)

crudo24h

resumen24h <- crudo24h %>% 
  group_by(Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen24h

#gráfica
grafica24h <- resumen24h %>% 
  ggplot(mapping = aes(x=Tratamiento,
                       y=mean))+
  geom_point()

grafica24h

supuesto24h <- crudo24h %>% 
  group_by(Tratamiento) %>% 
  identify_outliers(Datos)  #buscamos outliers

supuesto24h

modelo <- lm(Datos~Tratamiento, data = crudo24h)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudo24h$Datos)
ks.test(crudo24h$Datos, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(Datos~Tratamiento, data = crudo24h)
fligner.test(Datos~Tratamiento, data = crudo24h)
bartlett.test(Datos~Tratamiento, data = crudo24h)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVA24h <- aov( Datos~Tratamiento,data = crudo24h) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVA24h
#obtenemos la tabla e anova
summary.aov(ANOVA24h)

#Existe diferencia entre los tratamientos

#Prueba estadística
posthoc24h <- TukeyHSD(ANOVA24h)

posthoc24h

#obtenemos las similitudes

agrupados24h <- HSD.test(ANOVA24h,"Tratamiento", group=T, console=T,
                         main = "células con tratamiento por 24h con 8-OH")

agrupados24h

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","a","b","c","b","b","d", "e","f") #se construye con los resultados de las interacciones

resumen24h_ANOVA <- resumen24h %>% 
  select(Tratamiento, n, mean, sd) %>% 
  mutate(similitud)

resumen24h_ANOVA

write.csv(resumen24h_ANOVA,
          file = "ANOVA_24h.csv",
          row.names = F)
