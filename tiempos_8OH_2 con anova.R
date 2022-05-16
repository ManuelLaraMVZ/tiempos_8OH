library("pacman")

p_load("dplyr",
       "vroom",
       "ggplot2",
       "tidyr",
       "ggsci",
       "tidyverse")

tiempos <- vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/tiempos_8OH/main/Ordenados_8OH_tiempos")
head(tiempos)

#orden de las facetas

tiempos2 <- tiempos
tiempos2$Hours <- factor(tiempos2$Hours, 
                         levels = c("12h","3h", "24h","6h")) #Convertimos el caracter en factor par apoder ordenar
tiempos2$Treatment2<- factor(tiempos2$Treatment2, 
                         levels = c("DMSO", "31.25 然", "62.50 然", "125.00 然", "250.00 然",
                                    "500.00 然", "750.00 然", "1000.00 然"))
head(tiempos2)


write.csv(x=tiempos2,file = "TIEMPOS2.csv", row.names = F)
#Esa tabla construida se le incorporan los resultados de los ANOVAS y se sube a GitHUB

#Cargamos los nuevos valores

tiempos3 <-vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/tiempos_8OH/main/Resumenyanovas")

head (tiempos3)

tiempos3$Hours <- factor(tiempos3$Hours, 
                         levels = c("3h","12h", "6h","24h")) #Convertimos el caracter en factor par apoder ordenar
tiempos3$Treatment2<- factor(tiempos3$Treatment2, 
                             levels = c("DMSO", "31.25 然", "62.50 然", "125.00 然", "250.00 然",
                                        "500.00 然", "750.00 然", "1000.00 然"))

head(tiempos3)

graft <- tiempos3%>%
    ggplot(mapping = aes(x=Position,
                         y=Mean_value,
                         fill=Treatment2))+
    geom_bar(stat = "identity", colour="black", size=.8)+
  theme_classic()+
  facet_wrap(~Hours, dir = "v")+
  scale_fill_d3()+
  theme_bw()

graft

miny=0
maxy=1.1

marcasy <- seq(from=miny,
               to=maxy,
               by=.2)


graft2 <- graft+
  scale_y_continuous(limits=c(miny,maxy), #colocamos los l璥ites del eje y
                     breaks=marcasy,
                     expand=c(0,0))+
  theme(legend.position="right")+ #empezamos a meter lo que le pusimos
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
  
graft2

graft3 <- graft2+
    geom_errorbar( aes(ymin=Mean_value-sd, 
                     ymax=Mean_value+sd), 
                 width=0.2, colour="black", alpha=1, size=.8)+
  xlab("8-HQ Concentration")+
  ylab("Viability by MTT (p.d.u.)")

graft3

graft4 <- graft3+
  theme(axis.line = element_line(size = 0.8))+
  theme(axis.ticks.x = element_line(size = 0.8))+
  theme(axis.ticks.y = element_line(size = 0.8))+
  theme(legend.title= element_blank())
graft4

graft5 <- graft4+
  theme(strip.text.x = element_text(size = 16, color = "black", face = "bold"))+  #https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/
  theme(strip.background = element_rect(color="black", fill="gainsboro", size=1.2, linetype="solid"))+
  theme(legend.text = element_text(size = 16, face = "bold"))+
  theme(axis.title.x = element_text(size=16, face="bold"))+
  theme(axis.title.y = element_text(size=16, face="bold"))+
  theme(axis.text.y = element_text(size=16, face = "bold"))+
  theme(axis.ticks.x.bottom = element_blank())
graft5

#se agrega estad疄tica

graft6 <- graft5+
  geom_text(aes(label=Tukey),
            nudge_x=0.25,     #REspecto al eje x que tanto cambia
            nudge_y = 0.08,      #respecto al eje y que tanto cambia
            size=6,
            face="bold")+
  scale_fill_simpsons()
graft6



ggsave(filename = "Concentraciones_MTT.png",
       plot = graft6,
       dpi = 600,
       height = 7,
       width = 12)

#escala grises
graft7 <- graft6+
  scale_fill_grey()
graft7

ggsave(filename = "Concentraciones_MTTgris.png",
       plot = graft7,
       dpi = 600,
       height = 7,
       width = 12)
