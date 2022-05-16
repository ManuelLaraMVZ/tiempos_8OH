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
                         levels = c("3h", "12h", "6h", "24h")) #Convertimos el caracter en factor par apoder ordenar
tiempos2$Treatment2<- factor(tiempos2$Treatment2, 
                         levels = c("DMSO", "31.25 然", "62.50 然", "125.00 然", "250.00 然",
                                    "500.00 然", "750.00 然", "1000.00 然"))
head(tiempos2)
view(tiempos2)

write.csv(x=tiempos2,file = "TIEMPOS2.csv", row.names = F)

tiempos3 <-read_csv("TIEMPOS2.csv")

graft <- tiempos3%>%
    ggplot(mapping = aes(x=Position,
                         y=Mean_value,
                         fill=Treatment2))+
  geom_text(aes(label=Tukey),
            nudge_x=0.25,     #REspecto al eje x que tanto cambia
            nudge_y = 0.1,      #respecto al eje y que tanto cambia
            size=3)+
  geom_bar(stat = "identity", colour="black", size=.8)+
  theme_classic()+
  facet_wrap(~Hours, dir = "v")+
  scale_fill_d3()+
  theme_bw()

graft

miny=0
maxy=1.02

marcasy <- seq(from=miny,
               to=maxy,
               by=.2)


graft2 <- graft+
  scale_y_continuous(limits=c(miny,maxy), #colocamos los l璥ites del eje y
                     breaks=marcasy,
                     expand=c(0.02,0))+
  theme(legend.position="left")+ #empezamos a meter lo que le pusimos
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
  
graft2

graft3 <- graft2+
    geom_errorbar( aes(ymin=Mean_value-sd, 
                     ymax=Mean_value+sd), 
                 width=0.2, colour="black", alpha=1, size=.8)+
  xlab("8-HQ Concentration")+
  ylab("Viability by MTT (p.d.u.)")+
  scale_fill_grey()

graft3

graft4 <- graft3+
  
  theme(axis.line = element_line(size = 0.8))+
  theme(axis.ticks.x = element_line(size = 0.8))+
  theme(axis.ticks.y = element_line(size = 0.8))+
  theme(legend.title= element_blank())
graft4

graft5 <- graft4+
  theme(strip.text.x = element_text(size = 10, color = "black", face = "bold"))+  #https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/
  theme(strip.background = element_rect(color="black", fill="gainsboro", size=1.2, linetype="solid"))+
  theme(legend.text = element_text(size = 10, face = "bold"))+
  theme(axis.title.x = element_text(size=12, face="bold"))+
  theme(axis.title.y = element_text(size=12, face="bold"))+
  theme(axis.text.y = element_text(size=10, face = "bold"))
graft5

ggsave(filename = "Concentraciones_MTT.png",
       plot = graft5,
       dpi = 600,
       height = 7,
       width = 7)
