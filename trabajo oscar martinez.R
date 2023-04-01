rm(list = ls())

setwd("D:/omartinez/Agenda/1. ok/Curso analitica de datos y visualizacion de datos espaciales con R/trabajo/")
library(tidyverse)
library(grid)
library(gridExtra)
library(sf)
library(readxl)
library(ggrepel)

getwd()

dir.create("grafico")



# Definamos un directorio para guardar los output de mi analisis:
grafico <- paste0(getwd() , "/grafico/")


# importar base
base <- read.csv("OPENDATA_DS_01_2018_ATENCIONES.csv", sep = "|", fileEncoding='latin1',check.names=F)


base = base %>% 
  mutate(NOMBDEP = ifelse(REGION == "LIMA METROPOLITANA", "LIMA",
                          ifelse(REGION == "LIMA REGION", "LIMA",
                                 ifelse(REGION == "APURÍMAC","APURIMAC",
                                        ifelse(REGION == "HUÁNUCO","HUANUCO",
                                               ifelse(REGION == "JUNÍN","JUNIN",
                                                      ifelse(REGION == "SAN MARTÍN","SAN MARTIN",REGION)))))))


names(base)
# creamos variables

#atenciones por region y provincia
base_2018 = base %>% 
  group_by(REGION,PROVINCIA) %>% 
  dplyr::summarise(personas_atendidas = n(),
            Numero_atenciones = sum(ATENCIONES))


# pasarla a formato largo

library(reshape2)

base_2018_largo <- melt(base_2018, id.vars = c("REGION","PROVINCIA"))
base_2018_largo

# filtramos solo un departamento y persona atendidas

piura = base_2018_largo %>% 
  filter(variable == "personas_atendidas" &
         REGION == "PIURA" ) %>% 
  select(-variable)

# grafico

graf1=ggplot(piura,
       aes(as.character(PROVINCIA), value, fill=PROVINCIA)) +
  geom_bar(stat="identity", show.legend = F, alpha = 0.95) + 
  theme_bw() +
  labs(x="",    
       y="",
       caption = "Fuente: ") +
  theme(text = element_text(size=8),
        plot.title = element_text(size=rel(2.0), vjust=0.5, face="bold", color="darkgreen", lineheight=1.5),
        plot.caption = element_text(size=rel(1.5)),
        axis.text.x = element_text(angle = 0, size=rel(2), hjust = 0.5),
        axis.text.y = element_text(face="bold", colour="chocolate4", size=rel(2.0), angle=0, hjust=0.5),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="darkgreen", size=rel(1.5)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="darkgreen", size=rel(1.2)),
        axis.text = element_text(colour = "black")) +
  ggtitle(paste("PERSONAS ATENDIDAS REGION PIURA SIS 2018")) + #Configuro un t?tulo por grupo
  guides(color = "none") +
  theme(strip.text = element_text(size=12)) +
  geom_text(aes(label=format(value, big.mark = ",", 
                             scientific = FALSE)), vjust = -0.50, hjust = 0.4, size=3.4) 

graf1

ggsave(filename = paste0(grafico, "provincia_piura.png"),
       width = 6,
       height = 10)


#atenciones por region
base_2018_atenc = base %>% 
  group_by(REGION) %>% 
  summarise(personas_atendidas = n(),
         Numero_atenciones = sum(ATENCIONES))

# grafico
graf2 = ggplot(base_2018_atenc, aes(reorder(REGION,personas_atendidas), y =  personas_atendidas)) +
  geom_bar(stat="identity", fill="#0073C2FF") + 
  coord_flip() +
  theme_bw() +
  labs(x=" ",
       y=" PERSONAS ATENDIDAS",
       title = "PERSONAS ATENDIDAS EN EL SIS 2018 POR REGION",
       subtitle ="",
       caption = "Fuente: Estadisticas de datos abiertos") +
  theme(text = element_text(size=8),
        plot.title = element_text(size=rel(2.9), vjust=2, face="bold", color="darkgreen", lineheight=1.5), # tamanio y color al titulo
        axis.text.x = element_text(angle = 0, size=rel(1.5), hjust = 0.5) ,
        axis.text.y = element_text(face="bold", colour="chocolate4", size=rel(1.5), angle=0, hjust=0.5),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.0, colour="darkgreen", size=rel(1.5)),
        axis.title.y = element_text(face="bold", vjust=1.0, colour="darkgreen", size=rel(1.5)),
        axis.text = element_text(colour = "black"),
        plot.subtitle = element_text(size = 11)) +
  geom_text(aes(label=format(personas_atendidas, 
                             big.mark = ",", scientific = FALSE, digits = 1)), vjust = 0.5, hjust = -0.2) +
  theme(strip.text = element_text(size=15)) +
  scale_colour_gradientn(colours=rainbow(10)) +
  scale_y_continuous(labels = function(x) format(x,big.mark = ",", 
                                                 decimal.mark = ".",
                                                 scientific = FALSE)) 
  

graf2

ggsave(filename = paste0(grafico, "personas_atendidas_region.png"),
       width = 7, height = 10)



# atenciones por genero
base_2018_sex = base %>% 
  group_by(SEXO) %>% 
  summarise(personas_atendidas = n(),
            Numero_atenciones = sum(ATENCIONES))

# grafico
graf3 = ggplot(base_2018_sex, aes(x =SEXO, y =  personas_atendidas)) +
  geom_bar(stat="identity", fill="#0073C2FF") + 
  # coord_flip() +
  theme_bw() +
  labs(x=" ",
       y=" PERSONAS ATENDIDAS",
       title = "PERSONAS ATENDIDAS EN EL SIS 2028",
       subtitle ="",
       caption = "Fuente: Estadisticas de datos abiertos") +
  theme(text = element_text(size=8),
        plot.title = element_text(size=rel(2.9), vjust=2, face="bold", color="darkgreen", lineheight=1.5), # tamanio y color al titulo
        axis.text.x = element_text(angle = 0, size=rel(1.5), hjust = 0.5) ,
        axis.text.y = element_text(face="bold", colour="chocolate4", size=rel(1), angle=0, hjust=0.5),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.0, colour="darkgreen", size=rel(1.5)),
        axis.title.y = element_text(face="bold", vjust=1.0, colour="darkgreen", size=rel(1.5)),
        axis.text = element_text(colour = "black"),
        plot.subtitle = element_text(size = 11)) +
  geom_text(aes(label=format(personas_atendidas, 
                             big.mark = ",", scientific = FALSE, digits = 1)), vjust = -0.5, hjust = 0.5) +
  theme(strip.text = element_text(size=15)) +
  scale_colour_gradientn(colours=rainbow(10)) +
  scale_y_continuous(labels = function(x) format(x,big.mark = ",", 
                                                 decimal.mark = ".",
                                                 scientific = FALSE)) 

graf3

ggsave(filename = paste0(grafico, "personas_atendidas_edad.png"),
       width = 7, height = 10)




  
# realizar el mapa
  
departamental <- read_sf("mapasDpto/DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")

table(base$REGION)
table(departamental$NOMBDEP)

base$NOMBDEP = base %>% 
  dplyr::rename(LIMA == 'LIMA METROPOLITANA',
         LIMA = 'LIMA REGION',
         APURIMAC = 'APURÍMAC')


# generamos grafico de mapa


options(scipen=999)

graf4 <- departamental %>% 
  left_join(base %>% dplyr::count(REGION, name = "ATENCIONES_SIS_2018"),
            by =c("NOMBDEP"= "REGION")) %>% 
  mutate(ATENCIONES_SIS_2018 = as.numeric(ATENCIONES_SIS_2018)) %>% 
  ggplot()+
  geom_sf(aes(fill = ATENCIONES_SIS_2018), show.legend = T, colour = "white")+
  geom_label_repel(aes(label = NOMBDEP,
                       geometry = geometry), 
                   size = 2,
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   label.size = 1,
                   max.overlaps = Inf) +
  scale_fill_viridis_c(trans = "sqrt" ,  alpha = 0.4)+
  theme_void()+
  labs(title = "Mapa de las atenciones del SIS 2018")



graf4

ggsave(filename = paste0(grafico, "mapaatencionesregion.png"),
       width = 6,
       height = 10)

  