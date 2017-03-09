#' 
#' Piotr Sobczyk
#' 
#' Prosta mapa w ggplot - poziom wykształcenia według województw

library(dplyr)
library(tidyr)
library(ggplot2)

load("data/ksztalt_wojewodztw_data_frame.Rdata")
edu <- read.csv("data/education_data.csv")

edu %>% 
  group_by(wojewodztwo) %>%
  summarise(procent_wyzsze_wyksztalcenie = sum(waga[edukacja == 'wyższe i policealne'])/sum(waga)) %>%
  mutate(wojewodztwo = tolower(wojewodztwo)) %>% 
  inner_join(wojewodztwa_nazwy_kody, by = c("wojewodztwo"="woj")) -> edu_podsumowanie

plotData <- inner_join(Wojewodztwa, edu_podsumowanie, by = "id")

library(scales) #pretty breaks, percent
library(ggthemes)
p <- ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = procent_wyzsze_wyksztalcenie)) +
  scale_fill_distiller("%", palette = "YlGn", breaks = pretty_breaks(n = 6),
                       trans = "reverse", labels = percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle(label = "Procent osób z wyższym wykształceniem", subtitle = "W podziale na województwa") + 
  theme_map(base_size = 18) +
  theme(plot.title=element_text(size=24, hjust = 0.5),
        plot.subtitle=element_text(size=22, hjust = 0.5),
        legend.position="right")
p

#### dodanie etykietet z procentami ####

library(ggrepel)

#laczymy centroidy wojewodztw z danymi
plotData_centroidy <- inner_join(centroidy_wojewodztw, edu_podsumowanie, by = "id")

p +  
  geom_label(data=plotData_centroidy, 
             mapping = aes(x=x, y=y, 
                           label=paste0(formatC(100*procent_wyzsze_wyksztalcenie, 2), "%")), 
             show.legend = FALSE) 


#' TODO
#' Jak określać width i height, żeby kształt Polski był zgodny z wytycznymi GUS?
#' http://stat.gov.pl/statystyka-regionalna/publikacje-regionalne/podreczniki-atlasy/podreczniki/mapy-statystyczne-opracowanie-i-prezentacja-danych,1,1.html
#' 
