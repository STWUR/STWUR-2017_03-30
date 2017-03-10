#' 
#' Piotr Sobczyk
#' 
#' Prosta mapa w ggplot - poziom wykształcenia według podregionow (NUTS 3)
#' 
#' © EuroGeographics for the administrative boundaries

library(dplyr)
library(tidyr)
library(ggplot2)

load("data/ksztalt_podregionow_data_frame.Rdata")
edu <- read.csv("data/education_data.csv")

edu %>% 
  group_by(rok, podregion66) %>%
  summarise(procent_wyzsze_wyksztalcenie = sum(waga[edukacja == 'wyższe i policealne'])/sum(waga)) %>%
  mutate(podregion66 = tolower(podregion66),
         podregion66 = gsub(pattern = "m\\.[ ]{0,1}", replacement = "", podregion66)) %>%
  ungroup %>% data.frame-> edu_podsumowanie
edu_podsumowanie$podregion66 <- enc2utf8(edu_podsumowanie$podregion66)

edu_podsumowanie$podregion66[edu_podsumowanie$podregion66=="warszawski   zachodni" ] <- "warszawski-zachodni" 
edu_podsumowanie$podregion66[edu_podsumowanie$podregion66=="warszawski wschodni" ] <- "warszawski-wschodni" 

plotData <- edu_podsumowanie %>%  
  left_join(podregiony_nazwy_kody, by = c("podregion66" = "Description")) %>%
  inner_join(podregiony, by = "id")

library(scales) #pretty breaks, percent
library(ggthemes)
p <- ggplot(data = plotData, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(data = plotData[plotData$id %in% plotData[plotData$hole,]$id,], aes(fill = procent_wyzsze_wyksztalcenie), color = "white", size = 0.1) +
  geom_polygon(data = plotData[!plotData$id %in% plotData[plotData$hole,]$id,], aes(fill = procent_wyzsze_wyksztalcenie), color = "white", size= 0.1) +
  # geom_polygon(mapping = aes(group = group, fill = procent_wyzsze_wyksztalcenie), color = "white") +
  facet_wrap(facets = ~rok, ncol = 4) +
  scale_fill_distiller("%", palette = "RdYlGn", breaks = pretty_breaks(n = 6),
                       trans = "reverse", labels = percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle(label = "Procent osób z wyższym wykształceniem", subtitle = "W podziale na podregiony") + 
  theme_map(base_size = 18) +
  theme(plot.title=element_text(size=24, hjust = 0.5),
        plot.subtitle=element_text(size=22, hjust = 0.5),
        legend.position="right")
p

ggsave("wyksztalcenie_podregiony_w_czasie.png", plot = p, width = 12, height = 6)

