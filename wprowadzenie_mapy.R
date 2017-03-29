## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval = FALSE-------------------------------------------------------
## install.packages(c("dplyr", "ggplot2", "ggthemes", "ggmap"))

## ---- message=FALSE------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(ggthemes)

## ---- cache = TRUE-------------------------------------------------------
wroclaw_mapa <- get_map(location = "wrocław", zoom = 10)
ggmap(wroclaw_mapa, extent = "normal")

## ---- cache = TRUE-------------------------------------------------------
wroclaw_mapa <- get_map(location = c(17, 51.1), zoom = 12)
ggmap(wroclaw_mapa)

## ---- eval = FALSE-------------------------------------------------------
## devtools::install_github("hadley/ggplot2@v2.2.0")

## ---- message=FALSE------------------------------------------------------
miejsca <- c("Wydział Biotechnologii, Wrocław", "Infopunkt Łokietka", "Wydział Matematyki PWr")
wspolrzedne <- geocode(miejsca)
miejsca <- cbind(miejsca, wspolrzedne)
miejsca$liczba_spotkan <- c(3, 1, 1)

## ------------------------------------------------------------------------
ggmap(wroclaw_mapa) +
  geom_point(data = miejsca, aes(x = lon, y = lat))

## ------------------------------------------------------------------------
ggmap(wroclaw_mapa) +
  geom_point(data = miejsca, aes(x = lon, y = lat, size = liczba_spotkan)) +
  theme_map()

## ---- cache = TRUE-------------------------------------------------------
wroclaw_mapa <- get_map(location = "wrocław", zoom = 13, maptype = "roadmap", color = "bw")
ggmap(wroclaw_mapa) +
  geom_point(data = miejsca, aes(x = lon, y = lat, color = miejsca, size = liczba_spotkan)) +
  theme_map() +
  scale_fill_discrete(name = "")

## ------------------------------------------------------------------------
load("data/ksztalt_wojewodztw_data_frame.Rdata")
edu <- read.csv("data/education_data.csv")

edu_podsumowanie <- edu %>% 
  filter(rok == 2015) %>%
  group_by(wojewodztwo) %>%
  summarise(procent_wyzsze_wyksztalcenie = sum(waga[edukacja == 'wyższe i policealne'])/sum(waga)) %>%
  mutate(wojewodztwo = tolower(wojewodztwo)) %>% 
  inner_join(wojewodztwa_nazwy_kody, by = c("wojewodztwo"="woj"))

plotData <- inner_join(Wojewodztwa, edu_podsumowanie, by = "id")

## ---- eval = FALSE-------------------------------------------------------
## edu %>%
##   filter(rok == 2015)

## ---- eval = FALSE-------------------------------------------------------
## group_by(wojewodztwo) %>%
##   summarise(procent_wyzsze_wyksztalcenie = sum(waga[edukacja == 'wyższe i policealne'])/sum(waga))

## ---- eval = FALSE-------------------------------------------------------
## mutate(wojewodztwo = tolower(wojewodztwo)) %>%
##   inner_join(wojewodztwa_nazwy_kody, by = c("wojewodztwo"="woj"))

## ---- eval = FALSE-------------------------------------------------------
## plotData <- inner_join(Wojewodztwa, edu_podsumowanie, by = "id")

## ---- fig.height = 4, fig.width = 6, warning = FALSE---------------------
library(scales) #pretty breaks, percent
ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = procent_wyzsze_wyksztalcenie))

## ---- fig.height=4, fig.width=5------------------------------------------
ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = procent_wyzsze_wyksztalcenie)) +
  scale_fill_distiller("%", palette = "YlGn", breaks = pretty_breaks(n = 6),
                       trans = "reverse", labels = percent)

## ---- fig.height=6, fig.width=9------------------------------------------
ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = procent_wyzsze_wyksztalcenie)) +
  scale_fill_distiller("%", palette = "YlGn", breaks = pretty_breaks(n = 6),
                       trans = "reverse", labels = percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle(label = "Procent osób z wyższym wykształceniem", subtitle = "W podziale na województwa w 2015") + 
  theme_map(base_size = 18) +
  theme(plot.title = element_text(size = 24, hjust = 0.5, family = "mono"),
        plot.subtitle = element_text(size = 22, hjust = 0.5, family = "mono"),
        legend.position = "right",
        legend.key.height = unit(3, "cm"),
        legend.key.width = unit(1.5, "cm"))

