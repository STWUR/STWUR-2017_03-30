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

## ---- cache = TRUE, message = FALSE--------------------------------------
wroclaw_mapa <- get_map(location = "wrocław", zoom = 10)
ggmap(wroclaw_mapa, extent = "normal")

## ---- cache = TRUE, message=FALSE----------------------------------------
wroclaw_mapa <- get_map(location = c(17, 51.1), zoom = 12)
ggmap(wroclaw_mapa)

## ---- eval = FALSE-------------------------------------------------------
## devtools::install_github("hadley/ggplot2@v2.2.0")

## ---- message=FALSE, cache = TRUE----------------------------------------
miejsca <- c("Wydział Biotechnologii, Wrocław", "Infopunkt Łokietka", "Wydział Matematyki PWr")
wspolrzedne <- geocode(miejsca) # dzienny limit użyć funkcji: 2500
miejsca <- cbind(miejsca, wspolrzedne)
miejsca$liczba_spotkan <- c(3, 1, 1)

## ------------------------------------------------------------------------
ggmap(wroclaw_mapa) +
  geom_point(data = miejsca, aes(x = lon, y = lat))

## ------------------------------------------------------------------------
ggmap(wroclaw_mapa) +
  geom_point(data = miejsca, aes(x = lon, y = lat, size = liczba_spotkan)) +
  theme_map()

## ---- cache = TRUE, message = FALSE--------------------------------------
wroclaw_mapa <- get_map(location = "wrocław", zoom = 13, maptype = "roadmap", color = "bw")
ggmap(wroclaw_mapa) +
  geom_point(data = miejsca, aes(x = lon, y = lat, color = miejsca, size = liczba_spotkan)) +
  theme_map() + scale_fill_discrete(name = "")

## ----rysunekPodregiony, echo = FALSE, out.width = "800px"----------------
knitr::include_graphics("wyksztalcenie_podregiony_w_czasie.png")

## ----wczytanieDaneInternet, cache = TRUE, warning = FALSE----------------
internet_dat <- read.csv(file = "data/internet_data.csv")
load("data/ksztalt_wojewodztw_data_frame.Rdata")

internet_podsumowanie <- internet_dat %>% 
  mutate(woj_id = as.character(woj_id)) %>% 
  filter(rok == 2015) %>%
  group_by(wojewodztwo, woj_id) %>%
  summarise(przecietnie_godzin_internetu = sum(waga*godzin_internetu)/sum(waga)) 

plotData <- inner_join(Wojewodztwa, internet_podsumowanie, by = c("id" = "woj_id"))

## ---- eval = FALSE-------------------------------------------------------
## internet_dat %>%
##   mutate(woj_id = as.character(woj_id))

## ---- eval = FALSE-------------------------------------------------------
## filter(rok == 2015)

## ---- eval = FALSE-------------------------------------------------------
## group_by(wojewodztwo) %>%
##   summarise(procent_wyzsze_wyksztalcenie = sum(waga[edukacja == 'wyższe i policealne'])/sum(waga))

## ---- eval = FALSE-------------------------------------------------------
## plotData <- inner_join(Wojewodztwa, internet_podsumowanie, by = c("id" = "woj_id"))

## ---- fig.height = 4, fig.width = 6, warning = FALSE---------------------
library(scales) #pretty breaks, percent
ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = przecietnie_godzin_internetu))

## ---- fig.height=4, fig.width=5------------------------------------------
ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = przecietnie_godzin_internetu)) +
  scale_fill_distiller("h", palette = "YlGn", breaks = pretty_breaks(n = 6),
                       trans = "reverse")

## ---- eval = FALSE-------------------------------------------------------
## ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
##   geom_polygon(mapping = aes(group = group, fill = przecietnie_godzin_internetu)) +
##   scale_fill_distiller("Godzin", palette = "YlGn", breaks = pretty_breaks(n = 6),
##                        trans = "reverse") +
##   guides(fill = guide_legend(reverse = TRUE)) +
##   ggtitle(label = "Liczba godzin spędzanych tygodniowo w internecie", subtitle = "Średnia w podziale na województwa w 2015") +
##   theme_map(base_size = 18) +
##   theme(plot.title = element_text(size = 24, hjust = 0.5, family = "mono"),
##         plot.subtitle = element_text(size = 22, hjust = 0.5, family = "mono"),
##         legend.position = "right",
##         legend.key.height = unit(3, "cm"),
##         legend.key.width = unit(1.5, "cm"))

## ---- echo = FALSE, fig.height=5.5, fig.width=8.5------------------------
ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = przecietnie_godzin_internetu)) +
  scale_fill_distiller("Godzin", palette = "YlGn", breaks = pretty_breaks(n = 6),
                       trans = "reverse") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle(label = "Liczba godzin spędzanych tygodniowo w internecie", subtitle = "Średnia w podziale na województwa w 2015") + 
  theme_map(base_size = 18) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, family = "mono"),
        plot.subtitle = element_text(size = 18, hjust = 0.5, family = "mono"),
        legend.position = "right",
        legend.key.height = unit(1.7, "cm"),
        legend.key.width = unit(1.1, "cm"))

