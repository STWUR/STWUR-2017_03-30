
load("../stwur_17_01/Diagnoza_dane-master/osoby.RData")
load("../stwur_17_01/Diagnoza_dane-master/osobyDict.RData")


library(haven)
library(dplyr)

internet_pytanie <- which(grepl(pattern = ".*godzin w ostatnim tygodniu korzyst.*", x = tolower(osobyDict$description))) +1

internet_dat <- do.call(rbind, lapply(1:6, function(i) {
  j <- i
  if(j > 4) j <- j + 1
  
  if(i==5) {
    single_year_dat <- osoby[, c(6,  29+j, 66+j, 74, 77, internet_pytanie[i])] %>% 
      filter(!is.na(.[[2]]), !is.na(.[[6]])) %>%
      mutate_each(funs(as_factor), -c(2, 6))
  } else {
    single_year_dat <- osoby[, c(6,  29+j, 66+j, 74, 77, internet_pytanie[i])] %>% 
      filter(!is.na(.[[2]]), !is.na(.[[6]])) %>%
      mutate_each(funs(as_factor), -2)
  }

  
  year <- strsplit(colnames(single_year_dat)[[2]], "_")[[1]][2]
  colnames(single_year_dat) <- c("plec",
                                 "waga",
                                 "wiek",
                                 "wojewodztwo",
                                 "podregion66",
                                 "godzin_internetu")
  
  single_year_dat %>% mutate(rok = year) %>% data.frame
}))

internet_dat <- internet_dat %>%
  mutate(godzin_internetu = as.numeric(as.character(godzin_internetu))) %>%
  filter(!is.na(godzin_internetu)) 

write.csv(internet_dat, file = "internet_data.csv", row.names = FALSE)

internet_dat <- read.csv(file = "internet_data.csv")
load("data/ksztalt_wojewodztw_data_frame.Rdata")

internet_podsumowanie <- internet_dat %>% 
  filter(rok == 2015) %>%
  group_by(wojewodztwo) %>%
  summarise(przecietnie_godzin_internetu = sum(waga*godzin_internetu)/sum(waga)) %>%
  mutate(wojewodztwo = tolower(wojewodztwo)) %>% 
  inner_join(wojewodztwa_nazwy_kody, by = c("wojewodztwo"="woj"))

plotData <- inner_join(Wojewodztwa, internet_podsumowanie, by = "id")

ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = przecietnie_godzin_internetu)) +
  scale_fill_distiller("%", palette = "YlGn", breaks = pretty_breaks(n = 6),
                       trans = "reverse") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle(label = "Liczba godzin spędzanych tygodniowo w internecie", subtitle = "Średnia w podziale na województwa w 2015") + 
  theme_map(base_size = 18) +
  theme(plot.title = element_text(size = 24, hjust = 0.5, family = "mono"),
        plot.subtitle = element_text(size = 22, hjust = 0.5, family = "mono"),
        legend.position = "right",
        legend.key.height = unit(3, "cm"),
        legend.key.width = unit(1.5, "cm"))
