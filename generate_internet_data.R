# download.file(url = "https://github.com/michbur/Diagnoza_dane/archive/master.zip", 
#               destfile = "diagnoza.zip")
# unzip("diagnoza.zip", exdir = getwd())
# file.remove("diagnoza.zip")

load("./Diagnoza_dane-master/osoby.RData")
load("./Diagnoza_dane-master/osobyDict.RData")

library(haven)
library(dplyr)

internet_pytanie <- grep(pattern = ".*godzin w ostatnim tygodniu korzyst.*", x = tolower(osobyDict$description)) + 1
# samotnosc_pytanie <- grep(pattern = "osamotnion", x = tolower(osobyDict$description))+ 1
# dochod_pytanie <- grep(pattern = "asny (osobisty)", x = tolower(osobyDict$description), fixed = TRUE) + 1
# 
# data.frame(id = 1L:nrow(osoby), osoby[, internet_pytanie]) %>% 
#   mutate(number_of_na = apply(., 1, function(i) sum(is.na(i)))) %>% 
#   filter(number_of_na != length(internet_pytanie)) %>% 
#   filter(number_of_na != 5)

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
})) %>%
  mutate(godzin_internetu = as.numeric(as.character(godzin_internetu))) %>%
  filter(!is.na(godzin_internetu)) %>% 
  mutate(wojewodztwo2 = tolower(wojewodztwo)) %>% 
  inner_join(wojewodztwa_nazwy_kody, by = c("wojewodztwo2"="woj")) %>% 
  mutate(woj_id = id) %>% 
  select(-teryt, -wojewodztwo2, -id) 

write.csv(internet_dat, file = "internet_data.csv", row.names = FALSE)

internet_dat <- read.csv(file = "internet_data.csv")
load("data/ksztalt_wojewodztw_data_frame.Rdata")

internet_podsumowanie <- internet_dat %>% 
  mutate(woj_id = as.character(woj_id)) %>% 
  filter(rok == 2015) %>%
  group_by(wojewodztwo, woj_id) %>%
  summarise(przecietnie_godzin_internetu = sum(waga*godzin_internetu)/sum(waga)) 

plotData <- inner_join(Wojewodztwa, internet_podsumowanie, by = c("id" = "woj_id"))

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
