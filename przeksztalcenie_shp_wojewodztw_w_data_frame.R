#' 
#' Piotr Sobczyk
#' 
#' Przetworzenie plików shp do data.frame

require(rgdal) #funkcja readOGR
require(rgeos) #funkcja gSimplify i spTransform
library(ggplot2) #funkcja fortify

Wojewodztwa <- readOGR("data/wojewodztwa/", "wojewodztwa", encoding = "utf8")
#zapisujemy informacje o województwach, które utracimy przy przeksztłcenia w data.frame
wojewodztwa_nazwy_kody <- data.frame(id = as.character(0:15), 
                                     teryt = Wojewodztwa$jpt_kod_je,  
                                     woj = Wojewodztwa$jpt_nazwa_, 
                                     stringsAsFactors = FALSE)

#zmniejszamy dokładność, dzięki temu mapa będzie się szybciej rysować i zajmie mniej miejsca
wojewodztwa <- gSimplify(Wojewodztwa, tol = 50, topologyPreserve = TRUE)
wojewodztwa <- spTransform(wojewodztwa, CRS("+proj=longlat +datum=WGS84"))
Wojewodztwa <- fortify(wojewodztwa)

#wyznaczamy centroidy województw
centroidy_wojewodztw = data.frame(gCentroid(wojewodztwa, byid = TRUE))
centroidy_wojewodztw$id <- rownames((centroidy_wojewodztw))

save(Wojewodztwa, centroidy_wojewodztw, wojewodztwa_nazwy_kody, file = "data/ksztalt_wojewodztw_data_frame.Rdata")
