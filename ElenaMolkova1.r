library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(sf)


greendb = read_csv("~/Downloads/greendb.csv")

greendb %>% summary
greendb |> summary()


glimpse(greendb)
colnames(greendb)
names(greendb)



#Построить карту на которой будет по районам отображена 
#средняя высота доминирующего вида растений
# средняя высота березы повислой по районам

### MAPS
library(sf)
library(ggplot2)
library(ggthemes)

map = st_read("~/Downloads/boundary-polygon-lvl8.geojson", 
                 options = "ENCODING=UTF-8")
plot(map)

# Фильтруем по виду
average_heights = greendb %>% filter(species_ru == "Береза повислая (бородавчатая, обыкновенная)", adm_region != "NA") %>% group_by(adm_region )%>%
  summarise(
    mean_height = mean(height_m, na.rm = TRUE)
  )


average_heights_data = average_heights %>% group_by(adm_region, mean_height) %>%
  arrange(adm_region, desc(mean_height)) %>%

  rename(NAME = adm_region)


map = left_join(map, average_heights_data, by="NAME")


ggplot() + geom_sf(data = map, aes(fill=mean_height))+
  theme_foundation() + theme(legend.title = element_blank())

