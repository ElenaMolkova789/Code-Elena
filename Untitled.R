library (tidyverse)
library (lubridate)
library(rnoaa)
library(dplyr)
#library(raster)
library(sp)
library(sf)
library(elevatr)
#library(rLandsat)
library(rvest)
library(curl)

library(RCurl)
devtools::install_github("MatMatt/MODIS", ref = "develop")
#library(MODIS)
library(exactextractr)

#получаем список станций
stations = ghcnd_stations(refresh = FALSE)

#список станций, ближайшие к объекту

erevan = data.frame(id="Evn",
                    latitude = c(40.196345),
                    longitude= c(44.520704))
station_list = meteo_nearby_stations(lat_lon_df = erevan, 
                                     station_data = stations,
                      radius = 150, var = c("PRCP"),
                      year_min = 2008, year_max = 2019)

#фильтруем ближайшие станции
station_list = station_list[[1]]
station_list = station_list %>% filter(name %in% c("HRAZDAN", "APARAN", "ARMAVIR"))

one_station = meteo_tidy_ghcnd(
  "AM000037792")
two_station = meteo_tidy_ghcnd(
  "AM000037699")

one_station = one_station %>% select(id, date, prcp, tavg)
two_station = two_station %>% select(id, date, prcp, tavg)

all_data = rbind(one_station, two_station)

all_data = all_data %>% mutate(tavg /10, prcp = prcp /10)


#отфильтровать с 2007 по 2017
all_data = all_data %>% mutate(year = year(date)) %>% 
                        filter(year > 2008, year < 2019)  %>%
                        mutate( tavg = tavg/10, prcp = prcp/10)
                        


#занятие 2
#заменять "na" на "0"
all_data $ prcp[is.na(all_data $ prcp)] = 0
evn_cum = all_data %>% mutate(month = month(date)) %>% 
                      filter(month > 3 & month <11)  %>%
                      group_by(year) %>% 
                      mutate(prcp_cum = cumsum(prcp))

evn_cum %>% summarise(prcp_avg = max(prcp_cum)) , n = n()

#посмтореть в каком году наибольшее количество остадков и работать с этим годом



#Загрузка карты с парком
park_sf = read_sf("Ahtanak.geojson")

#Конвертация объекта в sp и загрузка для местности ЦМР из пакета elevatr
park_sp = as_Spatial(st_zm(park_sf), cast = TRUE, IDs = paste0("ID", seq_along(from)))
prj = proj4string(park_sp)
park_dem = elevatr::get_elev_raster(park_sp, 14, prj)
plot(park_dem)
plot(st_geometry(park_sf), add = TRUE)

park_dem_mask = crop(park_dem, park_sp)
plot(park_dem_mask)
plot(st_geometry(park_sf), add = TRUE)
#qmap(park_dem_mask, park,sp)


#самописная функция ландаст (подгрузить файл)

#library(devtools)
install_github("remotesensing.R")

#Скачиваем данные Landsat  для парка
search = rLandsat :: landsat_search(min_date = "2008-01-03",
                                    max_date = "2008-11-30",
                                    country = "Armenia",
                                    source = "aws")
your_lat = 40.19
your_lon = 44.52
search_result = search2 %>% filter(min_lat < your_lat, max_lat > your_lat,
                                   max_lon > your_lon, min_lon < your_lon,
                                   clou_1___2over < 15)

#Долгая операция
#качаем снимок
search_result_download(search_result[4,])

#Т.к. мы скачали исходные данные с путника, нам надо получить 
#обработать метаданные с параметрами датчиков и характеристика протела спутника
#для этого используем функции из пакета RStoolbox
ls8t = readMeta
lsst = stackMeta

ls8t_cor = radCor(lsst, metaData = lsr8t, method = "apref", atmosphere = "Clear", verbose =T)
plot(ls8t_cor[[4]])
plot(ls8t_cor)

#Получили и обработали сырые данные, теперь имеет растровые файлы
#посчитаем для имеющихся данных все вегетационные индексы
#NDVI = (NIR - RED)(NIR + RED), как наиболее используемый

lsst_tas = tasseledCap(ls8t_cor[[2:7]], "Landsat80LI")
indexes = spectralIndices(ls8t_cor, blue = 2, green = 3, red = 4,
                          nir = 5, redEdge1 = NULL, redEdge2 = NULL,
                          redEdge3 = NULL, swirl = NULL,
                          swir = 7, swir3 = NULL)
proj4string(indexes)
proj4string(park_sp)
park_sf_utn = st_transform(park_sf, crs = st_crs(lsst_tas$wetness))

#Отрисуем кратосхему NDVI для нашего парка
park_ndvi_crop = crop(index$NDVI, park_sp_utn)
plot(park_ndvi_crop)
plot(st_geometry(park_sf_utn), add = TRUE)

#Выделим все пиксели под полигоном в отдельную таблицу
ndvi_df = exact_extract(park_ndvi_crop, park_ndvi_crop, include_xy=TRUE, in)