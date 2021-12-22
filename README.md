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

ndvi_df = ndvi_df[[1]]

summary(ndvi_df)

#Оставим только пиксели где была зеленая растительность

ndvi_df = ndvi_df %>% filter(value > 0.4)

#Вычислим площадь зеленых насаждений, т.к. в полигон у нас могли попадать пискели не целиком, для тех кусочков что попали частично записана их доля попавшая в полигон - coverage_fraction,
#тогда общая площадь под зелеными насаждениями будет

green_square = sum(ndvi_df$coverage_fraction)*900

#Характеристики рельефа

park_dem_utm = raster::projectRaster(from = park_dem_mask, crs = crs(park_ndvi_crop))

area_slope = terrain(park_dem_utm, opt = 'slope', unit = 'degrees') #calculate slope

area_aspect = terrain(park_dem_utm, opt = 'aspect', unit = 'degrees') #calculate aspect

area_flowdir = terrain(park_dem_utm, opt = 'flowdir', unit = 'degrees') #calculate flowdir

plot(area_slope)
plot(st_geometry(park_sf_utm), add = TRUE)
plot(area_aspect)
plot(st_geometry(park_sf_utm), add = TRUE)
plot(area_flowdir)
plot(st_geometry(park_sf_utm), add = TRUE)

#MODISTools

#С помощью продуктов MODIS попытаемся оценить эвапотранспирацию в парке

# Для начала посмотрим какие показатели мы можем получить от продуктов MODIS

prods = MODISTools::mt_products()

# Нам подходит MOD16A2 - эвапотранспирация

# Посмотрим какие каналы мы можем получить по данному продукту

bands = MODISTools::mt_bands(product = "MOD16A2")

# Канал ET_500m содержит накопленные за 8 дней данные по эвапотраспирации в kg/m^2/8d

# Но мы так же должны учитывать scale factor = 0.1, что значит, что данные представлены в десятых долях килограммов и их нужно домножить на 0.1. Кроме того мы видим диапазон  допустимых значений величины, из которого следует, что значения выше 32700 надо отбросить.
# Проверим для каких дат есть данные для нашей территории

dates = MODISTools::mt_dates(product = "MOD16A2", lat = 40.19, lon = 44.52)


#Так как данные для интересующих нас дат для изучаемой территории имеются перейдем к их получению.

#Для этого в функцию mt_subset мы должны ввести название продукта, координаты территории, канал, 

#дату начала и конца мониторинга, а также параметры km_lr и km_ab, которые будут означать в каком 

#радиусе от указаной точки будут браться пиксели с данными. У нас указано 2, что значит 2км, т.е.
#данные будут браться из окружности радиусом 4 пикселя, т.к. разрешения пикселся 500м

erevan_ET = MODISTools::mt_subset(product = "MOD16A2",
                                 
                                 lat = 40.19,
                                 
                                 lon = 44.52,
                                 
                                 band = "ET_500m",
                                 
                                 start = "2008-01-03",
                                 
                                 end = "2021-11-30",
                                 
                                 km_lr = 2,
                                 
                                 km_ab = 2,
                                 
                                 site_name = "erevan",
                                 
                                 internal = TRUE,
                                 
                                 progress = TRUE)
#летит программа

# В результате мы получили таблицу со значениями из нескольких пикселей за интересующий нас 

# промежуток времени с шагом в 8 дней. Отбросим пропуски в данных и усредним значения 

# пикселей на каждую дату, добавив переменную день года

#переписано

erevan_ET = dubki_ET %>% filter(value < 32700) %>% select(units,calendar_date,value) %>%
  
  mutate(doy=yday(calendar_date), year=year(calendar_date)) %>% group_by(doy,year,units) %>%
  
  summarise(ET = mean(value))

# Т.к. данные у нас идут с шагом в 8 дней, построим их сглаженное графическое

# представление с помощью loess сглаживания в ggplot2

ggplot(erevan_ET, aes(x=doy,y=ET))+
  
  geom_point()+
  
  geom_smooth()+
  
  geom_ribbon(aes(ymin = 0,ymax = predict(loess(ET ~ doy))),
              
              alpha = 0.3,fill = 'blue')+
  
  ylim(c(0,300))+
  
  theme_bw()

#Было бы здорово получить площадь под кривой, т.к. она 
#будет соответствовать усредненной сумме эвапотраспирации за вегетационный
#период. Для окончательных рассчетов нам также надо вспомнить площадь парка и площадь зеленых насаждений в нем 

park_area =st_area(park_sf) %>% as.integer() # площадь парка

green_square # площадь под зелеными насаждениями

# А также данные по осадкам

Prcp_cum = erevan_cum %>% filter(year == 2017) %>% mutate(doy = yday(date)) %>% 
  
  select(doy,prcp_cum) %>% mutate(water_cum = prcp_cum*park_area/1000)

start_day = min(Prcp_cum$doy)

end_day = max(Prcp_cum$doy)

# Тогда общая эвапотраспирация будет рассчитана как

curve = loess(ET ~ doy, erevan_ET) # модель 

ET = (predict(curve,data.frame(doy = start_day:end_day), se = F))#0.1 * kg/m^2/8d

ET[is.na(ET)]=0

ETcum = cumsum(ET)* green_square*0.1/8/1000 #t/m2/d - вспоминаем scale factor

# делим на 8, т.к. данные это сумма за 8 дней и переводим в тонны или м3 воды

# Сводим данные по осадкам и эвапотраспирации в одну таблицу

Prcp_cum$ETcum = ETcum 

#Посчитаем полив как разницу между накопленной с осадками влагой и 

# эвапотранспирацией, усреднив эту разницу на площадь зеленых насаждений

Prcp_cum = Prcp_cum %>% mutate(irrigation = (ETcum - water_cum)/green_square)

# Кумуляты накопленных осадков и эвапотранспирации

ggplot(Prcp_cum, aes(x = doy,y = ETcum))+
  
  geom_line( color="green")+
  
  geom_line(aes(x=doy,y=water_cum))+
  
  ylab("ET vs Precipitation,m3 for Erevan park, 2008")+
  
  theme_bw()

# Необходимый полив - большую часть времени полив не нужен

ggplot(Prcp_cum, aes(x = doy,y = irrigation*1000))+
  
  geom_line( color="red")+
  
  geom_hline(yintercept = 0)+
  
  ylab("Irrigation needed,l/m2 for Erevan park, 2008")+
  
  theme_bw()

# Оставим только ту часть, где полив нужен

ggplot(Prcp_cum, aes(x = doy,y = irrigation*1000))+
  
  geom_line( color="red")+
  
  geom_hline(yintercept = 0)+
  
  ylim(c(-20,200))+ # Эти параметры вам надо подобрать исходя из ваших данных
  
  ylab("Irrigation needed,l/m2 for Erevan park, 2008")+
  
  theme_bw()
