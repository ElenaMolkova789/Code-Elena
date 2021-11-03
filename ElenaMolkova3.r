library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(sf)


greendb = read_csv("~/Downloads/greendb.csv")

greendb %>% summary
greendb |> summary()

##
### Посчитать регрессионную зависимость высоты от диаметра ствола для района
### Внуково вида Береза повислая
##

data = greendb %>% 
  filter(species_ru == "Береза повислая (бородавчатая, обыкновенная)", adm_region == "район Внуково") %>% select(
    height_m, d_trunk_m
  )

model = lm(d_trunk_m ~ height_m, data)

summary(model)

# d_trunk_m = 0.0159240 * heght_m = 0.0029725
# p < 0.001

anova(model)
# Analysis of Variance Table
# Response: d_trunk_m
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# height_m     1 48.801  48.801  8113.3 < 2.2e-16 ***
# Residuals 5835 35.097   0.006                         
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Зависит p < 0.05