#Мещерякова Екатерина Анатольевна – регион 25
#урожайность пшеницы в 2003 году, взяв для рассчета средние 
#суммы активных температур за предыдущие 12 лет, 
#с метеостанций на расстоянии от 90 до 180 км.

rm(list=ls())

library(rnoaa)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

setwd("D:/MathMod")

station_data = read.csv("station_data.csv")
#Получаем список станций ближайших к городу Владивосток,создав таблицу с именем региона и координатами его столицы
Vladivostok = data.frame(id = "VLADIVOSTOK", latitude = 43.162835, longitude = 131.932465)
Vladivostok_around = meteo_nearby_stations(lat_lon_df = Vladivostok, station_data = station_data,
                                 var = c("PRCP", "TAVG"),
                                 year_min = 1991, year_max = 2003)
Vladivostok_table = Vladivostok_around[[1]]
Vladivostok_table = filter (Vladivostok_table, distance =>90, distance <= 180)

Vladivostok_id1 = Vladivostok_around[["VLADIVOSTOK"]][["id"]][[1]]

str(Vladivostok_around)
all_Vladivostok_data = meteo_tidy_ghcnd(stationid = Vladivostok_id1)
#Чтобы получить таблицу всех метеостанций вокруг Владивостока нужно выбрать целиком первый объект из списка
Vladivostok_table = Vladivostok_around[[1]]
summary(Vladivostok_table)
#Создаем цикл, в котором бы скачивались нужные данные
#для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные
#с конкретной метеостанции
all_i = data.frame()
#Создадим объект, куда скачаем все данные всех метеостанций
all_Vladivostok = data.frame()
#Цикл для всех метеостанций
for(i in 1:2) 
{all_i = meteo_tidy_ghcnd(stationid = Vladivostok_around[["VLADIVOSTOK"]][["id"]][i])
all_i = all_i[ ,c("id","date","tavg")] 
print(all_i)
all_Vladivostok=rbind(all_Vladivostok, all_i)}
#Рассчитаем среднюю сумму активных температур для двух метеостанций Амурской области
Vladivostok_data = all_Vladivostok %>% 
  mutate(date=ymd(date),
         year=year(date),
         month=month(date)) %>%
  mutate(tavg=case_when( tavg<50 ~ 0, TRUE ~ tavg)/10) %>% 
  filter (year>1990 & year < 2004) %>% 
  group_by(id,year,month) %>% 
  summarize(tsum = sum(tavg,na.rm=TRUE)) %>% 
  mutate(tsum=case_when(tsum==NA~0, TRUE ~ tsum)) %>% ungroup() %>% 
  group_by(month) %>% summarize(St = mean(tsum))

afi=c(0.00,0.00,0.00,32.11,26.31,25.64,32.20,18.73,
      16.30,13.83,0.00,0.00)
bfi=c(0.00,0.00,0.00,11.30,9.26,9.03,8.16,6.59,5.73,
      4.87,0.00,0.00)
di=c(0.00,0.00,0.00,0.33,1.00,1.00,1.00,0.32,0.00,
     0.00,0.00,0.00)

y=1.0
Kf=300
Qj=1600
Lj=2.2
Ej=25
#Рассчитаем урожайность по месяцам
Vladivostok_data = Vladivostok_data %>% 
  mutate(Fi=(afi)+(bfi)*y*(Vladivostok_data$St))
Vladivostok_data = Vladivostok_data %>% mutate(Yj=(((Vladivostok_data$Fi)*(di)*Kf)/(Qj*Lj*(100-Ej))))
#Расчитываем суммарную урожайность как сумму по месяцам
YIELD=sum(Vladivostok_data$Yj);YIELD
# Ответ: 11.99 ц/га