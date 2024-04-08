setwd("~/Desktop/Final")
wct<-readRDS("wrasse2.RDS")

remotes::install_github("robertlennox/BTN")
require(raster)
library(dplyr)
library(ggplot2)
library(suncalc)

View(wct)
#install.packages("lunar")



BTN::storel %>%
  as.data.frame(xy=T) %>%
  dplyr::filter(!is.na(z)) %>%
  ggplot(aes(x, y, fill=z))+
  geom_raster()+
  coord_fixed()+
  theme_void()+ ###remove if you want grey background
  geom_point(data=wctcod %>%
               group_by(lon, lat, Spp) %>%
               count(),
             aes(lon, lat, size=n), inherit.aes = F)+
  scale_x_continuous(limits=c(-31700, -30500))+
  scale_y_continuous(limits=c(6732600, 6733500))+
  #facet_wrap(~Spp)+
  labs(fill="max backscatter")+
  theme(legend.position="top", legend.key.width=unit(3, "cm"))+
  scale_fill_gradientn(colours=c("cadetblue", "darkblue", "purple"))

#######SUNCALC
#install.packages("suncalc")
#install.packages("SunlightTimes")
#install.packages("devtools")
require(devtools)
devtools::install_github("datastorm-open/suncalc")
require(suncalc)
#?getSunlightTimes
#st<-getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET")
#getSunlightTimes(
#  date = NULL,
#  lat = NULL,
#  lon = NULL,
#  data = NULL,
#  keep = c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart", "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night", "goldenHourEnd",
#           "goldenHour"),
#  tz = "UTC"
#)

##HELPPPPPPPPPPPPPPPPP

wctcod<-wct %>%
  dplyr::filter(Spp=="Gadus morhua") %>%
  dplyr::filter(sensor=="depth")
BSun<-getSunlightTimes(date = as_date(ymd_hms(dt)),
                 keep = c("sunrise", "sunset"),
                 lat = 60.3913, lon = 5.3221, tz = "CET", data = wctcod)




##ROB THANK YOUUU
wctday<-wctcod %>% 
  dplyr::rename(x=lon, y=lat) %>% 
  ungroup() %>% 
  mutate(suncalc::getSunlightTimes(date=lubridate::date(.$dt), 
                                   lon=60.3838143638701, 
                                   lat=5.319242904684282,
                                   keep=c("sunrise", "sunset"), 
                                   tz="CET")) %>% 
  dplyr::select(-lon, -lat) %>% 
  dplyr::rename(lon=x, lat=y)
#left_join(wctdaynight, wctcod, by=dt) 

wctdaynight<-wctday %>%
  mutate(day=NA) %>%
  mutate(day=(ifelse(wctday$dt > wctday$sunrise & wctday$dt < wctday$sunset,"yes","no")))
wctdayyes<-wctdaynight %>%
  filter(day=="yes") %>%
  dplyr::select(lon, lat, dt, sunrise, sunset, date, day)

###issue because lon and lat are now made constant

ggplot

View(wctday)



##day==yes
require(lubridate)
BTN::storel %>%
  as.data.frame(xy=T) %>%
  dplyr::filter(!is.na(z)) %>%
  ggplot(aes(x, y, fill=z))+
  geom_raster()+
  coord_fixed()+
  theme_void()+ ###remove if you want grey background
  geom_point(data=wctdayyes %>%
               dplyr::filter(day=="yes") %>%
               mutate(month=month(dt)) %>%
               group_by(lon, lat, month, day) %>%
               count(),
             aes(lon, lat, size=n), inherit.aes = F)+
  scale_x_continuous(limits=c(-31700, -30500))+
  scale_y_continuous(limits=c(6732600, 6733500))+
  facet_wrap(~month)+
  labs(fill="max backscatter")+
  theme(legend.position="top", legend.key.width=unit(3, "cm"))+
  scale_fill_gradientn(colours=c("yellow", "blue", "green"))


#day==no
wctdayno<-wctdaynight %>%
  filter(day=="no") %>%
  dplyr::select(lon, lat, dt, sunrise, sunset, date, day)

BTN::storel %>%
  as.data.frame(xy=T) %>%
  dplyr::filter(!is.na(z)) %>%
  ggplot(aes(x, y, fill=z))+
  geom_raster()+
  coord_fixed()+
  theme_void()+ ###remove if you want grey background
  geom_point(data=wctdayno %>%
               dplyr::filter(day=="no") %>%
               mutate(month=month(dt)) %>%
               group_by(lon, lat, month, day) %>%
               count(),
             aes(lon, lat, size=n), inherit.aes = F)+
  scale_x_continuous(limits=c(-31700, -30500))+
  scale_y_continuous(limits=c(6732600, 6733500))+
  facet_wrap(~month)+
  labs(fill="max backscatter")+
  theme(legend.position="top", legend.key.width=unit(3, "cm"))+
  scale_fill_gradientn(colours=c("yellow", "blue", "green"))



###DATA NOW CLEAN

#CLEANED DATA

wctdaynight %>% count(month(dt))

data2<-wctdaynight %>% 
  dplyr::filter(month(dt)==6 | month(dt)==7) %>% 
  group_by(lon, lat, oid, day) %>% 
  count() %>%
  mutate(day=factor(day), oid=factor(oid))

require(mgcv)

m1<-gam(n ~ s(lon, lat, by=day, k=5) + day, data=data2, family="poisson") 

require(tidyr)
require(ggplot2)
expand_grid(lon=seq(-31700, -30500, by =10),
            lat=seq(6732600, 6733500, by=10),
            day=c("yes", "no")) %>% 
  mutate(value=predict.gam(m1, newdata=., type="response")) %>% 
  ggplot(aes(lon, lat, fill=value))+
  geom_raster()+
  facet_wrap(~day)+
  scale_fill_viridis_c()



##END

wctdaynightplot %>%  
  ggplot(aes(lon, lat)) +
  geom_point(data=wctdaynightplot %>%
                          group_by(lon, lat, day) %>%
                          count(),
                        aes(lon, lat, size=n), inherit.aes = F)

####wctcod %>%
  getSunlightTimes(date = as.Date(wctcod$dt),
                   keep = c("sunrise", "sunset"),
                   lat = 60.3913, lon = 5.3221, tz = "CET")

# install.packages("suncalc")
library(suncalc)
# install.packages("lubridate")
library(lubridate)
start.date = "20220916"; end.date = "20230710"
Dates <- seq(ymd(start.date),ymd(end.date), by = "days")
# install.packages("tidyverse")
library(tidyverse)
sun_df <- expand.grid(Dates = Dates,County = wctcod$dt) %>% 
  left_join(wctcod) %>%
  group_by(Dates, County, Latitude, Longitude) %>% 
  mutate(sunrise = getSunlightTimes(Dates,Latitude,Longitude,tz = "CET")$sunrise,
         sunset = getSunlightTimes(Dates,Latitude,Longitude,tz = "CET")$sunset)  

install.packages("suncalc")
library(suncalc)
require(suncalc)
calculate_sun_times <- function(dt, lat, lon) {
  sun_times <- suncalc(dt, lat, lon)
  sunrise <- as.POSIXct(sun_times$sunrise)
  sunset <- as.POSIXct(sun_times$sunset)
  return(data.frame(sunrise = sunrise, sunset = sunset))
}
install.packages(suncalc)
library(suncalc)
sun_times_df <- calculate_sun_times(wctcod$dt, wctcod$lat, wctcod$lon)

wctcod <- cbind(wctcod, sun_times_df)

##HELPPPPPPPPPPPPPPPPP

##

wct %>%
  dplyr::filter(sensor=="temp") %>%
  mutate(month=lubridate::month(dt)) %>%
  ggplot(aes(x=Spp, y=Data, fill=(factor(month))))+
  geom_point()+
  geom_boxplot()+
  coord_flip()+
  scale_fill_viridis_d()

require(lubridate)
wct %>%
  dplyr::select(dt, oid, Spp) %>%
  mutate(lunar=lunar::lunar.illumination(dt)) %>%
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>%
  distinct(dti, lunar) %>%
  dplyr::filter(month(dti)==10, year(dti)==2021) %>%
  dplyr::filter(Spp=="Gadus morhua") %>% ###Why isn't this working?
  ggplot(aes(dti, lunar))+
  geom_point()
  
BTN::storel %>%
  as.data.frame(xy=T) %>%
  dplyr::filter(!is.na(z)) %>%
  ggplot(aes(x, y, fill=z))+
  geom_raster()+
  coord_fixed()+
  theme_void()+ ###remove if you want grey background
  theme(legend.position="top", legend.key.width=unit(3, "cm"))+
  scale_fill_gradientn(colours=c("black", "white"))+
  geom_point(data=wct %>%
               dplyr::filter(Spp=="Gadus morhua") %>%
               dplyr::filter(sensor=="depth") %>%
               mutate(month=month(dt)) %>% 
               group_by(lon, lat, month) %>%
               dplyr::summarise(depth=mean(Data, na.rm=T)),
             aes(lon, lat, colour=depth, size=depth), inherit.aes=F)+
  scale_colour_gradientn(colours=c("yellow", "red"))+
    facet_wrap(~month)
  
