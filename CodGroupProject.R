
library(remotes)
library(ggplot2)
require(raster)
require(lubridate)
library(dplyr)



setwd("~/Documents/SCHOOL /5th YEAR /MARI 4323/Group ")
getwd()

wct <- readRDS("wrasse2.RDS")
 
##draw out cod from data set 
wctcod<-wct %>%
  dplyr::filter(Spp=="Gadus morhua")

#number of tagged individuals 
wct.n <- wctcod %>%  group_by(Spp) %>% summarise(n.individuals = n_distinct(oid))

##2022 

#OCTOBE
##Filter out october depth data 
october_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 10) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  
  ##boxplot of depths over october 
  ggplot(aes(x = format(dt, "%d"), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue2") +
  labs(title = "Gadus morhua October Depth",
       x = "Date", 
       y = "Depth (m)") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), # Remove x-axis title
        axis.text.x = element_blank(),# Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks 
        
print(october_depths)

library(lunar)

october_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==10, year(dti)==2022) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  ) + scale_x_date(date_breaks = "2 day", date_labels = "%d", 
               limit = as.Date(c("2022-10-01", "2022-10-31")), 
                               expand = expansion(add = 0.5))

print(october_lunar)

#combine the plots using patchwork
library(patchwork)

combined_plot10 <- october_depths/october_lunar
print(combined_plot10)

ggsave( )

#NOVEMBER
##Filter out november depth data 
november_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 11) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  
  ##boxplot of depths over november 
  ggplot(aes(x = format(dt, "%d"), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue2") +
  labs(title = "Gadus morhua November Depth",
       x = "Date", 
       y = "Depth (m)") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), # Remove x-axis title
        axis.text.x = element_blank(),# Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks 

print(november_depths)

november_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==11, year(dti)==2022) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  ) + scale_x_date(date_breaks = "2 day", date_labels = "%d", 
                   limit = as.Date(c("2022-11-01", "2022-11-30")), 
                   expand = expansion(add = 0.5))

print(november_lunar)

#combine the plots using patchwork
combined_plot11 <- november_depths/november_lunar

print(combined_plot11)

#DECEMBER
##Filter out december depth data 
december_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 12) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  
  ##boxplot of depths over december 
  ggplot(aes(x = format(dt, "%d"), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue2") +
  labs(title = "Gadus morhua December Depth",
       x = "Date", 
       y = "Depth (m)") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), # Remove x-axis title
        axis.text.x = element_blank(),# Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks 

print(december_depths)


december_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==12, year(dti)==2022) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  ) + scale_x_date(date_breaks = "2 day", date_labels = "%d", 
                   limit = as.Date(c("2022-12-01", "2022-12-31")), 
                   expand = expansion(add = 0.5))

print(december_lunar)

#combine the plots using patchwork
combined_plot12 <- december_depths/december_lunar

print(combined_plot12)


##2023
#JANUARY

##Filter out January depth data 
january_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 01) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  
  ##boxplot of depths over January 
  ggplot(aes(x = format(dt, "%d"), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue2") +
  labs(title = "Gadus morhua January Depth",
       x = "Date", 
       y = "Depth (m)") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), # Remove x-axis title
        axis.text.x = element_blank(),# Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks 

print(january_depths)

#january lunar
january_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==01, year(dti)==2023) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  ) + scale_x_date(date_breaks = "2 day", date_labels = "%d", 
                   limit = as.Date(c("2023-01-01", "2023-01-30")), 
                   expand = expansion(add = 0.5))

print(january_lunar)

#combine the plots using patchwork
combined_plot01 <- january_depths/january_lunar

print(combined_plot01)

#FEBRUARY

##Filter out february depth data 
february_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 02) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  
  ##boxplot of depths over february 
  ggplot(aes(x = format(dt, "%d"), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue2") +
  labs(title = "Gadus morhua February Depth",
       x = "Date", 
       y = "Depth (m)") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), # Remove x-axis title
        axis.text.x = element_blank(),# Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks 

print(february_depths)

#february lunar
february_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==02, year(dti)==2023) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  ) + scale_x_date(date_breaks = "2 day", date_labels = "%d", 
                   limit = as.Date(c("2023-02-01", "2023-02-28")), 
                   expand = expansion(add = 0.5))

print(february_lunar)

#combine the plots using patchwork
combined_plot02 <- february_depths/february_lunar

print(combined_plot02)


#MARCH

##Filter out march depth data 
march_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 03) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  
  ##boxplot of depths over march
  ggplot(aes(x = format(dt, "%d"), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue2") +
  labs(title = "Gadus morhua March Depth",
       x = "Date", 
       y = "Depth (m)") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), # Remove x-axis title
        axis.text.x = element_blank(),# Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks 

print(march_depths)

#march lunar
march_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==03, year(dti)==2023) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  ) + scale_x_date(date_breaks = "2 day", date_labels = "%d", 
                   limit = as.Date(c("2023-03-01", "2023-03-31")), 
                   expand = expansion(add = 0.5))

print(march_lunar)

#combine the plots using patchwork
combined_plot03 <- march_depths/march_lunar

print(combined_plot03)


##APRIL 

##boxplot of depths over April 
April_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 04) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  ggplot(aes(x = factor(format(dt, "%d")), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue3") +
  labs(title = "Gadus morhua April Depth",
       x = "Date",
       y = "Depth (m)") + theme_minimal() +
  theme(axis.title.x = element_blank(), # Remove x-axis title
        axis.text.x = element_blank(),# Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks 

print(April_depths)

April_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==04, year(dti)==2023) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  ) + scale_x_date(date_breaks = "2 day", date_labels = "%d", 
                  limit = as.Date(c("2023-04-01", "2023-04-30")), 
                        expand = expansion(add = 0.5))

print(April_lunar)

#combine the plots using patchwork
combined_plot04 <- April_depths/April_lunar

print(combined_plot04)


##MAY 

##boxplot of depths over may 
May_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 05) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  ggplot(aes(x = factor(format(dt, "%d")), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue3") +
  labs(title = "Gadus morhua April Depth",
       x = "Date",
       y = "Depth (m)") + theme_minimal() +
  theme(axis.title.x = element_blank(), # Remove x-axis title
        axis.text.x = element_blank(),# Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks 

print(may_depths)

may_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==05, year(dti)==2023) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  ) + scale_x_date(date_breaks = "2 day", date_labels = "%d", 
                   limit = as.Date(c("2023-05-01", "2023-05-31")), 
                   expand = expansion(add = 0.5))

print(April_lunar)



##boxplot of depths over July 
July_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 07) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%
  ggplot(aes(x = format(dt, "%d"), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue3") +
  labs(title = "Gadus morhua June Depth",
       x = "Date",
       y = "Depth (m)") + theme_minimal()

print(July_depths)

##check to see data points in june 

July_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==07, year(dti)==2021) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal()

print(July_lunar)

##Filter out october depth data 
october_depths <- wct %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(month == 10) %>%
  dplyr::filter(Spp == "Gadus morhua") %>%

##boxplot of depths over october 
  ggplot(aes(x = format(dt, "%d"), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue2") +
  labs(title = "Gadus morhua October Depth",
    x = "Date",
    y = "Depth (m)") + 
    theme_minimal() +
      theme(axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()) # Remove x-axis ticks

  print(october_depths)

october_lunar <- wct %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(month(dti)==10, year(dti)==2021) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Date",
    y = "Lunar Illumination"
  )+
  scale_x_date(date_breaks = "2 day", date_labels = "%d-%m")

#combine the plots using patchwork
combined_plot <- october_depths/october_lunar

print(combined_plot)
  




##practice with images of moons 

library(ggplot2)
library(png)
library(grid)
# Sample moon images dataframe (replace this with your actual moon images dataframe)
moon_images <- data.frame(
lunar_phase = c("New Moon", "First Quarter", "Full Moon", "Last Quarter"),
image_path = c("new_moon_image.png", "first_quarter_image.png",
"full_moon_image.png", "last_quarter_image.png"),
dti = as.Date(c("2021-10-06", "2021-10-13", "2021-10-21", "2021-10-29"))  # Add corresponding dates
)
# Read moon images
images <- lapply(moon_images$image_path, readPNG)
# Create a list of raster grobs
raster_grobs <- lapply(images, rasterGrob)
# Add moon images as annotations
october_lunar <- wct %>%
dplyr::select(dt, oid, Spp) %>%
mutate(lunar = lunar::lunar.illumination(dt)) %>%
mutate(dti = lubridate::date(dt)) %>%
ungroup() %>%
distinct(dti, lunar) %>%
dplyr::filter(month(dti) == 10, year(dti) == 2021)
gg <- ggplot(october_lunar, aes(dti, lunar)) +
geom_line() +
theme_minimal()
for (i in seq_along(raster_grobs)) {
gg <- gg + annotation_custom(raster_grobs[[i]],
xmin = as.numeric(moon_images$dti[i]) - 0.5,
xmax = as.numeric(moon_images$dti[i]) + 0.5,
ymin = 0.95, ymax = 1.05)
}
gg


#GAMS


dat_summary <- dat. ###### error object 'dat' not found 

wctcodtest <- wctcod %>%
  mutate(date = floor_date(dt, unit = "day"))

wctcodtest <- wctcodtest %>%
  filter(sensor %in% "depth") %>%
  group_by(oid, date) %>%
  summarize(mean_depth = mean(Data)) %>%
  ungroup()

wctcodfinal <- wctcodtest %>%
  mutate(LI = lunar::lunar.illumination(date), month = lubridate::month(date))

#Hypothesis: cod depth is influenced by lunar illumination

#depth as a function of lunar illumination
M1 <- wctcodfinal %>% glm(mean_depth ~ LI, data = .)
summary(M1)

M2 <-  glm(mean_depth ~ 1, data = wctcodfinal)
anova (M1, M2)

####
mgcv::gam(mean_depth ~ s(LI),data = wctcodfinal) %>%
plot()

summary(gam1)


library(gam)

require(mgcv)
wctcodfinal %>%
  gam(mean_depth ~ s(LI, k=5), data=.) %>% summary
  predict(.,type="response", newdata=tibble(LI=seq(0, 1, by=0.1))) %>%
  as_tibble %>%
  bind_cols(seq(0, 1, by=0.1)) %>%
  dplyr::rename(LI=2) %>%
  ggplot(aes(LI, value))+
  geom_point()+
  geom_point(data=wctcodfinal, aes(LI, mean_depth), colour="red", alpha=00.1)+
  theme_classic()+
  geom_hline(yintercept=3)

mgcv::gam(mean_depth ~ s(month, bs="cc") + LI,data = wctcodfinal) %>%
  plot




