
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
wct.n <- wctcod %>%  group_by(Spp) %>%
  summarise(n.individuals = n_distinct(oid))


#2022

depths2022 <- wctcod %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(lubridate::year(dt) == 2022) %>%
  ggplot(aes(x = factor(week(dt)), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue3") +
  labs(
    x = "Week of the Year",
    y = "Depth (m)") + theme_minimal() 

plot(depths2022)

library(lunar)

lunar2022 <- wctcod %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(year(dti)==2022) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Month",
    y = "Lunar Illumination"
  ) 

plot(lunar2022)

library(patchwork)

#combine plots 
combined_plot2022 = depths2022/lunar2022
plot(combined_plot2022)

ggsave("10:12,2022.png",last_plot(), width = 8, height = 6, 
       units = c("in"), dpi = 300)

##2023

##boxplot of depths 2023 (Jan to July)
depths2023 <- wctcod %>%
  dplyr::filter(sensor == "depth") %>%
  mutate(month = lubridate::month(dt)) %>%
  dplyr::filter(lubridate::year(dt) == 2023) %>%
  ggplot(aes(x = factor(week(dt)), y = Data, fill = factor(month))) +
  geom_boxplot(fill = "cadetblue3") +
  labs(
       x = "Week of the Year",
       y = "Depth (m)") + theme_minimal() + 
  scale_x_discrete(expand = c(0.05, 0.05))

print(depths2023)

lunar2023 <- wctcod %>% 
  dplyr::select(dt, oid, Spp) %>% 
  mutate(lunar=lunar::lunar.illumination(dt)) %>% 
  mutate(dti=lubridate::date(dt)) %>%
  ungroup %>% 
  distinct(dti, lunar) %>% 
  dplyr::filter(year(dti)==2023) %>% 
  ggplot(aes(dti, lunar))+
  geom_line() + theme_minimal() +
  labs(
    x = "Month",
    y = "Lunar Illumination"
  ) 

print(lunar2023)

#combine the plots using patchwork
combined_plot2023 <- depths2023/lunar2023
print(combined_plot2023)

ggsave("01:07,2023.png",last_plot(), width = 8, height = 6, 
       units = c("in"), dpi = 300)



#GAMS

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

library(gam)
require(mgcv)

#depth as a function of lunar illumination
M1 <- wctcodfinal %>% glm(mean_depth ~ LI, data = .)
summary(M1)
plot(M1)

M2 <-  glm(mean_depth ~ 1, data = wctcodfinal)
anova (M1, M2)

####
gam(mean_depth ~ s(LI),data = wctcodfinal) %>%
plot()


wctcodfinal %>%
  gam(mean_depth ~ s(LI, k=5) + s(month, bs="cc"), data=.) %>% summary 
  
predicted <- predict(M1, type = "response", newdata = tibble(LI = seq(0, 1, by = 0.1)))

# Creating a tibble with LI values and predicted mean_depth
predicted_df <- tibble(LI = seq(0, 1, by = 0.1),
  predicted_mean_depth = predicted)

# Renaming LI column for consistency
predicted_df <- predicted_df %>%
  dplyr::rename(LI = 1)

# Plotting predicted values and actual values
ggplot() +
geom_point(data = predicted_df, aes(LI, predicted_mean_depth), color = "blue") +
geom_point(data = wctcodfinal, aes(LI, mean_depth), color = "dodgerblue", 
          alpha = 0.2) +
          theme_classic() +
          labs(
            x = "Lunar Illumination",
            y = "Depth (m)"
          ) + 
          geom_hline(yintercept = 3)

ggsave("GamPlot.png",last_plot(), width = 8, height = 5, 
        units = c("in"), dpi = 300)

mgcv::gam(mean_depth ~ s(month, bs="cc") + LI,data = wctcodfinal) %>%
  plot


ggsave("GamPlotMonth.png",last_plot(), width = 8, height = 5, 
       units = c("in"), dpi = 300)

