library(tidyverse)
library(ggplot2)
nnmaps <- read.csv("~/chicago-nmmaps.csv")

nnmaps$celsius<-(nnmaps$temp-32)*5/9
monthly_avg <- nnmaps %>%
  group_by(month, season) %>%
  summarise(mean_temp = mean(celsius, na.rm = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

ggplot(data = monthly_avg, aes(x = month, y = mean_temp, group = season, color = season)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Mean Monthly Temperature",
       x = "Month",
       y = "Mean Temperature (°C)",
       color = "Season")


monthly_avg_all <- nnmaps %>%
  group_by(month, season) %>%
  summarise(mean_temp = mean(celsius, na.rm = TRUE),
            mean_o3 = mean(o3, na.rm = TRUE),
            mean_pm10 = mean(pm10, na.rm = TRUE),
            mean_dewpoint = mean(dewpoint, na.rm = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

ggplot(data = monthly_avg_all, aes(x = month, group = season)) +
  geom_line(aes(y = mean_temp, color = "Temperature(°C)"), size = 1) +
  geom_point(aes(y = mean_temp, color = "Temperature(°C)"), size = 2) +
  geom_line(aes(y = mean_o3, color = "O3"), size = 1) +
  geom_point(aes(y = mean_o3, color = "O3"), size = 2) +
  geom_line(aes(y = mean_pm10, color = "PM10"), size = 1) +
  geom_point(aes(y = mean_pm10, color = "PM10"), size = 2) +
  geom_line(aes(y = mean_dewpoint, color = "Dewpoint"), size = 1) +
  geom_point(aes(y = mean_dewpoint, color = "Dewpoint"), size = 2) +
  labs(title = "Mean Monthly Temperature(°C), O3, PM10 and Dewpoint",
       x = "Month",
       y = "Mean Value",
       color = "Variable")