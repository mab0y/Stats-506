library(tidyverse)
library(nycflights13)

airlines_data <- airlines
airports_data <- airports
flights_data <- flights
planes_data <- planes
weather_data <- weather

#https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
flights_data_origin <- flights_data %>% left_join(airports_data, by = c("origin" = "faa"))

flights_data_origin_summary <- flights_data_origin %>%
  group_by(name) %>%
  summarize(mean_dep_delay = mean(dep_delay, na.rm = TRUE), median_dep_delay = median(dep_delay, na.rm = TRUE)) %>%
  filter(!is.na(name)) %>%
  arrange(desc(mean_dep_delay))%>%
  ungroup()

print(flights_data_origin_summary,n=Inf)


flights_data_dest <- flights_data %>% left_join(airports_data, by = c("dest" = "faa"))

# https://stackoverflow.com/questions/22767893/count-number-of-rows-by-group-using-dplyr
flights_data_dest_summary <- flights_data_dest %>%
  group_by(name) %>%
  summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE), median_arr_delay = median(arr_delay, na.rm = TRUE), n_row = n()) %>%
  filter(n_row>=10& !is.na(name)) %>%
  arrange(desc(mean_arr_delay))%>%
  select(-n_row)%>%
  ungroup()

print(flights_data_dest_summary,n=Inf)

  