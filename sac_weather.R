
library(tidyverse)
library(here)

weatherdata <- here("data/data.csv")

all_cols <- read_csv(weatherdata) 

weather <- read_csv(weatherdata) %>% 
  select(DATE, TMAX, TMIN, WSFG, PRCP) %>% 
  rename("date" = "DATE",
         "t_max_f" = "TMAX",
         "t_min_f" = "TMIN",
         "peak_gust_speed"= "WSFG",
         "total_precip_inches" = "PRCP") %>% 
  mutate(t_max_f = ifelse(t_max_f > 130, NA, t_max_f),
         t_min_f = ifelse(t_min_f < 20, NA, t_min_f)) 
  
weather %>% ggplot(aes(x=t_min_f)) + geom_histogram(fill="Red") 
weather %>% ggplot(aes(x=t_max_f)) + geom_histogram(fill = "Dark Blue")
weather %>% ggplot(aes(peak_gust_speed)) + geom_histogram()

weather %>% 
  select(t_min_f, t_max_f) %>% 
  summary()

weather %>% 
  mutate(year = year(date)) %>% #focusing on the year, from the column "date"
  select(year, everything()) #this lists the year first then everything else after it


avg_high_temp_plot <- weather %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(avg_high_t = mean(t_max_f, na.rm = TRUE)) %>%
  ggplot(aes(x=year, y=avg_high_t))+
  geom_line(color = "red")+
  geom_smooth(color="black")
              
avg_low_temp_plot <- weather %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(avg_low_t = mean(t_min_f, na.rm = TRUE))%>%
  ggplot(aes(x=year, y=avg_low_t))+
  geom_line(color = "blue")+
  geom_smooth(color = "black")


  
  

  
