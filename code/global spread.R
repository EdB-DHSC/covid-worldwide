library(tidyverse)
library(directlabels)
library(rmarkdown)

# DATA FROM JKCSSE https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

countries_of_interest <- c('US','Italy','Spain','Germany','France','Japan','Iran')

update_date <- '21/03/2020'

confirmed <- read.csv('data/confirmed.txt.') %>% 
  select(-Province.State,-Lat,-Long)  %>% 
  rename(Country = Country.Region)
  
colnames(confirmed) <- c('Country',1:(length(confirmed)-1)) 
  
confirmed2 <- confirmed %>% 
  gather(date,confirmed,-Country) %>% 
  mutate(date = as.numeric(date)) %>% 
  group_by(Country,date) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(tot = sum(confirmed)) %>% 
  arrange(date) %>% 
  filter(confirmed > 50)

add_d_since <- function(country){
  confirmed2 %>% 
    filter(Country == country) %>% 
    mutate(d_since = row_number())
}

to_plot <- add_d_since('United Kingdom')

for(region in countries_of_interest){
t <- add_d_since(region)

to_plot <- rbind(to_plot,t)
}

con <- to_plot %>% ggplot(aes(d_since,confirmed,col = Country))+
  geom_line()+
  geom_dl(aes(label = Country), method = list(dl.combine("last.points"), cex = 0.8)) +
  theme_bw()+
  scale_y_log10()+
  labs(title = 'Number of Confirmed Cases',
       x = 'Days Since 50 Confirmed Cases',
       y = 'Number of Confirmed Cases',
       caption = paste('Source: Johns Hopkins CSSE (updated ',update_date,')'))

confirmed <- read.csv('data/deaths.txt.') %>% 
  select(-Province.State,-Lat,-Long)  %>% 
  rename(Country = Country.Region)

colnames(confirmed) <- c('Country',1:(length(confirmed)-1)) 

confirmed2 <- confirmed %>% 
  gather(date,confirmed,-Country) %>% 
  mutate(date = as.numeric(date)) %>% 
  group_by(Country,date) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(tot = sum(confirmed)) %>% 
  arrange(date) %>% 
  filter(confirmed > 10)

add_d_since <- function(country){
  confirmed2 %>% 
    filter(Country == country) %>% 
    mutate(d_since = row_number())
}

to_plot <- add_d_since('United Kingdom')

for(region in countries_of_interest){
  t <- add_d_since(region)
  
  to_plot <- rbind(to_plot,t)
}

deaths <- to_plot %>% ggplot(aes(d_since,confirmed,col = Country))+
  geom_line()+
  geom_dl(aes(label = Country), method = list(dl.combine("last.points"), cex = 0.8)) +
  theme_bw()+
    scale_y_log10()+
  labs(title = 'Number of Deaths',
       x = 'Days Since 10 Deaths',
       y = 'Number of Deaths',
       caption = paste('Source: Johns Hopkins CSSE (updated ',update_date,')'))

render(input = 'code/global spread.rmd',
       output_file = 'reports/global spread.docx')
