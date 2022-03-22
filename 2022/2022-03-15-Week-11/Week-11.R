
###########################################################################
###########################################################################
###                                                                     ###
###                      DATA IMPORT AND WRANGLING                      ###
###                                                                     ###
###########################################################################
###########################################################################

library(tidyverse)
library(lubridate)

bioc <- read_csv('2022/2022-03-22-Week-11/bioc.csv')

#cran <- read_csv('2022/2022-03-22-Week-11/cran.csv')



# Data Wrangling
pkg <- bioc %>%
  mutate(year=year(date)) %>%
  group_by(package) %>%
  mutate(first_release=min(year)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  count(first_release) %>%
  filter(first_release>=2010)


time_of_download <- bioc %>%
  mutate(hour=hour(date)) %>%
  count(hour)



# Visualize the result
ggplot(data = pkg,mapping = aes(x = first_release,n))+
  stat_summary(geom = 'point',fun = 'mean')+
  stat_summary(geom = 'line',fun = 'mean')+
  scale_x_continuous(breaks = seq(2010,2021))+
  labs(y='Number of downloads of bioconductor packages',x='Time (year)')+
  theme(panel.background = element_rect(colour = 'green',fill = '#0194b5'),panel.grid = element_blank(),text = element_text(family = 'serif',face = 'bold',size = 12))


# Save the output
ggsave('bioc.png',path = '2022/2022-03-22-Week-11',width = 10,height = 7,dpi = 350,units = 'cm')


