
###########################################################################
###########################################################################
###                                                                     ###
###                             DATA IMPORT                             ###
###                                                                     ###
###########################################################################
###########################################################################


babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')




# Load the necessary library
library(tidyverse)
library(ggdark) # dark theme for ggplot2



# Data Visualization

babynames %>%
  group_by(year,name) %>%
  count(sex) %>%
  ggplot(mapping = aes(year,n,fill=sex))+geom_col()+
  scale_y_continuous(expand = expansion(mult = 0,add = 0))+
  labs(y='Proportion of babynames',x='Year')+
  theme(panel.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))+
  scale_fill_brewer(palette = 'Dark2',direction = -1)+
  dark_theme_classic()+
  labs(title= "Sex of newborns from : 1880-2017",
       subtitle="Proportion of total births in the US, arranged in alphabetical order\n",
       caption="\n#TidyTuesday week 12 | Data source: babynames R package from Hadley Wickham")


# Save the image

ggsave('baby.png',width = 20,height = 8,units = 'cm',dpi = 450,path = here::here('E:/R Projects Files/Public TidyTuesday/2022/2022-03-22-Week-12'))


