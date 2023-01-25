
# Load the Required Packages ----------------------------------------------

library(tidyverse)
library(ggtext)
library(showtext)
library(camcorder)
library(scico)
library(tidytuesdayR)
library(ggalt)


# Import the dataset ------------------------------------------------------

TV <- tt_load(x = 2023,week = 4)
survivallists <- TV$survivalists
loadouts <- TV$loadouts
episodes <- TV$episodes
seasons <- TV$seasons


# Add Google fonts --------------------------------------------------------

font_add_google(name = 'Fira Sans',family = 'Fira Sans')

font <- 'Fira Sans'

showtext_auto(enable = TRUE)
showtext_opts(dpi=320)


# Data Wrangling ----------------------------------------------------------


alone_data <- episodes %>%
  inner_join(y = seasons,by = 'season') %>%
  inner_join(y = loadouts,by = 'season') %>%
  inner_join(y=survivallists,by = 'season')


Alone <- alone_data %>%
select(season,air_date,viewers,country.x,
n_survivors,age,gender,days_lasted,
reason_tapped_out,
profession,reason_category) %>%
  mutate(year=lubridate::year(air_date)) %>%
  relocate(year,.before = season) %>%
  mutate(across(.cols=c(1:2),.fns=factor))

Alone2 <- Alone %>% select(season,days_lasted,gender,country.x) %>%
pivot_wider(names_from = gender,values_from = days_lasted,
id_cols = c(season,country.x),
values_fn = function(x)mean(x,na.rm = TRUE))


# Plotting ----------------------------------------------------------------


ggplot(data = Alone2,aes(x = Male,xend=Female,y=factor(season),group=factor(season)))+
  geom_dumbbell(colour_x = 'goldenrod1',colour_xend = 'mediumpurple1',size_x = 8,
  size_xend = 8,dot_guide_size = 1,size=1)+
  annotate(geom = 'curve',x = 23,y = 7,xend = 38,yend = 7,arrow=arrow(type = 'closed'))+
  annotate(geom = 'curve',x = 97,y = 3,xend = 82,yend = 3,arrow=arrow(type = 'closed'))+
  annotate(geom = 'label',x = 17,y = 7,label=str_wrap(string = 'Less Male in the episode',width = 6),fontface='bold',col='goldenrod1',size=8,fill='#444444')+
  annotate(geom = 'label',x = 95,y = 2,label=str_wrap(string = 'More Female in the episode',width = 6),fontface='bold',col='mediumpurple1',size=8,fill='#444444')+
  labs(x='Number of days lasted in the in the episode',
  y='Season',caption ='Data: Alone Data | Design: @Oluwafemi Oyedele',title = "<span style = 'font-family:font;'>There were More  </span><span style = 'color:goldenrod1;'> male </span>that lasted in <br>the alone data episode than<span style = 'color:mediumpurple1;'> female.</span>")+
  scale_x_continuous(limits = c(0,NA),expand = expansion(mult = c(0,0.1),add = c(0,0.3)))+
  theme_classic()+
  theme(plot.title = element_markdown(family = 'font',face = 'bold',size = 45,colour = 'black',hjust = 0.5),
        plot.subtitle = element_text(family = font,face = 'bold',colour = 'black',size = 15),
        axis.text = element_text(family = font,face = 'bold',colour = 'black',size = 20),
        panel.grid = element_blank(),plot.caption = element_text(family = font,face = 'bold',colour = 'white',size = 21),
        panel.background = element_rect(fill = '#444444'),
        plot.background = element_rect(fill = '#444444'),
        axis.title = element_text(family = font,face = 'bold',colour = 'black',size = 25))





# save the plot -----------------------------------------------------------


ggsave(filename = 'Alone.png',plot = last_plot(),path = here::here('2023/2023-01-24-Week-04/'),width = 13,height = 13)
