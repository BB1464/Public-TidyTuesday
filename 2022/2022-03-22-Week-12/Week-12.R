
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
library(ggeasy)
library(ggdark) # dark theme for ggplot2



# Data Visualization

babynames %>%
  group_by(year,name) %>%
  count(sex) %>%
  ggplot(mapping = aes(year,n,fill=sex))+geom_col()+
  scale_y_continuous(expand = expansion(mult = 0,add = 0))+
  labs(y='Proportion of babynames',x='Year')+
  theme(panel.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))+
  scale_fill_brewer(palette = 'Dark2',direction = -1,label=c('Female','Male'))+
  dark_theme_classic()+
  labs(title= "Sex of newborns from : 1880-2017",
       subtitle="Proportion of total births in the US",
       caption="\n#TidyTuesday week 12 | Data source: babynames R package from Hadley Wickham")


# Save the image

ggsave('baby.png',width = 20,height = 8,units = 'cm',dpi = 450,path = here::here('E:/R Projects Files/Public TidyTuesday/2022/2022-03-22-Week-12'))




############################################################################
############################################################################
###                                                                      ###
###                            ANNIMATED PLOT                            ###
###                                                                      ###
############################################################################
############################################################################



babynames %>%
  filter(name %in% c("Anthony", "Maria", "Carmen", "Maximo", "Raul", "Rosario")) %>%
  count(year, name, wt = n) %>%
  mutate(name = fct_reorder(name, -n)) %>%
  ggplot(aes(year, n, fill = name)) +
  geom_col(show.legend = FALSE, alpha = 0.5) +
  geom_smooth(aes(color = name), method = "loess", se = FALSE, show.legend = FALSE) +
  expand_limits(y = 0) +
  #scale_y_log10() +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  labs(x = "",
       y = "# of babies",
       title = "Baby Name Usage from: 1880 - 2017",
       subtitle = "For names in the Galvan family",
       caption = "\n#TidyTuesday week 12 | Data source: babynames R package from Hadley Wickham")


library(gganimate)
top_names <- babynames %>%
  group_by(year, sex) %>%
  mutate(rank = rank(-n)) %>%
  filter(rank <= 10) %>%
  ungroup()
staticplot <- top_names %>%
  ggplot(aes(rank, group = name, fill = name, color = name)) +
  geom_tile(aes(y = n/2,
                height = n,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste0(name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = n,label = scales::comma(n, accuracy = 1), hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  facet_wrap(~sex, scales = "free", ncol = 1) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-0.5),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2, 2, 2, 4, "cm"))
anim <- staticplot +
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE) +
  labs(x = "# of babies",
       y = "",
       title = 'Top Baby Names: {closest_state}',
       subtitle = "Top 10 Names by Sex",
       caption = "\n#TidyTuesday week 12 | Data source: babynames R package from Hadley Wickham")
animate(anim, nframes = 138, detail = 5, fps = 5, width = 800, height = 1000,
        renderer = gifski_renderer("2022_03_22_TidyTuesday.gif"))


# Save Image

# This will save your most recent plot
ggsave('Annimation.png',width = 20,height = 8,units = 'cm',dpi = 450,path = here::here('E:/R Projects Files/Public TidyTuesday/2022/2022-03-22-Week-12'))





