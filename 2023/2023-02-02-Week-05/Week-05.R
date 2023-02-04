
# Load the Required Packages ----------------------------------------------

library(tidyverse)
library(camcorder)
library(ggtext)
library(tidytuesdayR)
library(ggimage)
library(showtext)



# Import the dataset ------------------------------------------------------

Pet <- tt_load(x = 2023,week = 5)


cats_uk <- Pet$cats_uk

cats_uk_ref <- Pet$cats_uk_reference


# Add Google fonts --------------------------------------------------------

font_add_google(name = 'Fira Sans',family = 'Fira Sans')

font <- 'Fira Sans'

showtext_auto(enable = TRUE)
showtext_opts(dpi=320)



# Data Wrangling ----------------------------------------------------------


Cats <- cats_uk %>% inner_join(y = cats_uk_ref,by = 'tag_id')


cats_clean <- Cats |>
  mutate(year=lubridate::year(timestamp)) %>%
  group_by(animal_sex) %>%
  ## just use 2017 data
  dplyr::filter(year == 2017) |>
  ## turn into lumped factors with capitalized names
  dplyr::mutate(
    animal_reproductive_condition = stringr::str_to_title(animal_reproductive_condition),
    animal_reproductive_condition = forcats::fct_lump(animal_reproductive_condition, n = 3)) |>
  ## add counts
  dplyr::count(animal_reproductive_condition, sort = TRUE) %>%
  ungroup()


## Pallete

pal <- rev(MetBrewer::met.brewer("Hokusai3")[3:7])

## Calculate the percentage
cats_clean_perc <- cats_clean %>%
  dplyr::mutate(
    perc = scales::percent(n / sum(n), accuracy = 1, trim = TRUE),
    ## customize label for the first category
    perc = if_else(row_number() == 1, paste(perc, "of all cats "), perc)
  )


# Data Visualization ------------------------------------------------------


ggplot(cats_clean_perc %>% drop_na(),aes(y=animal_reproductive_condition,x=n,fill=animal_sex))+
geom_col()+
  (geom_text(
    aes(label = perc),
  hjust = 0.2, nudge_x = -3,size=7,fontface='bold',colour='black'
  ))+
  #geom_textbox(aes(x=1,y=7500,label='96 % of all cats are Neutered',fill='white'),width = unit(0.3, "npc"))+

  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  coord_cartesian(xlim = c(0,10000),clip = 'off')+
  scale_fill_manual(name='Sex',values = c(m='#134b73',f='#74c8c3'),labels=c('Male','Female'))+
  labs(y='Reproductive Condition',x='Count',caption ='Data: Pet Cats UK | Design: @Oluwafemi Oyedele',
       title = str_wrap(string = 'Majority of the UK Cat has Neutered Reproductive System',width = 40))+
  theme_minimal()+
  theme(axis.line = element_line(size = 0.4,colour = 'black'),
        axis.text.y = element_text(family = font,face = 'bold',colour = 'black',margin = margin(t = 20,b = 30,r = 10),size = 15),
        axis.ticks = element_line(colour = 'black',size = 0.5),
        axis.text.x = element_text(family = font,face = 'bold',colour = 'black',size = 15),
        axis.title = element_text(family = font,face = 'bold',colour = 'black',size = 17),
        legend.title = element_text(family = font,face = 'bold',colour = 'black',size = 15),
        legend.text = element_text(family = font,colour = 'black',size = 13),
        panel.grid = element_blank(),
        plot.caption = element_text(family = font,face = 'bold',colour = '#999000',size = 18),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        plot.title = element_text(family = font,face = 'bold',colour = '#452d28',size = 41),
        plot.caption.position = 'plot')



# Save the Plot -----------------------------------------------------------


ggsave(
  filename = "Cats.png",
  width = 12,dpi = 300, height = 10,path = here::here('2023/2023-02-02-Week-05')
)






