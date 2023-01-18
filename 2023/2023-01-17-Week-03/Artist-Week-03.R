
# Load the Required Packages ----------------------------------------------

library(tidyverse)
library(ggtext)
library(showtext)
library(camcorder)
library(scico)
library(tidytuesdayR)



# Import the dataset ------------------------------------------------------

artist <- tt_load(x = '2023-01-17')
artists <- artist$artists


# Add Google fonts --------------------------------------------------------

font_add_google(name = 'Fira Sans',family = 'Fira Sans')

font <- 'Fira Sans'

showtext_auto(enable = TRUE)
showtext_opts(dpi=320)


# Data Wrangling ----------------------------------------------------------


artists_clean <- artists |>
  ## just use 2020 data
  dplyr::filter(year == 2020) |>
  ## turn into lumped factors with capitalized names
  dplyr::mutate(
    artist_nationality = stringr::str_to_title(artist_nationality),
    artist_nationality = forcats::fct_lump(artist_nationality, n = 10)) |>
  ## add counts
  dplyr::count(artist_nationality, sort = TRUE) |>
  ## order factor levels by number, put "Other" to end
  dplyr::mutate(
    artist_nationality = forcats::fct_rev(forcats::fct_inorder(artist_nationality)),
    artist_nationality = forcats::fct_relevel(artist_nationality, "Other", after = 0)
  ) |>
  filter(row_number()!=16)



## Calculate the percentage of the Artist Nationality
artists_clean_perc <- artists_clean %>%
  dplyr::mutate(
    perc = scales::percent(n / sum(n), accuracy = 1, trim = TRUE),
    ## customize label for the first category
    perc = if_else(row_number() == 1, paste(perc, "of all artist"), perc)
  )



## create color palette based on input data
pal <- c("gray85", rep("gray70", length(artists_clean_perc$artist_nationality) - 4),
                 "coral2", "mediumpurple1", "goldenrod1"
)



# Data Visualization ------------------------------------------------------


ggplot(artists_clean_perc, aes(x = n, y = artist_nationality,
fill = artist_nationality)) +
  geom_col() +
  geom_text(
    aes(label = perc),
    hjust = 1, nudge_x = -.1,size=9,fontface='bold',colour='blue'
  ) +
  ## reduce spacing between labels and bars
  scale_x_continuous(expand = c(.01, .01)) +
  scale_y_discrete(expand = expansion(mult = c(0,0),add = c(0,0)))+
  ## add custom colors
  scale_fill_scico_d(palette = 'bilbao',guide='none')+
  labs(x='Total number of Atrtist',y='Artist Nationality',title = 'The majority of the Artists in 2020 are from American and France \n',caption ='Data: Art History | Design: @Oluwafemi Oyedele' )+
  theme_minimal()+
  theme(panel.grid.minor.x = element_line(linetype =
  'dashed',size = 0.7),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
 plot.background = element_rect(fill = '#19222B'),
 plot.title.position = 'plot',
 plot.title = element_text(family = font,face = 'bold',colour = 'red',hjust = 0.5,size=41),
 plot.caption = element_text(family = font,face = 'bold',colour = 'white',size = 29),
 axis.text.x = element_text(family = font,face = 'bold',colour = 'white',size = 27),
 axis.text.y = element_text(family = font,face = 'bold',colour = 'white',size = 27,hjust = 1),
 axis.title.x = element_text(family = font,face = 'bold',colour = 'white',size = 27),
 axis.title.y = element_text(family = font,face = 'bold',colour = 'white',size = 27),
 plot.margin = margin(rep(15, 4)))



# Save the plot -----------------------------------------------------------

ggsave(filename = 'Artist.png',plot = last_plot(),path = here::here('2023/2023-01-17-Week-03/'),width = 19,height = 13,dpi = 300)
