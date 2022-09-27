###########################################################################
###########################################################################
###                                                                     ###
###                         TIDYTUESDAY WEEK 38                         ###
###                                                                     ###
###########################################################################
###########################################################################

## Import my library

library(tidyverse)
library(ggtext)
library(MetBrewer)
library(showtext)
library(patchwork)
#library(sf)
library(colorspace)
library(shades)

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)


## Load the google font

font_add_google("JetBrains Mono")
font_add_google("Eczar","ec", bold.wt = 600)
font_add_google("Open Sans")
font_add_google("Open Sans","os",bold.wt = 600)


## Load the artist dataset

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')



## Color palette hubs
greys <- c(0, 60, 40, 60, 0, 40, 60, 0)
pal1 <- paste0("grey", greys)

## Set up hubs map
hub_northwest <- c("AK", "OR", "ID", "WA")
hub_california <- "CA"
hub_southwest <- c("AZ", "HI", "NM", "NV", "UT")
hub_northern_plains <- c("CO", "MT", "ND", "NE", "SD", "WY")
hub_southern_plains <- c("KS", "OK", "TX")
hub_midwest <- c("IL", "IN", "MN", "IA", "MI", "MO", "OH", "WI")
hub_southeast <- c("AL", "AR", "LA", "MS", "TN", "KY", "GA", "NC", "FL", "GA", "SC", "VA")
hub_northeast <- c("CT", "DE", "ME", "MA", "MD", "NH", "NJ", "NY", "PA", "RI", "VT", "WV")
hubs_order <- c("Northwest", "California", "Southwest", "Northern Plains",
                "Southern Plains", "Midwest", "Southeast", "Northeast")




## Base map

hubs_map <-
  albersusa::usa_sf() %>%
  filter(name != "Alaska" & name != "Hawaii") %>% # Remove Alaska and Hawaii from map
  mutate(
    hub = case_when(
      iso_3166_2 %in% hub_northwest ~ "Northwest",
      iso_3166_2 %in% hub_california ~ "California",
      iso_3166_2 %in% hub_southwest ~ "Southwest",
      iso_3166_2 %in% hub_northern_plains ~ "Northern Plains",
      iso_3166_2 %in% hub_southern_plains ~ "Southern Plains",
      iso_3166_2 %in% hub_midwest ~ "Midwest",
      iso_3166_2 %in% hub_southeast ~ "Southeast",
      TRUE ~ "Northeast"
    ),
    hub = fct_relevel(hub, hubs_order)
  )
