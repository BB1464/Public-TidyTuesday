
# EDA ---------------------------------------------------------------------

library(tidyverse)
library(DT)
library(plotly)





# Import the dataset ------------------------------------------------------

birdfeeder <- read_csv(here::here('Data/PFW_2021-public.csv'))



#  Top sighted bird data --------------------------------------------------


top_sighted_bird <- birdfeeder %>%
  group_by(species_code) %>%
  summarise(total_sightings = sum(how_many, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total_sightings)) %>%
  head(1) %>%
  pull(species_code)


birdfeeder_US_top <- birdfeeder %>%
  filter(subnational1_code  %>% str_detect("US-")) %>%
  mutate(state_code = subnational1_code %>% str_remove("US-")) %>%
  select(latitude, longitude, state_code, species_code) %>%
  filter(species_code %in% top_sighted_bird)


#  Plot using Plotly ------------------------------------------------------


bird_plot <- plot_geo(birdfeeder_US_top,
                      lat = ~latitude,
                      lon = ~longitude,
                      # color = ~state_code,
                      marker = list(
                        size = 3,
                        opacity = 0.3,
                        color = "red"
                      ),
                      showlegend = FALSE
) %>%
  add_trace(text = ~state_code,
            hoverinfo = 'text') %>%
  layout(geo = list(
    scope = 'usa'
    , showland = TRUE
    , showsubunits = FALSE
    , landcolor = ('black')),
    title = list(text = str_glue("Bird sightings across the US from \n Nov, 2020 to Apr, 2021"), y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'),
    subtitle = "dd",
    font = list(size = 13, color = "green")
  ) %>%
  style(hoverlabel = list(font = list(size=20))) %>%
  # config(displayModeBar = FALSE) %>%
  # add_annotations(x = 0.75, y=0.66, text = str_glue("Highest sightings\nPA"), ax = 5, ay = -80, font = list(size = 14)) %>%
  add_annotations(
    showarrow = F,
    x = 0.1,
    y=0.08,
    text = str_glue("Data source: FeederWatch"),
    font = list(color = '#949494',
                size = 14)
  ) %>%
  add_annotations(
    showarrow = F,
    x = 0.1,
    y=0.005,
    text = str_glue("FeederWatch is a place-based citizen science program that asks participants \nto identify and count the birds that visit the area around their home.    "),
    font = list(color = '#949494',
                size = 12)
  )


# Save the Plot -----------------------------------------------------------


bird_plot


