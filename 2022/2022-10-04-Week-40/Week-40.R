
### Load the library
###

library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(showtext)


## Import the dataset

tt <- tt_load("2022-10-04")




# Data Wrangle














## Data Visualization







## Save the plot

ggsave(
  'Product_hunt.png',path = here::here('2022/2022-10-04-Week-40'),width = 9.75,height = 7,bg='white')

