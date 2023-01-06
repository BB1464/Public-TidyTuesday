############################################################################
############################################################################
###                                                                      ###
###                      LOAD THE REQUIRED PACKAGES                      ###
###                                                                      ###
############################################################################
############################################################################


library(tidyverse)
library(treemapify)
library(showtext)
library(ggsci)
library(camcorder)
library(ggtext)

## Record Plot

gg_record(dir = "temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)


# Import the dataset ------------------------------------------------------

chocolate <- read_csv('Data/2023_chocolate.csv')


# add google fonts

font_add_google(name = "Fira Sans", family = "Fira Sans")

showtext_auto(enable = TRUE)

# prep data
plot_data <- chocolate %>%
  group_by(country_of_bean_origin) %>%
  summarise(n = n(),
            rating = mean(rating)) %>%
  arrange(desc(n))

plot_data

# treemap plot

ggplot(plot_data, aes(area = n, fill = rating, label = country_of_bean_origin)) +
  geom_treemap(colour = "#452d28",show.legend = FALSE) +
  geom_treemap_text(fontface = "italic",
                    family="Fira Sans",
                    colour = "#452d28",
                    place = "centre",
                    grow = TRUE,
                    padding.x = grid::unit(4, "mm"),
                    padding.y = grid::unit(4, "mm")) +
  scale_fill_material(palette="deep-orange", name="",
                      limits=c(2.8, 3.6),
                      breaks=c(2.8, 3.6),
                      labels=c("<--\nLower rating", "-->\nHigher rating"),
                      guide = guide_colourbar(title.position = "top")) +
  labs(title = "Where do cocoa beans come from?",
       subtitle = "<span style = 'font-size:43pt; font-family:Fira Sans;'>\nCocoa beans from countries which are used by a larger number of manufacturers tend to result in</span><br><span style = 'color:#fd541f;'>higher rated</span>chocolate.The exception is blended beans which are commonly used but<span style = 'color:#90432c;'> score lower.\n \n</span>",caption = '\nOluwafemi Oyedele | Data: Flavors of Cocoa')+



       # subtitle = "\nCocoa beans from countries which are used by a larger number of manufacturers tend to result in higher rated\nchocolate. The exception is blended beans which are commonly used but score lower. \n",caption = 'Oluwafemi Oyedele | Data: Flavors of Cocoa') +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray97", colour="#452d28"),panel.background = element_rect(fill = "gray97", colour="#452d28"),plot.title = element_text(colour = 'blue', family="Fira Sans", face = "bold", size=130),plot.subtitle = element_markdown(colour = '#b29e97', family="Fira Sans", size=43,face='bold'),legend.text = element_text(colour = 'black', family="Fira Sans", size=26),
        legend.title = element_text(colour = '#b29e97', family="Fira Sans", size=15),    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),legend.position = 'bottom',plot.caption = element_text(colour = 'black', family="Fira Sans", size=40))





# Save the Plot -----------------------------------------------------------


ggsave("chocolate.png", plot = last_plot(), bg = "#ffffff", width = 15, height = 10, dpi = 200,path = here::here('2023/2023-01-05-Week-01'))


