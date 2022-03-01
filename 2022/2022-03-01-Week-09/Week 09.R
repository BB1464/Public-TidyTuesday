
############################################################################
############################################################################
###                                                                      ###
###                      LOAD THE NECESSARY LIBRARY                      ###
###                                                                      ###
############################################################################
############################################################################


library(showtext)
library(tidytuesdayR)
library(tidyverse)

# Import the data set

tuesdata <- tidytuesdayR::tt_load('2022-03-01')
stations <- tuesdata$stations

stations <- tuesday$stations



##################################################################
##                        Data Wrangling                        ##
##################################################################


us_states <- tibble(
  state_name = state.name,
  state_abb = state.abb)

stations1 <- stations %>%
  filter(FUEL_TYPE_CODE == "ELEC",
         STATUS_CODE == "E") %>%
  count(STATE) %>%
  rename(state_abb = STATE, total = n) %>%
  left_join(us_states) %>%
  mutate(state_name = case_when(state_abb == "DC" ~ "District of Columbia",
                                TRUE ~ state_name)) %>%
  mutate(state_name = tolower(state_name)) %>%
  select(state_name, total) %>%
  mutate(bin = case_when(total <= 1000 ~ "0-1000",
                         total > 1000 & total <= 2000 ~ "1000-2000",
                         total > 2000 & total <= 3000 ~ "2000-3000",
                         total > 3000 ~ "3000+"))

# Visualize the Map

us_elec_stations <- map_data("state") %>%
  left_join(stations1, by = c("region" = "state_name"))

us_map <- ggplot(data = us_elec_stations,
                 mapping = aes(x = long, y = lat, group = group,
                               fill = bin)) +
  geom_polygon(colour = "grey30") +
  ggtitle("Electric car charging stations in the U.S.") +
  scale_fill_manual(values = c("#f3e9d2", "#88d498", "#1a936f", "#114b5f")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#c2c8c5"),
        plot.title = element_text(family = "space", size = 45, hjust = 0.5,
                                  margin = margin(t = 20)),
        legend.title = element_blank(),
        legend.text = element_text(family = "space", size = 25),
        legend.margin = margin(r = 20))

# Save plot ----

ggsave("2022_03_01_fuel.png", us_map, dpi = 320, width = 12, height = 6,path = '2022/2022-03-01-Week-09')


# Data Wrangling 2

stations <-
  stations %>% mutate(
    GROUPS_WITH_ACCESS_CODE = fct_collapse(
      GROUPS_WITH_ACCESS_CODE,
      "private access" = c(
        "Private",
        "Private - Call ahead",
        "Private - Card key at all times",
        "Private - Credit card after hours",
        "Private - Credit card at all times",
        "Private - Fleet customers only",
        "Private - Government only",
        "TEMPORARILY UNAVAILABLE (Private - Government only)",
        "TEMPORARILY UNAVAILABLE (Private)"
      ),
      "public access" = c(
        "Public",
        "Public - Call ahead",
        "Public - Card key after hours",
        "Public - Card key at all times",
        "Public - Credit card after hours",
        "Public - Credit card at all times",
        "Public - Government only",
        "Public - Limited hours",
        "TEMPORARILY UNAVAILABLE (Public - Call ahead)",
        "TEMPORARILY UNAVAILABLE (Public - Card key at all times)",
        "TEMPORARILY UNAVAILABLE (Public - Credit card after hours)",
        "TEMPORARILY UNAVAILABLE (Public - Credit card at all times)",
        "TEMPORARILY UNAVAILABLE (Public)"
      ),
      planned = c(
        "PLANNED - not yet accessible (Private - Fleet customers only)",
        "PLANNED - not yet accessible (Private - Government only)",
        "PLANNED - not yet accessible (Private)",
        "PLANNED - not yet accessible (Public - Call ahead)",
        "PLANNED - not yet accessible (Public - Card key at all times)"
        ,
        "PLANNED - not yet accessible (Public - Credit card at all times)",
        "PLANNED - not yet accessible (Public)"
      )
    )
  )



stations %>% group_by(FUEL_TYPE_CODE, GROUPS_WITH_ACCESS_CODE) %>% count(GROUPS_WITH_ACCESS_CODE) %>% ggplot(aes(FUEL_TYPE_CODE, n, fill =
                                                                                                                   GROUPS_WITH_ACCESS_CODE)) + geom_col(position = position_dodge(.9)) + geom_text(aes(label =
                                                                                                                                                                                                         n), vjust = -0.5, position = position_dodge(.9)) + scale_fill_brewer(palette = 'Set1') +
  labs(
    title = 'Number of alternative vehicle fuel stations in the lower 48 states',
    y = 'Number of Access Code',
    x = 'Fuel Type',
    caption = 'Source: U.S. Energy Information Administration Based on US Department of Energy'
  ) + theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(0.1, 0.7))

ggsave('2022-03-01-bar.png',width = 12,height = 9,dpi=300,path = '2022/2022-03-01-Week-09')



