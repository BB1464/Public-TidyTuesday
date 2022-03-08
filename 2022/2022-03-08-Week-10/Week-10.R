
############################################################################
############################################################################
###                                                                      ###
###                      LOAD THE NECESSARY LIBRARY                      ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)
library(here)

tuesday <- read_csv(here::here('2022/2022-03-08-Week-10/tuesday.csv'))


############################################################################
############################################################################
###                                                                      ###
###                            DATA WRANGLING                            ###
###                                                                      ###
############################################################################
############################################################################


tuesday <-  tuesday %>%
  mutate(receiving_country_code=fct_recode(receiving_country_code,FRANCE='FR',
                                           'UNITED KINGDOM'='UK',
                                           ITALY='IT',GERMANY='DE',AUSTRIA='AT',BELGIUM='BE',BULGARIA='BG','CZECH REPUBLIC'='CZ',ESTONIA='EE',GREECE='EL',SPAIN='ES',CROATIA='HR',HUNGARY='HU',IRELAND='IE',LATVIA='LV',NETHERLAND='NL',NORWAY='NO',POLAND='PL',PORTUGAL='PT',ROMANIA='RO',SLOVAKIA='SK',TURKEY='TR',LIECHTENSTEIN='LI',LITHUANIA='LT',LUXEMBOURG='LU',SLOVENIA='SI',SWEDEN='SE',CYPRUS='CY',DENMARK='DK',FINLAND='FI',MALTA='MT',SERBIA='RS',FINLAND='FI',ISLAND='IS'))



tuesday %>%
  count(receiving_country_code) %>%
  ggplot(aes(x = reorder(receiving_country_code,-n),y = n,fill=receiving_country_code))+
  geom_col(show.legend = NULL)+labs(title = 'Erasmus Student by Country of Origin',y='Number of Student',x='')+
  coord_cartesian(expand = c(0,0))+
  theme_minimal()+theme(axis.text.x = element_text(family = 'serif',face = 'bold',hjust = 1,angle = 45),plot.title = element_text(family = 'serif',face = 'bold',colour = 'blue',hjust = 0.5),axis.title.y = element_text(family = 'serif',face = 'bold'))


# Export the image
ggsave(here::here('2022/2022-03-08-Week-10/plot.png'))
