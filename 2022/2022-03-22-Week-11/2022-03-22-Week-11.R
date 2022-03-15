
###########################################################################
###########################################################################
###                                                                     ###
###                      DATA IMPORT AND WRANGLING                      ###
###                                                                     ###
###########################################################################
###########################################################################

library(tidyverse)

bioc <- read_csv('2022/2022-03-22-Week-11/bioc.csv')




# Data Wrangling

bioc_subset <- bioc %>%
  filter(package%in%c('CARNIVAL','celda','flowDensity','flowWorkspace','IRanges','MSnbase','proBatch','STATegRa','TCGAbiolinks','xcms'))



#
ggplot(data = bioc_subset,aes(date,rmd,col=package))+geom_step()
