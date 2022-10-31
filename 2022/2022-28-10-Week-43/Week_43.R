
## Load the Packages
library(tidytuesdayR)
library(tidyverse)


tuesdata <- tidytuesdayR::tt_load(2022, week = 43)


tuesdata$bakers%>%
  data.frame()->bakers

glimpse(tuesdata$bakers)

bakers%>%
  select(age,baker)%>%
  group_by(age)%>%
  count()%>%
  arrange((age))%>%
  data.frame()->bdf


cut(bdf$age, c(seq(0, 80, by = 10), Inf), include.lowest = TRUE)->age1
aggregate(n ~ age1, bdf, sum)->agedf

agedf %>%
  mutate(age1=case_when(agedf$age1=="(10,20]"~"10-20",
                        agedf$age1=="(20,30]"~"21-30",
                        agedf$age1=="(30,40]"~"31-40",
                        agedf$age1=="(40,50]"~"41-50",
                        agedf$age1=="(50,60]"~"51-60",
                        agedf$age1=="(60,70]"~"61-70",
                        agedf$age1=="(70,80]"~"71-80"))->agefdf

agefdf%>%
  ggplot(aes(x=age1,y=n))+
  geom_segment( aes(x=age1, xend=age1, y=0, yend=n) , size=1, color="grey", linetype="dotdash" ) +
  geom_point(color="coral", size=4)+
  ylim(0,50)+
  xlab("-------------------AGE GROUP---------------------")+
  ylab("--------NUMBER OF PARTICIPANTS---------")+
  theme(plot.background=element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour="white",face = "bold"),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
  labs(title="HOW OLD WERE THE GREAT BRITISH BAKE OFF PARTICIPANTS?",
       subtitle=str_wrap("An analysis of the age of participants of the first 10 seasons of The Great British Bake Off show showed that 60% of them (or 72 participants) were in the 21-40 age group, 25% of them (or 30 participants) were in the 40-60 age group and 8% (or 10 participants) were 60 years and above. The rest (8 participants) were less than 20 years of age.",100),
       caption = "Data via Tidy Tuesday| Analysis and design: @annapurani93")->lollipopchart

ggsave("lollipopchart.png",lollipopchart,width=10,height=8)

