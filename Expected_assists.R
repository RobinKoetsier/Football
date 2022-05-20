library(tidyverse)
readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                   sheet = "Player xG") %>%
  group_by(playername) %>%
  summarise(Minutes_played = sum(Minutes_played),
            Assists = sum(Assists),
            xG_assisted = sum(xG_assisted),
            `xA.90` = xG_assisted/Minutes_played*90,
            Shots_assisted = sum(Shots_assisted)) %>%
  ungroup()%>%
  filter(Minutes_played > 1350) %>%
  top_n(15,`xA.90`) %>%
  ggplot() +
  geom_bar(aes(`xA.90`,reorder(playername,`xA.90`)),stat="identity",fill="#D75998") +
  geom_text(aes(x=`xA.90`,y=reorder(playername,`xA.90`),label=round(`xA.90`,2)),
            family="Spartan-Medium",colour="black",hjust=1,size=4)+
  labs(title = "Expected Assists per 90 minutes",
       subtitle = "1350 minutes minimum",
       caption= "@RobinWilhelmus",
       x="xA/90",
       y="")+
  theme_twitter() +
  theme(panel.grid.major.y = element_blank())


ggsave('plots/xA.png',height=7,width=7,device=png)
