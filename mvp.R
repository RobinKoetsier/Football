library(tidyverse)
readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                   sheet = "Player xG") %>%
  select(playername,Goals,Assists) %>%
  mutate(Assists = ifelse(playername == "Cody Gakpo", 13,Assists)) %>%
  group_by(playername) %>%
  summarise_all(sum) %>%
  #mutate(total = Goals+Assists) %>%
  ungroup() %>%
  top_n(15,Goals+Assists) %>%
  
  gather("Variable", "Value",-playername) %>%
  ggplot+
  geom_bar(aes(reorder(playername,Value,sum),Value,fill=Variable),stat="identity") +
  geom_text(aes(reorder(playername,Value,sum),Value,label = Value),
            position = position_stack(vjust = .5),family="Spartan-Medium",color="#120E41") +
  scale_fill_manual(values = c("#E172A8",
                               "#79CFDB"
                               ),
                    breaks = c("Goals","Assists")) +
  labs(fill = "",
       x="",
       y="",
       title = "Most Valuable Players Eredivisie",
       subtitle = "Number of Goals and Assists this season",
       caption = "@RobinWilhelmus")+
  coord_flip() +
  themes::theme_twitter() +
  theme(panel.grid.major = element_blank())

ggsave("plots/mvp.png",height=7,width=7,device=png)

readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                   sheet = "Player xG") %>%
  mutate(Goals = NPxG,
         Assists = xG_assisted) %>%
  select(playername,Goals,Assists) %>%
  group_by(playername) %>%
  summarise_all(sum) %>%
  #mutate(total = Goals+Assists) %>%
  ungroup() %>%
  top_n(15,Goals+Assists) %>%
  
  gather("Variable", "Value",-playername) %>%
  ggplot+
  geom_bar(aes(reorder(playername,Value,sum),Value,fill=Variable),stat="identity") +
  geom_text(aes(reorder(playername,Value,sum),Value,label = round(Value,1)),
            position = position_stack(vjust = .5),family="Spartan-Medium",color="#120E41") +
  scale_fill_manual(values = c("#E172A8",
                               "#79CFDB"
  ),
  breaks = c("Goals","Assists"),
  labels = c("NPxG", "xA")) +
  labs(fill = "",
       x="",
       y="",
       title = "Most Valuable Players Eredivisie",
       subtitle = "NPxG and xA this season",
       caption = "@RobinWilhelmus")+
  coord_flip() +
  themes::theme_twitter() +
  theme(panel.grid.major = element_blank())

ggsave("plots/mvpxA.png",height=7,width=7,device=png)



readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                   sheet = "Player xG") %>%
  select(playername,Goals,Assists) %>%
  group_by(playername) %>%
  summarise_all(sum) %>%
  #mutate(total = Goals+Assists) %>%
  ungroup() %>%
  top_n(15,Goals+Assists) %>%
  
  gather("Variable", "Value",-playername) %>%
  ggplot+
  geom_bar(aes(reorder(playername,Value,sum),Value,fill=Variable),stat="identity") +
  geom_text(aes(reorder(playername,Value,sum),Value,label = Value),
            position = position_stack(vjust = .5),family="Spartan-Medium",color="#120E41") +
  scale_fill_manual(values = c("#E172A8",
                               "#79CFDB"
  ),
  breaks = c("Goals","Assists")) +
  labs(fill = "",
       x="",
       y="",
       title = "Most Valuable Player Eredivisie",
       subtitle = "Number of Goals and Assists this season",
       caption = "@RobinWilhelmus")+
  coord_flip() +
  themes::theme_twitter() +
  theme(panel.grid.major = element_blank())
        