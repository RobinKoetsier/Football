library(tidyverse)

df <- readxl::read_excel("Export_TDL_NED_2021.xlsx", 
                         sheet = "Shots") %>%
  select(-date)
df1 <- readxl::read_excel("Export_TDL_NED_2021.xlsx", 
                          sheet = "Wedstrijden")  %>%
  mutate(match= glue::glue("{HomeName} - {AwayName}")) %>%
  select(date,match)


with_date <-  left_join(df1,df, by = "match") 
with_date$Goal[is.na(with_date$Goal)] <- 0
temp <- with_date %>%
  mutate(date = as.Date(date,"%d-%m-%Y")) %>%
  arrange(date,match,name) %>%
  group_by(date,name) %>%
  summarise(sum_xG = sum(xG)) %>%
  ungroup() %>%
  arrange(name,date) %>%
  group_by(name) %>%
  mutate(roll_sum = RcppRoll::roll_mean(sum_xG, 5, align = "right", fill = NA)) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_xG, 5, mean, partial = TRUE) ) %>%
  mutate(rollsum=cumsum(sum_xG)) %>%
  #mutate(match = 1:34) %>%
  
  left_join(
    
    with_date %>%
      mutate(match=gsub(" - ","",match)) %>%
      rowwise() %>%
      mutate(name=gsub(name,"",match)) %>%
      #select(opp1) %>%
      
      
      filter(Type_of_play != "Penalty") %>%
      mutate(date = as.Date(date,"%d-%m-%Y")) %>%
      arrange(date,match,name) %>%
      
      
      # select(date,match,xG_sum)
      group_by(date,name) %>%
      
      summarise(sum_xGA = sum(xG)) %>%
      ungroup() %>%
      arrange(name,date) %>%
      group_by(name) %>%
      mutate(roll_sumA = RcppRoll::roll_mean(sum_xGA, 5, align = "right", fill = NA,partial = FALSE))  %>%
      mutate(rollapply_sumA =zoo::rollapplyr(sum_xGA, 5, mean, partial = TRUE)))# %>%
      mutate(match = 1:34))




df2021 <- temp %>%
  ungroup() %>%
  mutate(sum_xGA = replace_na(sum_xGA,0)) %>%
  group_by(name) %>%
  summarise(xG= sum(sum_xG)/34,
            xGA = sum(sum_xGA)/34)


df <- readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                         sheet = "Shots") %>%
  select(-date)
df1 <- readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                          sheet = "Wedstrijden")  %>%
  mutate(match= glue::glue("{HomeName} - {AwayName}")) %>%
  select(date,match)


with_date <-  left_join(df1,df, by = "match") 
with_date$Goal[is.na(with_date$Goal)] <- 0
temp <- with_date %>%
  mutate(date = as.Date(date,"%d-%m-%Y")) %>%
  arrange(date,match,name) %>%
  group_by(date,name) %>%
  summarise(sum_xG = sum(xG)) %>%
  ungroup() %>%
  arrange(name,date) %>%
  group_by(name) %>%
  mutate(roll_sum = RcppRoll::roll_mean(sum_xG, 5, align = "right", fill = NA)) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_xG, 5, mean, partial = TRUE) ) %>%
  mutate(rollsum=cumsum(sum_xG)) %>%
  #mutate(match = 1:34) %>%
  
  left_join(
    
    with_date %>%
      mutate(match=gsub(" - ","",match)) %>%
      rowwise() %>%
      mutate(name=gsub(name,"",match)) %>%
      #select(opp1) %>%
      
      
      filter(Type_of_play != "Penalty") %>%
      mutate(date = as.Date(date,"%d-%m-%Y")) %>%
      arrange(date,match,name) %>%
      
      
      # select(date,match,xG_sum)
      group_by(date,name) %>%
      
      summarise(sum_xGA = sum(xG)) %>%
      ungroup() %>%
      arrange(name,date) %>%
      group_by(name) %>%
      mutate(roll_sumA = RcppRoll::roll_mean(sum_xGA, 5, align = "right", fill = NA,partial = FALSE))  %>%
      mutate(rollapply_sumA =zoo::rollapplyr(sum_xGA, 5, mean, partial = TRUE)))# %>%

df2122 <- temp %>%
  ungroup() %>%
  
  group_by(name) %>%
  summarise(xG= sum(sum_xG)/34,
            xGA = sum(sum_xGA)/34)

df <- left_join(df2021,df2122,by="name") %>% drop_na() %>%
  `colnames<-`(c("team", "xG21", "xGA21","xG22","xGA22")) %>%
  mutate(label = c("AJA","AZ","GRO","UTR","FEY","FOR","HER","PEC","PSV","RKC","HEE","SPA","TWE","VIT","WIL")) %>%
  rbind(c("21/22",2.75,1.8,2.75,1.6," ")) %>%
  mutate(xG21 = as.numeric(xG21),xGA21 = as.numeric(xGA21),xG22 = as.numeric(xG22),xGA22 = as.numeric(xGA22),
         diff = ifelse(xG21-xGA21 > xG22-xGA22,0,1))

ggplot(df) +
 # ggforce::geom_link(aes(x=xG21,y=xGA21,xend=xG22,yend=xGA22,alpha = stat(index)),colour="#79CFDB",size=1.5) +
  ggforce::geom_link(aes(x=xG21,y=xGA21,xend=xG22,yend=xGA22,alpha = stat(index),colour=as.factor(diff)),size=1.5) +
  scale_color_manual(values= c(
                               "#E172A8",
                               "#79CFDB")) +
  #ggforce::geom_link(aes(x=2.75,y=2.75,xend=2.75,yend=1.25,alpha = stat(index)),colour="#79CFDB",size=1.5) +
  #geom_point(aes(xG21,xGA21),shape=21,fill="red")+
  geom_point(aes(xG22,xGA22),shape=21,fill="#120E41",size=5.2,colour="white")+
  geom_text(aes(xG22,xGA22,label=label),colour="white",size=2) +
  geom_text(aes(x= 2.65,y=1.6),label = "21/22",colour="white",size=3,family="Roboto") +
  geom_text(aes(x= 2.65,y=1.8),label = "20/21",colour="white",size=3,family="Roboto") +
  coord_flip() +
  scale_y_reverse()+
  
  labs(title = "Average NPxG for and against per game",
       subtitle = "Difference in performance between the last two seasons",
       y= "NPxG against",
       x = "NPxG",
       caption = "@RobinWilhelmus") +
  themes::theme_twitter() +
  theme(legend.position = "none")

ggsave("plots/improvement.png",height=6,width=6,device=png)

library(ggh4x)

p <- temp %>%
  filter(name == TEAM) %>%
  ggplot() +
  geom_line(aes(match,rollapply_sumA),colour="#E172A8") +
  geom_line(aes(match,rollapply_sum),colour="#79CFDB") +
  geom_vline(xintercept = 5,linetype="dashed",alpha=.4,colour="white") +
  stat_difference(aes(x=match,ymin = rollapply_sum, ymax = rollapply_sumA), alpha = 0.3)+
  scale_fill_manual(
    values = c(
      colorspace::lighten("#E172A8"), 
      colorspace::lighten("#79CFDB"), 
      "grey60"
    ),
    labels = c("Negative xG Difference", "Positive xG Difference", "same")
  )+
  labs(title = "Performance Eredivisie teams this season",
       subtitle = "5 game rolling average NPxG <i style='color: #79CFDB'>for</i> and <i style= 'color: #E172A8'> against </i>",
       caption = "@RobinWilhelmus",
       x= "Matchday",
       y = "NPxG",
       fill = "") +
  #facet_wrap(~name) +
  themes::theme_twitter() +
  theme(panel.grid.major = element_line(size=0.01))

p

p1 <- shift_legend(p)
p2 <- grid::grid.draw(p1)
#p1 <- grid.draw(shift_legend(p))
ggsave("xGroll.png", width = 7.25, height = 7, res = 500,device = png,p1)
