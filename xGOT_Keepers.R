temp <- xGOT_Keepers %>%
  filter(Keeper %in% df$Keeper) %>%
  filter(expected_goals_on_target > 0) %>%
  #filter(event_type == "Goal") %>%
  filter(situation != "Penalty") %>%
  ggplot() +
  
  geom_segment(aes(x=30.184,xend=37.78,y=2.554,yend=2.554) ,colour="white")+
  geom_segment(aes(x=30.292,xend=37.67200,y=2.458,yend=2.458),colour="white" )+
  geom_segment(aes(x=30.292,xend=30.292,y=2.458,yend=0) ,colour="white")+
  geom_segment(aes(x=30.184,xend=30.184,y=2.554,yend=0),colour="white" ) +
  geom_segment(aes(x=37.78,xend=37.78,y=2.554,yend=0),colour="white" )+
  geom_segment(aes(x=37.67200,xend=37.67200,y=2.458,yend=0) ,colour="white")+
  geom_segment(aes(x=29.5,xend=38.5,y=0,yend=0) ,colour="white")+
  geom_point(aes(goal_crossed_y ,goal_crossed_z,size=expected_goals_on_target,alpha=expected_goals_on_target),
             shape=21,fill="#D75998") +
  facet_wrap(~Keeper,ncol=4) + 
  labs(size="xGOT",
       alpha = "xGOT",
       title = "Where on goal do keepers concede shots?",
       subtitle = "Shots on target, excluding penalties. 10 games minimum",
       caption = "Data: OPTA via FotMob\n@RobinWilhelmus") +
  themes::theme_twitter_pitch() +
  theme(legend.box = "horizontal",
        legend.position="bottom",
        legend.title.align=0.5) +
  scale_size(guide = guide_legend(title.position = "top")) 

ggsave("plots/shotsKeepers.png",width=8.5,height=7.5,device = png)

ggplot()+
  geom_point(aes(x,y,size=expected_goals))
footballFunctions::create_shotmap_twitter("Lennart Thy",TRUE)
ggsave("plots/shotsThy.png", height=5,width=8,device=png)
