p <- match_shots %>%
  filter(goal_crossed_z < 2.55) %>%
  filter(expected_goals_on_target > 0) %>%
  filter(event_type == "Goal") %>%
  #filter(event_type == "Post") %>%
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
facet_wrap(~team,ncol=4) + 
  labs(size="xGOT",
       alpha = "xGOT") +
  themes::theme_twitter_pitch() +
  theme(legend.box = "horizontal",
        legend.position="bottom",
        legend.title.align=0.5) +
  scale_size(guide = guide_legend(title.position = "top")) 

p1 <- shift_legend(p)
p2 <- grid::grid.draw(p1)


geom_segment(aes(x=2,xend=2,y=0,yend=0.66534392)) +
  geom_segment(aes(x=2-0.028571429,xend=2-0.028571429,y=0,yend=0.66534392 - 0.028571429)) +
  geom_segment(aes(x=0,xend=2,y=0.66534392,yend=0.66534392)) +
  geom_segment(aes(x=0.028571429,xend=2-0.028571429,y=0.66534392 - 0.028571429,yend=0.66534392 - 0.028571429)) +
 
  facet_wrap(~team) + 
  themes::theme_twitter_pitch() 


df <- match_shots %>%
  filter(event_type == "Post") %>%
  
  select(goal_crossed_y,
         goal_crossed_z)
max(df$goal_crossed_y)
min(df$goal_crossed_y)
max(df$goal_crossed_z)
