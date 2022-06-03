devtools::install_github("JaseZiv/worldfootballR")
worldfootballR::fotmob_get_season_stats(
  league_id = 47,
  season_name = "2020/2021",
  stat_name = "Expected goals",
  team_or_player = "team",
  stat_league_name = "FA Cup"
)
worldfootballR::fotmob_get_league_tables(league_id = 42)
fotmob_matches <- c(3609994, 3610132)
worldfootballR::fotmob_get_match_details(fotmob_matches)

players_urls <- data.frame()
urls <- tm_league_team_urls(start_year = 2020, league_url = "https://www.transfermarkt.com/eredivisie/startseite/wettbewerb/NL1")
for(url in urls){
  player <- tm_squad_stats(team_url = url) %>% select(player_url)
  players_urls <- rbind(players_urls,player)
}

players <- data.frame()
for(url in players_urls$player_url){
  player <- tm_player_bio(url)  %>% select(date_of_birth,citizenship)
  players <- rbind(players,player)
}

library(scales)
players <- players %>%
  
  mutate(month_of_birth = substr(date_of_birth,6,7),
         year_of_birth = substr(date_of_birth,1,4)) %>%
  mutate(birth = as.Date(date_of_birth,"%Y-%m-%d"),
         age = floor(eeptools::age_calc(birth,Sys.Date(),units="years")))
ggplot(players ) +
  geom_bar(aes(x=month_of_birth,y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent) +
  labs(title = "Percentage spelers Eredivisie per geboortemaand",
       y="") +
  facet_wrap(~age)+
  theme_bw()

table(players$month_of_birth)
