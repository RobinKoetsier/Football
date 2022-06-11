#devtools::install_github("JaseZiv/worldfootballR",force = TRUE)
library(cowplot)
library(showtext)
library(worldfootballR)
library(ggtext)
library(gt)
library(tidyverse)
library(janitor)
`%!in%` <- Negate(`%in%`)

league_matches <- fotmob_get_league_matches(
  country =     "NED",
  league_name = "Eredivisie"
)
dates <- gsub("-","",seq(as.Date("2021-08-01"), Sys.Date(), by="days"))
results <- fotmob_get_matches_by_date(date =dates)
filtered_results <- results %>%
  dplyr::select(primary_id, ccode, league_name = name, matches) %>%
  dplyr::filter(league_name == "Eredivisie", ccode == "NED")

# one way of getting data out of the results
unnested_results <- filtered_results %>% 
  tidyr::unnest_longer(matches)

match_ids <- unnested_results %>% 
  dplyr::pull(matches) %>% 
  dplyr::pull(id)

match_details <- fotmob_get_match_details(match_ids)
match_shots <- match_details %>% 
  dplyr::select(
    match_id,
    match_round,
    home_team_id,
    home_team,
    away_team_id,
    away_team,
    shots
  ) %>% 
  tidyr::unnest(shots) %>%
  mutate(team = ifelse(team_id == home_team_id,home_team,away_team))
dplyr::glimpse(match_shots)

match_shots$expected_goals_on_target[is.na(match_shots$expected_goals_on_target)] <- 0
match_shots$expected_goals[is.na(match_shots$expected_goals)] <- 0
glimpse(match_details)

xGOT_Keepers <- 
  match_shots %>%
  mutate(teamKeep =
           case_when(
             team == away_team ~ home_team,
             TRUE ~ away_team
           )) %>%
  mutate(Keeper = case_when(
    teamKeep == "NEC Nijmegen" ~ "Mattijs Branderhorst",
    teamKeep == "FC Groningen" ~ "Peter Leeuwenburgh",
    teamKeep == "PEC Zwolle" ~ "Konstantinos Lamprou",
    teamKeep == "FC Twente" & match_round != 11 & match_round != 20 ~ "Lars Unnerstall",
    teamKeep == "FC Twente" & match_round %in% c(11,20) ~ "Lars Unnerstall",
    
    teamKeep == "Fortuna Sittard" & match_round != 19 & match_round != 20 ~ "Yanick van Osch",
    teamKeep == "Fortuna Sittard" & match_round %in% c(19,20)~ "Michael Verrips",
    
    teamKeep == "AZ Alkmaar" & match_round %!in% c(1,3)  ~ "Jens Vindahl",
    teamKeep == "AZ Alkmaar" & match_round %in% c(1,3)  ~ "Hobie Verhulst",
    
    teamKeep == "Willem II" & match_round %!in% c(1,4,21)  ~ "Timon Wellenreuther",
    teamKeep == "Willem II" & match_round %in% c(21)  ~ "Jorn Brondeel",
    teamKeep == "Willem II" & match_round %in% c(4)  & min < 16  ~ "Jorn Brondeel",
    teamKeep == "Willem II" & match_round %in% c(4) & min > 15  ~ "Connor van den Berg",
    teamKeep == "Willem II" & match_round %in% c(1)  ~ "Robbin Ruiter",
    
    teamKeep == "RKC Waalwijk" & match_round %!in% c(1,2,25) ~ "Etienne Vaessen",
    teamKeep == "RKC Waalwijk" & match_round %in% c(1,2) ~ "Joel Pereira",
    teamKeep == "RKC Waalwijk" & match_round %in% c(25) ~ "Issam El Maach",
    
    teamKeep == "Sparta Rotterdam" & match_round %!in% c(14,15,16,19,20) ~ "Maduka Okoye",
    teamKeep == "Sparta Rotterdam" & away_team == "Ajax" & min < 25 ~ "Maduka Okoye",
    teamKeep == "Sparta Rotterdam" & match_round %in% c(16,19,20)~ "Tim Coremans",
    teamKeep == "Sparta Rotterdam" & match_round %in% c(15)~ "Benjamin van Leer",
    teamKeep == "Sparta Rotterdam" & away_team == "Ajax" & min > 24 ~ "Benjamin van Leer",
    
    teamKeep == "Cambuur" & match_round %!in% c(26:30) ~ "Sonny Stevens",
    teamKeep == "Cambuur" & match_round %in% c(25)  & min < 83 ~ "Sonny Stevens",
    teamKeep == "Cambuur" & match_round %in% c(26:30) ~ "Pieter Bos",
    teamKeep == "Cambuur" & match_round %in% c(25)  & min > 82~ "Pieter Bos",
    
    teamKeep == "PSV Eindhoven" & match_round %!in% c(1,21,29:34) ~ "Joël Drommel",
    teamKeep == "PSV Eindhoven" & match_round %in% c(1,21,29:34) ~ "Yvon Mvogo",
    
    teamKeep == "Feyenoord" & match_round %!in% c(14,4,15,26:34) ~ "Justin Bijlow",
    
    teamKeep == "Heracles" & match_round %!in% c(1,2,25:34) ~ "Janis Blaswich",
    
    teamKeep == "Vitesse" & match_round %!in% c(8,15:24,27,28)~ "Markus Schubert",
    
    
    teamKeep == "Ajax" & match_round %!in% c(1:3,24:34)~ "Remko Pasveer",
    teamKeep == "Ajax" & match_round %in% c(1:3,30:33)~ "Maarten Stekelenburg",
    teamKeep == "Ajax" & match_round %in% c(24:29)~ "André Onana",
    teamKeep == "Ajax" & match_round %in% c(34)~ "Jay Gorter",
    
    
    teamKeep == "Go Ahead Eagles" & match_round > 19 ~ "Andries Noppert",
    teamKeep == "Go Ahead Eagles" ~ "Warner Hahn",
    
    teamKeep == "SC Heerenveen" & match_round %in% c(7) & period == "FirstHalf" ~ "Erwin Mulder",
    teamKeep == "SC Heerenveen" & match_round %!in% c(8:22) ~ "Erwin Mulder",
    teamKeep == "SC Heerenveen" & match_round %in% c(8:22) ~ "Xavier Mous",
    
    
   
    teamKeep == "FC Utrecht" &  match_round %!in% c(16,19:34) ~ "Maarten Paes",
    teamKeep == "FC Utrecht" &  match_round %in% c(16,33:34) ~ "Eric Oelschlägel",
    teamKeep == "FC Utrecht" &  match_round %!in% c(32) & period == "SecondHalf" ~ "Eric Oelschlägel",
    
    teamKeep == "Vitesse" & match_round %in% c(8,15:25,27:29)  ~ "Jeroen Houwen",
    
    teamKeep == "FC Utrecht" &  match_round %in% c(19:32) ~ "Fabian de Keijzer",
    
    teamKeep == "Heracles" & match_round %in% c(1,2,25:34) ~ "Koen Bucker",
    
    teamKeep == "Feyenoord" & match_round %in% c(14,4,15,26:34) ~ "Ofir Marciano",
    
    
    TRUE ~ "ERROR"
  )) %>%
  mutate(Goal = ifelse(event_type == "Goal",1,0)) %>%
  filter(is_own_goal != TRUE)

xGOT_Keepers %>%
  group_by(Keeper,match_round) %>%
  mutate(n =n()) %>%
  summarise(xgot = sum(expected_goals_on_target),
            goals = sum(Goal)) %>%
  group_by(Keeper) %>%
  mutate( n = n()) %>%
  filter(n > 9) %>%
  summarise(xgot = sum(xgot),
           goals = sum(goals),
           diff = xgot-goals) %>%
  arrange(-diff) %>%
  ggplot() +
  geom_bar(aes(x=diff,y=reorder(Keeper,diff)),stat="identity",fill="#D75998") +
  scale_x_continuous(breaks=c(-10,-7.5,-5,-2.5,0,2.5,5))+
  labs(title = "Goals prevented",
       subtitle = "10 games minimum, own goals excluded",
       y= "",
       x= "Goals prevented\n (xGOT-Goals conceded)",
       caption = "Data from OPTA via FotMob\n@RobinWilhelmus") +
  themes::theme_twitter()

ggsave("plots/xGOT.png",device=png,height=7.5,width=7.5)



xGOT_Keepers %>%
  group_by(team) %>%
  summarise(xGOT = sum(expected_goals_on_target),
            xG = sum(expected_goals)) %>%
  ggplot()+
  ggtext::geom_richtext(
    aes(y = xGOT, x = xG, label = glue::glue("<img src='~/Documents/ScraperWhoScored/TDLXG/Clubs/{team}.png' width='30'/>")),
    size = 1,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  coord_cartesian(xlim=c(30,100),
                  ylim=c(30,100))
  
