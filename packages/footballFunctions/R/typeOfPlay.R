#' A mutate function
#' Adds the type of play to a data frame with events
#' @param OneGame df with data

#' @keywords mutate data
#' @export
#' @examples
#' typeOfPlay(allGames)
#' 


typeOfPlay <- function(OneGame){
  
  CornerTaken <- OneGame %>% filter_all(any_vars(. %in% "CornerTaken")) %>% mutate(Type_of_play = "CornerTaken") %>% select(PlayerId,id,Type_of_play)
  FreekickTaken <- OneGame %>% filter_all(any_vars(. %in% "FreekickTaken")) %>% mutate(Type_of_play = "FreekickTaken") %>% select(PlayerId,id,Type_of_play)
  RegularPlay <- OneGame %>% filter_all(any_vars(. %in% "RegularPlay")) %>% mutate(Type_of_play = "RegularPlay") %>% select(PlayerId,id,Type_of_play)
  Throwin <- OneGame %>% filter_all(any_vars(. %in% "ThrowIn")) %>% mutate(Type_of_play = "ThrowIn") %>% select(PlayerId,id,Type_of_play)
  Penalty <- OneGame %>% filter_all(any_vars(. %in% "Penalty")) %>% mutate(Type_of_play = "Penalty") %>% select(PlayerId,id,Type_of_play)
  
  
  Typ_of_play <- rbind(CornerTaken,FreekickTaken,RegularPlay,Throwin,Penalty)
  
  df <- left_join(OneGame,Typ_of_play,by=c("PlayerId", "id")) %>%
    mutate(Type_of_play=replace_na(Type_of_play, "RegularPlay"))
  return(df)
}
