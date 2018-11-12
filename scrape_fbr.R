# Scrape football-reference.com
require(tidyverse); require(rvest); require(pbapply)

get_all_plays <- function(){
  url <- paste0("https://www.pro-football-reference.com/play-index/play_finder.cgi?request=1&match=all&year_min=1994&year_max=2018&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&quarter%5B%5D=4&quarter%5B%5D=5&tr_gtlt=lt&minutes=0&seconds=8&down%5B%5D=0&down%5B%5D=1&down%5B%5D=2&down%5B%5D=3&down%5B%5D=4&field_pos_min_field=team&field_pos_min=1&field_pos_max_field=opp&field_pos_max=30&end_field_pos_min_field=team&end_field_pos_max_field=team&type%5B%5D=PASS&include_kneels=N&no_play=N&turnover_type%5B%5D=interception&turnover_type%5B%5D=fumble&score_type%5B%5D=touchdown&score_type%5B%5D=field_goal&score_type%5B%5D=safety&margin_min=-8&margin_max=0&rush_direction%5B%5D=LE&rush_direction%5B%5D=LT&rush_direction%5B%5D=LG&rush_direction%5B%5D=M&rush_direction%5B%5D=RG&rush_direction%5B%5D=RT&rush_direction%5B%5D=RE&pass_location%5B%5D=SL&pass_location%5B%5D=SM&pass_location%5B%5D=SR&pass_location%5B%5D=DL&pass_location%5B%5D=DM&pass_location%5B%5D=DR&order_by=yards")
  
  page <- read_html(url)
  
  page %>%
    html_nodes("table#all_plays") %>%
    html_table() -> play_data
  
  plays_df <- do.call("rbind", play_data)
  colnames(plays_df) <- c("date", "team", "opponent", "quarter", "time", "down", "togo", "location","score", "detail", "yds", "epb", "epa", "diff")
  plays_df %>%
    select(date,team,opponent,quarter,time,location,score,detail,yds) -> plays_df
  return(plays_df)
}

get_hook_laterals <- function(all_plays){
  all_plays %>%
    filter(grepl("lateral", detail)) %>%
    mutate(td = grepl("touchdown", detail) & yds > 0) -> hook_laterals
  return(hook_laterals)
}

get_hail_marys <- function(all_plays) {
  all_plays %>%
    filter(!grepl("lateral", detail)) %>%
    mutate(td = grepl("touchdown", detail) & yds > 0) -> hail_marys
  return(hail_marys)
}

all_play_data <- get_all_plays()
hook_lateral_data <- get_hook_laterals(all_play_data)
save(hook_lateral_data, file = "data/nfl_hook_laterals.Rdata")
hail_mary_data <- get_hail_marys(all_play_data)
save(hail_mary_data, file = "data/nfl_hail_marys.Rdata")
hook_lateral_tds <- sum(hook_lateral_data$td == TRUE)
hook_lateral_plays_count <- count(hook_lateral_data)
hail_mary_tds <- sum(hail_mary_data$td == TRUE)
hail_mary_plays_count <- count(hail_mary_data)

