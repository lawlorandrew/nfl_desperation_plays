# Scrape football-reference.com
require(tidyverse); require(rvest); require(pbapply)

get_all_plays <- function(){
  # 1994-2001
  page <- read_html("http://pfref.com/tiny/Q1KYE")
  # 2002-2018
  page2 <- read_html("http://pfref.com/tiny/Rx1g5")
  
  page %>%
    html_nodes("table#all_plays") %>%
    html_table() -> play_data1
  
  page2 %>%
    html_nodes("table#all_plays") %>%
    html_table() -> play_data2
  
  plays1 <- do.call("rbind", play_data1)
  plays2 <- do.call("rbind", play_data2)
  plays_df <- rbind(plays1, plays2)
  colnames(plays_df) <- c("date", "team", "opponent", "quarter", "time", "down", "togo", "location","score", "detail", "yds", "epb", "epa", "diff")
  plays_df %>%
    select(date,team,opponent,quarter,time,location,score,detail,yds, epa) -> plays_df
  return(plays_df)
}

get_hook_laterals <- function(all_plays){
  all_plays %>%
    filter(grepl("lateral", detail)) %>%
    mutate(td = grepl("touchdown", detail) & epa > 0) -> hook_laterals
  return(hook_laterals)
}

get_hail_marys <- function(all_plays) {
  all_plays %>%
    filter(!grepl("lateral", detail) & !grepl("spiked", detail)) %>%
    mutate(td = grepl("touchdown", detail) & epa > 0) -> hail_marys
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

#dont include spikes
#find out whats off about numbers
