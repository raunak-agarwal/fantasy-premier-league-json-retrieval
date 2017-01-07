library(tidyverse)
library(jsonlite)
library(RCurl)

base_url = "https://fantasy.premierleague.com/drf/"
temp_url = "https://fantasy.premierleague.com/drf/leagues-classic-standings/313?phase=1&le-page=1&ls-page="

#creating a dataframe with 1 row consisting of NA values 
league_1000 = data.frame(id = NA,
                         entry_name = NA,
                         event_total = NA,
                         player_name = NA,
                         movement = NA,
                         own_entry = NA,
                         rank = NA,
                         last_rank = NA,
                         total = NA,
                         entry = NA)
league_1000 = league_1000[!1] #removing the first row. The dataframe is now completely empty 

# Retrieving the top 1K player data because the url only displays 50 players' data
for(i in 1:20){
  url = paste0(temp_url, i)
  league_50 = fromJSON(url)
  df = league_50$standings$results
  league_1000 = rbind(league_1000, df)
}

#removing useless columns
league_1000 = select(league_1000,-own_entry,-start_event,-stop_event,-league)
View(league_1000)

#creating new columns
league_1000 = mutate(league_1000,player_region_name = NA,value = NA,favourite_team = NA)

player_data_1000 = data.frame(id = NA,
                              player_first_name = NA,
                              player_last_name = NA,
                              player_region_id = NA,
                              player_region_name = NA,
                              player_region_short_iso = NA, 
                              summary_overall_points = NA,
                              summary_overall_rank = NA, 
                              summary_event_points = NA, 
                              summary_event_rank = NA, 
                              joined_seconds = NA, 
                              current_event = NA, 
                              total_transfers = NA, 
                              total_loans = NA, 
                              total_loans_active = NA, 
                              transfers_or_loans = NA, 
                              deleted = NA, 
                              email = NA, 
                              joined_time = NA, 
                              name = NA, 
                              bank = NA, 
                              value = NA, 
                              kit = NA, 
                              event_transfers = NA, 
                              event_transfers_cost = NA, 
                              extra_free_transfers = NA, 
                              strategy = NA, 
                              favourite_team = NA, 
                              started_event = NA, 
                              player = NA,stringsAsFactors = FALSE
)


curl = getCurlHandle()
curlSetOpt(cookiejar="cookies.txt",  useragent = agent, followlocation = TRUE, curl=curl)
agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.95 Safari/537.36"


for(i in league_1000$entry){
  url = "https://fantasy.premierleague.com/drf/entry/"
  player_url = paste0(url,i)
  #myPage <- getURL(player_url, curl = curl)
  print(paste("Retrieved for id number", i))
  df2 = fromJSON(player_url, flatten = TRUE)
  print(paste("fromJSON done for id number", i))
  df2 = df2$entry
  print(paste("entry created for id number", i))
  if(is.null(df2$kit)){df2$kit = NA}
  if(is.null(df2$favourite_team)){df2$favourite_team = NA}
  if(is.null(df2$strategy)){df2$strategy = NA}
    

  player_data_1000 = rbind(player_data_1000,unlist(df2))
  print(paste("entry done for id number", i))
}

print(paste("The Mean points scored by top 1000 players is", mean(league_1000$total)))
ggplot(league_1000,aes((abs(league_1000$rank - league_1000$last_rank)),league_1000$total)) + geom_point()

your.aov = aov(test$summary_overall_points, test$player_region_name)
summary(your.aov)
