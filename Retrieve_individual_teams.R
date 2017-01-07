#This script is to retrieve individual teams in any list

top_1k_data = read.csv(file = "top_1k_data.csv")
df = data.frame(id = NA, selection = NA)
url = "https://fantasy.premierleague.com/drf/entry/"
p = fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")  #player info url
list = p$elements    #navigating to the player list and storing
el = data.frame(list$id, list$web_name)   #choosing the required columns from list

for(id in top_1k_data$id)	{
  print(paste("Retrieving for id:",i))    
  team_url = paste0(url, i, "/event/20/picks")  #url for team info. Unique
  print(paste("Parsing and Joining for id:",i))
  j = fromJSON(team_url)  #parsing the unique team
  j = j$picks #navigating to the user picks
  k = inner_join(j, el, by = c("element" = "list.id"))  
	selection = paste(k$list.web_name, collapse = ", ")
	j = data.frame(id, selection, stringsAsFactors = FALSE)
	print(paste("Binding for id:",i)) #binding to our data frame
	df = bind_rows(df, j)
}
