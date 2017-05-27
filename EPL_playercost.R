if (!require("rvest")){
  install.packages("rvest")
}
library(rvest)
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)
if (!require("stringr")) {
  install.packages("stringr")
}
library(stringr)


###web crawling about cost of player
playerCost = function(teamName, teamNum, season) {
  
  ###url
  url = paste("http://www.transfermarkt.co.uk/", teamName, "/kader/verein/", teamNum, "/saison_id/", season, sep = "")
  
  #data of player name
  playerName = url %>% read_html %>% html_nodes(".spielprofil_tooltip") %>% html_text()
  playerName = playerName[seq(1, length(playerName), 2)]
  
  #data of cost
  costData = url %>% read_html %>% html_nodes(".rechts") %>% html_text()
  costData = costData[2:(length(playerName)+1)]
  costData = gsub("\\s|\\t|\\n", "", costData)
  costData = str_sub(costData, 2, -1)
  for(i in 1:length(costData)) {
    if(costData[i] == "")
      costData[i] = NA
  }
  costData = gsub("k", "000", costData)
  costData = gsub("\\.", "", costData)
  costData = gsub("m", "0000", costData)
  costData = costData %>% as.numeric() %>% as.data.frame()
  
  season1 = str_sub(season, 3, -1) %>% as.integer()
  season2 = season1 + 1
  season = paste(season1, season2, sep = "/")
  
  playerCostData = cbind(playerName, costData, rep(season, length(playerName)), stringsAsFactors = F)
  colnames(playerCostData) = c("playerName", "cost", "season")
  
  return(playerCostData)
}


#variable
Name = c("leicester-city", "arsenal-fc", "tottenham-hotspur", "manchester-city", 
         "manchester-united", "southampton-fc", "west-ham-united", "liverpool-fc", "stoke-city", 
         "chelsea-fc", "everton-fc", "swansea-city", "watford-fc", "west-bromwich-albion", "crystal-palace",
         "afc-bournemouth", "sunderland-afc", "burnley-fc", "middlesbrough-fc", "hull-city")
Num = c(1003, 11, 148, 281, 985, 180, 379, 31, 512, 631, 29, 2288, 1010, 984, 873, 989, 289, 1132, 641, 3008)
season = c(2009:2016)

#dataframe
playerCost_team_temp = NULL
playerCost_team = NULL
playerCostData = NULL

#making dataframe playerCostData
for(i in 1:20){
  teamName = Name[i]
  teamNum = Num[i]
  for(i in season) {
    playerCost_team_temp_temp = playerCost(teamName, teamNum, i)
    playerCost_team_temp = rbind(playerCost_team_temp, playerCost_team_temp_temp)
    print(playerCost_team_temp)
  }
  playerCost_team = cbind("teamName" = rep(teamName, nrow(playerCost_team_temp)), playerCost_team_temp, stringsAsFactors = F)
  playerCostData = rbind(playerCostData, playerCost_team)
  print(playerCostData)
  playerCost_team_temp = NULL
}

