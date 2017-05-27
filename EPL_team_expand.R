library(XML)
library(rvest)
library(httr)
library(jsonlite)
library(RJSONIO)
library(reshape2)
library(dplyr)
library(stringr)

team_data <- function(team_number, myteam_name, season_number){
  team_info <- data.frame(stringsAsFactors=FALSE)
  
  url = paste("http://www.transfermarkt.co.uk/chelsea-fc/spielplandatum/verein/", team_number, "/plus/1?saison_id=", season_number, "&wettbewerb_id=GB1&day=&heim_gast=&punkte=&datum_von=-&datum_bis=-", sep="")
  squad_url = paste("http://www.transfermarkt.co.uk/liverpool-fc/kader/verein/", team_number, "/plus/0/galerie/0?saison_id=", season_number, sep="")
  
  response=GET(url)
  squad_response=GET(squad_url)#######
  htxt <- html(response)
  squad_htxt <- html(squad_response)#######
  
  response_table <- html_nodes(htxt, 'div.responsive-table') # data body
  table_row_nodes <- html_nodes(response_table, 'tr') # 1,2 row is tresh
  
  squad_response_table <- html_nodes(squad_htxt, 'div.responsive-table')#######
  porsrela <- html_nodes(squad_response_table, 'a.spielprofil_tooltip')
  array <- html_text(porsrela)[seq(1,length(porsrela), 2)]
  squad_list <- ""
  
  i = 0
  while(i < length(array)){
    i = i+1
    squad_list <- paste(squad_list, array[i], sep = "\t")
  }
  
  i = 2
  while(i < length(table_row_nodes)){ 
    i = i + 1
    temp_data <- data.frame(stringsAsFactors=FALSE)
    
    table_column_nodes <- html_nodes(table_row_nodes[i], 'td') #index is 3~40, table_column_nodes[11] is result
    team_nodes <- html_nodes(table_row_nodes[i], 'td.no-border-links') #index is 3~40
    a_nodes <- html_nodes(table_row_nodes[i], 'a') # a_nodes[6] is coach
    
    home_team <- html_text(team_nodes[1])
    home_team <- gsub("\u00a0.*","",home_team)
    
    away_team <- html_text(team_nodes[2])
    away_team <- gsub("\u00a0.*","",away_team)
    
    home_away <- "default"
    enemy_team <- "default"
    
    mylogic <- pmatch(away_team, myteam_name)
    if(is.na(mylogic)){
      mylogic <- 0
    }
    if(mylogic){
      home_away <- "away"
      enemy_team <- home_team
    }else{
      home_away <- "home"
      enemy_team <- away_team
    }
    
    coach_name <- html_text(a_nodes[6]) # a_nodes[6] is coach
    match_score_node <- table_column_nodes[11] # table_column_nodes[11] is result
    
    match_flag <- "default"
    match_point <- 0
    
    result <- paste(capture.output(match_score_node, file=NULL), collapse="\n")
    
    if(regexpr("greentext", result)[1] != -1){
      match_flag <- "win"
      match_point <- 3
    }else if(regexpr("redtext", result)[1] != -1){
      match_flag <- "lose"
      match_point <- 0
    }else{
      match_flag <- "draw"
      match_point <- 1
    }
    
    temp_data <- cbind(myteam_name)
    temp_data <- cbind(squad_list, temp_data)
    temp_data <- cbind(home_away, temp_data)
    temp_data <- cbind(enemy_team, temp_data)
    temp_data <- cbind(coach_name, temp_data)
    temp_data <- cbind(match_flag, temp_data)
    temp_data <- cbind(match_point, temp_data)
    temp_data <- cbind(season_number, temp_data)
    team_info <- rbind(team_info, temp_data)
  }
  
  return(team_info)
}

##################################################

Chelsea_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(631, "Chelsea", i)
  Chelsea_data <- rbind(Chelsea_data, temp_data)
}

##################################################

MU_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(985, "Manchester Utd.", i)
  MU_data <- rbind(MU_data, temp_data)
}

##################################################

Wat_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(1010, "Watford", i)
  Wat_data <- rbind(Wat_data, temp_data)
}

##################################################

Arsenal_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(11, "Arsenal", i)
  Arsenal_data <- rbind(Arsenal_data, temp_data)
}

##################################################

Liverpool_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(31, "Liverpool", i)
  Liverpool_data <- rbind(Liverpool_data, temp_data)
}

##################################################

ManchesterCity_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(281, "Manchester City", i)
  ManchesterCity_data <- rbind(ManchesterCity_data, temp_data)
}

##################################################

Spurs_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(148, "Spurs", i)
  Spurs_data <- rbind(Spurs_data, temp_data)
}

##################################################

Everton_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(29, "Everton", i)
  Everton_data <- rbind(Everton_data, temp_data)
}

##################################################

WestBrom_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(984, "West Brom", i)
  WestBrom_data <- rbind(WestBrom_data, temp_data)
}

##################################################

Bournemouth_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(989, "Bournemouth", i)
  Bournemouth_data <- rbind(Bournemouth_data, temp_data)
}

##################################################

BurnleyFC_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(1132, "Burnley FC", i)
  BurnleyFC_data <- rbind(BurnleyFC_data, temp_data)
}

##################################################

StokeCity_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(512, "Stoke City", i)
  StokeCity_data <- rbind(StokeCity_data, temp_data)
}

##################################################

Leicester_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(1003, "Leicester", i)
  Leicester_data <- rbind(Leicester_data, temp_data)
}

##################################################

Middlesbrough_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(641, "Middlesbrough", i)
  Middlesbrough_data <- rbind(Middlesbrough_data, temp_data)
}

##################################################

CrystalPalace_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(873, "Crystal Palace", i)
  CrystalPalace_data <- rbind(CrystalPalace_data, temp_data)
}

##################################################

WestHam_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(379, "West Ham", i)
  WestHam_data <- rbind(WestHam_data, temp_data)
}

##################################################

HullCity_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(3008, "Hull City", i)
  HullCity_data <- rbind(HullCity_data, temp_data)
}

##################################################

Sunderland_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(289, "Sunderland", i)
  Sunderland_data <- rbind(Sunderland_data, temp_data)
}

##################################################

Swansea_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(2288, "Swansea", i)
  Swansea_data <- rbind(Swansea_data, temp_data)
}

##################################################

Southampton_data <- data.frame(stringsAsFactors=FALSE)

for (i in 2009:2015) {
  temp_data <- team_data(180, "Southampton", i)
  Southampton_data <- rbind(Southampton_data, temp_data)
}

final_team_data <- data.frame(stringsAsFactors=FALSE)
final_team_data <- rbind(final_team_data, Arsenal_data)
final_team_data <- rbind(final_team_data, Bournemouth_data)
final_team_data <- rbind(final_team_data, BurnleyFC_data)
final_team_data <- rbind(final_team_data, Chelsea_data)
final_team_data <- rbind(final_team_data, CrystalPalace_data)
final_team_data <- rbind(final_team_data, Everton_data)
final_team_data <- rbind(final_team_data, HullCity_data)
final_team_data <- rbind(final_team_data, Leicester_data)
final_team_data <- rbind(final_team_data, Liverpool_data)
final_team_data <- rbind(final_team_data, ManchesterCity_data)
final_team_data <- rbind(final_team_data, MU_data)
final_team_data <- rbind(final_team_data, Southampton_data)
final_team_data <- rbind(final_team_data, Spurs_data)
final_team_data <- rbind(final_team_data, StokeCity_data)
final_team_data <- rbind(final_team_data, Sunderland_data)
final_team_data <- rbind(final_team_data, Swansea_data)
final_team_data <- rbind(final_team_data, Wat_data)
final_team_data <- rbind(final_team_data, WestHam_data)
final_team_data <- rbind(final_team_data, WestBrom_data)

View(final_team_data)

write.csv(final_team_data,"teamdata.csv")
