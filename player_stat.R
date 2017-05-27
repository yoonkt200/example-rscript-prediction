if (!require("XML")) {
  install.packages("XML",repos= 'https://cloud.r-project.org') 
  library(XML)
}
if (!require("rvest")) {
  install.packages("rvest")
  library(rvest)
}
if (!require("RSelenium")) {
  install.packages("https://cran.r-project.org/src/contrib/Archive/RSelenium/RSelenium_1.3.5.tar.gz", repos=NULL, type="source", dependencies = TRUE)
  checkForServer()
  library(RSelenium)
}
if (!require("stringr")) {
  library(stringr)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
library(e1071)
library(nnet)
library(mlbench)
library(car)
library(class)
library(caret)
library(C50)

func_player_stat <- function(position) {
  
  #Condition for position
  if(position == "defensive") {
    num_col = 14
  }else if(position == "offensive") {
    num_col = 15
  }else if(position == "passing") {
    num_col = 13
  }
  
  startServer()
  chrome=remoteDriver(browserName='chrome')
  chrome$open()
  player_data = NULL
  
  for(season in 2009:2016) {
    
    if(season == 2009) {
      num_season = 1849
      num_stage = 3115
    }else if(season == 2010) {
      num_season = 2458
      num_stage = 4345
    }else if(season == 2011) {
      num_season = 2935
      num_stage = 5476
    }else if(season == 2012) {
      num_season = 3389
      num_stage = 6531
    }else if(season == 2013) {
      num_season = 3853
      num_stage = 7794
    }else if(season == 2014) {
      num_season = 4311
      num_stage = 9155
    }else if(season == 2015) {
      num_season = 5826
      num_stage = 12496
    }else if(season == 2016) {
      num_season = 6335
      num_stage = 13796
    }
      
    #Navigating web
    chrome$navigate(paste0('https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/', num_season, '/Stages/', num_stage, '/PlayerStatistics/England-Premier-League-', season, '-', season+1))
    Sys.sleep(1)
    
    webElem <- chrome$findElement(using = "xpath", paste0("//*/li[@class = 'in-squad-detailed-view']/a[@href = '#stage-top-player-stats-", position, "']"))
    webElem$sendKeysToElement(list(key = "enter"))
    Sys.sleep(1)
    
    #Dummy data frame
    df_player_stat_temp = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0) %>% as.data.frame()
    df_player_stat = NULL
    
    #Crawlier(loop)
    while(!is.na(df_player_stat_temp[10,1])){
      Sys.sleep(1)
      h = read_html(chrome$getPageSource()[[1]])
      parser = html_nodes(h, 'tbody#player-table-statistics-body')
      parser_stat = html_nodes(parser, 'td')
      player_stat = html_text(parser_stat)[141 : length(parser_stat)]
      df_player_stat_temp = matrix(player_stat, ncol = num_col, byrow = T) %>% as.data.frame()
        
      parser_name = html_nodes(parser, 'a.player-link')
      player_name = html_text(parser_name)[11 : length(parser_name)] %>% as.data.frame()
        
      parser_team = html_nodes(parser, 'span.team-name')
      team_name = html_text(parser_team)[11 : length(parser_team)] %>% str_sub(1, -3) %>% as.data.frame()
      
      parser_meta = html_nodes(parser, 'span.player-meta-data')
      meta = html_text(parser_meta)[seq(2, length(parser_meta), 2)]
      meta = meta[11:length(meta)] %>% str_sub(2, -1) %>% as.data.frame()
      
      sson = rep(season, nrow(df_player_stat_temp)) %>% as.data.frame()
      
      
      df_player_stat_temp = cbind(df_player_stat_temp, player_name, team_name, meta, sson)
      
      
      df_player_stat = rbind(df_player_stat, df_player_stat_temp)
      
      
      
      Sys.sleep(1)
      #Changing the table page to next
      webElem_click <- chrome$findElement(using = 'xpath', 
                                          value = paste0("//*/div[@id = 'statistics-paging-", position, "']/div[@class = 'grid-toolbar']/dl/dd/a[@id = 'next']"))
      webElem_click$clickElement()
      Sys.sleep(1)
    }
    player_data = rbind(player_data, df_player_stat)
  }
  chrome$close()
  return(player_data)
}

passing_player_stat = func_player_stat("passing")
offensive_player_stat = func_player_stat("offensive")
defensive_player_stat = func_player_stat("defensive")


defensive_col_names = c("R", "Dummy"," Player", "Apps", "Mins", "Tackles", "Inter", "Fouls", "Offsides", "Clear", "Drb", "Blocks", "OwnG", "Rating", "Name", "Team", "Meta", "Season")
offensive_col_names = c("R", "Dummy"," Player", "Apps", "Mins", "Goals", "Assists", "SpG", "KeyP", "Drb", "Fouled", "Off", "Disp", "UnsTch", "Rating", "Name", "Team", "Meta", "Season")
passing_col_names = c("R", "Dummy"," Player", "Apps", "Mins", "Assists", "KeyP", "AvgP", "PS", "Crosses", "LongB", "ThrB", "Rating", "Name", "Team", "Meta", "Season")


colnames(defensive_player_stat) = defensive_col_names
colnames(offensive_player_stat) = offensive_col_names
colnames(passing_player_stat) = passing_col_names

defensive_player_stat = defensive_player_stat[, -c(1, 2, 3)]
offensive_player_stat = offensive_player_stat[, -c(1, 2, 3)]
passing_player_stat = passing_player_stat[, -c(1, 2, 3)]





for(col_num in 1:length(defensive_player_stat)){
  defensive_player_stat[, col_num] = gsub("\t", "", defensive_player_stat[, col_num])
  defensive_player_stat[, col_num] = gsub("-", "0", defensive_player_stat[, col_num])
}
defensive_player_stat$Meta = gsub("\\s", "", defensive_player_stat$Meta)
defensive_player_stat$Name = defensive_player_stat$Name %>% str_sub(1, -2)
defensive_player_stat$Apps = gsub("*\\(.*?\\) *", "", defensive_player_stat$Apps)

defensive_player_stat$Mins = defensive_player_stat$Mins %>% as.integer()
defensive_player_stat$Tackles = defensive_player_stat$Tackles %>% as.numeric()
defensive_player_stat$Inter = defensive_player_stat$Inter %>% as.numeric()
defensive_player_stat$Fouls = defensive_player_stat$Fouls %>% as.numeric()
defensive_player_stat$Offsides = defensive_player_stat$Offsides %>% as.numeric()
defensive_player_stat$Clear = defensive_player_stat$Clear %>% as.numeric()
defensive_player_stat$Drb = defensive_player_stat$Drb %>% as.numeric()
defensive_player_stat$Blocks = defensive_player_stat$Blocks %>% as.numeric()
defensive_player_stat$OwnG = defensive_player_stat$OwnG %>% as.integer()
defensive_player_stat$Rating = defensive_player_stat$Rating %>% as.numeric()
defensive_player_stat$Season = defensive_player_stat$Season %>% as.integer()





for(col_num in 1:length(offensive_player_stat)){
  offensive_player_stat[, col_num] = gsub("\t", "", offensive_player_stat[, col_num])
  offensive_player_stat[, col_num] = gsub("-", "0", offensive_player_stat[, col_num])
}
offensive_player_stat$Meta = gsub("\\s", "", offensive_player_stat$Meta)
offensive_player_stat$Name = offensive_player_stat$Name %>% str_sub(1, -2)
offensive_player_stat$Apps = gsub("*\\(.*?\\) *", "", offensive_player_stat$Apps)

offensive_player_stat$Mins = offensive_player_stat$Mins %>% as.integer()
offensive_player_stat$Goals = offensive_player_stat$Goals %>% as.integer()
offensive_player_stat$Assists = offensive_player_stat$Assists %>% as.integer()
offensive_player_stat$SpG = offensive_player_stat$SpG %>% as.numeric()
offensive_player_stat$KeyP = offensive_player_stat$KeyP %>% as.numeric()
offensive_player_stat$Drb = offensive_player_stat$Drb %>% as.numeric()
offensive_player_stat$Fouled = offensive_player_stat$Fouled %>% as.numeric()
offensive_player_stat$Off = offensive_player_stat$Off %>% as.numeric()
offensive_player_stat$Disp = offensive_player_stat$Disp %>% as.numeric()
offensive_player_stat$UnsTch = offensive_player_stat$UnsTch %>% as.numeric()
offensive_player_stat$Rating = offensive_player_stat$Rating %>% as.numeric()
offensive_player_stat$Season = offensive_player_stat$Season %>% as.integer()





for(col_num in 1:length(passing_player_stat)){
  passing_player_stat[, col_num] = gsub("\t", "", passing_player_stat[, col_num])
  passing_player_stat[, col_num] = gsub("-", "0", passing_player_stat[, col_num])
}
passing_player_stat$Meta = gsub("\\s", "", passing_player_stat$Meta)
passing_player_stat$Name = passing_player_stat$Name %>% str_sub(1, -2)
passing_player_stat$Apps = gsub("*\\(.*?\\) *", "", passing_player_stat$Apps)

passing_player_stat$Mins = passing_player_stat$Mins %>% as.integer()
passing_player_stat$Assists = passing_player_stat$Assists %>% as.integer()
passing_player_stat$KeyP = passing_player_stat$KeyP %>% as.numeric()
passing_player_stat$AvgP = passing_player_stat$AvgP %>% as.numeric()
passing_player_stat$PS = passing_player_stat$PS %>% as.numeric()
passing_player_stat$Crosses = passing_player_stat$Crosses %>% as.numeric()
passing_player_stat$LongB = passing_player_stat$LongB %>% as.numeric()
passing_player_stat$ThrB = passing_player_stat$ThrB %>% as.numeric()
passing_player_stat$Rating = passing_player_stat$Rating %>% as.numeric()
passing_player_stat$Season = passing_player_stat$Season %>% as.integer()







defensive_player_stat %>% str()
offensive_player_stat %>% str()
passing_player_stat %>% str()


write.csv(defensive_player_stat, "C:/Users/hjsbl/Desktop/defensive_player_stat.csv", row.names = F)
write.csv(offensive_player_stat, "C:/Users/hjsbl/Desktop/offensive_player_stat.csv", row.names = F)
write.csv(passing_player_stat, "C:/Users/hjsbl/Desktop/passing_player_stat.csv", row.names = F)

defensive_stat = merge(defensive_player_stat, playerCostData, all = T, by.x = c("Name", "Season"), by.y = c("Name", "Season"))
offensive_stat = merge(offensive_player_stat, playerCostData, all = T, by.x = c("Name", "Season"), by.y = c("Name", "Season"))
passing_stat = merge(passing_player_stat, playerCostData, all = T, by.x = c("Name", "Season"), by.y = c("Name", "Season"))

#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

teamdata = read.csv("C:/Users/hjsbl/Desktop/teamdata.csv", header = T, stringsAsFactors = F)
coachdata = read.csv("C:/Users/hjsbl/Desktop/coachdata_3.csv", header = T, stringsAsFactors = F)
defensive_player = read.csv("C:/Users/hjsbl/Desktop/defensivedata.csv", header = T, stringsAsFactors = F)
offensive_player = read.csv("C:/Users/hjsbl/Desktop/offensivedata.csv", header = T, stringsAsFactors = F)
passing_player = read.csv("C:/Users/hjsbl/Desktop/passingdata.csv", header = T, stringsAsFactors = F)
gk_player = read.csv("C:/Users/hjsbl/Desktop/GKdata.csv", header = T, stringsAsFactors = F)


#%in%

defensive_player$Mins %>% hist()
defensive_player$Tackles %>% log1p() %>% hist()
defensive_player$Tackles %>% log1p() %>% boxplot()
defensive_player$Tackles = defensive_player$Tackles %>% log1p()
defensive_player$Inter %>% hist()
defensive_player$Inter %>% boxplot()
defensive_player$Fouls %>% log1p() %>% hist()
defensive_player$Fouls %>% log1p() %>% boxplot()#remove
defensive_player$Fouls = defensive_player$Fouls %>% log1p()
defensive_player$Offsides %>% log1p() %>% hist()#remove
defensive_player$Clear %>% log1p() %>% hist()
defensive_player$Clear %>% log1p() %>% boxplot()
defensive_player$Clear = defensive_player$Clear %>% log1p()
defensive_player$Drb %>% log1p() %>% hist()
defensive_player$Drb = defensive_player$Drb %>% log1p()
defensive_player$Blocks %>% hist()
defensive_player$OwnG %>% hist()
defensive_player$Rating %>% hist()#oulier over 8.1
defensive_player$Rating %>% boxplot()
defensive_player$Cost %>% log1p() %>% hist()
defensive_player$Cost = defensive_player$Cost %>% log1p()
defensive_player = defensive_player[, -c(1, 4, 8, 9, 15, 16, 17)]
defensive_player$Season = defensive_stat$Season %>% as.factor()

offensive_player$Mins %>% hist()
offensive_player$Goal %>% log1p() %>% hist()#*
offensive_player$Goal = offensive_player$Goal %>% log1p()
offensive_player$Assists %>% log1p() %>% hist()#*
offensive_player$Assists = offensive_player$Assists %>% log1p()
offensive_player$SpG %>% log1p() %>% hist()#*
offensive_player$SpG = offensive_player$SpG %>% log1p()
offensive_player$KeyP %>% log1p() %>% hist()#*
offensive_player$KeyP = offensive_player$KeyP %>% log1p()
offensive_player$Drb %>% log1p() %>% hist()#*
offensive_player$Drb = offensive_player$Drb %>% log1p()
offensive_player$Fouled %>% log1p() %>% hist()#remove
offensive_player$Off %>% hist()#remove
offensive_player$Disp %>% log1p() %>% hist()
offensive_player$Disp = offensive_player$Disp %>% log1p()
offensive_player$UnsTch %>% log1p() %>% hist()
offensive_player$UnsTch = offensive_player$UnsTch %>% log1p()
offensive_player$Rating %>% hist()
offensive_player$Cost %>% log1p() %>% hist()
offensive_player$Cost = offensive_player$Cost %>% log1p()
offensive_player = offensive_player[, -c(1, 4, 11, 12, 16, 17, 18)]
offensive_player$Season = offensive_stat$Season %>% as.factor()


passing_player$Mins %>% hist()
passing_player$Assists %>% log1p() %>% hist()
passing_player$Assists = passing_player$Assists %>% log1p() 
passing_player$KeyP %>% log1p() %>% hist()#*
passing_player$KeyP = passing_player$KeyP %>% log1p()
passing_player$AvgP %>% hist()#*
passing_player$PS %>% hist()#*
passing_player$Crosses %>% hist()
passing_player$LongB %>% log1p() %>% hist()
passing_player$ThrB %>% log1p() %>% hist()
passing_player$Rating %>% hist()
passing_player$Cost %>% log1p() %>% hist()
passing_player = passing_player[, -c(1, 4, 14, 15, 16)]
passing_player$Season = passing_player$Season %>% as.factor()





#function for input data
func_makeData = function(myTeam, enemyTeam) {
  inputdata = teamdata %>% filter(myteam_name == myTeam, enemy_team == enemyTeam)
  if(is.na(inputdata[1,1])){
    return(data.frame(NULL))
  }
  
  #filtering match data
  #작년 승점, 3년 누계 승점
  #경기당 승점
  
  accumulated_team_match_point = NULL
  team_match_point = NULL
  for(i in 1:nrow(inputdata)) {
    if(inputdata$season_number[i] == 2009) {
      accumulated_team_match_point = c(accumulated_team_match_point, NA)
      team_match_point = c(team_match_point, NA)
    } else if(inputdata$season_number[i] == 2010) {
      filtered_teamdata1 = teamdata %>% filter(season_number == (inputdata$season_number[i]-1) &
                                                   myteam_name == myTeam)
      accumulated_team_match_point = c(accumulated_team_match_point, mean(filtered_teamdata1$match_point, na.rm = T))
      team_match_point = c(team_match_point, mean(filtered_teamdata1$match_point, na.rm = T))
    } else if(inputdata$season_number[i] == 2011) {
      filtered_teamdata2 = teamdata %>% filter((season_number == (inputdata$season_number[i]-1) |
                                                   season_number == (inputdata$season_number[i]-2)) &
                                                  myteam_name == myTeam)
      accumulated_team_match_point = c(accumulated_team_match_point, mean(filtered_teamdata2$match_point, na.rm = T))
      team_match_point = c(team_match_point, mean(filter(filtered_teamdata2, season_number == inputdata$season_number[i]-1)$match_point, na.rm = T))
    } else {
      filtered_temadata3 = teamdata %>% filter((season_number == (inputdata$season_number[i]-1) |
                                                   season_number == (inputdata$season_number[i]-2) |
                                                   season_number == (inputdata$season_number[i]-3)) &
                                                  myteam_name == myTeam)
      accumulated_team_match_point = c(accumulated_team_match_point, mean(filtered_temadata3$match_point, na.rm = T))
      team_match_point = c(team_match_point, mean(filter(filtered_temadata3, season_number == inputdata$season_number[i]-1)$match_point, na.rm = T))
    }
  }
  
  #filtering coach data
  #작년 승점, 3년 누계 승점
  #연도 구분
  #경기당 승점으로 계산
  accumulated_coach_match_point = NULL
  coach_match_point = NULL
  for(i in 1:nrow(inputdata)) {
    if(inputdata$season_number[i] == 2009) {
      accumulated_coach_match_point = c(accumulated_coach_match_point, NA)
      coach_match_point = c(coach_match_point, NA)
    } else if(inputdata$season_number[i] == 2010) {
      filtered_coachdata1 = coachdata %>% filter(season_edits == (inputdata$season_number[i]-1) &
                                                   coach_name == inputdata$coach_name[i])
      accumulated_coach_match_point = c(accumulated_coach_match_point, mean(filtered_coachdata1$match_point, na.rm = T))
      if(is.na(filter(coachdata, season_edits == (inputdata$season_number[i]-1) & coach_name == inputdata$coach_name[i])[1,1])){
        coach_match_point = c(coach_match_point, NA)
      } else{
        coach_match_point = c(coach_match_point, mean(filtered_coachdata1$match_point, na.rm = T))
      }
    } else if(inputdata$season_number[i] == 2011) {
      filtered_coachdata2 = coachdata %>% filter((season_edits == (inputdata$season_number[i]-1) |
                                                    season_edits == (inputdata$season_number[i]-2)) &
                                                   coach_name == inputdata$coach_name[i])
      accumulated_coach_match_point = c(accumulated_coach_match_point, mean(filtered_coachdata2$match_point, na.rm = T))
      if(is.na(filter(coachdata, season_edits == (inputdata$season_number[i]-1) & coach_name == inputdata$coach_name[i])[1,1])){
        coach_match_point = c(coach_match_point, NA)
      } else{
        coach_match_point = c(coach_match_point, mean(filter(filtered_coachdata2, season_edits == (inputdata$season_number[i]-1))$match_point, na.rm = T))
      }
    } else {
      filtered_coachdata3 = coachdata %>% filter((season_edits == (inputdata$season_number[i]-1) |
                                                         season_edits == (inputdata$season_number[i]-2) |
                                                         season_edits == (inputdata$season_number[i]-3)) &
                                                        coach_name == inputdata$coach_name[i])
      accumulated_coach_match_point = c(accumulated_coach_match_point, mean(filtered_coachdata3$match_point, na.rm = T))
      if(is.na(filter(coachdata, season_edits == (inputdata$season_number[i]-1) & coach_name == inputdata$coach_name[i])[1,1])){
        coach_match_point = c(coach_match_point, NA)
      } else{
      coach_match_point = c(coach_match_point, mean(filter(filtered_coachdata3, season_edits == (inputdata$season_number[i]-1))$match_point, na.rm = T))
      }
    }
  }
  
  #filtering player data분
  #연도 구분
  #선수 포지션별 스텟 데이터화
  
  filtered_defensive_data = NULL
  filtered_offensive_data = NULL
  filtered_passing_data = NULL
  filtered_gk_data = NULL
  
  filtered_defensive_player = NULL
  filtered_offensive_player = NULL
  filtered_passing_player = NULL
  filtered_gk_player = NULL
  
  defensive_mins = NULL
  defensive_tackles = NULL
  defensive_inter = NULL
  defensive_clear = NULL
  defensive_drb= NULL
  defensive_blocks = NULL
  defensive_owng = NULL
  offensive_mins = NULL
  offensive_goals = NULL
  offensive_assists = NULL
  offensive_spg = NULL
  offensive_keyp = NULL
  offensive_drb = NULL
  offensive_disp = NULL
  offensive_unstch = NULL
  passing_mins = NULL
  passing_assists = NULL
  passing_keyp = NULL
  passing_avgp = NULL
  passing_ps = NULL
  passing_crosses = NULL
  passing_Longb = NULL
  passing_thrb = NULL
  
  player_rating = NULL
  player_cost = NULL
  
  defensive_players = defensive_player$Name %>% unique()
  offensive_players = offensive_player$Name %>% unique()
  passing_players = passing_player$Name %>% unique()
  gk_players = gk_player$Name %>% unique()
  
  for(i in 1:nrow(inputdata)) {
    player_names = inputdata$squad_list[i]
    each_player_names = (player_names %>% strsplit(split = "\t") %>% unlist())[-1]
    player_season = inputdata$season[i]
    for(j in 2008:(player_season-1)){
      filtered_defensive_player_temp = defensive_player %>% filter(Season == player_season)
      filtered_offensive_player_temp = offensive_player %>% filter(Season == player_season)
      filtered_passing_player_temp = passing_player %>% filter(Season == player_season)
      filtered_gk_player_temp = gk_player %>% filter(Season == player_season)
      
      filtered_defensive_player = rbind(filtered_defensive_player, filtered_defensive_player_temp)
      filtered_offensive_player = rbind(filtered_offensive_player, filtered_offensive_player_temp)
      filtered_passing_player = rbind(filtered_passing_player, filtered_passing_player_temp)
      filtered_gk_player = rbind(filtered_gk_player, filtered_gk_player_temp)
    }
    
    for(j in each_player_names) {
      if(j %in% defensive_players) {
        filtered_defensive_data_temp = filtered_defensive_player %>% filter(Name == j)
        filtered_defensive_data = rbind(filtered_defensive_data, filtered_defensive_data_temp)
      } else if(j %in% offensive_players) {
        filtered_offensive_data_temp = filtered_offensive_player %>% filter(Name == j)
        filtered_offensive_data = rbind(filtered_offensive_data, filtered_offensive_data_temp)
      } else if(j %in% passing_players) {
        filtered_passing_data_temp = filtered_passing_player %>% filter(Name == j)
        filtered_passing_data = rbind(filtered_passing_data, filtered_passing_data_temp)
      } else {
        filtered_gk_data_temp = filtered_gk_player %>% filter(Name == j)
        filtered_gk_data = rbind(filtered_gk_data, filtered_gk_data_temp)
      }
    }
    
    defensive_mins = c(defensive_mins, mean(filtered_defensive_data$Mins, na.rm = T))
    defensive_tackles = c(defensive_tackles, mean(filtered_defensive_data$Tackles, na.rm = T))
    defensive_inter = c(defensive_inter, mean(filtered_defensive_data$Inter, na.rm = T))
    defensive_clear = c(defensive_clear, mean(filtered_defensive_data$Clear, na.rm = T))
    defensive_drb= c(defensive_drb, mean(filtered_defensive_data$Drb, na.rm = T))
    defensive_blocks = c(defensive_blocks, mean(filtered_defensive_data$Blocks, na.rm = T))
    defensive_owng = c(defensive_owng, mean(filtered_defensive_data$OwnG, na.rm = T))
    offensive_mins = c(offensive_mins, mean(filtered_offensive_data$Mins, na.rm = T))
    offensive_goals = c(offensive_goals, mean(filtered_offensive_data$Goals, na.rm = T))
    offensive_assists = c(offensive_assists, mean(filtered_offensive_data$Assists, na.rm = T))
    offensive_spg = c(offensive_spg, mean(filtered_offensive_data$SpG, na.rm = T))
    offensive_keyp = c(offensive_keyp, mean(filtered_offensive_data$KeyP, na.rm = T))
    offensive_drb = c(offensive_drb, mean(filtered_offensive_data$Drb, na.rm = T))
    offensive_disp = c(offensive_disp, mean(filtered_offensive_data$Disp, na.rm = T))
    offensive_unstch = c(offensive_unstch, mean(filtered_offensive_data$UnsTch, na.rm = T))
    passing_mins = c(passing_mins, mean(filtered_passing_data$Mins, na.rm = T))
    passing_assists = c(passing_assists, mean(filtered_passing_data$Assists, na.rm = T))
    passing_keyp = c(passing_keyp, mean(filtered_passing_data$KeyP, na.rm = T))
    passing_avgp = c(passing_avgp, mean(filtered_passing_data$AvgP, na.rm = T))
    passing_ps = c(passing_ps, mean(filtered_passing_data$PS, na.rm = T))
    passing_crosses = c(passing_crosses, mean(filtered_passing_data$Crosses, na.rm = T))
    passing_Longb = c(passing_Longb, mean(filtered_passing_data$LongB, na.rm = T))
    passing_thrb = c(passing_thrb, mean(filtered_passing_data$ThrB, na.rm = T))
    
    
    player_rating_temp = mean(c(filtered_defensive_data$Rating, filtered_offensive_data$Rating, filtered_passing_data$Rating, filtered_gk_data$Rating), na.rm = T)
    player_rating = c(player_rating, player_rating_temp)
    player_cost_temp = mean(c(filtered_defensive_data$Cost, filtered_offensive_data$Cost, filtered_passing_data$Cost, filtered_gk_data$Cost), na.rm = T)
    player_cost = c(player_cost, player_cost_temp)
  }
  
  
  inputdata = cbind(inputdata, 
                    team_match_point, accumulated_team_match_point, 
                    coach_match_point, accumulated_coach_match_point, 
                    defensive_mins, defensive_tackles, defensive_inter,  
                    defensive_clear, defensive_drb, defensive_blocks, defensive_owng, 
                    offensive_mins, offensive_goals, offensive_assists, offensive_spg, offensive_keyp, 
                    offensive_drb, offensive_disp, offensive_unstch, 
                    passing_mins, passing_assists, passing_keyp, passing_avgp, passing_ps, 
                    passing_crosses, passing_Longb, passing_thrb, 
                    player_rating, player_cost)
  return(inputdata)
}











data1 = func_makeData("West Ham", "West Brom")
data2 = func_makeData("West Brom", "West Ham")
rbind(data1, data2)

nadata = teamdata %>% filter(enemy_team == "Bournemouth", myteam_name == "Arsenal")
is.na(nadata[1,1])
coachdata %>% filter(coach_name == "Rob Kelly", season_edits == 2013)
coachdata %>% filter(coach_name == "Rob Kelly", season_edits == 2012)
coachdata %>% filter(coach_name == "Rob Kelly", season_edits == 2011)

myTeam = "Arsenal"
enemyTeam = "Bournemouth"
inputdata = teamdata %>% filter(myteam_name == myTeam, enemy_team == enemyTeam)
if(is.na(inputdata[1,1])){
  return(data.frame(NULL))
}

#filtering match data
#작년 승점, 3년 누계 승점
#경기당 승점

accumulated_team_match_point = NULL
team_match_point = NULL
for(i in 1:nrow(inputdata)) {
  if(inputdata$season_number[i] == 2009) {
    accumulated_team_match_point = c(accumulated_team_match_point, NA)
    team_match_point = c(team_match_point, NA)
  } else if(inputdata$season_number[i] == 2010) {
    filtered_teamdata1 = teamdata %>% filter(season_number == (inputdata$season_number[i]-1) &
                                               myteam_name == myTeam)
    accumulated_team_match_point = c(accumulated_team_match_point, mean(filtered_teamdata1$match_point, na.rm = T))
    team_match_point = c(team_match_point, mean(filtered_teamdata1$match_point, na.rm = T))
  } else if(inputdata$season_number[i] == 2011) {
    filtered_teamdata2 = teamdata %>% filter((season_number == (inputdata$season_number[i]-1) |
                                                season_number == (inputdata$season_number[i]-2)) &
                                               myteam_name == myTeam)
    accumulated_team_match_point = c(accumulated_team_match_point, mean(filtered_teamdata2$match_point, na.rm = T))
    team_match_point = c(team_match_point, mean(filter(filtered_teamdata2, season_number == inputdata$season_number[i-2])$match_point, na.rm = T))
  } else {
    filtered_temadata3 = teamdata %>% filter((season_number == (inputdata$season_number[i]-1) |
                                                season_number == (inputdata$season_number[i]-2) |
                                                season_number == (inputdata$season_number[i]-3)) &
                                               myteam_name == myTeam)
    accumulated_team_match_point = c(accumulated_team_match_point, mean(filtered_temadata3$match_point, na.rm = T))
    team_match_point = c(team_match_point, mean(filter(filtered_temadata3, season_number == inputdata$season_number[i-2])$match_point, na.rm = T))
  }
}

#filtering coach data
#작년 승점, 3년 누계 승점
#연도 구분
#경기당 승점으로 계산
accumulated_coach_match_point = NULL
coach_match_point = NULL
for(i in 1:nrow(inputdata)) {
  if(inputdata$season_number[i] == 2009) {
    accumulated_coach_match_point = c(accumulated_coach_match_point, NA)
    coach_match_point = c(coach_match_point, NA)
  } else if(inputdata$season_number[i] == 2010) {
    filtered_coachdata1 = coachdata %>% filter(season_edits == (inputdata$season_number[i]-1) &
                                                 coach_name == inputdata$coach_name[i])
    accumulated_coach_match_point = c(accumulated_coach_match_point, mean(filtered_coachdata1$match_point, na.rm = T))
    if(is.na(filter(coachdata, season_edits == (inputdata$season_number[i]-1) & coach_name == inputdata$coach_name[i])[1,1])){
      coach_match_point = c(coach_match_point, NA)
    } else{
      coach_match_point = c(coach_match_point, mean(filtered_coachdata1$match_point, na.rm = T))
    }
  } else if(inputdata$season_number[i] == 2011) {
    filtered_coachdata2 = coachdata %>% filter((season_edits == (inputdata$season_number[i]-1) |
                                                  season_edits == (inputdata$season_number[i]-2)) &
                                                 coach_name == inputdata$coach_name[i])
    accumulated_coach_match_point = c(accumulated_coach_match_point, mean(filtered_coachdata2$match_point, na.rm = T))
    if(is.na(filter(coachdata, season_edits == (inputdata$season_number[i]-1) & coach_name == inputdata$coach_name[i])[1,1])){
      coach_match_point = c(coach_match_point, NA)
    } else{
      coach_match_point = c(coach_match_point, mean(filter(filtered_coachdata2, season_edits == (inputdata$season_number-1))$match_point, na.rm = T))
    }
  } else {
    filtered_coachdata3 = coachdata %>% filter((season_edits == (inputdata$season_number[i]-1) |
                                                  season_edits == (inputdata$season_number[i]-2) |
                                                  season_edits == (inputdata$season_number[i]-3)) &
                                                 coach_name == inputdata$coach_name[i])
    accumulated_coach_match_point = c(accumulated_coach_match_point, mean(filtered_coachdata3$match_point, na.rm = T))
    if(is.na(filter(coachdata, season_edits == (inputdata$season_number[i]-1) & coach_name == inputdata$coach_name[i])[1,1])){
      coach_match_point = c(coach_match_point, NA)
    } else{
      coach_match_point = c(coach_match_point, mean(filter(filtered_coachdata3, season_edits == (inputdata$season_number-1))$match_point, na.rm = T))
    }
  }
}








teamName = teamdata$myteam_name %>% unique()
model_data = NULL
numLoop = 0
for(i in teamName) {
  numLoop = numLoop + 1
  next_teamName = teamName[numLoop : length(teamName)]
  for(j in next_teamName) {
    cat("i:", i, "j:", j, "\n")
    if(i != j) {
      inputdata1 = func_makeData(i, j)
      inputdata2 = func_makeData(j, i)
      
      if(length(inputdata1) != 0){
        inputdata1$match_flag = inputdata1$match_flag %>% as.factor()
        inputdata1$home_away = inputdata1$home_away %>% as.factor()
        
        inputdata2$match_flag = inputdata2$match_flag %>% as.factor()
        inputdata2$home_away = inputdata2$home_away %>% as.factor()
        
        inputdata1[,c(10:38)] =  inputdata1[,c(10:38)] - inputdata2[,c(10:38)]
        model_data = rbind(model_data, inputdata1)
      }
    }
  }
}
model_data = model_data[model_data$season_number != 2009, ]
model_data = model_data[, -c(1:3, 5:6, 8:9)]
model_data %>% str()

for(i in 3:length(model_data)) {
  hist(model_data[, i])
  boxplot(model_data[, i])
}


for(i in 3:length(model_data)) {
  model_data[,i] = model_data[,i] %>% scale()
}
set.seed(1)
(model_data$match_flag %>% table())/length(model_data$match_flag)
test_index = sample(1:nrow(model_data), nrow(model_data)/5)
test_data = model_data[test_index, ]
(test_data$match_flag %>% table())/length(test_data$match_flag)
train_data = model_data[-test_index, ]
(train_data$match_flag %>% table())/length(train_data$match_flag)

omit_model_data = na.omit(model_data)
for(i in 3:length(omit_model_data)) {
  omit_model_data[,i] = omit_model_data[,i] %>% scale()
}
set.seed(1000)
(omit_model_data$match_flag %>% table())/length(omit_model_data$match_flag)
test_index = sample(1:nrow(omit_model_data), nrow(omit_model_data)/5)
test_data = omit_model_data[test_index, ]
(test_data$match_flag %>% table())/length(test_data$match_flag)
train_data = omit_model_data[-test_index, ]
(train_data$match_flag %>% table())/length(train_data$match_flag)



#naive bayes
rfe_data = omit_model_data
rfe_data$home_away = rfe_data$home_away %>% as.character()
for(i in 1:nrow(omit_model_data)){
  if(rfe_data[i, 2] == "home") {
    rfe_data[i, 2] = 1
  } else {
    rfe_data[i, 2] = 0
  }
}
rfe_data$home_away = rfe_data$home_away %>% as.integer()
results <- rfe(rfe_data[, 2:31],rfe_data[, 1], sizes=(1:23),rfeControl = rfeControl(functions = ldaFuncs, method = "cv"))

colnames(train_data)

EPL_nb = naiveBayes(formula = match_flag ~ defensive_blocks+defensive_mins+defensive_clear+passing_mins+defensive_owng, 
                    data=train_data, laplace = 1)
test_predicted_EPL_nb = predict(EPL_nb, newdata = test_data)
#test error
table(test_data$match_flag, test_predicted_EPL_nb)


EPL_lm <- multinom(match_flag ~ ., train_data)
vif(EPL_lm)
EPL_formula = step(EPL_lm, direction = "both")
EPL_formula
EPL_lm <- multinom(EPL_formula, train_data)
vif(EPL_lm)
test_predicted_EPL_lm = predict(EPL_lm, type="class", newdata=test_data)
#test error
table(test_data$match_flag, test_predicted_EPL_lm)




#k-NN
knn_data = rfe_data
library(FSelector)
library(rpart)
?forward.search()
evaluator <- function(subset) {
  #k-fold cross validation
  k <- 10
  splits <- runif(nrow(knn_data))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- knn_data[test.idx, , drop=FALSE]
    train <- knn_data[train.idx, , drop=FALSE]
    test_predicted_EPL_knn = knn(train = train[,-1], test = test[,-1], cl = train[,1], fit_k)
    error.rate = mean(test$match_flag != test_predicted_EPL_knn)
    return(error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

subset <- forward.search(names(knn_data)[-1], evaluator)
f <- as.simple.formula(subset, "match_flag")
print(f)

knn_data = rfe_data
knn_data = knn_data[, c(1, 2, 14, 29, 30)]
#1, 2, 4, 6, 30, 31
#8, 12, 13, 17, 19, 26, 
test_data = knn_data[test_index, -1]
test_data_cl = knn_data[test_index, 1]
train_data = knn_data[-test_index, -1]
train_data_cl = knn_data[-test_index, 1]


test_predicted_EPL_knn = knn(train = train_data, test = test_data, cl = train_data_cl, 5)
mean(test_data_cl != test_predicted_EPL_knn)

set.seed(10)
cv_idx <- createFolds(train_data_cl, k=10)
sapply(cv_idx, length)

ks <- 1:150
res_k <- sapply(ks, function(k) {
  ##try out each version of k from 1 to 12
  res_k <- sapply(seq_along(cv_idx), function(i) {
    ##loop over each of the 10 cross-validation folds
    ##predict the held-out samples using k nearest neighbors
    train_predicted_EPL_knn <- knn(train=as.data.frame(train_data[ -cv_idx[[i]], ]),
                test=as.data.frame(train_data[ cv_idx[[i]], ]),
                cl=train_data_cl[ -cv_idx[[i]] ], k = k)
    ##the ratio of misclassified samples
    mean(train_data_cl[ cv_idx[[i]] ] != train_predicted_EPL_knn)
  })
  ##average over the 10 folds
  mean(res_k)
})
plot(res_k)
fit_k = which(res_k == min(res_k))

test_predicted_EPL_knn = knn(train = train_data, test = test_data, cl = train_data_cl, fit_k)
1-mean(test_data_cl != test_predicted_EPL_knn)
table(test_data_cl, test_predicted_EPL_knn)




#Decision Tree
set.seed(1000)
(omit_model_data$match_flag %>% table())/length(omit_model_data$match_flag)
test_index = sample(1:nrow(omit_model_data), nrow(omit_model_data)/5)
test_data = omit_model_data[test_index, ]
(test_data$match_flag %>% table())/length(test_data$match_flag)
train_data = omit_model_data[-test_index, ]
(train_data$match_flag %>% table())/length(train_data$match_flag)

evaluator <- function(subset) {
  #k-fold cross validation
  k <- 10
  splits <- runif(nrow(train_data))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- train_data[test.idx, , drop=FALSE]
    train <- train_data[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "match_flag"), train)
    error.rate = sum(test$match_flag != predict(tree, test, type="c")) / nrow(test)
    return(1-error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

subset <- forward.search(names(train_data)[-1], evaluator)
f <- as.simple.formula(subset, "match_flag")
print(f)

tree_data = omit_model_data
tree_data = tree_data[, c(1, 2, 14, 29, 30)]
train_data = tree_data[-test_index,]
test_data = tree_data[test_index,]
#1, 2, 4, 6, 30, 31
#8, 12, 13, 17, 19, 26, 
test_data = tree_data[test_index, -1]
test_data_cl = tree_data[test_index, 1]
train_data = tree_data[-test_index, -1]
train_data_cl = tree_data[-test_index, 1]

set.seed(10)
cv_idx <- createFolds(train_data_cl, k=10)
sapply(cv_idx, length)

ts <- 1:50
res_t <- sapply(ts, function(t) {
  ##try out each version of k from 1 to 12
  res_t<- sapply(seq_along(cv_idx), function(i) {
    ##loop over each of the 10 cross-validation folds
    ##predict the held-out samples using k nearest neighbors
    train__EPL_tree <- C5.0(formula = f, data=train_data[cv_idx[[i]], ], trials = t)
    train_predicted_EPL_tree = predict(train_predicted_EPL_tree, newdata = train_data[-cv_idx[[i]], ])
    ##the ratio of misclassified samples
    mean(train_data_cl[ cv_idx[[i]] ] != train_predicted_EPL_tree)
  })
  ##average over the 10 folds
  mean(res_t)
})
plot(res_t)
fit_t = which(res_t == min(res_t))

test_predicted_EPL_tree = C5.0(train = train_data, test = test_data, cl = train_data_cl, fit_t)
1-mean(test_data_cl != test_predicted_EPL_tree)
table(test_data_cl, test_predicted_EPL_tree)


train__EPL_tree <- C5.0(formula = f, data=train_data, trials = 50)
train_predicted_EPL_tree = predict(train__EPL_tree, newdata = test_data)
1-sum(test_data$match_flag != train_predicted_EPL_tree)/nrow(test_data)



