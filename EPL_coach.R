library(XML)
library(rvest)
library(httr)
library(jsonlite)
library(RJSONIO)
library(reshape2)
library(dplyr)
library(stringr)

coach_data <- function(coach_number, coach_name){
  url = paste("http://www.transfermarkt.co.uk/aaddwadaw/leistungsdatenDetail/trainer/", coach_number, "/plus/0?saison_id=&verein_id=&liga=&wettbewerb_id=&trainer_id=", sep="")
  
  response=GET(url)
  htxt <- read_html(response)
  response_table <- html_nodes(htxt, 'div.responsive-table') # data body
  
  match_list_odd <- html_nodes(response_table, 'tr.odd') # odd row -> home match
  match_list_even <- html_nodes(response_table, 'tr.even') # even row -> away match
  match_flag <- "default"
  match_point <- 0
  
  coach_info <- data.frame(stringsAsFactors=FALSE)
  temp_data1 <- data.frame(stringsAsFactors=FALSE)
  temp_data2 <- data.frame(stringsAsFactors=FALSE)
  
  i = 0
  while(i < length(match_list_odd)){ 
    i = i + 1
    match_score <- html_nodes(match_list_odd[i], 'span')
    match_score_node <- match_score[[1]]
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
    
    home_datarow <- html_nodes(match_list_odd[i], 'td.zentriert')
    home_season <- html_text(html_nodes(home_datarow[3], 'a'))
    
    temp_data1 <- cbind(coach_name)
    temp_data1 <- cbind(home_season, temp_data1)
    temp_data1 <- cbind(match_flag, temp_data1)
    temp_data1 <- cbind(match_point, temp_data1)
    coach_info <- rbind(coach_info, temp_data1)
  }
  
  j = 0
  while(j < length(match_list_even)){ 
    j = j + 1
    match_score <- html_nodes(match_list_even[j], 'span')
    match_score_node <- match_score[[1]]
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
    
    home_datarow <- html_nodes(match_list_even[j], 'td.zentriert')
    home_season <- html_text(html_nodes(home_datarow[3], 'a'))
    
    temp_data2 <- cbind(coach_name)
    temp_data2 <- cbind(home_season, temp_data2)
    temp_data2 <- cbind(match_flag, temp_data2)
    temp_data2 <- cbind(match_point, temp_data2)
    coach_info <- rbind(coach_info, temp_data2)
  }
  
  return(coach_info)
}

final_data <- data.frame(stringsAsFactors=FALSE)

AntonioConte_data=coach_data(3517, "Antonio Conte")
GuusHiddink_data=coach_data(506, "Guus Hiddink")
JoseMourinho_data=coach_data(781, "Jose Mourinho")
RafaelBenitez_data=coach_data(1522, "Rafael Benitez")
RobertoDiMatteo_data=coach_data(6783, "Roberto Di Matteo")
AndreVillasBoas_data=coach_data(1696, "Andre Villas-Boas")
CarloAncelotti_data=coach_data(523, "Carlo Ancelotti")
LouisvanGaal_data=coach_data(2029, "Louis van Gaal")
RyanGiggs_data=coach_data(29694, "Ryan Giggs")
DavidMoyes_data=coach_data(450, "David Moyes")
SirAlexFerguson_data=coach_data(4, "Sir Alex Ferguson")
ArseneWenger_data=coach_data(280, "Arsene Wenger")
JurgenKlopp_data=coach_data(118, "Jurgen Klopp")
BrendanRodgers_data=coach_data(1366, "Brendan Rodgers")
KennyDalglish_data=coach_data(12320, "Kenny Dalglish")
RoyHodgson_data=coach_data(1775, "Roy Hodgson")
PepGuardiola_data=coach_data(5672, "Pep Guardiola")
ManuelPellegrini_data=coach_data(1558, "Manuel Pellegrini")
BrianKidd_data=coach_data(24703, "Brian Kidd")
RobertoMancini_data=coach_data(524, "Roberto Mancini")
MarkHughes_data=coach_data(1646, "Mark Hughes")
MauricioPochettino_data=coach_data(9044, "Mauricio Pochettino")
TimSherwood_data=coach_data(8259, "Tim Sherwood")
HarryRedknapp_data=coach_data(448, "Harry Redknapp")
RonaldKoeman_data=coach_data(439, "Ronald Koeman")
DavidUnsworth_data=coach_data(15772, "David Unsworth")
RobertoMartinez_data=coach_data(4328, "Roberto Martinez")
QuiqueFlores_data=coach_data(1553, "Quique Flores")
TonyPulis_data=coach_data(617, "Tony Pulis")
RobKelly_data=coach_data(3219, "Rob Kelly")
AlanIrvine_data=coach_data(1686, "Alan Irvine")
PepeMel_data=coach_data(6722, "Pepe Mel")
KeithDowning_data=coach_data(8329, "Keith Downing")
SteveClarke_data=coach_data(2638, "Steve Clarke")
RoyHodgson_data=coach_data(1775, "Roy Hodgson")
EddieHowe_data=coach_data(10976, "Eddie Howe")
ClaudePuel_data=coach_data(1240, "Claude Puel")
NigelAdkins_data=coach_data(4152, "Nigel Adkins")
SeanDyche_data=coach_data(15856, "Sean Dyche")
BrianLaws_data=coach_data(4079, "Brian Laws")
OwenCoyle_data=coach_data(4121, "Owen Coyle")
ClaudioRanieri_data=coach_data(456, "Claudio Ranieri")
NigelPearson_data=coach_data(2676, "Nigel Pearson")
AlanPardew_data=coach_data(1988, "Alan Pardew")
KeithMillen_data=coach_data(1712, "Keith Millen")
NeilWarnock_data=coach_data(1527, "Neil Warnock")
IanHolloway_data=coach_data(1524, "Ian Holloway")
SlavenBilic_data=coach_data(3598, "Slaven Bilic")
SamAllardyce_data=coach_data(445, "Sam Allardyce")
KevinKeen_data=coach_data(5387, "Kevin Keen")
AvramGrant_data=coach_data(5232, "Avram Grant")
GianfrancoZola_data=coach_data(6209, "Gianfranco Zola")
MikePhelan_data=coach_data(7278, "Mike Phelan")
SteveBruce_data=coach_data(447, "Steve Bruce")
IainDowie_data=coach_data(1598, "Iain Dowie")
PhilBrown_data=coach_data(1690, "Phil Brown")
DickAdvocaat_data=coach_data(3, "Dick Advocaat")
GustavoPoyet_data=coach_data(4078, "Gustavo Poyet")
KevinBall_data=coach_data(3029, "Kevin Ball")
PaoloDiCanio_data=coach_data(7556, "Paolo Di Canio")
MartinONeill_data=coach_data(176, "Martin O'Neill")
EricBlack_data=coach_data(1701, "Eric Black")
BobBradley_data=coach_data(3208, "Bob Bradley")
FrancescoGuidolin_data=coach_data(530, "Francesco Guidolin")
AlanCurtis_data=coach_data(7887, "Alan Curtis")
GarryMonk_data=coach_data(33665, "Garry Monk")
MichaelLaudrup_data=coach_data(554, "Michael Laudrup")

final_data <- rbind(final_data, AlanCurtis_data)
final_data <- rbind(final_data, AlanPardew_data)
final_data <- rbind(final_data, AlanIrvine_data)
final_data <- rbind(final_data, AndreVillasBoas_data)
final_data <- rbind(final_data, AntonioConte_data)
final_data <- rbind(final_data, ArseneWenger_data)
final_data <- rbind(final_data, AvramGrant_data)
final_data <- rbind(final_data, BobBradley_data)
final_data <- rbind(final_data, BrendanRodgers_data)
final_data <- rbind(final_data, BrianLaws_data)
final_data <- rbind(final_data, BrianKidd_data)
final_data <- rbind(final_data, CarloAncelotti_data)
final_data <- rbind(final_data, ClaudioRanieri_data)
final_data <- rbind(final_data, ClaudePuel_data)
final_data <- rbind(final_data, DavidUnsworth_data)
final_data <- rbind(final_data, DavidMoyes_data)
final_data <- rbind(final_data, DickAdvocaat_data)
final_data <- rbind(final_data, EddieHowe_data)
final_data <- rbind(final_data, EricBlack_data)
final_data <- rbind(final_data, FrancescoGuidolin_data)
final_data <- rbind(final_data, GarryMonk_data)
final_data <- rbind(final_data, GianfrancoZola_data)
final_data <- rbind(final_data, GustavoPoyet_data)
final_data <- rbind(final_data, GuusHiddink_data)
final_data <- rbind(final_data, HarryRedknapp_data)
final_data <- rbind(final_data, IanHolloway_data)
final_data <- rbind(final_data, IainDowie_data)
final_data <- rbind(final_data, JoseMourinho_data)
final_data <- rbind(final_data, JurgenKlopp_data)
final_data <- rbind(final_data, KeithMillen_data)
final_data <- rbind(final_data, KeithDowning_data)
final_data <- rbind(final_data, KennyDalglish_data)
final_data <- rbind(final_data, KevinBall_data)
final_data <- rbind(final_data, KevinKeen_data)
final_data <- rbind(final_data, LouisvanGaal_data)
final_data <- rbind(final_data, ManuelPellegrini_data)
final_data <- rbind(final_data, MartinONeill_data)
final_data <- rbind(final_data, MarkHughes_data)
final_data <- rbind(final_data, MauricioPochettino_data)
final_data <- rbind(final_data, MichaelLaudrup_data)
final_data <- rbind(final_data, MikePhelan_data)
final_data <- rbind(final_data, NeilWarnock_data)
final_data <- rbind(final_data, NigelPearson_data)
final_data <- rbind(final_data, NigelAdkins_data)
final_data <- rbind(final_data, OwenCoyle_data)
final_data <- rbind(final_data, PaoloDiCanio_data)
final_data <- rbind(final_data, PepeMel_data)
final_data <- rbind(final_data, PepGuardiola_data)
final_data <- rbind(final_data, PhilBrown_data)
final_data <- rbind(final_data, QuiqueFlores_data)
final_data <- rbind(final_data, RafaelBenitez_data)
final_data <- rbind(final_data, RobertoMancini_data)
final_data <- rbind(final_data, RobertoMartinez_data)
final_data <- rbind(final_data, RobertoDiMatteo_data)
final_data <- rbind(final_data, RobKelly_data)
final_data <- rbind(final_data, RonaldKoeman_data)
final_data <- rbind(final_data, RoyHodgson_data)
final_data <- rbind(final_data, RyanGiggs_data)
final_data <- rbind(final_data, SamAllardyce_data)
final_data <- rbind(final_data, SeanDyche_data)
final_data <- rbind(final_data, SirAlexFerguson_data)
final_data <- rbind(final_data, SlavenBilic_data)
final_data <- rbind(final_data, SteveBruce_data)
final_data <- rbind(final_data, SteveClarke_data)
final_data <- rbind(final_data, TimSherwood_data)
final_data <- rbind(final_data, TonyPulis_data)

final_data

write.csv(final_data,"coachdata.csv")