############################################################
##############      UTILITIES     ##########################
############################################################
library("rvest")
library("stringr")
source("fm_functions.r")
local_path<-"C:\\git\\fightmetrics\\data\\"

################################################################################
##########################     TO REFRESH      #################################
################################################################################

# 
# ############################################################
# ##############      GENERATE EVENTS TABLES      ############
# ############################################################
# up_events<-read_html('http://www.fightmetric.com/statistics/events/upcoming')
# dn_events<-read_html('http://www.fightmetric.com/statistics/events/completed?page=all')
# up_events<-create_event_table(up_events)
# dn_events<-create_event_table(dn_events)
# write.csv(up_events, file=paste(local_path,"up_events",".csv"), row.names=FALSE)
# write.csv(dn_events, file=paste(local_path,"dn_events",".csv"), row.names=FALSE)
# 
# ############################################################
# ##############      GENERATE FIGHTER TABLE      ############
# ##############      TAKES A BIT THE LONGER YOU GO   ########
# ############################################################
# 
# dn_urls<-as.list(dn_events[2:150,1]) #PAST 3 YEARS (USADA's BEEN IN ~2?)
# fighter_table<-NULL
# for(i in seq(1:length(dn_urls))){
#   hld<-get_event_stats(as.character(dn_urls[i]))
#   fighter_table<-rbind(fighter_table,hld)
# }
# write.csv(fighter_table, file=paste(local_path,"master_fighter_table",".csv"), row.names=FALSE)
# 
# ############################################################
# ##############      GENERATE UPCOMING CARD     #############
# ############################################################
# #get upcoming event card
# card<-pull_upcoming_fighters()
# 
# # save table for each fighter
# for(i in seq(1,length(card[,2]),1)){
#   fighter<-get_fighter_fights(card[i])
#   if(is.null(fighter)==FALSE) {fighter<-cbind(fighter,calculate_table_score(fighter))}
#   write.csv(fighter, file=paste(local_path,card[i],".csv"), row.names=FALSE)
# }
# 
# write.csv(card, file=paste(local_path,"up_card",".csv"), row.names=FALSE)
# 
# ############################################################
# ##############      GET VEGAS ODDS              ############
# ############################################################
# odds<-get_betting_odds()
# write.csv(odds, file=paste(local_path,"odds",".csv"), row.names=FALSE)



#################################################################################
###########################     TO LOAD     #####################################
#################################################################################
up_events <- read.csv(file=paste(local_path,"up_events",".csv"), header=TRUE, sep=",")
dn_events <- read.csv(file=paste(local_path,"dn_events",".csv"), header=TRUE, sep=",")
fighter_table <- read.csv(file=paste(local_path,"master_fighter_table",".csv"), header=TRUE, sep=",")
card <- read.csv(file=paste(local_path,"up_card",".csv"), header=TRUE, sep=",")
odds <- read.csv(file=paste(local_path,"odds",".csv"), header=TRUE, sep=",")
#path <- fighter name or card[]
#fighter <- read.csv(file=paste(local_path,path,".csv"), header=TRUE, sep=",")


#################################################################################
###########################     MANIPULATION     ################################
#################################################################################

#http://www.gamblerspalace.com/lines/martial-arts/ -> fightmetrics 
#namings differ...if no odds then check line naming differences
for(i in seq(1,length(odds[,1]),1)){
  if (odds[i,1] == "LUIS HENRIQUE DA SILVA"){odds[i,1] <- "HENRIQUE DA SILVA"}
  if (odds[i,1] == "JOSH BURKMAN"){odds[i,1] <- "JOSHUA BURKMAN"}
}
#merge odds to upcoming card & add stats 
up_card<-merge(card, odds, by.x="name", by.y="name", all.x=TRUE)
up_card<-get_fights_sums(up_card)
#set up for side b side
up_card[c("against")] <- lapply(up_card[c("against")], toupper)
sbs_up_card<-create_fights_view(up_card)
#up_card = vertical data 
#sbs_up_card = horizontal data


#######IMPORT DRAFT KINGS SALARIES
#######DEFINE VALUES
#######MAKE LINEUP OPTIMIZER (BET THERE ARE GOOD EXAMPLES ON GITHUB)

#Tips from Github
  #optional
# cut weak players off the bat -- salary ge other salary w/ points ge on lesser salary
# add intake from sbs_up_card for fighter pool/settings
  #mandatory
# generate all combos under a salary cap
# cut to highest predicted points



































