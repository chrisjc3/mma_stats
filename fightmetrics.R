# ############################################################
# ##############      UTILITIES     ##########################
# ############################################################
# library("rvest")
# library("stringr")
# require("combinat")
# require("compare")
source("fm_functions.r")
# local_path<-"C:\\git\\fightmetrics\\data\\"

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


# 
# #################################################################################
# ###########################     TO LOAD     #####################################
# #################################################################################
# up_events <- read.csv(file=paste(local_path,"up_events",".csv"), header=TRUE, sep=",")
# dn_events <- read.csv(file=paste(local_path,"dn_events",".csv"), header=TRUE, sep=",")
# fighter_table <- read.csv(file=paste(local_path,"master_fighter_table",".csv"), header=TRUE, sep=",")
# card <- read.csv(file=paste(local_path,"up_card",".csv"), header=TRUE, sep=",")
# odds <- read.csv(file=paste(local_path,"odds",".csv"), header=TRUE, sep=",")
# #path <- fighter name or card[]
# #fighter <- read.csv(file=paste(local_path,path,".csv"), header=TRUE, sep=",")
# 
# 
# #################################################################################
# ###########################     MANIPULATION     ################################
# #################################################################################
# 
# #http://www.gamblerspalace.com/lines/martial-arts/ -> fightmetrics 
# #IF ERRORS FIRE ADD N/A NAMING CONVERSION TO FUNCTION
# odds <- read.csv(file=paste(local_path,"odds",".csv"), header=TRUE, sep=",")
# odds<-fix_odds_names(odds)
# 
# #merge odds to upcoming card & add stats 
# up_card<-merge(card, odds, by.x="name", by.y="name", all.x=TRUE)
# up_card<-get_fights_sums(up_card)
# #set up for side b side
# up_card[c("against")] <- lapply(up_card[c("against")], toupper)
# 
# #################################################################################
# ############     INSERT DRAFT KINGS SALARIES     ################################
# #################################################################################
# 
# salaries <- read.csv(file=paste0(local_path,"DKSalaries",".csv"), header=TRUE, sep=",")
# salaries <-salaries[,2:3]
# salaries[,1] <-toupper(salaries[,1])
# up_card<-append_dk_salary(up_card,salaries)
# 
# #make export/import card#
# sbs_up_card<-create_fights_view(up_card)
# sbs_up_card<-cbind(sbs_up_card, "", "")
# colnames(sbs_up_card)<-c(colnames(sbs_up_card[1:14]),"pool","proj")
# 
# #up_card = vertical data 
# #sbs_up_card = horizontal data
# #################################################################################
# ###########################     OUTPUT FORM     #################################
# #################################################################################
# write.csv(sbs_up_card, file=paste(local_path,"output\\card_form",".csv"), row.names=FALSE)


#################################################################################
###########################         INPUT FORMS             #####################
########################### 1) card_form.csv w/ POOL entry  #####################
########################### 2) draftkings salary info       #####################
########################### 3) projections = manually input confidence value   ##
#################################################################################


###########################################################################
########MAKE THIS ALL A FUNCTION TO RUN IN A LOOP ON A CARD FOLDER#########
#################       BELOW                                 #############
###########################################################################
pool_entry <- read.csv(file=paste0(local_path,"output\\card_form ",".csv"), header=TRUE, sep=",")
pool_entry <- pool_entry[,15:16] 
pool_entry <- pool_entry[complete.cases(pool_entry),]

salaries <- read.csv(file=paste0(local_path,"output\\DKSalaries",".csv"), header=TRUE, sep=",")
salaries <-salaries[,1:4]
salaries[salaries==""]<-NA
salaries <-salaries[complete.cases(salaries),]
salaries[,2] <-toupper(salaries[,2])

pool_f<-append_dk_salary(pool_entry,salaries)

total_combos<-get_all_lineups_v3(pool_f) #STILL NEEDS TO BE SMARTER
# -> Should take full pool and automatically not place dual combatants

ids <- read.csv(file=paste0(local_path,"output\\DKSalaries",".csv"), header=TRUE, sep=",")
ids <-ids[,1:2]
ids[ids==""]<-NA
ids <-ids[complete.cases(ids),]
ids[,2] <-toupper(ids[,2])

dk_lineups<-format_to_dk(total_combos, ids)

#####################################################
##################  WRITE OUTPUT  ###################
#####################################################

lu_check<-tryCatch({
  read.csv(file=paste0(local_path,"output\\DK_LINE",".csv"), header=TRUE, sep=",")
}, warning = function(w){lu_check<-NULL})
if(is.null(lu_check)==TRUE){
  write.csv(dk_lineups, file=paste0(local_path,"output\\DK_LINE",".csv"), row.names=FALSE)
} else{
  #WriteAppend/De-Dupe/Write#
  write.table(dk_lineups, file=paste0(local_path,"output\\DK_LINE",".csv"), sep=",", row.names=FALSE,col.names = FALSE, append=TRUE)
  luc<-read.csv(file=paste0(local_path,"output\\DK_LINE",".csv"), header=TRUE, sep=",")
  luc<-check_diff_ooo_by_dim(luc)
  colnames(luc)<-c("F","F","F","F","F","F","pro")
  write.table(luc, file=paste0(local_path,"output\\DK_LINE",".csv"), sep=",", row.names=FALSE,col.names = TRUE)
}


###########################################################################
########MAKE THIS ALL A FUNCTION TO RUN IN A LOOP ON A CARD FOLDER#########
#################       ABOVE                                 #############
###########################################################################

##########MIGHT WANT TO MAKE A 'CONTROLLER' AT THIS POINT...

#################################################################################
###########################     FINISH!     #####################################
#################################################################################




















