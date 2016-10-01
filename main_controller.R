###############################################################################
#########################     LOCAL PATH      #################################
#########################     DIR NEEDS output\\ & cards\\      ###############
###############################################################################
local_path<-"C:\\git\\fightmetrics\\data\\"
source("source_functions.r")

###############################################################################
#########################     TO REFRESH      #################################
###############################################################################
refresh_mma_data(r_events=FALSE, r_evtdist=150, r_odds=FALSE)
#TRUE TO REFRESH..150 is about 3-4 years out#
#RETURNS NOTHING -> PROCEED @ TO LOAD
  #^^^^^^^^
  #function improvement:
    #noticing fights occuring in UFC 2015 that are not listed
      #check larger r_evtdist
    #function could use some sort of validation on page acquisition (...)

#################################################################################
###########################     TO LOAD     #####################################
#################################################################################
up_events <- read.csv(file=paste(local_path,"up_events",".csv"), header=TRUE, sep=",")
dn_events <- read.csv(file=paste(local_path,"dn_events",".csv"), header=TRUE, sep=",")
fighter_table <- read.csv(file=paste(local_path,"master_fighter_table",".csv"), header=TRUE, sep=",")
card <- read.csv(file=paste(local_path,"up_card",".csv"), header=TRUE, sep=",")
odds <- read.csv(file=paste(local_path,"odds",".csv"), header=TRUE, sep=",")
dk2 <- read.csv(file=paste0(local_path,"output\\DKSalaries",".csv"), header=TRUE, sep=",")
###########SPECIFIC FIGHTER PATHS################################################
#path <- fighter name or card[]
#fighter <- read.csv(file=paste(local_path,path,".csv"), header=TRUE, sep=",")
#I like to check them out manually for reference
#################################################################################
###########################     WRITE SIDE BY SIDE CARD    ######################
#################################################################################
write_sbs_card(odds,card,dk2)#WRITE AND SAVE CARDS OFF TEMPLATE TO cards\\
#RETURNS NOTHING...MANUALLY PICK FIGHTERS AND SAVE CARDS FROM HERE#

#################################################################################
###########################     LOAD CARDS in cards\\     #######################
#################################################################################
crdlst <- list.files(paste0(local_path,"\\cards"))

for(i in seq(1,length(crdlst),1)){
  crd <- crdlst[i]
  card_ent<-read.csv(file=paste0(local_path,"cards\\",crd), header=TRUE, sep=",")
  dk_lineups<-NULL
  dk_lineups<-generate_lu_off_card(card_ent,dk2)
  write.table(dk_lineups, file=paste0(local_path,"output\\DK_LINE",".csv"), sep=",", row.names=FALSE,col.names=TRUE)
}
print(paste0("DONE!"))
