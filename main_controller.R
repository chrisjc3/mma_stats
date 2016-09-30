###############################################################################
#########################     LOCAL PATH      #################################
#########################     DIR NEEDS output\\ & cards\\      ###############
###############################################################################
local_path<-"C:\\git\\fightmetrics\\data\\"

###############################################################################
#########################     TO REFRESH      #################################
###############################################################################
refresh_mma_data(r_events=FALSE, r_evtdist=150, r_odds=FALSE)
#TRUE TO REFRESH..150 is about 3-4 years out#

#################################################################################
###########################     TO LOAD     #####################################
#################################################################################
up_events <- read.csv(file=paste(local_path,"up_events",".csv"), header=TRUE, sep=",")
dn_events <- read.csv(file=paste(local_path,"dn_events",".csv"), header=TRUE, sep=",")
fighter_table <- read.csv(file=paste(local_path,"master_fighter_table",".csv"), header=TRUE, sep=",")
card <- read.csv(file=paste(local_path,"up_card",".csv"), header=TRUE, sep=",")
odds <- read.csv(file=paste(local_path,"odds",".csv"), header=TRUE, sep=",")
# GENERATE FROM DRAFTKINGS (EDIT LINEUPS -> DOWNLOAD AFTER SLATE IS ESTABLISHED) #
dk <- read.csv(file=paste0(local_path,"output\\DKSalaries",".csv"), header=TRUE, sep=",")
###########SPECIFIC FIGHTER PATHS################################################
#path <- fighter name or card[]
#fighter <- read.csv(file=paste(local_path,path,".csv"), header=TRUE, sep=",")

#################################################################################
###########################     WRITE SIDE BY SIDE CARD    ######################
#################################################################################
write_sbs_card()#WRITE AND SAVE CARDS OFF TEMPLATE TO cards\\

#################################################################################
###########################     LOAD CARDS in cards\\     #######################
#################################################################################
crdlst <- list.files(paste0(local_path,"\\cards"))
for(i in seq(1,length(crdlst),1)){

  crd <- crdlst[i]
  card_ent<-read.csv(file=paste0(local_path,"cards\\",crd), header=TRUE, sep=",")
  dk_lineups<-generate_lu_off_card(card_ent, dk)
  
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
}

