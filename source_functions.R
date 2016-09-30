############################################################
##############      UTILITIES     ##########################
############################################################
library("rvest")
library("stringr")
require("combinat")
require("compare")
source("fm_functions.r")


refresh_mma_data<-function(r_events=TRUE, r_evtdist=150, r_odds=TRUE){
  ###############################################################################
  #########################     TO REFRESH      #################################
  ###############################################################################
  if(r_events==TRUE){
  ############################################################
  ##############      GENERATE EVENTS TABLES      ############
  ############################################################
  up_events<-read_html('http://www.fightmetric.com/statistics/events/upcoming')
  dn_events<-read_html('http://www.fightmetric.com/statistics/events/completed?page=all')
  up_events<-create_event_table(up_events)
  dn_events<-create_event_table(dn_events)
  write.csv(up_events, file=paste(local_path,"up_events",".csv"), row.names=FALSE)
  write.csv(dn_events, file=paste(local_path,"dn_events",".csv"), row.names=FALSE)
  ############################################################
  ##############      GENERATE FIGHTER TABLE      ############
  ##############      TAKES A BIT THE LONGER YOU GO   ########
  ############################################################
  dn_urls<-as.list(dn_events[2:as.numeric(r_evtdist),1]) #PAST 3 YEARS (USADA's BEEN IN ~2?)
  fighter_table<-NULL
  for(i in seq(1:length(dn_urls))){
    hld<-get_event_stats(as.character(dn_urls[i]))
    fighter_table<-rbind(fighter_table,hld)
  }
  write.csv(fighter_table, file=paste(local_path,"master_fighter_table",".csv"), row.names=FALSE)
  
  ############################################################
  ##############      GENERATE UPCOMING CARD     #############
  ############################################################
  #get upcoming event card
  fight<-read_html(as.character(up_events[1]))
  event<- fight %>%
    html_nodes("td p") %>%
    html_text()
  
  hld<-NULL;name<-NULL;weight<-NULL;against<-NULL;
  for(i in seq(1,length(event),10)){
    name[1] <- toupper(str_trim(event[i+2]))
    name[2] <- toupper(str_trim(event[i+3]))
    against[1] <- str_trim(event[i+3])
    against[2] <- str_trim(event[i+2])
    weight[1] <- str_trim(event[i+5])
    weight[2] <- str_trim(event[i+5])
    hldrow<-cbind(name,against,weight)
    hld<-rbind(hld,hldrow)
  }
  card<-hld

  # save table for each fighter
  for(i in seq(1,length(card[,2]),1)){
    fighter<-get_fighter_fights(card[i])
    if(is.null(fighter)==FALSE) {fighter<-cbind(fighter,calculate_table_score(fighter))}
    write.csv(fighter, file=paste(local_path,card[i],".csv"), row.names=FALSE)
  }
  
  write.csv(card, file=paste(local_path,"up_card",".csv"), row.names=FALSE)
  }
  if(r_odds==TRUE){
  ############################################################
  ##############      GET VEGAS ODDS              ############
  ############################################################
    odds<-get_betting_odds()
    write.csv(odds, file=paste(local_path,"odds",".csv"), row.names=FALSE)
  }
}




write_sbs_card<-function(x,y,z){
  #################################################################################
  ###########################     MANIPULATION     ################################
  #################################################################################
  odds <- x
  car <- y
  salaries <- z
  #http://www.gamblerspalace.com/lines/martial-arts/ -> fightmetrics 
  #IF ERRORS FIRE ADD N/A NAMING CONVERSION TO FUNCTION

  odds<-fix_odds_names(odds)
  
  #merge odds to upcoming card & add stats 
  up_card<-merge(card, odds, by.x="name", by.y="name", all.x=TRUE)
  up_card<-get_fights_sums(up_card)
  #set up for side b side
  up_card[c("against")] <- lapply(up_card[c("against")], toupper)
  
  #################################################################################
  ############     INSERT DRAFT KINGS SALARIES     ################################
  #################################################################################

  salaries <-salaries[,2:3]
  salaries[,1] <-toupper(salaries[,1])
  hldrow<-NULL;hld<-NULL;salary<-NULL;
  for(i in seq(1,length(up_card[,1]),1)){
    name<-as.character(up_card[i,1])
    for(k in seq(1,length(salaries[,1]),1)){
      if(as.character(salaries[k,1])==name){
        salary<-as.character(salaries[k,2])
      }
    } 
    hldrow<-cbind(up_card[i,], salary)
    hld<-rbind(hld,hldrow)
  }  
  up_card<-hld
  
  #make export/import card#
  sbs_up_card<-create_fights_view(up_card)
  sbs_up_card<-cbind(sbs_up_card, "", "")
  colnames(sbs_up_card)<-c(colnames(sbs_up_card[1:14]),"pool","proj")
  
  #up_card = vertical data 
  #sbs_up_card = horizontal data
  #################################################################################
  ###########################     OUTPUT FORM     #################################
  #################################################################################
  write.csv(sbs_up_card, file=paste0(local_path,"output\\card_form",".csv"), row.names=FALSE)
}




generate_lu_off_card<-function(x,y,z){
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
#    x<-read.csv(file=paste0(local_path,"cards\\","c2.csv"), header=TRUE, sep=",")
#    y<-dk1
#    z<-dk2
  pool_entry <- x
  pool_entry <- pool_entry[,15:16] 
  pool_entry <- pool_entry[complete.cases(pool_entry),]
  
  salaries <- y
  salaries <-salaries[,1:4]
  salaries[salaries==""]<-NA
  salaries <-salaries[complete.cases(salaries),]
  salaries[,2] <-toupper(salaries[,2])
  
  ids <- z
  ids <-ids[,1:2]
  ids[ids==""]<-NA
  ids <-ids[complete.cases(ids),]
  ids[,2] <-toupper(ids[,2])
 
  
  x<-pool_entry
  y<-salaries
  hldrow<-NULL;hld<-NULL;
  for(i in seq(1,length(x[,1]),1)){
    name<-as.character(x[i,1])
    for(k in seq(1,length(y[,1]),1)){
      if(as.character(y[k,2])==name){
        salary<-as.character(y[k,3])
      }
    } 
    hldrow<-cbind(x[i,], salary)
    hld<-rbind(hld,hldrow)
  }  
  
  pool_f<-hld
  
  #ALL POSSIBLE COMBOS GIVEN ENTRY
  #SUMS PROJECTION AND SALARY (FROM ENTRY)
  x <- pool_f
  hld<-combn(x[,1],6)
  hld<-as.data.frame(hld)
  hldrow1<-NULL;hldrow2<-NULL;
  for(i in seq(1,length(colnames(hld)),1)){
    score<-0
    salary<-0
    for(j in seq(1,length(hld[,1]),1)){
      name <- hld[j,i]
      for(k in seq(1,length(x[,1]),1)){
        if(as.character(x[k,1]) == as.character(name)){
          score<-score + as.numeric(as.character(x[k,2]))
          salary<-salary + as.numeric(as.character(x[k,3]))
        }
      }
    }
    hldrow1<-cbind(hldrow1,as.character(score));hldrow2<-cbind(hldrow2,as.character(salary))
  }
  colnames(hldrow1)<-colnames(hld);colnames(hldrow2)<-colnames(hld)
  hld<-rbind(hld,hldrow1,hldrow2)
  
  #CUTTING OVER L/U LIMIT / UNDER MEAN POINT VALUE
  hldf<-NULL
  for(i in seq(1,length(colnames(hld)),1)){
    if(as.numeric(as.character(hld[8,i])) <= 50000 & as.numeric(as.character(hld[8,i])) >= 47000){
      if(is.null(hldf)==TRUE){
        hldf<-as.vector(hld[,i])
      } else {
        hldf<-cbind(hldf, as.vector(hld[,i]))
      }
    }
  }

  if(is.null(hldf) == FALSE){
    
    total_combos<-hldf
  
    x<-total_combos
    y<-ids
    hld<-NULL
    for(i in seq(1,length(x[1,]),1)){
      hldrow<-c(x[1,i],x[2,i],x[3,i],x[4,i],x[5,i],x[6,i],x[7,i])
      hld<-rbind(hld,hldrow)
    }
    
    dklus<-NULL;hr<-NULL;
    for(k in seq(1,length(hld[,1]),1)){
      for(i in seq(1,length(hld[1,]),1)){
        name<-as.character(hld[k,i])
        for(j in seq(1,length(y[,1]),1)){
          if(as.character(y[j,2])==name){
            if(is.null(hr)==FALSE){hr<-cbind(hr, as.character(y[j,1]))}
            else {hr<-as.character(y[j,1]);}
          }
        }
      }
      hr<-cbind(hr,as.character(hld[k,7]))
      dklus<-rbind(dklus,hr)
      hr<-NULL
    }
    colnames(dklus)<-c("F","F","F","F","F","F","pro")
    rownames(dklus)<-c(seq(1,length(dklus[,1]),1))
    dk_lineups<-dklus
    
    return(dk_lineups)
  }
}


check_diff_ooo_by_dim<-function(x){
  skip<-FALSE;hldfinal<-NULL;
  for(i in seq(1,length(as.character(x[,1])),1)){
    for(j in i+1:length(as.character(x[,1]))){
      if(!isTRUE(skip)){
        skip<-compareIgnoreAttrs(x[i,1:5],x[j,1:5])
      }
    }
    if(!isTRUE(skip)){
      if(is.null(hldfinal)==TRUE){
        hldfinal<-x[i,]
      }else{
        hldfinal<-rbind(hldfinal,x[i,])
      }
    }
    skip<-FALSE
  }
  return(hldfinal)
}

