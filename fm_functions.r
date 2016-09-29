
create_event_table<-function(x){
  ####################GET UPCOMING EVENT URLS#####################
  event_urls<- x %>%
    html_nodes("i a") %>%
    html_attrs() %>%
    as.list()
  
  tags<-("http\\:(//www.fightmetric.com\\/event-details\\/)(.){16}")
  event_urls<-str_extract_all(event_urls, tags)
  
  ####################GET UPCOMING EVENT DATES#####################
  event_dates<- x %>%
    html_nodes("i span") %>%
    html_text() %>%
    as.list()
  
  tags<-("[A-Z]\\w+\\s\\d\\d.\\s\\d\\d\\d\\d")
  event_dates<-str_extract_all(event_dates,tags)
  
  y<-cbind(event_urls,event_dates)
  return(y)
}


get_event_stats<-function(x){
  ############# GET FIGHTERS BASED ON EVENT URL #######
  fight<-read_html(x)
  stats<- fight %>%
    html_nodes("td p") %>%
    html_text()
  
  ########## LOOP FIGHTERS TO TABLE ###################
  hld<-NULL;wincol<-NULL;name<-NULL;
  strikes<-NULL;takedowns<-NULL;subattempts<-NULL;
  pospass<-NULL;weightclass<-NULL;result<-NULL;
  round<-NULL;time<-NULL;
  
  for(i in seq(1,length(stats),16)){
    if(str_trim(stats[i]) == "win"){
      wincol[1] <- 1
      wincol[2] <- 0
      name[1] <- toupper(str_trim(stats[i+1]))
      name[2] <- toupper(str_trim(stats[i+2]))
      strikes[1] <- str_trim(stats[i+3])
      strikes[2] <- str_trim(stats[i+4])
      takedowns[1] <- str_trim(stats[i+5])
      takedowns[2] <- str_trim(stats[i+6])
      subattempts[1] <- str_trim(stats[i+7])
      subattempts[2] <- str_trim(stats[i+8])
      pospass[1] <- str_trim(stats[i+9])
      pospass[2] <- str_trim(stats[i+10])
      weightclass[1] <- str_trim(stats[i+11])
      weightclass[2] <- str_trim(stats[i+11])
      result[1] <- str_trim(stats[i+12])
      result[2] <- str_trim(stats[i+12])
      round[1] <- str_trim(stats[i+14])
      round[2] <- str_trim(stats[i+14])
      time[1] <- str_trim(stats[i+15])
      time[2] <- str_trim(stats[i+15])
      hldrow<-cbind(wincol,name,strikes,takedowns,subattempts,pospass,weightclass,result,round,time)
      hld<-rbind(hld,hldrow)
    }
    if(str_trim(stats[i]) == "draw"){
      wincol[1] <- ""
      wincol[2] <- ""
      name[1] <- toupper(str_trim(stats[i+2]))
      name[2] <- toupper(str_trim(stats[i+3]))
      strikes[1] <- str_trim(stats[i+4])
      strikes[2] <- str_trim(stats[i+5])
      takedowns[1] <- str_trim(stats[i+6])
      takedowns[2] <- str_trim(stats[i+7])
      subattempts[1] <- str_trim(stats[i+8])
      subattempts[2] <- str_trim(stats[i+9])
      pospass[1] <- str_trim(stats[i+10])
      pospass[2] <- str_trim(stats[i+11])
      weightclass[1] <- str_trim(stats[i+12])
      weightclass[2] <- str_trim(stats[i+12])
      result[1] <- str_trim(stats[i+13])
      result[2] <- str_trim(stats[i+13])
      round[1] <- str_trim(stats[i+14])
      round[2] <- str_trim(stats[i+14])
      time[1] <- str_trim(stats[i+15])
      time[2] <- str_trim(stats[i+15])
      hldrow<-cbind(wincol,name,strikes,takedowns,subattempts,pospass,weightclass,result,round,time)
      hld<-rbind(hld,hldrow)
    }
    
  }
  return(hld)
}


pull_upcoming_fighters<-function(x){
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
  return(hld)
}


get_fighter_fights<-function(x){
  hld<-NULL
  for(i in seq(1,length(fighter_table[,2]),1)){
    if(as.character(x) == as.character(fighter_table[i,2])){
      if(fighter_table[i,1] == "1"){
        hldf <- fighter_table[i,]
        hldo <- fighter_table[i+1,]
        hld<-rbind(hld,hldf,hldo)
      }
      if(fighter_table[i,1] == "0"){
        hldf <- fighter_table[i,]
        hldo <- fighter_table[i-1,]
        hld<-rbind(hld,hldf,hldo)
      }
    }
  }
  return(hld)
}


calculate_table_score<-function(x){
  hld<-NULL
  for(i in seq(1,length(x[,c("name")]),1)){
    score<-0
    if(x[i,c("wincol")]=="1"){
      if(str_detect(x[i,c("result")],"KO")==TRUE | str_detect(x[i,c("result")],"SUB")==TRUE){
        if(x[i,c("round")]=="1"){score<-100}
        if(x[i,c("round")]=="2"){score<-70}
        if(x[i,c("round")]=="3"){score<-50}
        if(x[i,c("round")]=="4"){score<-40}
        if(x[i,c("round")]=="5"){score<-40}
      }
      if(str_detect(x[i,c("result")],"DEC")==TRUE){score<-25}
    }
    score <- score + (as.numeric(x[i,c("strikes")])*.5)
    score <- score + (as.numeric(x[i,c("takedowns")])*2)
    hld <- rbind(hld,score)
    colnames(hld)<-c("score")
  }
  #MIGHT WANT TO PUT A SUM ROW!
  return(hld)
}


get_betting_odds<-function(x){
  vegas<-read_html('http://www.gamblerspalace.com/lines/martial-arts/')
  odds<- vegas %>%
    html_nodes("tr td") %>%
    html_text()
  up_odds<-NULL
  for(i in seq(1,length(odds),1)){
    if(str_detect(odds[i],"^[A-Z][a-z]+\\s.+") & str_trim(odds[i]) != "Bet Now"){
      h1<-toupper(odds[i])
      h2<-odds[i+3]
      hld<-cbind(h1,h2)
      up_odds<-rbind(up_odds,hld)
    }
  }
  colnames(up_odds)<-c("name","ou")
  return(up_odds)
}


get_fights_sums<-function(x){
  hld<-NULL
  for(i in seq(1,length(x[,1]),1)){
    fights<-NULL
    try(fights<-read.csv(file=paste(local_path,x[i,c("name")],".csv")), TRUE)
    if(is.null(fights)==FALSE){
      fights<-as.data.frame(fights)
      fights<-fights[fights$name==as.character(x[i,c("name")]),c("score")]
      fights<-summary(fights)
      fights<-fights[c("Min.", "Mean", "Max.")]
      h1<-x[i,]
      h2<-cbind(fights[1],fights[2],fights[3])
      colnames(h2)<-c("min","avg","max")
      hr<-cbind(h1,h2)
      hld<-rbind(hld,hr)
    } else {
      hr<-cbind(x[i,], "NA","NA","NA")
      colnames(hr)<-c(colnames(x),"min","avg","max")
      hld<-rbind(hld,hr)
    }
  }
  return(hld)
}



create_fights_view<-function(x){
  hld<-NULL;nmlst<-NULL;
  for(i in seq_along(x[,1])){
    name<-as.character(x[i,1])
    found <-FALSE
    if(is.null(nmlst)==TRUE){
      for(j in seq_along(x[,1])){
        if(name == as.character(x[j,2])){
          h1<-as.data.frame(x[i,])
          names(h1)<-paste0(names(h1),"_f1")
          h1<-h1[,!(names(h1) %in% c("against_f1"))]
          h2<-as.data.frame(x[j,])
          names(h2)<-paste0(names(h2),"_f2")
          h2<-h2[,!(names(h2) %in% c("against_f2"))]
          hr<-cbind(h1,h2)
          hld<-rbind(hld,hr)
          nmlst<-c(as.character(x[j,1]),as.character(x[j,2]))
          hr<-NULL
        }
      }
    }
    for(k in seq_along(nmlst)){if(name == as.character(nmlst[k])){found<-TRUE}}
    if(found == FALSE){
      for(j in seq_along(x[,1])){
        if(name == as.character(x[j,2])){
          h1<-as.data.frame(x[i,])
          names(h1)<-paste0(names(h1),"_f1")
          h1<-h1[,!(names(h1) %in% c("against_f1"))]
          h2<-as.data.frame(x[j,])
          names(h2)<-paste0(names(h2),"_f2")
          h2<-h2[,!(names(h2) %in% c("against_f2"))]
          hr<-cbind(h1,h2)
          hld<-rbind(hld,hr)
          nmlst<-c(nmlst, as.character(x[j,1]), as.character(x[j,2]))
          hr<-NULL
        }
      }
    } 
  }
  return(hld)
}


append_dk<-function(x,y){
  hldrow<-NULL;hld<-NULL;
  for(i in seq(1,length(x[,1]),1)){
    name<-as.character(x[i,1])
    for(k in seq(1,length(y[,1]),1)){
      if(as.character(y[k,2])==name){
        salary<-as.character(y[k,3])
        id<-as.character(y[k,1])
      }
    } 
    hldrow<-cbind(x[i,], id, salary)
    hld<-rbind(hld,hldrow)
  }  
  return(hld)
}





fix_odds_names<-function(x){
  hr<-NULL;hld<-NULL
  for(i in seq(1,length(x[,1]),1)){
    if (x[i,1] == "LUIS HENRIQUE DA SILVA"){hr <- cbind("HENRIQUE DA SILVA", x[i,2])}
    else if (x[i,1] == "JOSH BURKMAN"){hr <- cbind("JOSHUA BURKMAN", x[i,2])}
    ###ADD MORE AS FOUND (WILL SHOW WARNINGS IF FAILS)###
    else {hr<-x[i,]}
    colnames(hr)<-colnames(x)
    hld<-rbind(hld,hr)
  }
  return(hld)
}




get_all_lineups<-function(x){
  #ALL POSSIBLE COMBOS GIVEN ENTRY
  #SUMS PROJECTION AND SALARY (FROM ENTRY)
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
  #SALARY CAP CUT
  hldf<-NULL
  for(i in seq(1,length(colnames(hld)),1)){
    if(as.numeric(as.character(hld[8,i])) <= 50000){
      if(is.null(hldf)==TRUE){
        hldf<-as.vector(hld[,i])
      } else {
        hldf<-cbind(hldf, as.vector(hld[,i]))
      }
    }
  }
  return(hldf)
}


append_dk_salary<-function(x,y){
  hldrow<-NULL;hld<-NULL;
  for(i in seq(1,length(x[,1]),1)){
    name<-as.character(x[i,1])
    for(k in seq(1,length(y[,1]),1)){
      if(as.character(y[k,2])==name){
        salary<-as.character(y[k,4])
      }
    } 
    hldrow<-cbind(x[i,], salary)
    hld<-rbind(hld,hldrow)
  }  
  return(hld)
}



format_to_dk<-function(x,y){
  hld<-NULL
  for(i in seq(1,length(x[1,]),1)){
    hldrow<-c(x[1,i],x[2,i],x[3,i],x[4,i],x[5,i],x[6,i])
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
    dklus<-rbind(dklus,hr)
    hr<-NULL
  }
  colnames(dklus)<-c("F","F","F","F","F","F")
  rownames(dklus)<-c(seq(1,length(dklus[,1]),1))
  return(dklus)
}








