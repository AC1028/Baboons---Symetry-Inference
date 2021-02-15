
# Function for selecting the number of blocks -----------------------------

get_nblocks<-function(data){
  names<-unique(data$Nom)
  avgs<-list()
  for (i in 1:length(names)){
    avgs[[i]]<-list()
    P1<-data[data$Nom==names[i],]
    phases<-unique(P1$Condition_Exp)
    for (j in 1:length(phases)){
      P2<-P1[P1$Condition_Exp==phases[j],]
      if(P2$Error_pair1[nrow(P2)]<=4 &P2$error_pair2[nrow(P2)]<=4&nrow(P2)%%40==0){
        finished=TRUE
      }
      else{
        finished=FALSE
      }
      avgs[[i]][[j]]<-data.frame(Name=names[i],
                                 Phase=phases[j],
                                 Type=unique(P2$Phase),
                                 Blocks=max(P2$nbloc),
                                 Finished=finished)
    }
    avgs[[i]]<-do.call(rbind,avgs[[i]])
  }
  avgs<-do.call(rbind,avgs)
  avgs
}


# Keep only monkeys with n phases -----------------------------------------

# keep_only_some_monkeys<-function(data,n,mod="equal"){
#   if (mod!="less" & mod!="equal" & mod!="more"){
#     stop("The mod parameter has to be either less,equal or more")
#   }
#   names<-unique(data$Name)
#   avgs2<-list()
#   k<-1
#   if (mod == "less"){
#     for (i in 1:length(names)){
#       P1<-data[data$Name==names[i],]
#       if(nrow(P1)<=n){
#         avgs2[[k]]<-P1
#         k<-k+1
#       }
#     }
#     avgs2<-do.call(rbind,avgs2)
#   }
#   if (mod == "equal"){
#     for (i in 1:length(names)){
#       P1<-data[data$Name==names[i],]
#       if(nrow(P1)==n){
#         avgs2[[k]]<-P1
#         k<-k+1
#       }
#     }
#     avgs2<-do.call(rbind,avgs2)
#   }
#   if (mod == "more"){
#     for (i in 1:length(names)){
#       P1<-data[data$Name==names[i],]
#       if(nrow(P1)>=n){
#         avgs2[[k]]<-P1
#         k<-k+1
#       }
#     }
#     avgs2<-do.call(rbind,avgs2)
#   }
#   avgs2
# }

# Keep the first n phases of a monkey --------------------------------------



# keep_first_n_phases<-function(data,n){
#   names<-unique(data$Name)
#   avgs2<-list()
#   k<-1
#   for (i in 1:length(names)){
#     P1<-data[data$Name==names[i],]
#     if(nrow(P1)>=n){
#       avgs2[[k]]<-P1[1:n,]
#       k<-k+1
#     }
#   }
#   avgs2<-do.call(rbind,avgs2)
#   avgs2
# }

# Second variant - better, to replace old ones with this

keep_phases<-function(data,to_keep){
  names<-unique(data$Name)
  avgs2<-list()
  k<-1
  phases<-gsub(" ","",paste("phase",to_keep))
  for (i in 1:length(names)){
    P1<-data[data$Name==names[i],]
    if(nrow(P1)>=max(to_keep)&P1$Finished[max(to_keep)]==TRUE){
      avgs2[[k]]<-P1[P1$Phase %in% phases,]
      k<-k+1
    }
  }
  avgs2<-do.call(rbind,avgs2)
  avgs2
}

# Get the phase each monkey reached ---------------------------------------

get_reached_phases<-function(data){
  names<-unique(data$Nom)
  phases<-data.frame(Name=0,Phases=0)
  for(i in 1:length(names)){
    pha<-unique(data[data$Nom==names[i],]$Condition_Exp)
    pha<-max(as.numeric(substr(pha,6,7)))
    phases[i,]<-c(names[i],pha)
  }
  phases
}

# Get the names of the monkeys that reached phase n -----------------------

reached_phase_n<-function(data,n){
  phases<-get_reached_phases(data)
  phases<-phases[phases$Phases==n,]
  phases$Name
}


# Get 0 and 1 series for one monkey ---------------------------------------

get_01_for_monkey<-function(check_RT,name){
  Mon_RT<-list()
  for(i in 1:length(check_RT[[name]])){
    Mon_RT[[i]]<-list()
    for(j in 1:2){
      Mon_RT[[i]][[j]]<-data.frame(Phase=rep(i,length(check_RT[[name]][[i]][[j]])),Score=rep(j-1,length(check_RT[[name]][[i]][[j]])),RT=check_RT[[name]][[i]][[j]])
    }
  }
  for( i in 1:length(check_RT[[name]])){
    Mon_RT[[i]]<-do.call(rbind,Mon_RT[[i]])
  }
  Mon_RT<-do.call(rbind,Mon_RT)
  Mon_RT
}

# Perform 0-1 Analysis ----------------------------------------------------

perform_01<-function(check_RT,name,wilcox_PI){
  Monkey_RT<-get_01_for_monkey(check_RT,name)
  p1<-ggplot(Monkey_RT)+aes(group=Score,y=RT)+geom_boxplot()+facet_wrap(~Phase)+ggtitle(name)
  a1<-aggregate(RT~Score+Phase,data=Monkey_RT,mean)
  diffs<-data.frame(Phase=0,Diff=0)
  for(i in seq(2,nrow(a1),2)){
    diffs[i/2,]<-c(gsub(" ","",paste("phase",a1[i,]$Phase)),a1[i,]$RT-a1[i-1,]$RT)
  }
  a2<-merge(wilcox_PI[wilcox_PI$Name==name,],diffs,by="Phase",all=TRUE)
  p2<-ggplot(a2)+aes(x=factor(substr(Phase,6,7),levels = c("1","2","3","4","5","6","7","8","9","10","11","12")),y=as.numeric(Diff),fill=different)+geom_col()+
    xlab("Number of Phase")+
    ylab("Time difference between a correct and a wrong answer")+
    ggtitle(name)
  grid.arrange(p1,p2,ncol=2)
  a2
}
