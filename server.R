#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

# Outputs -----------------------------------------------------------------

    
    output$name_select<-renderUI({
        selectInput(inputId = "names", label = "Select the names: ", 
                    selected = unique(data[which(data$Age >min(input$age) & data$Age<max(input$age) & (data$Sexe %in% input$sex)),]$Nom)[1],
                    choices = unique(data[which(data$Age >min(input$age) & data$Age<max(input$age)& (data$Sexe %in% input$sex)),]$Nom), multiple = TRUE)
    })
    output$name_select1<-renderUI({
        selectInput(inputId = "names1", label = "Select the names: ", 
                    selected = unique(data[which(data$Age >min(input$age1) & data$Age<max(input$age1) & (data$Sexe %in% input$sex1)),]$Nom)[1],
                    choices = unique(data[which(data$Age >min(input$age1) & data$Age<max(input$age1)& (data$Sexe %in% input$sex1)),]$Nom), multiple = FALSE)
    })
    output$name_select3<-renderUI({
        selectInput(inputId = "names3", label = "Select the names: ", 
                    selected = unique(data[which(data$Age >min(input$age3) & data$Age<max(input$age3) & (data$Sexe %in% input$sex3)),]$Nom)[1],
                    choices = unique(data[which(data$Age >min(input$age3) & data$Age<max(input$age3)& (data$Sexe %in% input$sex3)),]$Nom), multiple = FALSE)
    })
    output$phase_select<-renderUI({
        selection<-data[data$Nom==input$names1,]
        selectInput(inputId = "phases1", label = "Select the phase: ", selected = unique(selection[selection$Phase%in%input$phase_type,]$Condition_Exp)[1],
                    choices = unique(selection[selection$Phase%in%input$phase_type,]$Condition_Exp), multiple = TRUE)
    })
    output$name2<-renderUI({
        selection<-data[which(data$Age >min(input$age2) & data$Age<max(input$age2) & (data$Sexe %in% input$sex2)),]$Nom
        selectInput(inputId = "names2", 
                    label = "Select the names: ", 
                    selected = unique(selection)[1],
                    choices = unique(selection), 
                    multiple = FALSE)
    })
    output$phase_select2<-renderUI({
        selection<-data[data$Nom==input$names2,]
        selectInput(inputId = "phases2", label = "Select the phase: ", selected = unique(selection[selection$Phase%in%input$phase_type2,]$Condition_Exp)[1],
                    choices = unique(selection[selection$Phase%in%input$phase_type2,]$Condition_Exp), multiple = TRUE)
    })
    output$phase_select3<-renderUI({
        selection<-data[data$Nom==input$names3,]
        selectInput(inputId = "phases3", label = "Select the phase: ", selected = unique(selection[selection$Phase%in%input$phase_type3,]$Condition_Exp)[1],
                    choices = unique(selection[selection$Phase%in%input$phase_type3,]$Condition_Exp), multiple = FALSE)
    })
    output$index_select3<-renderUI({
        sliderInput(inputId = "index3",label = "Select an index range",
                    min=1,max=nrow(data[data$Nom==input$names3&data$Condition_Exp==input$phases3,])/2+1,step=1,value=c(1,100))
    })

# Preliminary plots -------------------------------------------------------

    
    plotInput_learn<-reactive({
        # generate bins based on input$bins from ui.R
        names<-input$names
        question<-unique(data$Pair)
        blocks<-sum(as.numeric(input$blocks))
        P1<-data[which(data$Condition_Exp==input$phases),]
        avgs<-list()
        for ( i in 1:length(names)){
            avgs[[i]]<-list()
            P2<-P1[which(P1$Nom==names[i]),]
            for(k in 1:length(question)){
                avgs[[i]][[k]]<-vector()
                P3<-P2[which(P2$Pair==question[k]),]
                score<-P3$Score
                sum<-0
                for (j in 1:((length(score)%/%20)*20)){
                    sum<-sum+score[j]
                    avgs[[i]][[k]][j]<-sum/j
                }
                avgs[[i]][[k]]<-data.frame(index=1:length(avgs[[i]][[k]]),Nom=names[i],avg=avgs[[i]][[k]],Pair=question[k],avg_block=NA)
                for (j in 0:(length(score)%/%20-1)){
                    avgs[[i]][[k]][(j*20+20),5]<-sum(score[(j*20+1):(j*20+20)])/20
                }
            }
        }
        for (i in 1:length(avgs)){
            if(input$av==1){
                temp<-data.frame(index=avgs[[i]][[1]]$index,
                                 Nom=avgs[[i]][[1]]$Nom,
                                 avg=(avgs[[i]][[1]]$avg+avgs[[i]][[2]]$avg)/2,
                                 Pair=rep("avg of pairs",length(avgs[[i]][[1]]$Pair)),
                                 avg_block=(avgs[[i]][[1]]$avg_block+avgs[[i]][[2]]$avg_block)/2)
                avgs[[i]]<-do.call(rbind,avgs[[i]])
                avgs[[i]]<-rbind(avgs[[i]],temp)
            }
            else{
                avgs[[i]]<-do.call(rbind,avgs[[i]])
            }
        }
        
        to_plot<-do.call(rbind,avgs)
        to_plot<-to_plot[which(to_plot$Pair%in%input$question | to_plot$Pair=="avg of pairs"),]
        if (input$wrap==TRUE){
            if(blocks==1){
                ggplot(to_plot)+aes(y=avg_block,x=index,colour=interaction(Nom,Pair,sep="-"))+
                    facet_wrap(~Nom)+
                    labs(colour="Name and question")+
                    geom_point()+
                    geom_line(data=(na.omit(to_plot)),aes(y=avg_block,x=index,colour=interaction(Nom,Pair,sep="-")))+
                    ylim(0,1)+
                    geom_hline(yintercept=0.8,color="red",linetype="dashed")
            }
            else if(blocks==2){
                ggplot(to_plot)+
                    aes(y=avg,x=index,colour=interaction(Nom,Pair,sep="-"))+
                    geom_line()+
                    facet_wrap(~Nom)+
                    labs(colour="Name and question")+
                    ylim(0,1)
            }
            else if(blocks==3){
                ggplot(to_plot)+
                    aes(y=avg,x=index,colour=interaction(Nom,Pair,sep="-"))+
                    geom_line()+
                    facet_wrap(~Nom)+
                    labs(colour="Name and question")+
                    geom_point(aes(y=avg_block,x=index,colour=interaction(Nom,Pair,sep="-")))+
                    geom_line(data=(na.omit(to_plot)),aes(y=avg_block,x=index,colour=interaction(Nom,Pair,sep="-")))+
                    ylim(0,1)+
                    geom_hline(yintercept=0.8,color="red",linetype="dashed")
                
            }
        }
        else{
            if(blocks==1){
                ggplot(to_plot)+aes(y=avg_block,x=index,colour=interaction(Nom,Pair,sep="-"))+
                    labs(colour="Name and question")+
                    geom_point()+
                    geom_line(data=(na.omit(to_plot)),aes(y=avg_block,x=index,colour=interaction(Nom,Pair,sep="-")))+
                    ylim(0,1)+
                    geom_hline(yintercept=0.8,color="red",linetype="dashed")
            }
            else if(blocks==2){
                ggplot(to_plot)+
                    aes(y=avg,x=index,colour=interaction(Nom,Pair,sep="-"))+
                    geom_line()+
                    labs(colour="Name and question")+
                    ylim(0,1)
            }
            else if(blocks==3){
                ggplot(to_plot)+
                    aes(y=avg,x=index,colour=interaction(Nom,Pair,sep="-"))+
                    geom_line()+
                    labs(colour="Name and question")+
                    geom_point(aes(y=avg_block,x=index,colour=interaction(Nom,Pair,sep="-")))+
                    geom_line(data=(na.omit(to_plot)),aes(y=avg_block,x=index,colour=interaction(Nom,Pair,sep="-")))+
                    ylim(0,1)+
                    geom_hline(yintercept=0.8,color="red",linetype="dashed")
                
            }
        }
    })
    output$learnplot <- renderPlot({
        print(plotInput_learn())
    })
    output$dow_performance<-downloadHandler(
        filename = function(){paste("plot_performance.png")},
        content = function(file) {
            dev.new()
            ggsave(file, plot = plotInput_learn(), device = "png")
            dev.off()
            
        }
    )
    

# Age plot ----------------------------------------------------------------

    
#    output$ageplot <- renderPlot({
 #       phases<-input$phases11
 #       nbloks<-list()
 #       for ( i in 1:length(phases)){
 #           P1<-data[which(data$Condition_Exp==phases[i]),]
 #           nbloks[[i]]<-matrix(0,length(unique(P1$Nom)),4)
 #           names<-unique(P1$Nom)
 #           for (j in 1:length(names)){
 #               P2<-P1[which(P1$Nom==names[j]),]
 #               nbloks[[i]][j,1]<-unique(P1$Nom)[j]
 #               nbloks[[i]][j,2]<-max(P2$nbloc)
 #               nbloks[[i]][j,3]<-i
  #              nbloks[[i]][j,4]<-mean(P2$Age)
  #          }
  #      }
  #      to_plot<-do.call(rbind,nbloks)
   #     to_plot<-data.frame(Nom=to_plot[,1],Phase=to_plot[,3],Bloks<-to_plot[,2],Age<-to_plot[,4])
   #     ggplot(to_plot)+aes(y=Bloks,x=Age)+
    #        facet_wrap(~Phase)+
    #        labs(y="Number of blocks to finish this phase",x="Age")+
     #       geom_point()
    #})
   

# Phase plot --------------------------------------------------------------

    
    plotInput_phase<-reactive({
        # generate bins based on input$bins from ui.R
        names<-input$names1
        question<-unique(data$Pair)
        blocks<-sum(as.numeric(input$blocks1))
        phases<-input$phases1
        P1<-data[which(data$Nom==names),]
        avgs<-list()
        for ( i in 1:length(phases)){
            avgs[[i]]<-list()
            P2<-P1[which(P1$Condition_Exp==phases[i]),]
            for(k in 1:length(question)){
                avgs[[i]][[k]]<-vector()
                P3<-P2[which(P2$Pair==question[k]),]
                score<-P3$Score
                sum<-0
                for (j in 1:((length(score)%/%20)*20)){
                    sum<-sum+score[j]
                    avgs[[i]][[k]][j]<-sum/j
                }
                if(input$wrap1==0){
                    if(i==1){
                        avgs[[i]][[k]]<-data.frame(index=1:length(avgs[[i]][[k]]),Nom=names,avg=avgs[[i]][[k]],Pair=question[k],avg_block=NA,phase=phases[i])
                    }
                    else{
                        avgs[[i]][[k]]<-data.frame(index=(avgs[[i-1]][[k]]$index[nrow(avgs[[i-1]][[k]])]+1):(avgs[[i-1]][[k]]$index[nrow(avgs[[i-1]][[k]])]+length(avgs[[i]][[k]])),Nom=names,avg=avgs[[i]][[k]],Pair=question[k],avg_block=NA,phase=phases[i])
                    }
                }
                else{
                    avgs[[i]][[k]]<-data.frame(index=1:length(avgs[[i]][[k]]),Nom=names,avg=avgs[[i]][[k]],Pair=question[k],avg_block=NA,phase=phases[i])
                }
                for (j in 0:(length(score)%/%20-1)){
                    avgs[[i]][[k]][(j*20+20),5]<-sum(score[(j*20+1):(j*20+20)])/20
                }
            }
        }
        for (i in 1:length(avgs)){
            if(input$av1==1){
                temp<-data.frame(index=avgs[[i]][[1]]$index,
                                 Nom=avgs[[i]][[1]]$Nom,
                                 avg=(avgs[[i]][[1]]$avg+avgs[[i]][[2]]$avg)/2,
                                 Pair=rep("avg of pairs",length(avgs[[i]][[1]]$Pair)),
                                 avg_block=(avgs[[i]][[1]]$avg_block+avgs[[i]][[2]]$avg_block)/2,
                                 phase=phases[i])
                avgs[[i]]<-do.call(rbind,avgs[[i]])
                avgs[[i]]<-rbind(avgs[[i]],temp)
            }
            else{
                avgs[[i]]<-do.call(rbind,avgs[[i]])
            }
        }
        
        to_plot<-do.call(rbind,avgs)
        to_plot<-to_plot[which(to_plot$Pair%in%input$question1 | to_plot$Pair=="avg of pairs"),]
        to_plot$phase<-factor(to_plot$phase,levels=c("phase1","phase2","phase3","phase4","phase5","phase6","phase7","phase8","phase9","phase10","phase11","phase12"))
        if (input$wrap1==TRUE){
            if(blocks==1){
                ggplot(to_plot)+aes(y=avg_block,x=index,colour=interaction(phase,Pair,sep="-"))+
                    facet_wrap(~phase)+
                    labs(colour="Phase and question")+
                    geom_point()+
                    geom_line(data=(na.omit(to_plot)),aes(y=avg_block,x=index,colour=interaction(phase,Pair,sep="-")))+
                    ylim(0,1)+
                    geom_hline(yintercept=0.8,color="red",linetype="dashed")
            }
            else if(blocks==2){
                ggplot(to_plot)+
                    aes(y=avg,x=index,colour=interaction(phase,Pair,sep="-"))+
                    geom_line()+
                    facet_wrap(~phase)+
                    labs(colour="Phase and question")+
                    ylim(0,1)
            }
            else if(blocks==3){
                ggplot(to_plot)+
                    aes(y=avg,x=index,colour=interaction(phase,Pair,sep="-"))+
                    geom_line()+
                    facet_wrap(~phase)+
                    labs(colour="Phase and question")+
                    geom_point(aes(y=avg_block,x=index,colour=interaction(phase,Pair,sep="-")))+
                    geom_line(data=(na.omit(to_plot)),aes(y=avg_block,x=index,colour=interaction(phase,Pair,sep="-")))+
                    ylim(0,1)+
                    geom_hline(yintercept=0.8,color="red",linetype="dashed")
                
            }
        }
        else{
            if(blocks==1){
                ggplot(to_plot)+aes(y=avg_block,x=index,colour=interaction(phase,Pair,sep="-"))+
                    labs(colour="Phase and question")+
                    geom_point()+
                    geom_line(data=(na.omit(to_plot)),aes(y=avg_block,x=index,colour=interaction(phase,Pair,sep="-")))+
                    ylim(0,1)+
                    geom_hline(yintercept=0.8,color="red",linetype="dashed")
            }
            else if(blocks==2){
                ggplot(to_plot)+
                    aes(y=avg,x=index,colour=interaction(phase,Pair,sep="-"))+
                    geom_line()+
                    labs(colour="Phase and question")+
                    ylim(0,1)
            }
            else if(blocks==3){
                ggplot(to_plot)+
                    aes(y=avg,x=index,colour=interaction(phase,Pair,sep="-"))+
                    geom_line()+
                    labs(colour="Phase and question")+
                    geom_point(aes(y=avg_block,x=index,colour=interaction(phase,Pair,sep="-")))+
                    geom_line(data=(na.omit(to_plot)),aes(y=avg_block,x=index,colour=interaction(phase,Pair,sep="-")))+
                    ylim(0,1)+
                    geom_hline(yintercept=0.8,color="red",linetype="dashed")
                
            }
        }
    })
    output$phaseplot <- renderPlot({
        print(plotInput_phase())
    })
    output$dow_performance1<-downloadHandler(
        filename = function(){paste("plot_phases.png")},
        content = function(file) {
            dev.new()
            ggsave(file, plot = plotInput_phase(), device = "png")
            dev.off()
            
        }
    )

# 2 D Plot ----------------------------------------------------------------
    plotInput_2D<-reactive({
        # generate bins based on input$bins from ui.R
        names<-input$names2
        phases<-input$phases2
        Dist<-0
        if(input$dist_type2==1){
            Dist<-"Do"
        }
        else if(input$dist_type2==2){
            Dist<-"D01"
        }
        else{
            Dist<-"D10"
        }
        
        P1<-data[which(data$Nom==names),]
        avgs<-list()
        limits<-list()
        for ( i in 1:length(phases)){
            
            P2<-P1[which(P1$Condition_Exp==phases[i]),]
            avgs[[i]]<-matrix(0,ncol = 2,nrow=(nrow(P2)%/%40))
            for( j in 1:(nrow(P2)%/%40)){
                avgs[[i]][j,1]<-(20-P2$Error_pair1[j*40])/20
                avgs[[i]][j,2]<-(20-P2$error_pair2[j*40])/20
            }
            avgs[[i]]<-data.frame(Index=1:nrow(avgs[[i]]),Nom=rep(names,nrow(avgs[[i]])),Phase=rep(phases[i],nrow(avgs[[i]])),
                                  Pair1=avgs[[i]][,1],
                                  Pair2=avgs[[i]][,2],
                                  Do=sqrt(avgs[[i]][,1]^2+avgs[[i]][,2]^2),
                                  D01=sqrt((avgs[[i]][,1]-0)^2+(avgs[[i]][,2]-1)^2),
                                  D10=sqrt((avgs[[i]][,1]-0)^2+(avgs[[i]][,2]-1)^2)
                                  )
            limits[[i]]<-data.frame(Nom=rep(names,2),Phase=rep(phases[i],2),Place=c("first","last"),Pair1=c(avgs[[i]][1,4],avgs[[i]][nrow(avgs[[i]]),4]),
                                    Pair2=c(avgs[[i]][1,5],avgs[[i]][nrow(avgs[[i]]),5]))
        }
        to_plot<-do.call(rbind,avgs)
        to_plot$Phase<-factor(to_plot$Phase,levels=c("phase1","phase2","phase3","phase4","phase5","phase6","phase7","phase8","phase9","phase10","phase11","phase12"))
        limits<-do.call(rbind,limits)
        limits$Phase<-factor(limits$Phase,levels=c("phase1","phase2","phase3","phase4","phase5","phase6","phase7","phase8","phase9","phase10","phase11","phase12"))
        ggplot(to_plot)+
            aes(x=Pair1,y=Pair2)+
            geom_point()+
            geom_path(aes(colour=Index))+
            scale_colour_gradient(low = "blue", high = "red")+
            geom_point(data=limits,aes(x=Pair1,y=Pair2),colour="red")+
            facet_wrap(~Phase)+
            annotate("rect", xmin=0.8, xmax=1, ymin=0.8, ymax=1, alpha=0.2, fill="red")+
            ylim(0,1)+
            xlim(0,1)
    })
    output$twoDplot <- renderPlot({
        print(plotInput_2D())
    })
    output$dow_performance2<-downloadHandler(
        filename = function(){paste("plot_2D.png")},
        content = function(file) {
            dev.new()
            ggsave(file, plot = plotInput_2D(), device = "png")
            dev.off()
            
        }
    )
    
# Chunk plot --------------------------------------------------------------
    
    
    plotInput_chunck<-reactive({
        # generate bins based on input$bins from ui.R
        names<-input$names3
        blocks<-sum(as.numeric(input$blocks3))
        phases<-input$phases3
        P1<-data[which(data$Nom==names),]
        P2<-P1[which(P1$Condition_Exp==phases),]
        q1<-1
        q2<-1
        to_work<-data.frame(Rav=P2$Score[1],Chunk=1,Question=P2$Pair[1])
        for ( i in 2:nrow(P2)){
            to_work[i,1]<-P2$Score[i]
            to_work[i,3]<-P2$Pair[i]
            if ( to_work[i,3]==to_work[i-1,3]){
                to_work[i,2]<-to_work[i-1,2]
            }
            else{
                to_work[i,2]<-max(na.omit(to_work[to_work$Question==to_work$Question[i],2]))+1
                if(to_work[i,2]==-Inf){
                    to_work[i,2]<-1
                }
            }
        }    
        to_work1<-to_work[to_work$Question=="paire1",]
        to_work1<-cumu120(to_work1,input$sat_max)
        to_work2<-to_work[to_work$Question=="paire2",]
        to_work2<-cumu120(to_work2,input$sat_max)
        
        to_plot<-rbind(to_work1,to_work2)
        ggplot(to_plot[to_plot$index<max(input$index3)&to_plot$index>min(input$index3),])+
            aes(x=index,y=cumulative,colour=as.factor(Rav),group=as.factor(Chunk))+
            geom_point(show.legend = FALSE)+
            geom_line(show.legend = FALSE)+
            ggtitle(as.character(interaction(names,phases)))+
            facet_wrap(~Question,nrow = 2)
        
    })
    output$chunckplot <- renderPlot({
        print(plotInput_chunck())
    })
    output$dow_performance3<-downloadHandler(
        filename = function(){paste("plot_chuncks.png")},
        content = function(file) {
            dev.new()
            ggsave(file, plot = plotInput_chunck(), device = "png")
            dev.off()
            
        }
    )
    
    
})
