library(readxl)
library(RColorBrewer)
library(ggplot2)
data<-read_excel("Exp01_résultats_brut_totauxJ21.xlsx")



# Functions ---------------------------------------------------------------

cumu120<-function(to_work,sat_max){
  cumul<-vector()
  for(i in 1:nrow(to_work)){
    if(i<=sat_max){
      cumul[i]<-sum(to_work[1:i,1])
    }
    else{
      cumul[i]<-cumul[i-1]+to_work[i,1]-to_work[i-sat_max,1]
    }
  }
  to_work<-data.frame(to_work,cumulative=cumul,index=1:length(cumul))
  to_work
}
