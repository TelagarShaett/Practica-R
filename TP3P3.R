hospdata<-read.csv("hospital-data.csv")
outcome<-read.csv("outcome-of-care-measures.csv")

#"C:/Users/Julien/Desktop/Ejercicios programacion/Coursera/tp3"

rankhospital<-function(state,cause,rank){
  
  library(plyr)
  
  causes<-c("heart attack","heart failure","pneumonia")
  
  if(is.na(match(state,outcome$State)) == TRUE ){
    
    print("not a valid state")
    stop()
    
  }
  
  if(cause == causes[1]){
    colv=11
  }else if(cause == causes[2]){
    colv=17
  }else if(cause == causes[3]){
    colv=23
  }else{
    print("not a valid case")
    stop() 
  }
  
  outcome<-arrange(outcome,outcome$State)
  
  justa<-outcome[match(state,outcome$State):(match(state,outcome$State)+sum(outcome$State==state)-1),]
  
  filas<-c()
  
  for(i in 1:nrow(justa)){
    if(is.na(as.numeric(justa[i,colv]))==TRUE){
      
      filas<-c(filas,i)
      
    }
    
  }
  
  justa<-justa[-(filas),]
  
  justa<-arrange(justa,as.numeric(justa[,colv]))
  
  if(rank > nrow(justa)){
    
    print(NA)
    stop()
    
  }
  
  if(rank == "best"){
    
    rank=1
    
  }else if(rank == "worst"){
    
    rank= nrow(justa)
    
  }
  
  
  justa<-justa[order(as.numeric(justa[,colv]), justa[,2]), ] #ordenados por rate y alfa
  
  justaf<-data.frame()
  
  i=1
  
  while(i <= nrow(justa)){
    
    reps<-sum(as.numeric(justa[i,colv])==as.numeric(justa[,colv]))
    
    if(reps > 1){
      
      justaf<-rbind(justaf,justa[i,])
      
      i=i+reps
      
      
    }else{
      
      justaf<-rbind(justaf,justa[i,])
      
      i=i+1
      
    }
    
  }
  
  
  justaf<-justaf[order(as.numeric(justaf[,colv]), justaf[,2]), ]
  
  
  justaf<-justaf[1:rank,]
  
  ranks<-c(1:rank)
  
  justaresp<-data.frame()
  
  justaresp<-data.frame(cbind(justaf$Provider.Number,justaf$Hospital.Name,justaf[,colv]))
  
  justaresp$ranking<-ranks
  
  colnames(justaresp)[1:3]<-c("Provider.Number","Hospital.Name","Rate")
  
  justaresp
  
}

