hospdata<-read.csv("hospital-data.csv")
outcome<-read.csv("outcome-of-care-measures.csv")

#"C:/Users/Julien/Desktop/Ejercicios programacion/Coursera/tp3"

rankall<-function(cause,ranking,nums){
  
  library(plyr)
  
  causes<-c("heart attack","heart failure","pneumonia")
  
  ranking=tolower(as.character(ranking))
  
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
  
  allstates<-c(unique(outcome$State))
  
  starank<-outcome
  
  filas<-c()
  
  for(i in 1:nrow(starank)){
    if(is.na(as.numeric(starank[i,colv]))==TRUE){
      
      filas<-c(filas,i)
      
    }
    
  }
  
  starank<-starank[-(filas),]
  
  starank[,colv]<-as.numeric(starank[,colv])
  
  starank<-starank[order(starank$State,starank[,colv]),]
  
  firstlog<-!duplicated(starank$State)
  
  firstele<-c()
  
  for(n in 1:length(firstlog)){
    
    if((firstlog[n]) == TRUE){
      
      firstele<-c(firstele,n)
      
    }
    
  }
  
  bestranks<-data.frame()

  for(m in 1:length(firstele)) {
    
    bestranks<-rbind(bestranks,starank[firstele[m],])
    
  } 
  
  bestranks<-bestranks[order(bestranks[,colv]),]
  
  if(ranking == "best"){
    
    headlist<-data.frame()
    
    headlist<-data.frame(head(bestranks,nums))
   
    headlist<-data.frame(cbind(headlist$Hospital.Name,headlist$State))
    
    colnames(headlist)[1:2]<-c("Hospital.Name","State")
    
    headlist
     
  }else if(ranking == "worst"){
    
    taillist<-data.frame()
    
    taillist<-data.frame(tail(bestranks,nums))
    
    taillist<-data.frame(cbind(taillist$Hospital.Name,taillist$State))
    
    colnames(taillist)[1:2]<-c("Hospital.Name","State")
    
    taillist
    
  }else{
    
    print("not a viable rank")
    
  }
  
  
  
}