best<-function(state,cause){
  
  causes<-c("heart attack","heart failure","pneumonia")
  
  if(!match(state,outcome$State) == FALSE){
    
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

  justa<-outcome[match(state,outcome$State):(match(state,outcome$State)+sum(outcome$State==state)-1),]
  
  filas<-c()
  
  for(i in 1:nrow(justa)){
    if(is.na(as.numeric(justa[i,colv]))==TRUE){
      
      filas<-c(filas,i)
      
    }
    
  }
  
  justa<-justa[-(filas),]
  
  minimo<-min(as.numeric(justa[1:nrow(justa),colv]))
  
  vof<-c(as.numeric(justa[,colv])==minimo)
  
  if(sum(vof) > 1){
    
    pos<-which(!is.na(match(vof,"TRUE"))) 
    
    multipos<-c()
    
    for(i in 1:length(pos)){
      
      
      multipos<-append(multipos,justa[pos[i],2])
      
      
    }
    
    multipos<-sort(multipos)
    
    besthos<-multipos[1]
    
  }else{
  
    pos<-as.numeric(match("TRUE",vof))
  
    besthos<-justa[pos,2]
  
  }
  
  besthos
}



