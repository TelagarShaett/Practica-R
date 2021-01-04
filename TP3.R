hospdata<-read.csv("hospital-data.csv")
outcome<-read.csv("outcome-of-care-measures.csv")

options(max.print=999999)

mortrate<-as.numeric(outcome[,11])

mortrate<-mortrate[!is.na(mortrate)]

hist(mortrate, xlab = "Percentage", ylab = "Amount of hospitals",ylim = c(0,800))
