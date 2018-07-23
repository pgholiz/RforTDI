rm(list=ls())

#Q1

TDIdata <- read.csv("TDIChallenge.csv",header = T)#the "Complaints_Allegations" sheet was saved as "TDIChallenge.csv"
dim(TDIdata)
CompleteData <- na.omit(TDIdata)
dim(CompleteData)
#a
length(levels(as.factor(CompleteData$UniqueComplaintId)))
#b
uniqueData <- unique( CompleteData[ , 1:14 ] )
dim(uniqueData)
length(levels(uniqueData$Borough.of.Occurrence))
a <- table(uniqueData$Borough.of.Occurrence)
round((max(a)/sum(a)),digits=13)
format(round((max(a)/sum(a)),digits=13), nsmall = 10)
