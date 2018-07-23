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
#c
CompleteNYC <- subset(uniqueData, uniqueData$Borough.of.Occurrence!="Outside NYC")
dim(CompleteNYC)
borough <- table(droplevels(CompleteNYC$Borough.of.Occurrence))
boroughP <- c(1455720, 2629150, 1643734, 2333054, 476015)
boroughCPC <- borough/boroughP
max(boroughCPC)
format(round(((100000*borough[1])/boroughP[1]),10),nsmall=10)
#d
ComplaintLength <- uniqueData$Close.Year- uniqueData$Received.Year
format(round(mean(ComplaintLength),10), nsmall=10)
#e
sum(uniqueData$Complaint.Contains.Stop...Frisk.Allegations)

b <- table(uniqueData$Complaint.Contains.Stop...Frisk.Allegations,
           uniqueData$Received.Year)
b
max(b[2,])
LRYears <- subset(uniqueData,
          as.factor(uniqueData$Received.Year)%in%
            c("2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"))
summary(LRYears$Received.Year)
dim(LRYears)
is.integer(LRYears$Received.Year)
c <- table(LRYears$Complaint.Contains.Stop...Frisk.Allegations,
      LRYears$Received.Year)
d <-as.data.frame(c) 
d <- subset(d,d$Var1!="FALSE")
d <- d[,-c(1)]
names(d) <- c("Year", "SF")
d
mod1 <- lm(SF~as.integer(Year),data = d)
summary(mod1)
round(predict(mod1,newdata=data.frame(Year=c(12))),0)
as.integer(predict(mod1,newdata=data.frame(Year=c(12))))
#f
e <- table(uniqueData$Is.Full.Investigation, uniqueData$Complaint.Has.Video.Evidence)
e <- as.matrix(e)
chie <-chisq.test(e)
format(round(chie$statistic,digits=10), nsmall = 10)
#g
dim(CompleteData)
levels(CompleteData$Allegation.FADO.Type)
AoA <- as.integer(CompleteData$Allegation.FADO.Type=="Abuse of Authority")
Disc <- as.integer(CompleteData$Allegation.FADO.Type=="Discourtesy")
force <- as.integer(CompleteData$Allegation.FADO.Type=="Force")
OL <- as.integer(CompleteData$Allegation.FADO.Type=="Offensive Language")

dAoA <- subset(CompleteData,CompleteData$Allegation.FADO.Type=="Abuse of Authority")
uniquedAoA <- unique( dAoA[ , 1:14 ] )
dim(uniquedAoA)
test <- data.frame(ID=CompleteData$UniqueComplaintId,AoA,Disc,force,OL)
modtest<-lm(ID~AoA,data = test)
summary(modtest)


NoC <- rep(0,length(uniqueData$UniqueComplaintId))
tt <- table(test$ID,test$Disc)

for (i in 1:length(uniqueData$UniqueComplaintId)){
  NoC[i]=sum(tt[i,])
}

NoDisc <-  rep(0,length(uniqueData$UniqueComplaintId))
for (i in 1:length(uniqueData$UniqueComplaintId)){
  if (tt[i,2]!=0){
    NoDisc[i]=1
    
  }else{
    NoDisc[i]=0
  }
  
}

NoAoA <-  rep(0,length(uniqueData$UniqueComplaintId))
tAoA<-table(test$ID,test$AoA)
for (i in 1:length(uniqueData$UniqueComplaintId)){
  if (tAoA[i,2]!=0){
    NoAoA[i]=1
    
  }else{
    NoAoA[i]=0
  }
}

NoForce <-  rep(0,length(uniqueData$UniqueComplaintId))
tForce<-table(test$ID,test$force) 
for (i in 1:length(uniqueData$UniqueComplaintId)){
  if (tForce[i,2]!=0){
    NoForce[i]=1
    
  }else{
    NoForce[i]=0
  }
}

NoOL <-  rep(0,length(uniqueData$UniqueComplaintId))
tOL<-table(test$ID,test$OL) 
for (i in 1:length(uniqueData$UniqueComplaintId)){
  if (tOL[i,2]!=0){
    NoOL[i]=1
    
  }else{
    NoOL[i]=0
  }
}

regData <- cbind(NoC,NoAoA,NoDisc,NoForce,NoOL)
regData <- as.data.frame(regData)
dim(regData)
head(regData)
model <- lm(NoC~.,data = regData)
summary(model)
max(coef(model))
format(round(max(coef(model)),digits=13), nsmall = 10)

#h
TDIdata16 <- subset(TDIdata, TDIdata$Incident.Year==2016)
dim(TDIdata16)
NYC16 <- subset(TDIdata16, TDIdata16$Borough.of.Occurrence!="Outside NYC")
borough16 <- table(droplevels(NYC16$Borough.of.Occurrence))
boroughP <- c(1455720, 2629150, 1643734, 2333054, 476015)
boroughCPC16 <- borough16/boroughP
sum(boroughCPC16)
boroughratios <-boroughCPC16/sum(boroughCPC16)
boroughPolice <- boroughratios*36000
PrecinctBorough <- c(12,23,22,16,4)
PricinctAve <- boroughPolice/PrecinctBorough
format(round(max(PricinctAve)/min(PricinctAve),digits=13), nsmall = 10)


#Q2

#a
Na <- 64
Ta <- 5
Sa <- rep(0,5)

for (i in 1:Ta){
  set.seed(312)
  points <- sample(1:(Na-1),2,replace=FALSE)
  rope <- Na-0
  rope1 <- min(points)-0
  rope2 <- max(points)-min(points)
  rope3 <- Na-max(points)
  ropes <- c(rope1,rope2,rope3)
  Na <- max(ropes)
  Sa[i] <- Na
  
}

format(round(mean(Sa),digits=10), nsmall = 10)
#b
format(round(sd(Sa),digits=10), nsmall = 10)
#c
Nc <- 1024
Tc <- 10
Sc <- rep(0,10)

for (i in 1:Tc){
  set.seed(224)
  points <- sample(1:(Nc-1),2,replace=FALSE)
  rope <- Nc-0
  rope1 <- min(points)-0
  rope2 <- max(points)-min(points)
  rope3 <- N-max(points)
  ropes <- c(rope1,rope2,rope3)
  Nc <- max(ropes)
  Sc[i] <- Nc
  
}

format(round(mean(Sc),digits=10), nsmall = 10)

#d
format(round(sd(Sc),digits=10), nsmall = 10)

#e
format(round(mean(Sa[Sa>=4]>=8),digits=10), nsmall = 10)
#f

format(round(mean(Sc[Sc>=6]>=12),digits=10), nsmall = 10)

