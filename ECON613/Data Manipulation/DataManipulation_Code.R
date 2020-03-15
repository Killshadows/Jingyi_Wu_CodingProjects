#===================================================================
# Data Manipulation
# Author: Jingyi Wu
# Date Modified: Jan 2020
#===================================================================
rm(list=ls())

library(foreign)
library(weights)
library(ggplot2)
library(gridExtra)
library(reshape)
library(gdata)
library(Rmisc)
library(pROC)
library(grid)
library(readstata13)
library(xtable)
library(Rmisc)
library(dplyr)
library(nnet)
library(fastDummies)


#========================================================
# load the data
#========================================================
modpath  = "https://raw.githubusercontent.com/Killshadows/Jingyi_Wu_CodingProjects/master/Applied%20Econometrics/Data%20Manipulation"

datstu = read.csv(paste0(modpath,"/","datstu.csv"))
datjss = read.csv(paste0(modpath,"/","datjss.csv"))
datsss = read.csv(paste0(modpath,"/","datsss.csv"))


#========================================================
# Section 1 Data Overview and Missing data
#========================================================
# provide a good overview of the student data 
str(datstu) 

#====================
#number of students 
#====================
nrow(datstu)

#====================
#number of schools
#====================
# find all the variables with schoolcode and vectorize it
# then find unique set of schools, and then length it 
# get rid of the missing observations
length(unique(c(as.matrix(datstu[,grep("schoolcode",names(datstu))])),na.rm=T))

#====================
#number of programs
#====================
length(unique(c(as.matrix(datstu[,grep("pgm",names(datstu))])),na.rm=T))

#====================
#number of choices
#====================
#create choice columns (combination of school and program)
datstu$choice1  = paste0(datstu$schoolcode1,datstu$choicepgm1)
datstu$choice2  = paste0(datstu$schoolcode2,datstu$choicepgm2)
datstu$choice3  = paste0(datstu$schoolcode3,datstu$choicepgm3)
datstu$choice4  = paste0(datstu$schoolcode4,datstu$choicepgm4)
datstu$choice5  = paste0(datstu$schoolcode5,datstu$choicepgm5)
datstu$choice6  = paste0(datstu$schoolcode6,datstu$choicepgm6)

nc = ncol(datstu)
length(unique(c(as.matrix(datstu[,(nc-5):nc])),na.rm=T))

#====================
#missing test score
#====================
table(is.na(datstu$score))

#====================
#apply to the same school
#====================
schools = apply(as.matrix(datstu[,grep("schoolcode",names(datstu))]),1,function(x)length(unique((x))))
table(schools)


#========================================================
# Section 2 School-Level Dataset
#========================================================

#====================
#get the list of school that has admitted students
#====================
#subset school info
#omit na value
#omit duplications
school=datsss[,2:6] 
school=na.omit(school) 
school=unique.data.frame(school) 
schoollist=school[!duplicated(school$schoolcode),] 

#====================
#integrated admitted information
#====================
rankchoice = as.data.frame(datstu[c(1,2,5:16,18,17)])
rank1 = subset(rankchoice, rankchoice[,15] == 1, select = c(X,score,schoolcode1,choicepgm1)) 
colnames(rank1)=c("X","score","schoolcode","program")
rank2 = subset(rankchoice, rankchoice[,15] == 2, select = c(X,score,schoolcode2,choicepgm2))
colnames(rank2)=c("X","score","schoolcode","program")
rank3 = subset(rankchoice, rankchoice[,15] == 3, select = c(X,score,schoolcode3,choicepgm3))
colnames(rank3)=c("X","score","schoolcode","program")
rank4 = subset(rankchoice, rankchoice[,15] == 4, select = c(X,score,schoolcode4,choicepgm4))
colnames(rank4)=c("X","score","schoolcode","program")
rank5 = subset(rankchoice, rankchoice[,15] == 5, select = c(X,score,schoolcode5,choicepgm5))
colnames(rank5)=c("X","score","schoolcode","program")
rank6 = subset(rankchoice, rankchoice[,15] == 6, select = c(X,score,schoolcode6,choicepgm6))
colnames(rank6)=c("X","score","schoolcode","program")
rankall = rbind(rank1,rank2,rank3,rank4,rank5,rank6)
colnames(rankall)=c("X","score","schoolcode","program")

#====================
#calculate cutoff and quality for each choice
#====================
min_score = as.data.frame(aggregate(score~schoolcode+program, data = rankall, FUN = "min")) 
colnames(min_score) = c("schoolcode","program","cutoff")
mean_score = as.data.frame(aggregate(score~schoolcode+program, data = rankall, FUN = "mean"))
colnames(mean_score) = c("schoolcode","program","quality")

#====================
#calculate size for each choice
#====================
num_score = as.data.frame(table(rankall[,3:4]))
colnames(num_score) = c("schoolcode","program","size")

#====================
#merge geometrical information
#====================
#combine info
admitted = cbind(min_score, mean_score = mean_score[,3]) 
#merge to add "num_admitted"
admitted = merge(admitted, num_score, by.admitted = c(1,2), by.num_score = c(1,2), all = FALSE, sort=TRUE) 
#merge to add geometrical info
admitted = merge(admitted, schoollist, by.admitted = schoolcode, by.schoollist = schoolcode, all.x = TRUE, sort=TRUE) 



#======================================================================
# Section 3 Descriptive Characteristics
#======================================================================

#====================
#get distance info
#====================
sdis=datsss[,4:6] 
sdis=unique.data.frame(sdis) 
sdis=na.omit(sdis) 
jdis=datjss[,2:4] 
colnames(jdis)=c("jssdistrict","jsslong","jsslat")
sdis=sdis[rep(1:nrow(sdis),each=139),] #replicate each row by 139 times
jdis=jdis[rep(1:nrow(jdis),time=111),] #replicate each row by 111 times
distance=cbind(sdis[,1],jdis[,1],sdis[,2:3],jdis[,2:3]) #combine geometrical info

colnames(distance)=c("sssdistrict","jssdistrict","ssslong","ssslat","jsslong","jsslat")
ddistance=as.data.frame(sqrt((69.172*(distance[,3]-distance[,5])*cos(distance[,6]/57.3))^2+(69.172*(distance[,4]-distance[,6]))^2)) 
colnames(ddistance)=c("distance")
distance=cbind(distance,ddistance) 

#====================
#get average and standar deviation of 
#cutoff, quality, and distance
#for each choice
#====================
averank = matrix(0:0, nrow = 6, ncol = 3)
sd_cutoffrank = matrix(0, nrow = 6, ncol = 1)
sd_qualityrank = matrix(0, nrow = 6, ncol = 1)
sd_distancerank = matrix(0, nrow = 6, ncol = 1)
for (i in 1:6)
{
  rank_int = as.data.frame(datstu[c(1,2,i+4,i+10,17)]) #combine info
  colnames(rank_int)=c("X","score","schoolcode","program","jssdistrict")
  rank_int[rank_int==""] = NA #save empty value as NA value
  rankchoice_i=merge(rank_int,admitted, by=c("schoolcode","program"), all.x= TRUE,sort=TRUE) #add cutoff, quality and sssdistrict
  rankchoice_ii=merge(rankchoice_i,distance, by=c("sssdistrict","jssdistrict"), all.x=TRUE, sort=TRUE) #add distance
  rankchoice_i=rankchoice_ii[,c(7,8,17)] #subset "cutoff", "quality", "distance" info
  averank[i,]=colMeans(rankchoice_i,na.rm=TRUE) #calculate columns average
  sd_cutoffrank[i]=sd(rankchoice_i[,1],na.rm=TRUE) #calculate sd
  sd_qualityrank[i]=sd(rankchoice_i[,2],na.rm=TRUE)
  sd_distancerank[i]=sd(rankchoice_i[,3],na.rm=TRUE)
}
colnames(averank)=c("cutoff", "quality", "distance")
averank
sdrank =cbind(sd_cutoffrank,sd_qualityrank,sd_distancerank)
colnames(averank)=c("cutoff", "quality", "distance")
sdrank 