###
### 
### Figure 1: Mapping the metadata onto a map of Africa

library(rworldmap)
library(ggmap)
library(rworldxtra)
library(GISTools)
library(mapplots)
library(adegenet)
library(lme4)
library(sjPlot)

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(c(x))))

par(mfrow=c(1,1))
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-15, 50), ylim = c(-35, 40), asp = 1,col="grey95", border="grey65")
points(36.88,-7.87,col="orange",cex=2,pch=17) ## Killeen et al 2006 ##**HUMAN BED BEHAVIOUR
points(2.116,6.35,col="orange",cex=2, pch=17) ## Moiroux et al 2014 (in Moiroux et al 2012 too) ## HUMAN INDOOR BEHAVIOUR
points(2.09,6.26,col="orange",cex=2, pch=17) ## Moiroux et al 2014 (in Moiroux et al 2012 too)  ## HUMAN INDOOR BEHAVIOUR
points(39.23,-6.81,col="orange",cex=2,pch=17) ## Geissbuhler et al. 1997 ##**HUMAN BED BEHAVIOUR ##**HUMAN INDOOR BEHAVIOUR 

points(36.6,-8.1,col="aquamarine3",cex=2, pch=20)##Russell et al. 2011 ## HUMAN INDOOR BEHAVIOUR
points(39.23,-6.81,col="aquamarine3",cex=2,pch=20) ## Geissbuhler et al. 1997 ##**HUMAN BED BEHAVIOUR ##**HUMAN INDOOR BEHAVIOUR 
points(-1.33,12.6,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (1)  ##HUMAN INDOOR BEHAVIOUR
points(-1.78,12.587,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (2)##HUMAN INDOOR BEHAVIOUR
points(29.83,-15.43,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (3)##HUMAN INDOOR BEHAVIOUR
points(36.6,-9.07,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (4)  ##HUMAN INDOOR BEHAVIOUR
points(39.15,-7.9,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (5)  ##HUMAN INDOOR BEHAVIOUR
points(34.78,-0.1,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (6)  ##HUMAN INDOOR BEHAVIOUR
points(2.116,6.35,col="aquamarine3",cex=2, pch=20) ## Moiroux et al 2014 (in Moiroux et al 2012 too) ## HUMAN INDOOR BEHAVIOUR
points(2.09,6.26,col="aquamarine3",cex=2, pch=20) ## Moiroux et al 2014 (in Moiroux et al 2012 too)  ## HUMAN INDOOR BEHAVIOUR

points(34.9,-0.175,col="purple",cex=2, pch=22) ## Githeko et al. 1996
points(32.48,-25.9,col="purple",cex=2, pch=22) ## Mendis et al 2006
points(9.47,0.399,col="purple",cex=2, pch=22) ## Mourou et al 2012
points(30.40,1.57,col="purple",cex=2, pch=22) ## Ojuka et al. 2015 (1)
points(30.48,2.98,col="purple",cex=2, pch=22) ## Ojuka et al. 2015 (2)
points(8.7,3.5,col="purple",cex=2, pch=22) ## Overgaard et al. 2012
points(0.44,6.35,col="purple",cex=2, pch=22) ## Owusu et al 2016
points(-15.3,13.6,col="purple",cex=2, pch=22) ## Quinones et al 1997
points(33.0,1.0,col="purple",cex=2, pch=22) ##Kabbale et al. 2013
points(34.38,-0.18,col="purple",cex=2, pch=22) ##Bayoh et al. 2014
points(8.7,3.5,col="purple",cex=2, pch=22) ##Bradley et al. 2015
points(34.92,-0.43,col="purple",cex=2, pch=22) ##Cooke et al. 2015
points(-16.4,13.72,col="purple",cex=2, pch=22) ## Fontenille et al. 1997
points(0.06,5.69,col="purple",cex=2, pch=22) ## Tchouassi et al 2012
points(9.1,4.02,col="purple",cex=2, pch=22) ## Tanga et al 2011 (in Tanga et al 2010 too)
points(-1.3,6.57,col="purple",cex=2, pch=22) ## Tuno et al 2010

legend(-15,-5,legend=c("Mosquito biting behaviour",
                    "Human indoor behaviour",
                    "Human bed behaviour"),
       col=c("purple","aquamarine3","orange"),
       pch=c(22,20,17))

##########################
###
###
### Figure 2: Summary metadata, mosquitoes activity times, people indoors and in bed
dat_mosq = read.csv("C:\\Users\\Ellie\\Documents\\STUDENTS\\BITING TIMES_Janetta Skarp\\phiI_phiB_rawdata.csv",header=TRUE)
dat_indoor = read.csv("C:\\Users\\Ellie\\Documents\\STUDENTS\\BITING TIMES_Janetta Skarp\\Human_indoor_vs_time.csv",header=TRUE)
dat_inbed = read.csv("C:\\Users\\Ellie\\Documents\\STUDENTS\\BITING TIMES_Janetta Skarp\\Human_sleeping_vs_time(1).csv",header=TRUE)

par(mfrow=c(1,3))

veca = seq(1,nrow(dat_mosq),24)
vecb = c(veca[2:61]-1,1464)
vecc = c("red","purple","grey","aquamarine3","coral3","yellow","blue")
plot(dat_mosq[veca[1]:vecb[1],4] ~ c(1:24),ylim=c(-0.5,0.5),pch="",
     bty="n",ylab="Proportion of active mosquitoes",yaxt="n",
     xlab="Time (hours)",cex.lab=1.4,xaxt="n",las=2,cex.axis=1.4)
axis(2,las=2,at=c(-0.4,-0.2,0,0.2,0.4),labels=c(0.4,0.2,0.0,0.2,0.4),cex.axis=1.4,cex=1.4)
for(i in 1:length(veca)){
  lines(dat_mosq[veca[i]:vecb[i],2] ~ c(1:24))
  lines(-dat_mosq[veca[i]:vecb[i],3] ~ c(1:24),col="grey")
}

dat_gamb = subset(dat_mosq,dat_mosq$species_cleaned == "A_gambiae")
veca_G = seq(1,nrow(dat_gamb),24)
vecb_G = c(veca_G[2:22]-1,524)
for(i in 1:length(veca_G)){
  lines(dat_gamb[veca_G[i]:vecb_G[i],2] ~ c(1:24),col="red",lty=2)
  lines(-dat_gamb[veca_G[i]:vecb_G[i],3] ~ c(1:24),col="darkred",lty=2)
}

dat_fun = subset(dat_mosq,dat_mosq$species_cleaned == "A_funestus")
veca_F = seq(1,nrow(dat_fun),24)
vecb_F = c(veca_F[2:22]-1,456)
for(i in 1:length(veca_F)){
  lines(dat_fun[veca_F[i]:vecb_F[i],2] ~ c(1:24),col="cyan2")
  lines(-dat_fun[veca_F[i]:vecb_F[i],3] ~ c(1:24),col="blue")
}

text(21,0.05,"Indoor activity",cex=1.6)
text(21,-0.05,"Outdoor activity",cex=1.6)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
text(24,0.45,"A",cex=2)

legend(15,-0.2,legend=c("An. funestus","An. gambiae"),
       col=c("blue","red"),lty=2,cex=1.4)

plot(dat_indoor[,2]~dat_indoor[,1],pch="",bty="n",ylab="Proportion of people indoors",
     xlab="Time (hours)",cex.lab=1.2,ylim=c(0,1),xaxt="n",las=2,cex.axis=1.4)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
for(i in 3:5){
  lines(dat_indoor[1:24,i]~dat_indoor[1:24,1],col="orange",lwd=2)##Tanzania
}
lines(dat_indoor[1:24,6]~dat_indoor[1:24,1],col="purple",lwd=2)##Burkina
lines(dat_indoor[1:24,7]~dat_indoor[1:24,1],col="purple",lwd=2)##Burkina
lines(dat_indoor[1:24,8]~dat_indoor[1:24,1],col="orange",lwd=2)##Tanzania
lines(dat_indoor[1:24,9]~dat_indoor[1:24,1],col="blue",lwd=2)##Zambia
lines(dat_indoor[1:24,10]~dat_indoor[1:24,1],col="aquamarine3",lwd=2)##Kenya
for(i in 11:12){
  lines(dat_indoor[1:24,i]~dat_indoor[1:24,1],col="darkred",lwd=2)##Benin
}
lines(dat_indoor[1:24,13]~dat_indoor[1:24,1],col="orange",lwd=2)##Tanzania

lines(dat_indoor[,14]~dat_indoor[,1],lty=2,lwd=3)##Average
text(24,1,"B",cex=2)

plot(dat_inbed[,3]~dat_inbed[1:24,1],pch="",bty="n",ylab="Proportion of people in bed",
     xlab="Time (hours)",cex.lab=1.2,ylim=c(0,1),xaxt="n",las=2,cex.axis=1.4)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
lines(dat_inbed[1:24,3]~dat_inbed[1:24,1],col="orange",lwd=2)##Tanzania
lines(dat_inbed[1:24,4]~dat_inbed[1:24,1],col="orange",lwd=2)##Tanzania

lines(dat_inbed[,5]~dat_inbed[,1],lty=2,lwd=3)##Average
text(24,1,"C",cex=2)

legend(16,0.8,legend=c("Tanzania",
                     "Burina Faso",
                     "Zambia",
                     "Kenya",
                     "Benin"),
       lty=1,cex=1.4,col=c("orange","purple","blue","aquamarine3","darkred"))

##########################
###
###
### Figure 3: Temporal trends and phi I and phi B


################################################
##
## Calculate:
## All possible combinations of phiI AND phiB

head(dat_mosq)

##Calculate phiI and B for the only study we have all the data (code #16)
phiB_TESTnum16 <- dat_inbed[,4] * dat_mosq$Inside_mosq[dat_mosq$Study == 16][1:24]
phiI_TESTnum16 <- dat_indoor[,13] * dat_mosq$Inside_mosq[dat_mosq$Study == 16][1:24]
phiI_TESTdenom16 <- (1-dat_indoor[,13]) * dat_mosq$Outside_mosq[dat_mosq$Study == 16][1:24]

phiB_TESTnum16B <- dat_inbed[,4] * dat_mosq$Inside_mosq[dat_mosq$Study == 16][25:48]
phiI_TESTnum16B <- dat_indoor[,13] * dat_mosq$Inside_mosq[dat_mosq$Study == 16][25:48]
phiI_TESTdenom16B <- (1-dat_indoor[,13]) * dat_mosq$Outside_mosq[dat_mosq$Study == 16][25:48]

phiB_TESTnum16C <- dat_inbed[,4] * dat_mosq$Inside_mosq[dat_mosq$Study == 16][49:72]
phiI_TESTnum16C <- dat_indoor[,13] * dat_mosq$Inside_mosq[dat_mosq$Study == 16][49:72]
phiI_TESTdenom16C <- (1-dat_indoor[,13]) * dat_mosq$Outside_mosq[dat_mosq$Study == 16][49:72]

phiI_16_a = sum(phiI_TESTnum16) / sum(phiI_TESTdenom16+phiI_TESTnum16)
phiB_16_a = sum(phiB_TESTnum16) / sum(phiI_TESTdenom16+phiI_TESTnum16)

phiI_16_b = sum(phiI_TESTnum16B) / sum(phiI_TESTdenom16B+phiI_TESTnum16B)
phiB_16_b = sum(phiB_TESTnum16B) / sum(phiI_TESTdenom16B+phiI_TESTnum16B)

phiI_16_c = sum(phiI_TESTnum16C) / sum(phiI_TESTdenom16C+phiI_TESTnum16C)
phiB_16_c = sum(phiB_TESTnum16C) / sum(phiI_TESTdenom16C+phiI_TESTnum16C)

vec_sets = seq(0,1464,24) ##This is the start of each set of data for 24 hours
vec2 = c(2:61,1) ##This is the start of each set of data for 24 hours
phiB_TESTnum <- phiI_TESTnum <- phiI_TESTdenom <- array(dim=c(24,61),data=NA)
phiI1 <- phiB1 <- phiI1test <- numeric(61)

for(j in 1:61){
  for(i in 1:24) {
    phiB_TESTnum[i,j] <- dat_mosq$Prop_Bed[vec_sets[j]+i] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiI_TESTnum[i,j] <- dat_mosq$Prop_In[vec_sets[j]+i] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiI_TESTdenom[i,j] <- (1-dat_mosq$Prop_In[vec_sets[j]+i]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
  }
}


for(j in 1:61){
  phiI1[j] <- sum(phiI_TESTnum[1:24,j]) / sum(phiI_TESTdenom[1:24,j] + phiI_TESTnum[1:24,j])
  phiB1[j] <- sum(phiB_TESTnum[1:24,j]) / sum(phiI_TESTdenom[1:24,j] + phiI_TESTnum[1:24,j])
}
##phiI and phiB are the data derived "average human behaviour" estimates

##Now simulate for all of the data combinations i.e. of all the mosquito populations crossed with each piece of human data
##dat_indoor[,3:11]  ## individual studies on humans indoors
##dat_inbed[,3:4]     ## individual studies on humans in bed

phiB_TESTnumALL <- array(dim=c(24,61,2),data=NA)
  phiI_TESTnumALL <- phiI_TESTdenomALL <-array(dim=c(24,61,11),data=NA) 

  
  for(i in 1:24) {
    for(j in 1:61){
      phiB_TESTnumALL[i,j,1]   <- dat_inbed[i,3] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiB_TESTnumALL[i,j,2]   <- dat_inbed[i,4] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      
      phiI_TESTnumALL[i,j,1]   <- dat_indoor[i,3] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,2]   <- dat_indoor[i,4] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,3]   <- dat_indoor[i,5] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,4]   <- dat_indoor[i,6] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,5]   <- dat_indoor[i,7] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,6]   <- dat_indoor[i,8] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,7]   <- dat_indoor[i,9] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,8]   <- dat_indoor[i,10] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,9]   <- dat_indoor[i,11] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,10]   <- dat_indoor[i,12] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTnumALL[i,j,11]   <- dat_indoor[i,13] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      
      phiI_TESTdenomALL[i,j,1] <- dat_indoor[i,3] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,3]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,2] <- dat_indoor[i,4] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,4]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,3] <- dat_indoor[i,5] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,5]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,4] <- dat_indoor[i,6] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,6]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,5] <- dat_indoor[i,7] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,7]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,6] <- dat_indoor[i,8] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,8]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,7] <- dat_indoor[i,9] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,9]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,8] <- dat_indoor[i,10] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,10]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,9] <- dat_indoor[i,11] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,11]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,10] <- dat_indoor[i,12] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,12]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,11] <- dat_indoor[i,13] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,13]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
    }
  }

phiB1ALL <- array(dim=c(61,22),data=NA)
    for(j in 1:61){
      phiB1ALL[j,1] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,1])
      phiB1ALL[j,2] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,2])
      phiB1ALL[j,3] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,3])
      phiB1ALL[j,4] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,4])
      phiB1ALL[j,5] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,5])
      phiB1ALL[j,6] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,6])
      phiB1ALL[j,7] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,7])
      phiB1ALL[j,8] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,8])
      phiB1ALL[j,9] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,9])
      phiB1ALL[j,10] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,10])
      phiB1ALL[j,11] <- sum(phiB_TESTnumALL[1:24,j,1]) / sum(phiI_TESTdenomALL[1:24,j,11])
      
      phiB1ALL[j,12] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,1])
      phiB1ALL[j,13] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,2])
      phiB1ALL[j,14] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,3])
      phiB1ALL[j,15] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,4])
      phiB1ALL[j,16] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,5])
      phiB1ALL[j,17] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,6])
      phiB1ALL[j,18] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,7])
      phiB1ALL[j,19] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,8])
      phiB1ALL[j,20] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,9])
      phiB1ALL[j,21] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,10])
      phiB1ALL[j,22] <- sum(phiB_TESTnumALL[1:24,j,2]) / sum(phiI_TESTdenomALL[1:24,j,11])
      
    }    



phiI1ALL <- array(dim=c(61,11),data=NA)
for(k in 1:11){
    for(j in 1:61){
      phiI1ALL[j,k]     <- sum(phiI_TESTnumALL[1:24,j,k]) / sum(phiI_TESTdenomALL[1:24,j,k])
      }
  }    
summary(phiI1ALL)  

##Each row is the confidence intervals for the specific mosquito data
dat_mosq2 = read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\behaviour_paper\\Data from Janetta\\phiB+phiI.csv",header=TRUE)

##if using the mean data for each country 
##with mean for indoor and in bed proportion of people
##then use dat_mosq2[,3]##indoors
##and dat_mosq2[,4]##in bed

##if using the average for all possibilities
##use mean(phiI1ALL[i,],na.rm=TRUE)
Allmean_phiI = Allmean_phiB = numeric(61)
Allmedian_phiI = Allmedian_phiB = numeric(61)
for(i in 1:61){
  Allmean_phiI[i] = mean(phiI1ALL[i,],na.rm=TRUE)
  Allmean_phiB[i] = mean(phiB1ALL[i,],na.rm=TRUE)
  
  Allmedian_phiI[i] = median(phiI1ALL[i,],na.rm=TRUE)
  Allmedian_phiB[i] = median(phiB1ALL[i,],na.rm=TRUE)
  
}
dat_mosq2$Allmean_phiI = Allmean_phiI
dat_mosq2$Allmean_phiB = Allmean_phiB

dat_mosq2$Allmedian_phiI = Allmedian_phiI
dat_mosq2$Allmedian_phiB = Allmedian_phiB

#plot(dat_mosq2[,3]~dat_mosq2$Allmean_phiI,ylim=c(0,1),xlim=c(0,1))
#points(dat_mosq2[,2]~dat_mosq2$Allmean_phiB,ylim=c(0,1),xlim=c(0,1),pch=15,col="red")
#abline(a = 0, b = 1, col = 4)

par(mfrow=c(1,3))
par(mar=c(5,6,5,5))
plot(dat_mosq2[,14] ~ dat_mosq2[,4],xlab="Year",xlim=c(1994,2015),ylim=c(0,1),
     yaxt="n",bty="n",cex.lab=1.5,cex.axis=1.5,pch="",
     ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])))##phiB

for(i in 1:61){
  segments(x0=dat_mosq2[,4][i],x1=dat_mosq2[,4][i],
           y0=quantile(phiI1ALL[i,],na.rm=TRUE,0.975),
           y1=quantile(phiI1ALL[i,],na.rm=TRUE,0.025),
           lty=1,col="grey")
  
}
points(dat_mosq2[,14][dat_mosq2$Species.grouped == "1A_gambiae_sl"] ~ dat_mosq2[,4][dat_mosq2$Species.grouped == "1A_gambiae_sl"],pch=0)
points(dat_mosq2[,14][dat_mosq2$Species.grouped == "A_funestus"] ~ dat_mosq2[,4][dat_mosq2$Species.grouped == "A_funestus"],pch=2)
points(dat_mosq2[,14][dat_mosq2$Species.grouped != "1A_gambiae_sl" & dat_mosq2$Species.grouped != "A_funestus"] ~ 
         dat_mosq2[,4][dat_mosq2$Species.grouped != "1A_gambiae_sl" & dat_mosq2$Species.grouped != "A_funestus"],pch=1)
points(dat_mosq2[,14][dat_mosq2$Country == "Tanzania" & 
                       dat_mosq2$Species.grouped == "1A_gambiae_sl" &
                       dat_mosq2$Site == "Lupiro" |dat_mosq2$Site == "Njagi" ] ~ 
         dat_mosq2[,4][dat_mosq2$Country == "Tanzania" & 
                         dat_mosq2$Species.grouped == "1A_gambiae_sl"&
                         dat_mosq2$Site == "Lupiro"|dat_mosq2$Site == "Njagi" ],
       pch=15,cex=2,col="orange")
points(dat_mosq2[,14][dat_mosq2$Country == "Tanzania" & dat_mosq2$Species.grouped == "A_funestus"] ~ 
         dat_mosq2[,4][dat_mosq2$Country == "Tanzania" & dat_mosq2$Species.grouped == "A_funestus"],
       pch=17,cex=2,col="orange")

points(dat_mosq2[,14][dat_mosq2$Country == "Benin" & 
                       dat_mosq2$Species.grouped == "A_funestus"&
                       dat_mosq2$Site == "Lokohoue"] ~ 
         dat_mosq2[,4][dat_mosq2$Country == "Benin" & 
                         dat_mosq2$Species.grouped == "A_funestus"&
                         dat_mosq2$Site == "Lokohoue"],
       pch=17,cex=2,col="red")

points(dat_mosq2[,14][dat_mosq2$Country == "Benin" & 
                       dat_mosq2$Species.grouped == "A_funestus"&
                       dat_mosq2$Site == "Tokoli"] ~ 
         dat_mosq2[,4][dat_mosq2$Country == "Benin" & 
                         dat_mosq2$Species.grouped == "A_funestus"&
                         dat_mosq2$Site == "Tokoli"],
       pch=17,cex=2,col="darkred")

axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.lab=1.5,cex.axis=1.5,cex=1.5)

points(c(phiI_16_a,phiI_16_b,phiI_16_c)~rep(2006,3),pch=8,cex=2,col="blue")
#for(i in 1:61){
#  segments(x0=dat_mosq2[,4][i],x1=dat_mosq2[,4][i],
#           y0=min(phiI1ALL[i,],na.rm=TRUE),y1=max(phiI1ALL[i,],na.rm=TRUE),
#           lty=1,col="blue")
#  
#}


legend(1995,0.25,legend=c("Kolombero Valley, Tanzania",
                          "Tokoli-Vidjinnagnimon, Benin",
                          "Lokohouè, Benin",
                          "An. funestus",
                          "An. gambiae s.l.",
                          "Other Anopheles","Matched data: Geissbuhler et al. 2007"),
       col=c("orange","darkred","red","grey","grey","black","blue"),
       ncol=1,pch=c(19,19,19,17,15,1,8),cex=1.4)

plot(dat_mosq2[,15] ~ dat_mosq2[,4],xlab="Year",xlim=c(1994,2015),ylim=c(0,1),
     yaxt="n",bty="n",cex.lab=1.5,cex.axis=1.5,pch="",
     ylab=expression(paste("Proportion of mosquitoes biting in bed  ", phi[B])))##phiB

phiB1ALL[phiB1ALL >= 1] = NA
for(i in 1:61){
  segments(x0=dat_mosq2[,4][i],x1=dat_mosq2[,4][i],
           y0=quantile(phiB1ALL[i,],na.rm=TRUE,0.975),
           y1=quantile(phiB1ALL[i,],na.rm=TRUE,0.025),
           lty=1,col="grey")
  
}

points(dat_mosq2[,15][dat_mosq2$Species.grouped == "1A_gambiae_sl"] ~ dat_mosq2[,4][dat_mosq2$Species.grouped == "1A_gambiae_sl"],pch=0)
points(dat_mosq2[,15][dat_mosq2$Species.grouped == "A_funestus"] ~ dat_mosq2[,4][dat_mosq2$Species.grouped == "A_funestus"],pch=2)
points(dat_mosq2[,15][dat_mosq2$Species.grouped != "1A_gambiae_sl" & dat_mosq2$Species.grouped != "A_funestus"] ~ 
         dat_mosq2[,4][dat_mosq2$Species.grouped != "1A_gambiae_sl" & dat_mosq2$Species.grouped != "A_funestus"],pch=1)
points(dat_mosq2[,15][dat_mosq2$Country == "Tanzania" & 
                       dat_mosq2$Species.grouped == "1A_gambiae_sl" &
                       dat_mosq2$Site == "Lupiro" |dat_mosq2$Site == "Njagi" ] ~ 
         dat_mosq2[,4][dat_mosq2$Country == "Tanzania" & 
                         dat_mosq2$Species.grouped == "1A_gambiae_sl"&
                         dat_mosq2$Site == "Lupiro"|dat_mosq2$Site == "Njagi" ],
       pch=15,cex=2,col="orange")
points(dat_mosq2[,15][dat_mosq2$Country == "Tanzania" & dat_mosq2$Species.grouped == "A_funestus"] ~ 
         dat_mosq2[,4][dat_mosq2$Country == "Tanzania" & dat_mosq2$Species.grouped == "A_funestus"],
       pch=17,cex=2,col="orange")

points(dat_mosq2[,15][dat_mosq2$Country == "Benin" & 
                       dat_mosq2$Species.grouped == "A_funestus"&
                       dat_mosq2$Site == "Lokohoue"] ~ 
         dat_mosq2[,4][dat_mosq2$Country == "Benin" & 
                         dat_mosq2$Species.grouped == "A_funestus"&
                         dat_mosq2$Site == "Lokohoue"],
       pch=17,cex=2,col="red")

points(dat_mosq2[,15][dat_mosq2$Country == "Benin" & 
                       dat_mosq2$Species.grouped == "A_funestus"&
                       dat_mosq2$Site == "Tokoli"] ~ 
         dat_mosq2[,4][dat_mosq2$Country == "Benin" & 
                         dat_mosq2$Species.grouped == "A_funestus"&
                         dat_mosq2$Site == "Tokoli"],
       pch=17,cex=2,col="darkred")

axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.lab=1.5,cex.axis=1.5,cex=1.5)

points(c(phiB_16_a,phiB_16_b,phiB_16_c)~rep(2006,3),pch=8,cex=2,col="blue")


#for(i in 1:61){
#  segments(x0=dat_mosq2[,4][i],x1=dat_mosq2[,4][i],
#           y0=min(phiB1ALL[i,],na.rm=TRUE),y1=max(phiB1ALL[i,],na.rm=TRUE),
#           lty=1,col="grey")
  
#}

PHII = c(phiI1ALL) 
PHIB = c(phiB1ALL) 

hist(PHII,breaks=50,col=transp("red",0.6),border=NA,main="",xlim=c(0,1),ylim=c(0,200),
     ylab="Frequency", xlab="Proportion of mosquitoes biting",cex.lab=1.4,cex.axis=1.4,yaxt="n")
axis(2,las=2,at=seq(0,5000,1000),labels=seq(0,5000,1000), cex=1.4,cex.lab=1.4,cex.axis=1.4)
hist(PHIB,add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
abline(v=mean(PHIB,na.rm=TRUE),lty=2,lwd=2,col="blue4")
abline(v=mean(PHII,na.rm=TRUE),lty=2,lwd=2,col="darkred")
text(mean(PHIB,na.rm=TRUE)-0.15,200,expression(paste("In bed bites ", phi[B])),cex=1.5)
text(mean(PHII,na.rm=TRUE)-0.05,100,expression(paste("Indoor bites ", phi[I])),cex=1.5)

text(mean(PHIB,na.rm=TRUE)-0.07,190,"Mean: 0.746",cex=1.5,col="blue4",font=4)
text(mean(PHII,na.rm=TRUE)-0.04,90,"Mean: 0.859",cex=1.5,col="darkred",font=4)

text(median(PHIB,na.rm=TRUE)-0.07,180,"Median: 0.785",cex=1.5,col="blue4",font=4)
text(median(PHII,na.rm=TRUE)-0.06,80,"Median: 0.891",cex=1.5,col="darkred",font=4)

summary(PHII,na.rm=TRUE)
summary(PHIB,na.rm=TRUE)

##########################
###
###
### Is there a statistical trend in phiI and phiB over time?
summary(lm(dat_mosq2[,14] ~ dat_mosq2[,4]))
summary(lm(dat_mosq2[,15] ~ dat_mosq2[,4]))

dat_mosq2$trans_phiI = 57.295*asin(dat_mosq2[,13])
dat_mosq2$trans_phiB = 57.295*asin(dat_mosq2[,14])

levels(dat_mosq2$Species.grouped)[1] <- "A_gambiae_sl"
levels(dat_mosq2$Species.grouped)[4] <- "A_gambiae_sl"
levels(dat_mosq2$Species.grouped)[4:7] <- "An_other"
summary(dat_mosq2$Species.grouped)

#MOD1 <- glmm(trans_phiI ~ Species.grouped + Year + (1|Country))

    #i.glm<-glm(Allmean_phiI~Species.grouped+Year+Country+setting, data=dat_mosq2,family=quasibinomial(link= "logit" ))
    i.glm<-glm(Allmean_phiI~Year+Country, data=dat_mosq2) #,family=quasibinomial(link= "logit" )
    summary.lm(i.glm)
    theta = i.glm$deviance / i.glm$df.residual ##not over-dispersed
    plot(resid(i.glm,type="deviance")~i.glm$fitted.values)
    plot(resid(i.glm,type="deviance")~dat_mosq2$Country)
    plot(resid(i.glm,type="deviance")~dat_mosq2$Year)
    plot(resid(i.glm,type="deviance")~dat_mosq2$Species.grouped) 
exp(cbind(coef(i.glm), confint(i.glm)))  


    mod1<-lmer(Allmean_phiI~Year+(1|Country), data=dat_mosq2)
    chmod1<-lmer(Allmean_phiI~1+(1|Country), data=dat_mosq2)
    anova(mod1,chmod1)
    summary(mod1)
    confint(mod1)
se2 <- sqrt(diag(vcov(mod1)))
tab <- cbind(Est = fixef(mod1), LL = fixef(mod1) - 1.96 * se2, UL = fixef(mod1) + 1.96 * se2)
print(exp(tab),digits=3) #to get quick odds ratios for glmm - these show you how important the different levels of each factor are


sjp.setTheme(theme = "forestgrey", 
             geom.label.size = 5, 
             axis.textsize = 1.6, 
             axis.title.size = 1.9,
             title.size=2)

sjp.glmer(mod1, y.offset = .2)
# sort all predictors
sjp.glmer(mod1,
          facet.grid = FALSE,
          sort = "sort.all",
          y.offset = .2)

# plot qq-plot of random effects: dots should be along the line
sjp.glmer(mod1, type = "re.qq")
sjp.glmer(mod1, type = "fe.pc")

# plot probability curves for each covariate
# grouped by random intercepts
sjp.glmer(mod1,
          type = "ri.pc",
          facet.grid = FALSE)

par(mfrow=c(1,2))
datAnArab <- data.frame(preds = predict(mod1, data.frame(Year = dat_mosq2$Year), type="response", re.form = NA),
                        exp1 = dat_mosq2$Year)
pv = predict(mod1, classify=list("Year"),
             levels=list("Year"=list("Year" = 1992:2015)))
#plot(predict(mod1, data.frame(Year = dat_mosq2$Year), type="response", re.form = NA) ~
#       dat_mosq2$Year,frame=FALSE,bty="n",pch="",ylim=c(0,1),xaxt="n",
#     ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])),xlab="Time (Years)",
#     main="Trends in years",cex.main=1.8,
#     cex.lab=1.6,yaxt="n")
#axis(1, at=1992:2015,labels=1992:2015,cex.axis=1.8)
#axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)     

plot(Allmean_phiI~Year,data=dat_mosq2,pch="",bty="n",ylim=c(0,1),yaxt="n",cex.main=1.8,
     cex.lab=1.6, ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])))
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)  

##PREDICTIONS FROM THE GLMM WITH COUNTRY AS A RANDOM EFFECT
lines(rev(sort(datAnArab$preds)) ~ sort(datAnArab$exp1),lty=1,lwd=1)
polygon(c(sort(datAnArab$exp1),rev(sort(datAnArab$exp1))),
        c(rev(sort(datAnArab$preds-6*se(datAnArab$preds))),
          sort(datAnArab$preds+6*se(datAnArab$preds))),
        col=transp("grey",0.2),border=NA)             

##PREDICTIONS WITH COUNTRY AS A FIXED EFFECT
cols=c("red","red","red","red","red","red","red","blue","purple","red","blue","aquamarine3","purple")
pchs=c(6:18)
ltys=c(1:7,1,1,8,2,1,2)
for(i in 1:length(unique(dat_mosq2$Country))){
  lines(min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):
          max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]), 
        predict(i.glm, 
                newdata=
                  data.frame(Year = min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]),
                             Country = rep(levels(unique(dat_mosq2$Country))[i],
                                           length(min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]])))),
                type = "response"),lty=ltys[i],lwd=2,col=cols[i])
  points(dat_mosq2$Allmean_phiI[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]~
           dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]],col=cols[i],pch=pchs[i])
}
legend(1992,0.4,legend=c("West Africa","East Africa","Southern Africa","Central Africa"),
       col=c("red","blue","purple","aquamarine3"),pch=20,cex=1.4)
legend(1992,0.2,legend=c("Kenya","Tanzania","Guinea","Benin","Ghana","Zambia"),
       col=c("blue","blue","red","red","red","aquamarine3"),
       pch=c(13,16,12,6,11,17),ncol=2,
       lty=c(1,2,1,1,6,1),cex=1.4)

#
##
##
## Now repeat for phiB
#i.glm<-glm(Allmean_phiB~Species.grouped+Year+Country, data=dat_mosq2,family=quasibinomial(link= "logit" ))
i.glmb<-glm(Allmean_phiB~Year+Country, data=dat_mosq2)#,family=quasibinomial(link= "logit" ))
summary.lm(i.glmb)
theta = i.glmb$deviance / i.glmb$df.residual ##not over-dispersed
#plot(resid(i.glmb,type="deviance")~i.glmb$fitted.values)
#plot(resid(i.glmb,type="deviance")~dat_mosq2$Country)
#plot(resid(i.glmb,type="deviance")~dat_mosq2$Year)
#plot(resid(i.glmb,type="deviance")~dat_mosq2$Species.grouped) 
exp(cbind(coef(i.glmb), confint(i.glmb)))

mod2<-lmer(Allmean_phiB~Year+(1|Country), data=dat_mosq2)
chmod2<-lmer(Allmean_phiB~1+(1|Country), data=dat_mosq2)
anova(mod2,chmod2)

summary(mod2)
confint(mod2)
se2 <- sqrt(diag(vcov(mod2)))
tab <- cbind(Est = fixef(mod2), LL = fixef(mod2) - 1.96 * se2, UL = fixef(mod2) + 1.96 * se2)
print(exp(tab),digits=3) #to get quick odds ratios for glmm - these show you how important the different levels of each factor are

#sjp.glmer(mod2, y.offset = .2)
## sort all predictors
#sjp.glmer(mod2,
#          facet.grid = FALSE,
#          sort = "sort.all",
#          y.offset = .2)#
#
## plot qq-plot of random effects: dots should be along the line
#sjp.glmer(mod2, type = "re.qq")
#sjp.glmer(mod2, type = "fe.pc")#

# plot probability curves for each covariate
# grouped by random intercepts
#sjp.glmer(mod2,
#          type = "ri.pc",
#          facet.grid = FALSE)

#par(mfrow=c(1,1))
datAnArab <- data.frame(preds = predict(mod2, data.frame(Year = dat_mosq2$Year), type="response", re.form = NA),
                        exp1 = dat_mosq2$Year)
pv = predict(mod2, classify=list("Year"),
             levels=list("Year"=list("Year" = 1992:2015)))
#plot(predict(mod2, data.frame(Year = dat_mosq2$Year), type="response", re.form = NA) ~
#       dat_mosq2$Year,frame=FALSE,bty="n",pch="",ylim=c(0,1),xaxt="n",
#     ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])),xlab="Time (Years)",
#     main="Trends in years",cex.main=1.8,
#     cex.lab=1.6,yaxt="n")
#axis(1, at=1992:2015,labels=1992:2015,cex.axis=1.8)
#axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)     

plot(Allmean_phiB~Year,data=dat_mosq2,pch="",bty="n",ylim=c(0,1),yaxt="n",cex.main=1.8,
     cex.lab=1.6, ylab=expression(paste("Proportion of mosquitoes biting in bed  ", phi[B])))
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)  

##PREDICTIONS FROM THE GLMM WITH COUNTRY AS A RANDOM EFFECT
lines(rev(sort(datAnArab$preds)) ~ sort(datAnArab$exp1),lty=1,lwd=1)
polygon(c(sort(datAnArab$exp1),rev(sort(datAnArab$exp1))),
        c(rev(sort(datAnArab$preds-6*se(datAnArab$preds))),
          sort(datAnArab$preds+6*se(datAnArab$preds))),
        col=transp("grey",0.2),border=NA)             

##PREDICTIONS WITH COUNTRY AS A FIXED EFFECT
cols=c("red","red","red","red","red","red","red","blue","purple","red","blue","aquamarine3","purple")
pchs=c(6:18)
ltys=c(1:7,1,1,8,2,1,2)
for(i in 1:length(unique(dat_mosq2$Country))){
  lines(min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):
          max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]), 
        predict(i.glmb, 
                newdata=
                  data.frame(Year = min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]),
                             Country = rep(levels(unique(dat_mosq2$Country))[i],
                                           length(min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]])))),
                type = "response"),lty=ltys[i],lwd=2,col=cols[i])
  points(dat_mosq2$Allmean_phiB[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]~
           dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]],col=cols[i],pch=pchs[i])
}
#legend(1992,0.4,legend=c("Kenya","Tanzania","Guinea","Benin","Ghana","Zambia"),
#       col=c("blue","blue","red","red","red","aquamarine3"),
#       pch=c(13,16,12,6,11,17),ncol=2,
#       lty=c(1,2,1,1,6,1),cex=1.4)

##########################
###
###
### fIG 1 alternative
par(mfrow=c(1,3))
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-15, 50), ylim = c(-35, 40), asp = 1,col="grey95", border="grey65")
points(36.88,-7.87,col="orange",cex=2,pch=17) ## Killeen et al 2006 ##**HUMAN BED BEHAVIOUR
points(2.116,6.35,col="orange",cex=2, pch=17) ## Moiroux et al 2014 (in Moiroux et al 2012 too) ## HUMAN INDOOR BEHAVIOUR
points(2.09,6.26,col="orange",cex=2, pch=17) ## Moiroux et al 2014 (in Moiroux et al 2012 too)  ## HUMAN INDOOR BEHAVIOUR
points(39.23,-6.81,col="orange",cex=2,pch=17) ## Geissbuhler et al. 1997 ##**HUMAN BED BEHAVIOUR ##**HUMAN INDOOR BEHAVIOUR 

points(36.6,-8.1,col="aquamarine3",cex=2, pch=20)##Russell et al. 2011 ## HUMAN INDOOR BEHAVIOUR
points(39.23,-6.81,col="aquamarine3",cex=2,pch=20) ## Geissbuhler et al. 1997 ##**HUMAN BED BEHAVIOUR ##**HUMAN INDOOR BEHAVIOUR 
points(-1.33,12.6,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (1)  ##HUMAN INDOOR BEHAVIOUR
points(-1.78,12.587,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (2)##HUMAN INDOOR BEHAVIOUR
points(29.83,-15.43,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (3)##HUMAN INDOOR BEHAVIOUR
points(36.6,-9.07,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (4)  ##HUMAN INDOOR BEHAVIOUR
points(39.15,-7.9,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (5)  ##HUMAN INDOOR BEHAVIOUR
points(34.78,-0.1,col="aquamarine3",cex=2, pch=20) ## Huho et al. 1996 (6)  ##HUMAN INDOOR BEHAVIOUR
points(2.116,6.35,col="aquamarine3",cex=2, pch=20) ## Moiroux et al 2014 (in Moiroux et al 2012 too) ## HUMAN INDOOR BEHAVIOUR
points(2.09,6.26,col="aquamarine3",cex=2, pch=20) ## Moiroux et al 2014 (in Moiroux et al 2012 too)  ## HUMAN INDOOR BEHAVIOUR

points(34.9,-0.175,col="purple",cex=2, pch=22) ## Githeko et al. 1996
points(32.48,-25.9,col="purple",cex=2, pch=22) ## Mendis et al 2006
points(9.47,0.399,col="purple",cex=2, pch=22) ## Mourou et al 2012
points(30.40,1.57,col="purple",cex=2, pch=22) ## Ojuka et al. 2015 (1)
points(30.48,2.98,col="purple",cex=2, pch=22) ## Ojuka et al. 2015 (2)
points(8.7,3.5,col="purple",cex=2, pch=22) ## Overgaard et al. 2012
points(0.44,6.35,col="purple",cex=2, pch=22) ## Owusu et al 2016
points(-15.3,13.6,col="purple",cex=2, pch=22) ## Quinones et al 1997
points(33.0,1.0,col="purple",cex=2, pch=22) ##Kabbale et al. 2013
points(34.38,-0.18,col="purple",cex=2, pch=22) ##Bayoh et al. 2014
points(8.7,3.5,col="purple",cex=2, pch=22) ##Bradley et al. 2015
points(34.92,-0.43,col="purple",cex=2, pch=22) ##Cooke et al. 2015
points(-16.4,13.72,col="purple",cex=2, pch=22) ## Fontenille et al. 1997
points(0.06,5.69,col="purple",cex=2, pch=22) ## Tchouassi et al 2012
points(9.1,4.02,col="purple",cex=2, pch=22) ## Tanga et al 2011 (in Tanga et al 2010 too)
points(-1.3,6.57,col="purple",cex=2, pch=22) ## Tuno et al 2010

legend(-15,-5,legend=c("Mosquito biting behaviour",
                       "Human indoor behaviour",
                       "Human bed behaviour"),
       col=c("purple","aquamarine3","orange"),
       pch=c(22,20,17))

datAnArab <- data.frame(preds = predict(mod1, data.frame(Year = dat_mosq2$Year), type="response", re.form = NA),
                        exp1 = dat_mosq2$Year)
pv = predict(mod1, classify=list("Year"),
             levels=list("Year"=list("Year" = 1992:2015)))
#plot(predict(mod1, data.frame(Year = dat_mosq2$Year), type="response", re.form = NA) ~
#       dat_mosq2$Year,frame=FALSE,bty="n",pch="",ylim=c(0,1),xaxt="n",
#     ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])),xlab="Time (Years)",
#     main="Trends in years",cex.main=1.8,
#     cex.lab=1.6,yaxt="n")
#axis(1, at=1992:2015,labels=1992:2015,cex.axis=1.8)
#axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)     

plot(Allmean_phiI~Year,data=dat_mosq2,pch="",bty="n",ylim=c(0,1),yaxt="n",cex.main=1.8,
     cex.lab=1.6, ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])))
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)  

##PREDICTIONS FROM THE GLMM WITH COUNTRY AS A RANDOM EFFECT
lines(rev(sort(datAnArab$preds)) ~ sort(datAnArab$exp1),lty=1,lwd=1)
polygon(c(sort(datAnArab$exp1),rev(sort(datAnArab$exp1))),
        c(rev(sort(datAnArab$preds-6*se(datAnArab$preds))),
          sort(datAnArab$preds+6*se(datAnArab$preds))),
        col=transp("grey",0.2),border=NA)             

##PREDICTIONS WITH COUNTRY AS A FIXED EFFECT
cols=c("red","red","red","red","red","red","red","blue","purple","red","blue","aquamarine3","purple")
pchs=c(6:18)
ltys=c(1:7,1,1,8,2,1,2)
for(i in 1:length(unique(dat_mosq2$Country))){
  lines(min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):
          max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]), 
        predict(i.glm, 
                newdata=
                  data.frame(Year = min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]),
                             Country = rep(levels(unique(dat_mosq2$Country))[i],
                                           length(min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]])))),
                type = "response"),lty=ltys[i],lwd=2,col=cols[i])
  points(dat_mosq2$Allmean_phiI[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]~
           dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]],col=cols[i],pch=pchs[i])
}
legend(1992,0.4,legend=c("West Africa","East Africa","Southern Africa","Central Africa"),
       col=c("red","blue","purple","aquamarine3"),pch=20,cex=1.4)
legend(1992,0.2,legend=c("Kenya","Tanzania","Guinea","Benin","Ghana","Zambia"),
       col=c("blue","blue","red","red","red","aquamarine3"),
       pch=c(13,16,12,6,11,17),ncol=2,
       lty=c(1,2,1,1,6,1),cex=1.4)


datAnArab <- data.frame(preds = predict(mod2, data.frame(Year = dat_mosq2$Year), type="response", re.form = NA),
                        exp1 = dat_mosq2$Year)
pv = predict(mod2, classify=list("Year"),
             levels=list("Year"=list("Year" = 1992:2015)))
#plot(predict(mod2, data.frame(Year = dat_mosq2$Year), type="response", re.form = NA) ~
#       dat_mosq2$Year,frame=FALSE,bty="n",pch="",ylim=c(0,1),xaxt="n",
#     ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])),xlab="Time (Years)",
#     main="Trends in years",cex.main=1.8,
#     cex.lab=1.6,yaxt="n")
#axis(1, at=1992:2015,labels=1992:2015,cex.axis=1.8)
#axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)     

plot(Allmean_phiB~Year,data=dat_mosq2,pch="",bty="n",ylim=c(0,1),yaxt="n",cex.main=1.8,
     cex.lab=1.6, ylab=expression(paste("Proportion of mosquitoes biting in bed  ", phi[B])))
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)  

##PREDICTIONS FROM THE GLMM WITH COUNTRY AS A RANDOM EFFECT
lines(rev(sort(datAnArab$preds)) ~ sort(datAnArab$exp1),lty=1,lwd=1)
polygon(c(sort(datAnArab$exp1),rev(sort(datAnArab$exp1))),
        c(rev(sort(datAnArab$preds-6*se(datAnArab$preds))),
          sort(datAnArab$preds+6*se(datAnArab$preds))),
        col=transp("grey",0.2),border=NA)             

##PREDICTIONS WITH COUNTRY AS A FIXED EFFECT
cols=c("red","red","red","red","red","red","red","blue","purple","red","blue","aquamarine3","purple")
pchs=c(6:18)
ltys=c(1:7,1,1,8,2,1,2)
for(i in 1:length(unique(dat_mosq2$Country))){
  lines(min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):
          max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]), 
        predict(i.glmb, 
                newdata=
                  data.frame(Year = min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]),
                             Country = rep(levels(unique(dat_mosq2$Country))[i],
                                           length(min(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]):max(dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]])))),
                type = "response"),lty=ltys[i],lwd=2,col=cols[i])
  points(dat_mosq2$Allmean_phiB[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]]~
           dat_mosq2$Year[dat_mosq2$Country==levels(unique(dat_mosq2$Country))[i]],col=cols[i],pch=pchs[i])
}

##
write.csv(phiI1ALL,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\behaviour_paper\\Data from Janetta\\phiI1ALL.csv")
write.csv(phiB1ALL,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\behaviour_paper\\Data from Janetta\\phiB1ALL.csv")
## Add in the phiI + phiB.CSV data so that the 9th column is the first distribution of phi values

DAT_Box = read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\behaviour_paper\\Data from Janetta\\phiI1ALL.csv")
DAT_Box = DAT_Box[with(DAT_Box, order(DAT_Box$Year)), ]
Tanzania = subset(DAT_Box,DAT_Box$Country=="Tanzania")
phiI_tan = t(Tanzania[,9:1339])
#par(mfrow=c(1,1))
par(mar=c(6,5,5,5))
boxplot(NA,NA,c(phiI_tan[,1],phiI_tan[,2]),NA,NA,NA,NA,
        c(phiI_tan[,3],phiI_tan[,4]),NA,
        c(phiI_tan[,5],phiI_tan[,6]),NA,
        c(phiI_tan[,7],phiI_tan[,9]),NA,NA,        
        c(phiI_tan[,10],phiI_tan[,11]),NA,NA,NA,NA,NA,NA,
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,
        ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])),
        xlab="Year",cex.lab=1.5,cex.axis=1.8,col=transp("dodgerblue"))
unique(Tanzania$Year)
axis(1,las=0,at=seq(1,21,2),labels=seq(1995,2015,2),cex=1.8,cex.axis=1.8)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)  
par(new=T)
par(mar=c(6,5,5,4.5))
Kenya = subset(DAT_Box,DAT_Box$Country=="Kenya")
phiI_ken = t(Kenya[,9:1339])
Kenya[1:8,1:10]
boxplot(c(phiI_ken[,1],phiI_ken[,2]),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        c(phiI_ken[,3],phiI_ken[,4]),NA,
        c(phiI_ken[,5],phiI_ken[,8]),NA,NA,NA,NA,
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,col=transp("firebrick",0.2))
par(new=T)
par(mar=c(6,5,5,4))
Ghana = subset(DAT_Box,DAT_Box$Country=="Ghana")
phiI_gha = t(Ghana[,9:1339]);dim(phiI_gha)
Ghana[1:6,1:10]
boxplot(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        c(phiI_gha[,1]),NA,
        c(phiI_gha[,2],phiI_gha[,5]),NA,NA,NA,NA,NA,NA,NA,
        c(phiI_gha[,6]),
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,col=transp("gold"))
par(new=T)
par(mar=c(6,5,5,3.5))
Uganda = subset(DAT_Box,DAT_Box$Country=="Uganda")
phiI_uga = t(Ghana[,9:1339]);dim(phiI_uga)
Uganda[1:6,1:10]
boxplot(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        c(phiI_uga[,1],phiI_uga[,2]),
        c(phiI_uga[,3]),
        c(phiI_uga[,4],phiI_uga[,5]),NA,NA,NA,
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,col=transp("red"))
par(new=T)
par(mar=c(6,5,5,3))
Benin = subset(DAT_Box,DAT_Box$Country=="Benin")
phiI_ben = t(Benin[,9:1339]);dim(phiI_ben)
Benin[1:8,1:10]
boxplot(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        c(phiI_ben[,1],phiI_ben[,2]),
        c(phiI_ben[,3],phiI_ben[,4]),NA,
        c(phiI_ben[,5],phiI_ben[,8]),NA,NA,NA,NA,
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,col=transp("aquamarine3"))

legend(1,0.5,legend=c("Benin","Ghana","Kenya","Tanzania","Uganda"),
       col=c("aquamarine3","gold",transp(c("firebrick"),0.2),"dodgerblue","red"),
       pch=15,cex=1.4)


DAT_Box2 = read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\behaviour_paper\\Data from Janetta\\phiB1ALL.csv")
DAT_Box2 = DAT_Box2[with(DAT_Box2, order(DAT_Box2$Year)), ]
Tanzania = subset(DAT_Box2,DAT_Box2$Country=="Tanzania")
phiI_tan = t(Tanzania[,9:250])
#par(mfrow=c(1,1))
par(mar=c(6,5,5,5))
boxplot(NA,NA,c(phiI_tan[,1],phiI_tan[,2]),NA,NA,NA,NA,
        c(phiI_tan[,3],phiI_tan[,4]),NA,
        c(phiI_tan[,5],phiI_tan[,6]),NA,
        c(phiI_tan[,7],phiI_tan[,9]),NA,NA,        
        c(phiI_tan[,10],phiI_tan[,11]),NA,NA,NA,NA,NA,NA,
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,
        ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[B])),
        xlab="Year",cex.lab=1.5,cex.axis=1.8,col=transp("dodgerblue"))
unique(Tanzania$Year)
axis(1,las=0,at=seq(1,21,2),labels=seq(1995,2015,2),cex=1.8,cex.axis=1.8)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.8)  
par(new=T)
par(mar=c(6,5,5,4.5))
Kenya = subset(DAT_Box2,DAT_Box2$Country=="Kenya")
phiI_ken = t(Kenya[,9:250])
Kenya[1:8,1:10]
boxplot(c(phiI_ken[,1],phiI_ken[,2]),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        c(phiI_ken[,3],phiI_ken[,4]),NA,
        c(phiI_ken[,5],phiI_ken[,8]),NA,NA,NA,NA,
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,col=transp("firebrick",0.2))
par(new=T)
par(mar=c(6,5,5,4))
Ghana = subset(DAT_Box2,DAT_Box2$Country=="Ghana")
phiI_gha = t(Ghana[,9:250]);dim(phiI_gha)
Ghana[1:6,1:10]
boxplot(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        c(phiI_gha[,1]),NA,
        c(phiI_gha[,2],phiI_gha[,5]),NA,NA,NA,NA,NA,NA,NA,
        c(phiI_gha[,6]),
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,col=transp("gold"))
par(new=T)
par(mar=c(6,5,5,3.5))
Uganda = subset(DAT_Box2,DAT_Box2$Country=="Uganda")
phiI_uga = t(Ghana[,9:250]);dim(phiI_uga)
Uganda[1:6,1:10]
boxplot(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        c(phiI_uga[,1],phiI_uga[,2]),
        c(phiI_uga[,3]),
        c(phiI_uga[,4],phiI_uga[,5]),NA,NA,NA,
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,col=transp("red"))
par(new=T)
par(mar=c(6,5,5,3))
Benin = subset(DAT_Box2,DAT_Box2$Country=="Benin")
phiI_ben = t(Benin[,9:250]);dim(phiI_ben)
Benin[1:8,1:10]
boxplot(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
        c(phiI_ben[,1],phiI_ben[,2]),
        c(phiI_ben[,3],phiI_ben[,4]),NA,
        c(phiI_ben[,5],phiI_ben[,8]),NA,NA,NA,NA,
        xaxt="n",yaxt="n",ylim=c(0,1),frame=FALSE,col=transp("aquamarine3"))

legend(1,0.5,legend=c("Benin","Ghana","Kenya","Tanzania","Uganda"),
       col=c("aquamarine3","gold",transp(c("firebrick"),0.2),"dodgerblue","red"),
       pch=15,cex=1.4)

##########################
###
###
### Figure 4: 
    ##LLIN efficacy, 
    ##different coverages,
    ##phiB (as behavioural resistance) 
    ##EIR, and 
    ##Mechanical resistance

