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

legend(-30,-5,legend=c("Mosquito biting behaviour",
                    "Human indoor behaviour",
                    "Human bed behaviour"),
       col=c("purple","aquamarine3","orange"),
       pch=c(22,20,17))

##**Add for the PMI sites!!**##
#points(34.9,-0.175,col="darkred",cex=2.5, pch=18) ## Githeko et al. 1996
#points(32.48,-25.9,col="darkred",cex=2.5, pch=18) ## Mendis et al 2006

##########################
###
###
### Figure 2: Summary metadata, mosquitoes activity times, people indoors and in bed
dat_mosq = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI_phiB_rawdata.csv",header=TRUE)
dat_indoor = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_indoor_vs_time.csv",header=TRUE)
dat_inbed = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_sleeping_vs_time(1).csv",header=TRUE)

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
  lines(-dat_mosq[veca[i]:vecb[i],3] ~ c(1:24),col="black")
}

dat_gamb = subset(dat_mosq,dat_mosq$species_cleaned == "A_gambiae")
veca_G = seq(1,nrow(dat_gamb),24)
vecb_G = c(veca_G[2:22]-1,524)
for(i in 1:length(veca_G)){
  lines(dat_gamb[veca_G[i]:vecb_G[i],2] ~ c(1:24),col="red",lty=2)
  lines(-dat_gamb[veca_G[i]:vecb_G[i],3] ~ c(1:24),col="red",lty=2)
}

dat_fun = subset(dat_mosq,dat_mosq$species_cleaned == "A_funestus")
veca_F = seq(1,nrow(dat_fun),24)
vecb_F = c(veca_F[2:22]-1,456)
for(i in 1:length(veca_F)){
  lines(dat_fun[veca_F[i]:vecb_F[i],2] ~ c(1:24),col="cyan2")
  lines(-dat_fun[veca_F[i]:vecb_F[i],3] ~ c(1:24),col="cyan2")
}

text(21,0.05,"Indoor activity",cex=1.6)
text(21,-0.05,"Outdoor activity",cex=1.6)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
text(24,0.45,"A",cex=2)

legend(15,-0.2,legend=c("An. funestus","An. gambiae","Other"),
       col=c("cyan2","red","black"),lty=1,lwd=2,cex=1.4)

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

legend(16,0.8,legend=c("Tanzania",
                       "Burina Faso",
                       "Zambia",
                       "Kenya",
                       "Benin"),
       lty=1,lwd=2,cex=1.4,col=c("orange","purple","blue","aquamarine3","darkred"))

plot(dat_inbed[,3]~dat_inbed[1:24,1],pch="",bty="n",ylab="Proportion of people in bed",
     xlab="Time (hours)",cex.lab=1.2,ylim=c(0,1),xaxt="n",las=2,cex.axis=1.4)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
lines(dat_inbed[1:24,3]~dat_inbed[1:24,1],col="orange",lwd=2)##Tanzania
lines(dat_inbed[1:24,4]~dat_inbed[1:24,1],col="orange",lwd=2)##Tanzania

lines(dat_inbed[,5]~dat_inbed[,1],lty=2,lwd=3)##Average
text(24,1,"C",cex=2)


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
as.numeric(quantile(c(phiB1ALL),c(0.025,0.05,0.1,0.25,0.35,0.5,0.65,0.75,0.9,0.95,0.975))  )
as.numeric(quantile(c(phiI1ALL),c(0.025,0.05,0.1,0.25,0.35,0.5,0.65,0.75,0.9,0.95,0.975))  )

##Each row is the confidence intervals for the specific mosquito data
dat_mosq2 = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiB+phiI.csv",header=TRUE)

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

quantile(c(phiI1ALL),c(0.025,0.25,0.5,0.75,0.975))  

LOW1 = c(0,0.5045155,0.8140084,0.8908540,0.9400264,0.9890247,1)
val = c(0.2,0.4,0.6,0.6,0.4,0.2)
for(i in 1:6){
  polygon(c(min(dat_mosq2[,4]),max(dat_mosq2[,4]+1),
            max(dat_mosq2[,4]+1),min(dat_mosq2[,4])),
          c(LOW1[i],LOW1[i],
            LOW1[i+1],LOW1[i+1]),
          col=transp("grey",val[i]),border = FALSE)
}
abline(h=median(c(phiI1ALL)),lwd=2,col="grey")

for(i in 1:61){
  segments(x0=dat_mosq2[,4][i],x1=dat_mosq2[,4][i],
           y0=quantile(phiI1ALL[i,],na.rm=TRUE,0.975),
           y1=quantile(phiI1ALL[i,],na.rm=TRUE,0.025),
           lty=1,col="black")
  
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


legend(1995,0.38,legend=c("Kolombero Valley, Tanzania",
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

quantile(c(phiB1ALL),c(0.025,0.25,0.5,0.75,0.975),na.rm=TRUE)  

LOW1 = c(0,0.3758026,0.6669899,0.7851768,0.8560957,0.9533279,1)
val = c(0.2,0.4,0.6,0.6,0.4,0.2)
for(i in 1:6){
  polygon(c(min(dat_mosq2[,4]),max(dat_mosq2[,4]+1),
            max(dat_mosq2[,4]+1),min(dat_mosq2[,4])),
          c(LOW1[i],LOW1[i],
            LOW1[i+1],LOW1[i+1]),
          col=transp("grey",val[i]),border = FALSE)
}
abline(h=median(c(phiB1ALL),na.rm=TRUE),lwd=2,col="grey")

for(i in 1:61){
  segments(x0=dat_mosq2[,4][i],x1=dat_mosq2[,4][i],
           y0=quantile(phiB1ALL[i,],na.rm=TRUE,0.975),
           y1=quantile(phiB1ALL[i,],na.rm=TRUE,0.025),
           lty=1,col="black")
  
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

hist(PHII,breaks=50,col=transp("red",0.6),border=NA,main="",xlim=c(0,1),ylim=c(0,100),
     ylab="Frequency", xlab="Proportion of mosquitoes biting",cex.lab=1.4,cex.axis=1.4,yaxt="n")
axis(2,las=2,at=seq(0,100,20),labels=seq(0,100,20), cex=1.4,cex.lab=1.4,cex.axis=1.4)
hist(PHIB,add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
abline(v=mean(PHIB,na.rm=TRUE),lty=2,lwd=2,col="blue4")
abline(v=mean(PHII,na.rm=TRUE),lty=2,lwd=2,col="darkred")
#text(mean(PHIB,na.rm=TRUE)-0.15,100,expression(paste("In bed bites ", phi[B])),cex=1.5)
#text(mean(PHII,na.rm=TRUE)-0.05,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
text(0.2,100,expression(paste("In bed bites ", phi[B])),cex=1.5,col="darkblue")
text(0.3,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")

text(0.3,95,"Mean: 0.746",cex=1.5,col="blue4",font=4)
text(0.4,45,"Mean: 0.859",cex=1.5,col="darkred",font=4)

text(0.3,90,"Median: 0.785",cex=1.5,col="blue4",font=4)
text(0.4,40,"Median: 0.891",cex=1.5,col="darkred",font=4)

summary(PHII,na.rm=TRUE)
summary(PHIB,na.rm=TRUE)

##################################
##
## Supplementary figure 
##
##
dat_mosq = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI_phiB_rawdata.csv",header=TRUE)
dat_indoor = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_indoor_vs_time.csv",header=TRUE)
dat_inbed = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_sleeping_vs_time(1).csv",header=TRUE)

veca = seq(1,nrow(dat_mosq),24)
vecb = c(veca[2:61]-1,1464)
vecc = c("red","purple","grey","aquamarine3","coral3","yellow","blue")

dat_gamb = subset(dat_mosq,dat_mosq$species_cleaned == "A_gambiae")
veca_G = seq(1,nrow(dat_gamb),24)
vecb_G = c(veca_G[2:22]-1,524)

dat_fun = subset(dat_mosq,dat_mosq$species_cleaned == "A_funestus")
veca_F = seq(1,nrow(dat_fun),24)
vecb_F = c(veca_F[2:22]-1,456)

par(mar=c(5,5,5,5))
par(mfrow=c(2,3))

func = function(datam,val1,tab1,col1){
  plot(dat_indoor[,2]~dat_indoor[,1],pch="",bty="n",ylab="Proportion of people indoors",
       xlab="Time (hours)",cex.lab=1.6,ylim=c(0,1),xaxt="n",las=2,cex.axis=1.6)
  axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.6,cex=1.6)
  axis(4,las=2,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2),cex.axis=1.4,cex.lab=1.6)
  mtext(side=4,line=3,"Proportion of mosquito bites",cex=1)
  polygon(c(dat_indoor[,1]),
          c(dat_indoor[,14]),
          col="grey",border=NA)##Average
  
  polygon(c(dat_inbed[,1]),c(dat_inbed[,5]),
          col="grey55",border=NA)
  #lines(dat_gamb[veca_G[39]:vecb_G[39],2] ~ c(1:24),col="blue",lty=1,lwd=1)
  lines(datam[veca_F[val1]:vecb_F[val1],2] ~ c(1:24),col=col1,lty=1,lwd=1)
  lines(datam[veca_F[val1]:vecb_F[val1],3] ~ c(1:24),col=col1,lty=2,lwd=1)
  text(24,1,tab1,cex=2)
}
func(dat_fun,13,"A","cyan2")
func(dat_fun,15,"B","cyan2")
func(dat_fun,17,"C","cyan2")

func(dat_fun,14,"D","cyan2")
func(dat_fun,16,"E","cyan2")
func(dat_fun,18,"F","cyan2")

##########################
###
###
### Is there a statistical trend in phiI and phiB over time?
summary(lm(dat_mosq2[,14] ~ dat_mosq2[,4]))
summary(lm(dat_mosq2[,15] ~ dat_mosq2[,4]))

summary(lm(dat_mosq2[,16] ~ dat_mosq2[,4]))
summary(lm(dat_mosq2[,17] ~ dat_mosq2[,4]))

summary(lm(dat_mosq2[,2] ~ dat_mosq2[,4]))
summary(lm(dat_mosq2[,3] ~ dat_mosq2[,4]))

plot(dat_mosq2[,3] ~ dat_mosq2[,14],ylim=c(0,1),xlim=c(0,1))
abline(lm(dat_mosq2[,3] ~ dat_mosq2[,14]+0))
points(dat_mosq2[,16] ~ dat_mosq2[,14],pch=20)
abline(lm(dat_mosq2[,16] ~ dat_mosq2[,14]+0),lty=2)
points(dat_mosq2[,3] ~ dat_mosq2[,16],pch=15)
abline(lm(dat_mosq2[,3] ~ dat_mosq2[,16]+0),lty=3)
points(dat_mosq2[,14] ~ dat_mosq2[,16],pch=20,col="red")
abline(lm(dat_mosq2[,14] ~ dat_mosq2[,16]+0),lty=2,col="red")

dat_mosq2$trans_phiI = 57.295*asin(dat_mosq2[,13])
dat_mosq2$trans_phiB = 57.295*asin(dat_mosq2[,14])

levels(dat_mosq2$Species.grouped)[1] <- "A_gambiae_sl"
levels(dat_mosq2$Species.grouped)[4] <- "A_gambiae_sl"
levels(dat_mosq2$Species.grouped)[4:7] <- "An_other"
summary(dat_mosq2$Species.grouped)

MOD1 <- lmer(phiI ~ Year + (1|Species.grouped) + (1|Country),data=dat_mosq2)
MOD2 <- lmer(phiB ~ Year + (1|Species.grouped) + (1|Country),data=dat_mosq2)

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
#write.csv(phiI1ALL,"C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI1ALL.csv")
#write.csv(phiB1ALL,"C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiB1ALL.csv")
## Add in the phiI + phiB.CSV data so that the 9th column is the first distribution of phi values

DAT_Box = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI1ALL.csv")
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


DAT_Box2 = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiB1ALL.csv")
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
        ylab=expression(paste("Proportion of mosquitoes biting in bed  ", phi[B])),
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

######################
## Run the model with phiI and phiB 
## for quantiles 25% 50% and 75% 

## also do 5% ... 95% quantiles to estimate relationship between prevalence 2-12 and phiI and phiB
library(MalariaLaunchR)

phi_function<-function(site,
                       
                       ITN, IRS,
                       
                       phi_I,phi_B,
                        
                       irs_decay_mort1_1, irs_decay_mort2_1,irs_decay_succ1_1,
                       irs_decay_succ2_1,	irs_decay_det1_1,	irs_decay_det2_1,
                       itn_repel_fun_1,	itn_repel_gamb_ss_1,	itn_repel_arab_1,
                       itn_kill_fun_1,	itn_kill_gamb_ss_1,	itn_kill_arab_1,
                       itn_half_life,
                       
                       run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/seasonal_sites/Site_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'output_type 0 itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    
                    'fun_Q_in',phi_I,'fun_Q_bed',phi_B,
                    'arab_Q_in',phi_I,'arab_Q_bed',phi_B,
                    'gamb_ss_Q_in',phi_I,'gamb_ss_Q_bed',phi_B,
                    
                    'add change_irs 1 change_irs_time 0 change_itn 1 change_itn_time 0',  
                    'irs_decay_mort1_1', irs_decay_mort1_1,	'irs_decay_mort2_1', irs_decay_mort2_1,	'irs_decay_succ1_1', irs_decay_succ1_1,
                    'irs_decay_succ2_1', irs_decay_succ2_1,	'irs_decay_det1_1', irs_decay_det1_1,	'irs_decay_det2_1', irs_decay_det2_1,
                    'itn_repel_fun_1', itn_repel_fun_1, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_1, 'itn_repel_arab_1', itn_repel_arab_1,
                    'itn_kill_fun_1', itn_kill_fun_1, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_1, 'itn_kill_arab_1', itn_kill_arab_1,
                    'itn_half_life_1', itn_half_life,
                    
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_'),
                 OutputRoot = paste0("F:/Ellies_output_folder/", Run_name, '/draw_', draw),
                 Options=Options,
                 Exe = "F:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="F:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('seasonal_sites/Site_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}


## ON COMP
#inp_phi = read.csv("F:/phiI_B_estimates_res0_cov80.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res0_cov80_LLINonly.csv",header=TRUE)

#inp_phi = read.csv("F:/phiI_B_estimates_res0_cov20.csv",header=TRUE)
inp_phi = read.csv("F:/phiI_B_estimates_res0_cov20_LLINonly.csv",header=TRUE)

#inp_phi = read.csv("F:/phiI_B_estimates_res0_cov50.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res0_cov50_LLINonly.csv",header=TRUE)

#inp_phi = read.csv("F:/phiI_B_estimates_res50_cov80.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res50_cov80_LLINonly.csv",header=TRUE)
###inp_phi = read.csv("F:/phiI_B_estimates_noVecInterventions.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res20_cov50_LLINonly.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res20_cov50.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res40_cov50_LLINonly.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res40_cov50.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res60_cov50_LLINonly.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res60_cov50.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res80_cov50_LLINonly.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B_estimates_res80_cov50.csv",header=TRUE)

#for(i in 1:3){
#  phi_function(site=inp_phi[i,1], 
#                
#                ITN=inp_phi[i,2], IRS=inp_phi[i,3],
#                
#                phi_I=inp_phi[i,4],phi_B=inp_phi[i,5],
#                irs_decay_mort1_1=inp_phi[i,6], irs_decay_mort2_1=inp_phi[i,7], 
#                irs_decay_succ1_1=inp_phi[i,8], irs_decay_succ2_1=inp_phi[i,9], 
#                irs_decay_det1_1=inp_phi[i,10], irs_decay_det2_1=inp_phi[i,11],
#                itn_repel_fun_1=inp_phi[i,12], itn_repel_gamb_ss_1=inp_phi[i,13], itn_repel_arab_1=inp_phi[i,14], 
#                itn_kill_fun_1=inp_phi[i,15], itn_kill_gamb_ss_1=inp_phi[i,16], itn_kill_arab_1=inp_phi[i,17],
#                
#               itn_half_life = inp_phi[i,18],
#                run_name="phi")
#  
#}

phi_res0_prev = 

phi_res0_prev_irs80_nets80 = 
  phi_res0_prev_nets80 = 
  phi_res0_prev_irs50_nets50 = 
  phi_res0_prev_nets50 = 
  phi_res0_prev_irs20_nets20 = 
  phi_res0_prev_nets20 = 
  
  phi_res20_prev_irs50_nets50 = 
  phi_res20_prev_nets50 = 
  phi_res40_prev_irs50_nets50 = 
  phi_res40_prev_nets50 = 
  phi_res60_prev_irs50_nets50 = 
  phi_res60_prev_nets50 = 
  phi_res80_prev_irs50_nets50 = 
  phi_res80_prev_nets50 = 
  
  phi_res50_prev_irs80_nets80 = 
  phi_res50_prev_nets80 = 
  phi_res50_prev_irs50_nets50 = 
  phi_res50_prev_nets50 = 
  phi_res50_prev_irs20_nets20 = 
  phi_res50_prev_nets20 = 
  
  phi_res100_prev_irs80_nets80 = 
  phi_res100_prev_nets80 = 
  phi_res100_prev_irs50_nets50 = 
  phi_res100_prev_nets50 = 
  phi_res100_prev_irs20_nets20 = 
  phi_res100_prev_nets20 = 
  
  expand.grid(time = seq(-5,20,length=1301))
for(i in 1:44){
  phi_res0_prev[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_noVecInterventions/draw_0/draw_0_noVecInterventions",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
    
  phi_res0_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_IRS50andLLIN50/draw_0/draw_0_res0_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res0_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_LLIN50/draw_0/draw_0_res0_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res20_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res20_IRS50andLLIN50/draw_0/draw_0_res20_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res20_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res20_LLIN50/draw_0/draw_0_res20_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res40_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res40_IRS50andLLIN50/draw_0/draw_0_res40_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res40_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res40_LLIN50/draw_0/draw_0_res40_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_IRS50andLLIN50/draw_0/draw_0_res50_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_LLIN50/draw_0/draw_0_res50_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res60_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res60_IRS50andLLIN50/draw_0/draw_0_res60_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res60_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res60_LLIN50/draw_0/draw_0_res60_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res80_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res80_IRS50andLLIN50/draw_0/draw_0_res80_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res80_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res80_LLIN50/draw_0/draw_0_res80_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_IRS50andLLIN50/draw_0/draw_0_res100_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_LLIN50/draw_0/draw_0_res100_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
  phi_res0_prev_irs80_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_IRS80andLLIN80/draw_0/draw_0_res0_IRS80andLLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res0_prev_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_LLIN80/draw_0/draw_0_res0_LLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res0_prev_irs20_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_IRS20andLLIN20/draw_0/draw_0_res0_IRS20andLLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res0_prev_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_LLIN20/draw_0/draw_0_res0_LLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
  phi_res50_prev_irs80_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_IRS80andLLIN80/draw_0/draw_0_res50_IRS80andLLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_LLIN80/draw_0/draw_0_res50_LLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_irs20_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_IRS20andLLIN20/draw_0/draw_0_res50_IRS20andLLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_LLIN20/draw_0/draw_0_res50_LLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
  phi_res100_prev_irs80_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_IRS80andLLIN80/draw_0/draw_0_res100_IRS80andLLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_LLIN80/draw_0/draw_0_res100_LLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_irs20_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_IRS20andLLIN20/draw_0/draw_0_res100_IRS20andLLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_LLIN20/draw_0/draw_0_res100_LLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
}
##Estimating lost efficacy
##No resistance comparison for each season with IRS only are columns 2 to 5
##20, 50, 80 and 100 are cols 10-13, 18-21, 26-29, 34-37

vec_hs = seq(2,42,by=4)
vec_pe = vec_hs + 1
vec_se = vec_hs + 2
vec_bi = vec_hs + 3

vec_s = vec_pe
prev_res0 = prev_res0_irs20_nets20 = prev_res50_irs20_nets20 =  prev_res100_irs20_nets20 =  
  prev_res0_irs50_nets50 = prev_res50_irs50_nets50 =  prev_res100_irs50_nets50 =  
  prev_res0_irs80_nets80 = prev_res50_irs80_nets80 =  prev_res100_irs80_nets80 =  
  prev_res0_nets20 = prev_res50_nets20 = prev_res100_nets20 =  
  prev_res0_nets50 = prev_res50_nets50 = prev_res100_nets50 =   
  prev_res0_nets80 = prev_res50_nets80 = prev_res100_nets80 =  
  prev_res20_irs50_nets50 = prev_res40_irs50_nets50 = prev_res50_irs50_nets50 =
prev_res60_irs50_nets50 = prev_res80_irs50_nets50 = prev_res100_irs50_nets50 =
prev_res20_nets50 = prev_res40_nets50 = prev_res50_nets50 =
prev_res60_nets50 = prev_res80_nets50 = prev_res100_nets50 = numeric(11)

for(i in 1:11){
  prev_res0[i]  = phi_res0_prev[572,vec_s[i]]
  
  prev_res0_irs20_nets20[i]  = phi_res0_prev_irs20_nets20[572,vec_s[i]]
  prev_res50_irs20_nets20[i] = phi_res50_prev_irs20_nets20[572,vec_s[i]]
  prev_res100_irs20_nets20[i] = phi_res100_prev_irs20_nets20[572,vec_s[i]]
  
  prev_res0_irs50_nets50[i]  = phi_res0_prev_irs50_nets50[572,vec_s[i]]
  prev_res50_irs50_nets50[i] = phi_res50_prev_irs50_nets50[572,vec_s[i]]
  prev_res100_irs50_nets50[i] = phi_res100_prev_irs50_nets50[572,vec_s[i]]
  
  prev_res0_irs80_nets80[i]  = phi_res0_prev_irs80_nets80[572,vec_s[i]]
  prev_res50_irs80_nets80[i] = phi_res50_prev_irs80_nets80[572,vec_s[i]]
  prev_res100_irs80_nets80[i] = phi_res100_prev_irs80_nets80[572,vec_s[i]]
  
  prev_res0_nets20[i]  = phi_res0_prev_nets20[572,vec_s[i]]
  prev_res50_nets20[i] = phi_res50_prev_nets20[572,vec_s[i]]
  prev_res100_nets20[i] = phi_res100_prev_nets20[572,vec_s[i]]
  
  prev_res0_nets50[i]  = phi_res0_prev_nets50[572,vec_s[i]]
  prev_res50_nets50[i] = phi_res50_prev_nets50[572,vec_s[i]]
  prev_res100_nets50[i] = phi_res100_prev_nets50[572,vec_s[i]]
  
  prev_res0_nets80[i]  = phi_res0_prev_nets80[572,vec_s[i]]
  prev_res50_nets80[i] = phi_res50_prev_nets80[572,vec_s[i]]
  prev_res100_nets80[i] = phi_res100_prev_nets80[572,vec_s[i]]
  
  prev_res20_irs50_nets50[i] =phi_res20_prev_irs50_nets50[572,vec_s[i]]
  prev_res40_irs50_nets50[i] =phi_res40_prev_irs50_nets50[572,vec_s[i]]
  prev_res60_irs50_nets50[i] =phi_res60_prev_irs50_nets50[572,vec_s[i]]
  prev_res80_irs50_nets50[i] =phi_res80_prev_irs50_nets50[572,vec_s[i]]
  
  prev_res20_nets50[i] =phi_res20_prev_nets50[572,vec_s[i]]
  prev_res40_nets50[i] =phi_res40_prev_nets50[572,vec_s[i]]
  prev_res60_nets50[i] =phi_res60_prev_nets50[572,vec_s[i]]
  prev_res80_nets50[i] =phi_res80_prev_nets50[572,vec_s[i]]
}

phiI = unique(inp_phi$phi_I)
phiB = unique(inp_phi$phi_B)

dat_sums = expand.grid(year=rep(6,209))
dat_sums[,2] = c(prev_res0,
                 prev_res0_irs20_nets20,prev_res50_irs20_nets20,prev_res100_irs20_nets20,
                 prev_res0_nets20,prev_res50_nets20,prev_res100_nets20,
                 prev_res0_irs50_nets50,prev_res50_irs50_nets50,prev_res100_irs50_nets50,
                 prev_res0_nets50,prev_res50_nets50,prev_res100_nets50,
                 prev_res0_irs80_nets80,prev_res50_irs80_nets80,prev_res100_irs80_nets80,
                 prev_res0_nets80,prev_res50_nets80,prev_res100_nets80)
dat_sums[,3] = rep(phiI,19)
dat_sums[,4] = rep(phiB,19)
dat_sums[,5] = c(rep(0,length(phiI)),rep(c(rep(0,length(phiI)),rep(50,length(phiI)),rep(100,length(phiI))),6)) # Resistance
dat_sums[,6] = c(rep(0,length(phiI)),
                 c(rep(20,3*length(phiI)),rep(0,3*length(phiI)),
                       rep(50,3*length(phiI)),rep(0,3*length(phiI)),
                       rep(80,3*length(phiI)),rep(0,3*length(phiI))) ) # IRS cover
dat_sums[,7] = c(rep(0,length(phiI)),rep(20,6*length(phiI)),rep(50,6*length(phiI)),rep(80,6*length(phiI)) ) # net cover
colnames(dat_sums) = c("Year","Prevalence_2_10","phiI","phiB","Resistance","IRS_cov","LLIN_cov")
head(dat_sums)

#############################################
##
## Efficacy of res_0
##
##Efficacy = (1 - (prev_with_intervention / prev_without_intervention)) * 100
Efficacy = array(dim=c(11,6))
Efficacy50 = array(dim=c(11,6))
Efficacy100 = array(dim=c(11,6))
vec_net = c(20,50,80,20,50,80)
vec_irs = c(0,0,0,20,50,80)
for(i in 1:6){
  Efficacy[,i] = (1 - (dat_sums$Prevalence_2_10[dat_sums$Resistance=="0" & 
                                              dat_sums$IRS_cov == vec_irs[i] &
                                              dat_sums$LLIN_cov == vec_net[i]] / 
                     dat_sums$Prevalence_2_10[dat_sums$Resistance=="0" & 
                                                dat_sums$IRS_cov == "0" &
                                                dat_sums$LLIN_cov == "0"])) * 100
 
  Efficacy50[,i] = (1 - (dat_sums$Prevalence_2_10[dat_sums$Resistance=="50" & 
                                                  dat_sums$IRS_cov == vec_irs[i] &
                                                  dat_sums$LLIN_cov == vec_net[i]] / 
                         dat_sums$Prevalence_2_10[dat_sums$Resistance=="0" & 
                                                    dat_sums$IRS_cov == "0" &
                                                    dat_sums$LLIN_cov == "0"])) * 100
  
  Efficacy100[,i] = (1 - (dat_sums$Prevalence_2_10[dat_sums$Resistance=="100" & 
                                                  dat_sums$IRS_cov == vec_irs[i] &
                                                  dat_sums$LLIN_cov == vec_net[i]] / 
                         dat_sums$Prevalence_2_10[dat_sums$Resistance=="0" & 
                                                    dat_sums$IRS_cov == "0" &
                                                    dat_sums$LLIN_cov == "0"])) * 100
  
}


dat_sums2 = expand.grid(year=rep(6,11*15))
dat_sums2$Prevalence_2_10 =c(prev_res0,
                             prev_res0_irs50_nets50,
                             prev_res20_irs50_nets50,prev_res40_irs50_nets50,prev_res50_irs50_nets50,
                             prev_res60_irs50_nets50,prev_res80_irs50_nets50,prev_res100_irs50_nets50,
                             prev_res0_nets50,
                             prev_res20_nets50,prev_res40_nets50,prev_res50_nets50,
                             prev_res60_nets50,prev_res80_nets50,prev_res100_nets50)

dat_sums2$Resistance = c(rep(c(0,0,20,40,50,60,80,100),each=11),rep(c(0,20,40,50,60,80,100),each=11))
dat_sums2$IRS_cov = c(rep(0,11),rep(50,11*7),rep(0,11*7))
dat_sums2$LLIN_cov = c(rep(0,11),rep(50,11*14)) 

vec_res = c(0,0,20,20,40,40,50,50,60,60,80,80,100,100)
vec_irs2 = c(0,50,0,50,0,50,0,50,0,50,0,50,0,50)


Efficacy_cov50 = array(dim=c(11,14))
for(i in 1:14){
  Efficacy_cov50[,i] = (1 - (dat_sums2$Prevalence_2_10[dat_sums2$Resistance == vec_res[i] & 
                                                  dat_sums2$IRS_cov == vec_irs2[i] &
                                                  dat_sums2$LLIN_cov == "50"] / 
                         dat_sums2$Prevalence_2_10[dat_sums2$Resistance=="0" & 
                                                    dat_sums2$IRS_cov == "0" &
                                                    dat_sums2$LLIN_cov == "0"])) * 100
}

(1 - (dat_sums2$Prevalence_2_10[dat_sums2$Resistance == "80" & 
                                  dat_sums2$IRS_cov == "50" &
                                  dat_sums2$LLIN_cov == "50"] / 
        dat_sums2$Prevalence_2_10[dat_sums2$Resistance=="0" & 
                                    dat_sums2$IRS_cov == "0" &
                                    dat_sums2$LLIN_cov == "0"])) * 100

par(mfrow=c(1,2))
plot(Efficacy[,1]~phiI,ylim=c(0,100),xlim=c(0.4,1),
     ylab="Vector intervention efficacy (%)",bty="n",pch="",
     xlab=expression(phi[I]),cex.lab=1.6,cex.axis=1.6,yaxt="n")
axis(2,las=2,at=seq(0,100,by=20),label=seq(0,100,by=20),cex.lab=1.6,cex.axis=1.6)
lty_vec=c(2,2,2,1,1,1)
lwd_vec=c(1,2,3,1,2,3)
for(i in 1:6){
  lines(Efficacy[,i]~phiI,lty=lty_vec[i],lwd=lwd_vec[i])
}
effs_irs_nets=effs_nets=array(dim=c(7,11))
for(i in 1:11){
  effs_irs_nets[,i] = c(Efficacy_cov50[i,1],Efficacy_cov50[i,3],Efficacy_cov50[i,5],Efficacy_cov50[i,7],
                        Efficacy_cov50[i,9],Efficacy_cov50[i,11],Efficacy_cov50[i,13])
  effs_nets[,i] =     c(Efficacy_cov50[i,2],Efficacy_cov50[i,4],Efficacy_cov50[i,6],Efficacy_cov50[i,8],
                        Efficacy_cov50[i,10],Efficacy_cov50[i,12],Efficacy_cov50[i,14])
}

res2 = c(0,20,40,50,60,80,100)
plot(effs_irs_nets[,1]~res2,ylim=c(0,100),xlim=c(0,100),
     ylab="Vector intervention efficacy (%)",bty="n",pch="",
     xlab="Physiological resistance",cex.lab=1.6,cex.axis=1.6,yaxt="n")
axis(2,las=2,at=seq(0,100,by=20),label=seq(0,100,by=20),cex.lab=1.6,cex.axis=1.6)

#for(i in 1:11){
#  lines(effs_irs_nets[,i]~res2,lty=1,lwd=1)
#  lines(effs_nets[,i]~res2,lty=2,lwd=1)
#}

vec_col = c(0.1,0.3,0.5,0.7,0.9,0.9,0.7,0.5,0.3,0.1)
for(i in 1:10){
  polygon(c(res2,rev(res2)),
          c(effs_irs_nets[,i],rev(effs_irs_nets[,i+1])),col=transp("grey",vec_col[i]),border=NA)
  polygon(c(res2,rev(res2)),
          c(effs_nets[,i],rev(effs_nets[,i+1])),col=transp("grey",vec_col[i]),border=NA)
}
lines(effs_irs_nets[,6]~res2,lty=1,lwd=2)
lines(effs_nets[,6]~res2,lty=2,lwd=2)

explore_seas_f = function(vec_s,seas1,choose_col,
                          phi_dat,phi_datb,
                          phi_dat1,phi_dat2,phi_dat3,title_2){

  
  par(mfrow=c(1,2))
  plot(phi_dat[157:520,seas1]~phi_dat[157:520,1],
       ylab="Prevalence 2 - 10 year olds",yaxt = "n",cex.axis=1.6,cex.lab=1.6,
       xlab="Time (years)",bty="n",pch="",ylim=c(0,0.4))
  axis(2,las=2,at=seq(0,0.4,by=0.1),labels=seq(0,0.4,by=0.1),cex.axis=1.6,cex.lab=1.6)
  vec_lty=rep(c(1,2,3,4),11)
  
  for(i in 1:11){
    lines(phi_dat[157:520,vec_s[i]]~phi_dat[157:520,1],lty=1,col="grey")
    lines(phi_datb[157:520,vec_s[i]]~phi_datb[157:520,1],lty=2,col="grey")
  }
  abline(v=0,lty=2,col="grey")
  
  vec=c(0.1,0.3,0.5,0.7,0.9,0.9,0.7,0.5,0.3,0.1)
  for(i in 1:10){
    polygon(c(phi_dat[157:520,1],rev(phi_dat[157:520,1])),
            c(phi_dat[157:520,vec_s[i]],rev(phi_dat[157:520,vec_s[i+1]])),
            col=transp("grey",vec[i]),border = FALSE)
  
    polygon(c(phi_datb[157:520,1],rev(phi_datb[157:520,1])),
            c(phi_datb[157:520,vec_s[i]],rev(phi_datb[157:520,vec_s[i+1]])),
            col=transp("grey",vec[i]),border = FALSE)
    
    }
  
  
  lines(phi_dat[157:520,seas1]~phi_dat[157:520,1],lty=1,lwd=2,col="black")
  lines(phi_datb[157:520,seas1]~phi_datb[157:520,1],lty=2,lwd=2,col="black",0.6)
  
  par(mar=c(6,6,2,2))
  boxplot(phi_dat1,phi_dat2,phi_dat3,ylim=c(0,0.4),
          ylab="Prevalence 2 - 10 years",yaxt="n",col=transp("darkred",0.6),
          xlab=title_2,xaxt="n",cex.lab=1.6,cex.axis=1.6,frame=FALSE)
  axis(2,las=2,at=c(0,0.1,0.2,0.3,0.4),labels=c(0,0.1,0.2,0.3,0.4),cex.lab=1.6,cex.axis=1.6)
  axis(1,at=c(1,2,3),labels=c(20,50,80),cex.lab=1.6,cex.axis=1.6)
  
}
seas1=23
phi_datb=phi_res0_prev_nets50
phi_dat=phi_res0_prev_irs50_nets50
explore_seas_f(vec_pe,23,"blue",
               phi_res0_prev_irs50_nets50,phi_res0_prev_nets50,
               prev_res0_irs20_nets20,prev_res0_irs50_nets50,prev_res0_irs80_nets80,
               "Intervention coverage level")

plot(prev_res0_irs20_nets20~phiI,ylim=c(0,0.3))
lines(prev_res0_irs20_nets20~phiI,col="blue",lwd=2)
lines(prev_res50_irs20_nets20~phiI,col="darkred",lwd=2)
lines(prev_res100_irs20_nets20~phiI,col="orange",lwd=2)

lines(prev_res0_irs50_nets50~phiI,col="blue",lwd=2)
lines(prev_res50_irs50_nets50~phiI,col="darkred",lwd=2)
lines(prev_res100_irs50_nets50~phiI,col="orange",lwd=2)

lines(prev_res0_irs80_nets80~phiI,col="blue",lwd=2)
lines(prev_res50_irs80_nets80~phiI,col="darkred",lwd=2)
lines(prev_res100_irs80_nets80~phiI,col="orange",lwd=2)

lines(prev_res0_nets20~phiI,col="blue",lwd=2,lty=2)
lines(prev_res50_nets20~phiI,col="darkred",lwd=2,lty=2)
lines(prev_res0_nets50~phiI,col="blue",lwd=2,lty=2)
lines(prev_res50_nets50~phiI,col="darkred",lwd=2,lty=2)
lines(prev_res0_nets80~phiI,col="blue",lwd=2,lty=2)
lines(prev_res50_nets80~phiI,col="darkred",lwd=2,lty=2)

################################################################
##
## Try looking at what happens when you have a shift in phi and resistance
##
################################################################
phi2_function<-function(site,
                       
                       ITN, IRS,
                       
                       phi_I,phi_B,
                       
                       irs_decay_mort1_1, irs_decay_mort2_1,irs_decay_succ1_1,
                       irs_decay_succ2_1,	irs_decay_det1_1,	irs_decay_det2_1,
                       itn_repel_fun_1,	itn_repel_gamb_ss_1,	itn_repel_arab_1,
                       itn_kill_fun_1,	itn_kill_gamb_ss_1,	itn_kill_arab_1,
                       itn_half_life,
                       
                       run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/seasonal_sites/Site_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'output_type 0 itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    
                    'fun_Q_in',phi_I,'fun_Q_bed',phi_B,
                    'arab_Q_in',phi_I,'arab_Q_bed',phi_B,
                    'gamb_ss_Q_in',phi_I,'gamb_ss_Q_bed',phi_B,
                    
                    'add change_irs 1 change_irs_time 0 change_itn 1 change_itn_time 0',  
                    'irs_decay_mort1_1', irs_decay_mort1_1,	'irs_decay_mort2_1', irs_decay_mort2_1,	'irs_decay_succ1_1', irs_decay_succ1_1,
                    'irs_decay_succ2_1', irs_decay_succ2_1,	'irs_decay_det1_1', irs_decay_det1_1,	'irs_decay_det2_1', irs_decay_det2_1,
                    'itn_repel_fun_1', itn_repel_fun_1, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_1, 'itn_repel_arab_1', itn_repel_arab_1,
                    'itn_kill_fun_1', itn_kill_fun_1, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_1, 'itn_kill_arab_1', itn_kill_arab_1,
                    'itn_half_life', itn_half_life,
                    
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_'),
                 OutputRoot = paste0("F:/Ellies_output_folder/", Run_name, '/draw_', draw),
                 Options=Options,
                 Exe = "F:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="F:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('seasonal_sites/Site_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}


## ON COMP
inp_phi = read.csv("F:/phiI_B_beh_res50_peren_seas.csv",header=TRUE)

#for(i in 1:nrow(inp_phi)){
#  phi2_function(site=inp_phi[i,1], 
#               
#               ITN=inp_phi[i,2], IRS=inp_phi[i,3],
#               
#               phi_I=inp_phi[i,4],phi_B=inp_phi[i,5],
#               irs_decay_mort1_1=inp_phi[i,6], irs_decay_mort2_1=inp_phi[i,7], 
#               irs_decay_succ1_1=inp_phi[i,8], irs_decay_succ2_1=inp_phi[i,9], 
#               irs_decay_det1_1=inp_phi[i,10], irs_decay_det2_1=inp_phi[i,11],
#               itn_repel_fun_1=inp_phi[i,12], itn_repel_gamb_ss_1=inp_phi[i,13], itn_repel_arab_1=inp_phi[i,14], 
#               itn_kill_fun_1=inp_phi[i,15], itn_kill_gamb_ss_1=inp_phi[i,16], itn_kill_arab_1=inp_phi[i,17],
#               itn_half_life = inp_phi[i,18],
#               
#               run_name="phiI_B_beh_res50_peren_seas")
#  
#}

phi_behav_res_vec_pe = 
  phi_behav_res_vec_se = 
  expand.grid(time = seq(-5,20,length=1301))
vec_intervene_pe = seq(2,46,by=4)
vec_intervene_se = vec_intervene_pe+1
for(i in 1:12){
  phi_behav_res_vec_pe[,i+1] <- read.table(paste("F:/Ellies_output_folder/phiI_B_beh_res50_peren_seas/draw_0/phiI_B_beh_res50_peren_seas",vec_intervene_pe[i],"0.txt",sep="_"),header=TRUE)[,2]
  phi_behav_res_vec_se[,i+1] <- read.table(paste("F:/Ellies_output_folder/phiI_B_beh_res50_peren_seas/draw_0/phiI_B_beh_res50_peren_seas",vec_intervene_se[i],"0.txt",sep="_"),header=TRUE)[,2]
}
par(mfrow=c(1,2))
plot(phi_behav_res_vec_pe[157:520,2]~phi_behav_res_vec_pe[157:520,1],
     ylab="Prevalence 2 - 10 year olds",yaxt = "n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time (years)",bty="n",pch="",ylim=c(0,0.4),
     main="Perennial setting")
axis(2,las=2,at=seq(0,0.4,by=0.1),labels=seq(0,0.4,by=0.1),cex.axis=1.6,cex.lab=1.6)
vec_lty=rep(c(1,2,3,4),11)

vec_col=rep(c("grey","blue","darkred"),each=2)
vec_lty=rep(c(1,2),3)
vec_lty2=rep(c(3,4),3)
for(i in 1:6){
  lines(phi_behav_res_vec_pe[157:520,i+1]~phi_behav_res_vec_pe[157:520,1],lty=vec_lty[i],col=vec_col[i])
#  lines(phi_behav_res_vec_se[157:520,i+1]~phi_behav_res_vec_pe[157:520,1],lty=vec_lty[i],col=vec_col[i])
}
for(i in 1:6){
  lines(phi_behav_res_vec_pe[157:520,i+7]~phi_behav_res_vec_pe[157:520,1],lwd=2,lty=vec_lty2[i],col=vec_col[i])
  #  lines(phi_behav_res_vec_se[157:520,i+1]~phi_behav_res_vec_pe[157:520,1],lwd=2,lty=vec_lty[i],col=vec_col[i])
}

abline(v=0,lty=2,col="grey")

legend(-1.8,0.4,legend=c("Baseline",
                "Behavioural change: (25th percentile)",
                "Physiological change: (50% resistance)",
                "Behavioural and physiological change"),
       bty="n",lty=c(1,2,3,4),lwd=2)
legend(-1.9,0.2,title = "IRS and LLIN coverage",
       legend=c("20%",
                "50%",
                "80%"),
       col=c("grey","blue","darkred"),lty=1,bty="n",lwd=2)

plot(phi_behav_res_vec_se[157:520,2]~phi_behav_res_vec_se[157:520,1],
     ylab="Prevalence 2 - 10 year olds",yaxt = "n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time (years)",bty="n",pch="",ylim=c(0,0.4),
     main="Seasonal setting")
axis(2,las=2,at=seq(0,0.4,by=0.1),labels=seq(0,0.4,by=0.1),cex.axis=1.6,cex.lab=1.6)
vec_lty=rep(c(1,2,3,4),11)

vec_col=rep(c("grey","blue","darkred"),each=2)
vec_lty=rep(c(1,2),3)
vec_lty2=rep(c(3,4),3)
for(i in 1:6){
  lines(phi_behav_res_vec_se[157:520,i+1]~phi_behav_res_vec_se[157:520,1],lty=vec_lty[i],col=vec_col[i])
  #  lines(phi_behav_res_vec_se[157:520,i+1]~phi_behav_res_vec_pe[157:520,1],lty=vec_lty[i],col=vec_col[i])
}
for(i in 1:6){
  lines(phi_behav_res_vec_se[157:520,i+7]~phi_behav_res_vec_se[157:520,1],lwd=2,lty=vec_lty2[i],col=vec_col[i])
  #  lines(phi_behav_res_vec_se[157:520,i+1]~phi_behav_res_vec_pe[157:520,1],lwd=2,lty=vec_lty[i],col=vec_col[i])
}

abline(v=0,lty=2,col="grey")

##############################################
##
## Quantifying these interactions
##
##############################################

## Percentage increase in prevalence relative to baseline (no behavioural or physiological resistance)
## Lost protection 3 years after intervention starts
##For 20% coverage
a20 = (1 - (phi_behav_res_vec_se[416,2]/
  phi_behav_res_vec_se[416,3]) )* 100 ## due to behavioural change

b20 = (1 - (phi_behav_res_vec_se[416,2]/
       phi_behav_res_vec_se[416,8]) )* 100 ## due to physiological change

c20 = (1 - (phi_behav_res_vec_se[416,2]/
       phi_behav_res_vec_se[416,9]) )* 100 ## due to behavioural and physiological change


##For 50% coverage
a50 = (1 - (phi_behav_res_vec_se[416,4]/
        phi_behav_res_vec_se[416,5]) )* 100 ## due to behavioural change

b50 = (1 - (phi_behav_res_vec_se[416,4]/
        phi_behav_res_vec_se[416,10]) )* 100 ## due to physiological change

c50 = (1 - (phi_behav_res_vec_se[416,4]/
        phi_behav_res_vec_se[416,11]) )* 100 ## due to behavioural and physiological change

##For 80% coverage
a80 = (1 - (phi_behav_res_vec_se[416,6]/
        phi_behav_res_vec_se[416,7]) )* 100 ## due to behavioural change

b80 = (1 - (phi_behav_res_vec_se[416,6]/
        phi_behav_res_vec_se[416,12]) )* 100 ## due to physiological change

c80 = (1 - (phi_behav_res_vec_se[416,6]/
        phi_behav_res_vec_se[416,13]) )* 100 ## due to behavioural and physiological change

par(mfrow= c(1,1))
plot(c(a20,a50,a80)~c(20,50,80),ylim=c(0,100))
points(c(b20,b50,b80)~c(20,50,80))
points(c(c20,c50,c80)~c(20,50,80))
##############################################
##
## Quantifying these interactions
##
##############################################

##Figure 1 (option 1)
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
#text(48,42,"A",cex=2)
legend(-22,-10,legend=c("Mosquito biting behaviour",
                       "Human indoor behaviour",
                       "Human bed behaviour"),
       col=c("purple","aquamarine3","orange"),
       pch=c(22,20,17),cex=1)

##Figure 5 (option 1)
par(mfrow=c(1,3))

vec_s = vec_pe
explore_seas_f = function(vec_s,seas1,choose_col,
                          phi_dat,phi_datb,
                          phi_dat1,phi_dat2,phi_dat3,title_2){
  
  
  plot(phi_dat[157:520,seas1]~phi_dat[157:520,1],
       ylab="Prevalence 2 - 10 year olds",yaxt = "n",cex.axis=1.6,cex.lab=1.6,
       xlab="Time (years)",bty="n",pch="",ylim=c(0,0.4))
  axis(2,las=2,at=seq(0,0.4,by=0.1),labels=seq(0,0.4,by=0.1),cex.axis=1.6,cex.lab=1.6)
  vec_lty=rep(c(1,2,3,4),11)
  
  for(i in 1:11){
    lines(phi_dat[157:520,vec_s[i]]~phi_dat[157:520,1],lty=1,col="grey")
    lines(phi_datb[157:520,vec_s[i]]~phi_datb[157:520,1],lty=2,col="grey")
  }
  abline(v=0,lty=2,col="grey")
  
  vec=c(0.1,0.3,0.5,0.7,0.9,0.9,0.7,0.5,0.3,0.1)
  for(i in 1:10){
    polygon(c(phi_dat[157:520,1],rev(phi_dat[157:520,1])),
            c(phi_dat[157:520,vec_s[i]],rev(phi_dat[157:520,vec_s[i+1]])),
            col=transp("grey",vec[i]),border = FALSE)
    
    polygon(c(phi_datb[157:520,1],rev(phi_datb[157:520,1])),
            c(phi_datb[157:520,vec_s[i]],rev(phi_datb[157:520,vec_s[i+1]])),
            col=transp("grey",vec[i]),border = FALSE)
    
  }
  
  
  lines(phi_dat[157:520,seas1]~phi_dat[157:520,1],lty=1,lwd=2,col="black")
  lines(phi_datb[157:520,seas1]~phi_datb[157:520,1],lty=2,lwd=2,col="black",0.6)
  
}
seas1=23
explore_seas_f(vec_pe,23,"blue",
               phi_res0_prev_irs50_nets50,phi_res0_prev_nets50,
               prev_res0_irs20_nets20,prev_res0_irs50_nets50,prev_res0_irs80_nets80,
               "Intervention coverage level")
legend(-1.9,0.2,cex=1.4,
       title = "IRS and LLIN coverage",
       legend=c("50% LLIN",
                "50% IRS & LLIN"),
       col="black",lty=c(1,2),bty="n",lwd=2)

text(5,0.4,"A",cex=2)
vec=c(0.1,0.3,0.5,0.7,0.9,1,0.9,0.7,0.5,0.3,0.1)
phi_b_ranges = round(as.numeric(quantile(c(phiB1ALL),c(0.025,0.05,0.1,0.25,0.35,0.5,0.65,0.75,0.9,0.95,0.975))  ),3)
phi_i_ranges = round(as.numeric(quantile(c(phiI1ALL),c(0.025,0.05,0.1,0.25,0.35,0.5,0.65,0.75,0.9,0.95,0.975))  ),3)

legend(1.5,0.4,title = expression(paste(phi[I], "   &   ", phi[B])),
       legend= c(phi_i_ranges,phi_b_ranges),ncol=2,cex=1.4,
       lty=1,col=c(transp("black",vec)))


plot(phi_behav_res_vec_pe[157:520,2]~phi_behav_res_vec_pe[157:520,1],
     ylab="Prevalence 2 - 10 year olds",yaxt = "n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time (years)",bty="n",pch="",ylim=c(0,0.4))
     #main="Perennial setting"
axis(2,las=2,at=seq(0,0.4,by=0.1),labels=seq(0,0.4,by=0.1),cex.axis=1.6,cex.lab=1.6)
vec_lty=rep(c(1,2,3,4),11)

vec_col=rep(c("grey","blue","darkred"),each=2)
vec_lty=rep(c(1,2),3)
vec_lty2=rep(c(3,4),3)
for(i in 1:6){
  lines(phi_behav_res_vec_pe[157:520,i+1]~phi_behav_res_vec_pe[157:520,1],lty=vec_lty[i],col=vec_col[i])
  #  lines(phi_behav_res_vec_se[157:520,i+1]~phi_behav_res_vec_pe[157:520,1],lty=vec_lty[i],col=vec_col[i])
}
for(i in 1:6){
  lines(phi_behav_res_vec_pe[157:520,i+7]~phi_behav_res_vec_pe[157:520,1],lwd=2,lty=vec_lty2[i],col=vec_col[i])
  #  lines(phi_behav_res_vec_se[157:520,i+1]~phi_behav_res_vec_pe[157:520,1],lwd=2,lty=vec_lty[i],col=vec_col[i])
}

abline(v=0,lty=2,col="grey")

legend(-1.8,0.4,cex=1.4,legend=c("Baseline",
                         "Behavioural change: (25th percentile)",
                         "Physiological change: (50% resistance)",
                         "Behavioural and physiological change"),
       bty="n",lty=c(1,2,3,4),lwd=2)
legend(-1.9,0.2,cex=1.4,title = "IRS and LLIN coverage",
       legend=c("20% IRS & LLIN",
                "50% IRS & LLIN",
                "80% IRS & LLIN"),
       col=c("grey","blue","darkred"),lty=1,bty="n",lwd=2)
text(5,0.4,"B",cex=2)

##(option 2)
res2 = c(0,20,40,50,60,80,100)
plot(effs_irs_nets[,1]~res2,ylim=c(0,100),xlim=c(0,100),
     ylab="Vector intervention efficacy (%)",bty="n",pch="",
     xlab="Physiological resistance",cex.lab=1.6,cex.axis=1.6,yaxt="n")
axis(2,las=2,at=seq(0,100,by=20),label=seq(0,100,by=20),cex.lab=1.6,cex.axis=1.6)

#for(i in 1:11){
#  lines(effs_irs_nets[,i]~res2,lty=1,lwd=1)
#  lines(effs_nets[,i]~res2,lty=2,lwd=1)
#}

vec_col = c(0.1,0.3,0.5,0.7,0.9,0.9,0.7,0.5,0.3,0.1)
for(i in 1:10){
  polygon(c(res2,rev(res2)),
          c(effs_irs_nets[,i],rev(effs_irs_nets[,i+1])),col=transp("grey",vec_col[i]),border=NA)
  polygon(c(res2,rev(res2)),
          c(effs_nets[,i],rev(effs_nets[,i+1])),col=transp("grey",vec_col[i]),border=NA)
}
lines(effs_irs_nets[,6]~res2,lty=2,lwd=2)
lines(effs_nets[,6]~res2,lty=1,lwd=2)

text(100,100,"C",cex=2)
#################################
##
## Impact of resistance
##
TIME = 1:365

#Lagos
seasonal_a0=	0.2859
seasonal_a1=	-0.2431
seasonal_b1=	0.0019
seasonal_a2=	-0.0279
seasonal_b2=	-0.0964
seasonal_a3=	-0.0271
seasonal_b3=	0.1008
#Doma and Nasawara
seasonal_a0=	0.2862
seasonal_a1=	-0.2656
seasonal_b1=	-0.105
seasonal_a2=	-0.0908
seasonal_b2=	0.0277
seasonal_a3=	0.0929
seasonal_b3=	0.0385
#Plateau
seasonal_a0=	0.2861
seasonal_a1=	-0.28
seasonal_b1=	-0.0985
seasonal_a2=	-0.081
seasonal_b2=	0.0294
seasonal_a3=	0.1069
seasonal_b3=	0.0376
#Rivers
seasonal_a0=	0.2856
seasonal_a1=	-0.1812
seasonal_b1=	-0.082
seasonal_a2=	-0.0893
seasonal_b2=	0.0017
seasonal_a3=	0.0335
seasonal_b3=	0.0329


##Liberia - Bong County
seasonal_a0=	0.2853
seasonal_a1=	-0.1574
seasonal_b1=	0.0083
seasonal_a2=	-0.093
seasonal_b2=	-0.0388
seasonal_a3=	0.0097
seasonal_b3=	0.0278

ssa0 = seasonal_a0
ssa1 = seasonal_a1
ssb1 =  seasonal_b1
ssa2 = seasonal_a2
ssb2 = seasonal_b2
ssa3 = seasonal_a3
ssb3 = seasonal_b3

###divide by theta_c to make mean 1

data = (ssa0+ssa1*cos(2*pi*TIME/365)+ssa2*cos(2*2*pi*TIME/365)+ssa3*cos(3*2*pi*TIME/365)+ssb1*sin(2*pi*TIME/365)+ssb2*sin(2*2*pi*TIME/365)+ ssb3*sin(3*2*pi*TIME/365) )

data[which(data < 0)] = 0.001

mean(data)
plot(TIME,data)


#seas_mosqs = expand.grid(Doma = c(data[91:365],data[1:90])) ##Doma
#seas_mosqs[,2] = c(data[91:365],data[1:90]) ##Lagos
#seas_mosqs[,3] = c(data[91:365],data[1:90]) ##Nasarawa
#seas_mosqs[,4] = c(data[91:365],data[1:90]) ##Plateau
#seas_mosqs[,5] = c(data[91:365],data[1:90]) ##Rivers

## Nigeria seasonal patterns of phiI and phiB
dat_nig = read.table("H:/Ellie/IRS and resistance/behaviour_paper/Data from Janetta/data_Nigeria.txt",header=TRUE)
head(dat_nig)

vec_lab = c("A","B","C","D","E")
par(mar=c(5,5,5,5))
par(mfrow=c(2,3))
for(i in 1:5){
  
  plot(seas_mosqs[,i]~TIME,pch="",bty="n",yaxt="n",xaxt="n",ylab="",xlab="")
  lines(seas_mosqs[,i]~TIME,lwd=3,col="grey",lty=3)
  
  par(new=T)
  plot(dat_nig$phi_i[dat_nig$place == levels(dat_nig$place)[i]] ~ 
         dat_nig$order[dat_nig$place == levels(dat_nig$place)[i]],bty="n",pch="",
       ylim=c(0,1),xlim=c(1,12),ylab="Proportion of mosquitoes biting",
       xlab = "Time (months)",xaxt="n",main=levels(dat_nig$place)[i],yaxt="n",
       cex.lab=1.6,cex.axis=1.6)
  
    lines(dat_nig$phi_i[dat_nig$place == levels(dat_nig$place)[i]] ~ 
          dat_nig$order[dat_nig$place == levels(dat_nig$place)[i]],lty=1,lwd=2)
    lines(dat_nig$phi_b[dat_nig$place == levels(dat_nig$place)[i]] ~ 
          dat_nig$order[dat_nig$place == levels(dat_nig$place)[i]],lty=1,col="blue",lwd=2)
    
    text(12,0.95,vec_lab[i],cex=2)
    
    axis(1,at=c(1,4,7,10),labels=c("March","June","Sept","Dec"),cex.lab=1.6,cex.axis=1.6)
    axis(2,las=2,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2),cex.lab=1.6,cex.axis=1.6)
    axis(4,las=2,at=seq(0,0.8,by=0.2),labels=seq(0,0.8,by=0.2),cex.lab=1.6,cex.axis=1.6)
    mtext(text="Seasonal peak in mosquitoes",side=4,line=3,cex.lab=1.6,cex.axis=1.6)
}

boxplot(dat_nig$phi_i,dat_nig$phi_b,xaxt="n",yaxt="n",ylim=c(0,1),cex.lab=1.6,
        ylab="Proportion of mosquitoes biting",col=c("white",transp("blue")))
axis(1,at=c(1,2),labels=c(expression(phi[I]),expression(phi[B])),cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2),cex.lab=1.6,cex.axis=1.6)
abline(h= median(PHII),lty=2,col="grey")
abline(h= median(PHIB,na.rm=TRUE),lty=2,col="blue")

text(2.45,0.95,"A",cex=2)




## Liberia seasonal patterns of phiI and phiB
dat_lib = read.table("H:/Ellie/IRS and resistance/PMI/Liberia2013biting_behaviour.txt",header=TRUE)
head(dat_lib)

par(mar=c(5,5,5,5))
par(mfrow=c(1,2))
i=3
  
  plot(data~TIME,pch="",bty="n",yaxt="n",xaxt="n",ylab="",xlab="")
  lines(data~TIME,lwd=3,col="grey",lty=3)
  
  par(new=T)
  plot(dat_lib$PHII[dat_lib$place == levels(dat_lib$place)[i]] ~ 
         dat_lib$order[dat_lib$place == levels(dat_lib$place)[i]],bty="n",pch="",
       ylim=c(0,1),xlim=c(1,12),ylab="Proportion of mosquitoes biting",
       xlab = "Time (months)",xaxt="n",main="Sergeant Kollie Town",yaxt="n",
       cex.lab=1.6,cex.axis=1.6)
  
  lines(dat_lib$PHII[dat_lib$place == levels(dat_lib$place)[i]] ~ 
          dat_lib$order[dat_lib$place == levels(dat_lib$place)[i]],lty=1,lwd=2)
  lines(dat_lib$PHIB[dat_lib$place == levels(dat_lib$place)[i]] ~ 
          dat_lib$order[dat_lib$place == levels(dat_lib$place)[i]],lty=1,col="blue",lwd=2)
  
  text(12,0.95,"F",cex=2)
  
  axis(1,at=c(1,4,7,10),labels=c("March","June","Sept","Dec"),cex.lab=1.6,cex.axis=1.6)
  axis(2,las=2,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2),cex.lab=1.6,cex.axis=1.6)
  axis(4,las=2,at=seq(0,0.8,by=0.2),labels=seq(0,0.8,by=0.2),cex.lab=1.6,cex.axis=1.6)
  mtext(text="Seasonal peak in mosquitoes",side=4,line=3,cex=1.2,cex.axis=1.2)


boxplot(dat_lib$PHII,dat_lib$PHIB,xaxt="n",yaxt="n",ylim=c(0,1),cex.lab=1.6,
        ylab="Proportion of mosquitoes biting",col=c("white",transp("blue")))
axis(1,at=c(1,2),labels=c(expression(phi[I]),expression(phi[B])),cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2),cex.lab=1.6,cex.axis=1.6)
abline(h= median(PHII),lty=2,col="grey")
abline(h= median(PHIB,na.rm=TRUE),lty=2,col="blue")

text(2.45,0.95,"B",cex=2)



######################
## test if correlations
Months_abundance = array(dim=c(12,5))
for(i in 1:5){
  Months_abundance[,i] = c(seas_mosqs[1,i],seas_mosqs[32,i],seas_mosqs[60,i],
                           seas_mosqs[91,i],seas_mosqs[121,i],seas_mosqs[152,i],
                           seas_mosqs[183,i],seas_mosqs[213,i],seas_mosqs[244,i],
                           seas_mosqs[274,i],seas_mosqs[304,i],seas_mosqs[334,i])
  
}
phi_i_nig = c(dat_nig$phi_i[dat_nig$place == levels(dat_nig$place)[1]],
              dat_nig$phi_i[dat_nig$place == levels(dat_nig$place)[2]],
              dat_nig$phi_i[dat_nig$place == levels(dat_nig$place)[3]],
              dat_nig$phi_i[dat_nig$place == levels(dat_nig$place)[4]],
              dat_nig$phi_i[dat_nig$place == levels(dat_nig$place)[5]])#,
              #c(dat_lib$PHII[dat_lib$place == levels(dat_lib$place)[3]],NA,NA))

phi_b_nig = c(dat_nig$phi_b[dat_nig$place == levels(dat_nig$place)[1]],
              dat_nig$phi_b[dat_nig$place == levels(dat_nig$place)[2]],
              dat_nig$phi_b[dat_nig$place == levels(dat_nig$place)[3]],
              dat_nig$phi_b[dat_nig$place == levels(dat_nig$place)[4]],
              dat_nig$phi_b[dat_nig$place == levels(dat_nig$place)[5]])#,
              #c(dat_lib$PHIB[dat_lib$place == levels(dat_lib$place)[3]],NA,NA))
ab_mosq = c(Months_abundance)#,data[1],data[32],data[60],
            #data[91],data[121],data[152],
            #data[183],data[213],data[244],
            #data[274],data[304],data[334])
country = rep(c("Doma", "Lagos", "Nasawara", "Plateau", "Rivers"),each=12)#"SKT"
n_mosq = c(dat_nig$n_MOSQ[dat_nig$place == levels(dat_nig$place)[1]],
           dat_nig$n_MOSQ[dat_nig$place == levels(dat_nig$place)[2]],
           dat_nig$n_MOSQ[dat_nig$place == levels(dat_nig$place)[3]],
           dat_nig$n_MOSQ[dat_nig$place == levels(dat_nig$place)[4]],
           dat_nig$n_MOSQ[dat_nig$place == levels(dat_nig$place)[5]])#,
           #c(dat_lib$PHII[dat_lib$place == levels(dat_lib$place)[3]],NA,NA))

plot(phi_i_nig ~ n_mosq)
abline(lm(phi_i_nig ~ n_mosq))


moda = lmer(phi_i_nig ~ ab_mosq + n_mosq + (1|country), REML = FALSE)
modb1 = lmer(phi_i_nig ~ 1 + n_mosq + (1|country), REML = FALSE)
modb2 = lmer(phi_i_nig ~ 1 + ab_mosq + (1|country), REML = FALSE)
anova(moda,modb1, test="F")
anova(moda,modb2, test="F")

moda = lmer(phi_i_nig ~ ab_mosq + (1|country), REML = FALSE)

modc = glm(phi_i_nig ~ ab_mosq + n_mosq + country)

modc_i1 = glm(phi_i_nig[1:12] ~ ab_mosq[1:12] + n_mosq[1:12])
summary.lm(modc_i1)
modc_i2 = glm(phi_i_nig[1:12] ~ ab_mosq[1:12])
summary.lm(modc_i2)
dat1 = data.frame(ab_mosq = seq(min(ab_mosq[1:12]),max(ab_mosq[1:12]),length=length(sort(predict(modc_i2)))) )
preds <- predict(modc_i2)



summary(moda)
coef(moda)

newDat <- data.frame(country = rep(c("Doma", "Lagos", "Nasawara", "Plateau", "Rivers"), 
                                   each=length(sort(predict(moda)))), 
                     #n_mosq = mean(n_mosq),
                     ab_mosq = rep(seq(min(ab_mosq),max(ab_mosq),length=length(sort(predict(moda)))),5) )
newDat$pred <- predict(moda, newDat)
#newDat

library(merTools)
preds <- predictInterval(moda, newdata = newDat, n.sims = 999)
head(preds)

preds_dat = data.frame(country = rep(c("Doma", "Lagos", "Nasawara", "Plateau", "Rivers"), 
                                     each=length(sort(predict(moda)))), 
                       ab_mosq = rep(seq(min(ab_mosq),max(ab_mosq),length=length(sort(predict(moda)))),5),
                       n_mosq = rep(mean(n_mosq),length(sort(predict(moda)))),
                       preds = preds$fit,
                       preds_upp = preds$upr,
                       preds_low = preds$lwr)

par(mfrow=c(1,2))
dtab = data.frame(phi_i_nig,phi_b_nig,ab_mosq,country)
plot(dtab$phi_i_nig[dtab$country == levels(dtab$country)[1]] ~ 
       dtab$ab_mosq[dtab$country == levels(dtab$country)[1]],
     ylab=expression(phi[I]),xlab="Mosquito relative abundance",
     ylim=c(0.2,1),xlim=c(0,1),cex.lab=1.6,cex.axis=1.6,yaxt="n",bty="n")
axis(2,las=2,seq(0.2,1,0.2),labels=seq(0.2,1,0.2),cex.lab=1.6,cex.axis=1.6)
vec_pch=1:5
vec_lty=1:5
vec_col = c("darkred","blue","darkgreen","darkorange","grey")
for(i in 5){
  
  polygon(c(preds_dat$ab_mosq[preds_dat$country == levels(preds_dat$country)[i]],rev(preds_dat$ab_mosq[preds_dat$country == levels(preds_dat$country)[i]])),
          c(preds_dat$preds_upp[preds_dat$country == levels(preds_dat$country)[i]],rev(preds_dat$preds_low[preds_dat$country == levels(preds_dat$country)[i]])),col=transp(vec_col[i],0.2),border=NA)
  points(dtab$phi_i_nig[dtab$country == levels(dtab$country)[i]] ~ 
           dtab$ab_mosq[dtab$country == levels(dtab$country)[i]],pch=vec_pch[i],col=vec_col[i])
  lines(preds_dat$preds[preds_dat$country == levels(preds_dat$country)[i]] ~ 
          preds_dat$ab_mosq[preds_dat$country == levels(preds_dat$country)[i]],lty=vec_lty[i],col=vec_col[i],lwd=2)
  
}
#lines(sort(predict(moda))~seq(min(ab_mosq),max(ab_mosq),length=length(sort(predict(moda)))))

moda = lmer(phi_b_nig ~ ab_mosq + n_mosq + (1|country), REML = FALSE)
modb1 = lmer(phi_b_nig ~ 1 + n_mosq + (1|country), REML = FALSE)
modb2 = lmer(phi_b_nig ~ 1 + ab_mosq + (1|country), REML = FALSE)
anova(moda,modb1, test="F")
moda = lmer(phi_b_nig ~ ab_mosq + (1|country), REML = FALSE)

summary(moda)
coef(moda)

modc_i = glm(phi_b_nig[61:72] ~ ab_mosq[61:72])
summary.lm(modc_i)


newDat <- data.frame(country = rep(c("Doma", "Lagos", "Nasawara", "Plateau", "Rivers"), 
                                   each=length(sort(predict(moda)))), 
                     ab_mosq = rep(seq(min(ab_mosq),max(ab_mosq),length=length(sort(predict(moda)))),5) )
newDat$pred <- predict(moda, newDat)
#newDat

#library(merTools)
preds <- predictInterval(moda, newdata = newDat, n.sims = 999)
head(preds)

preds_dat = data.frame(country = rep(c("Doma", "Lagos", "Nasawara", "Plateau", "Rivers"), 
                                     each=length(sort(predict(moda)))), 
                       ab_mosq = rep(seq(min(ab_mosq),max(ab_mosq),length=length(sort(predict(moda)))),5),
                       preds = preds$fit,
                       preds_upp = preds$upr,
                       preds_low = preds$lwr)


plot(dtab$phi_b_nig[dtab$country == levels(dtab$country)[1]] ~ 
       dtab$ab_mosq[dtab$country == levels(dtab$country)[1]],
     ylab=expression(phi[B]),xlab="Mosquito relative abundance",
     ylim=c(0.2,1),xlim=c(0,1),cex.lab=1.6,cex.axis=1.6,yaxt="n",bty="n")
axis(2,las=2,seq(0.2,1,0.2),labels=seq(0.2,1,0.2),cex.lab=1.6,cex.axis=1.6)

for(i in 1){
  
  polygon(c(preds_dat$ab_mosq[preds_dat$country == levels(preds_dat$country)[i]],rev(preds_dat$ab_mosq[preds_dat$country == levels(preds_dat$country)[i]])),
          c(preds_dat$preds_upp[preds_dat$country == levels(preds_dat$country)[i]],rev(preds_dat$preds_low[preds_dat$country == levels(preds_dat$country)[i]])),col=transp(vec_col[i],0.2),border=NA)
  points(dtab$phi_b_nig[dtab$country == levels(dtab$country)[i]] ~ 
           dtab$ab_mosq[dtab$country == levels(dtab$country)[i]],pch=vec_pch[i],col=vec_col[i])
  lines(preds_dat$preds[preds_dat$country == levels(preds_dat$country)[i]] ~ 
          preds_dat$ab_mosq[preds_dat$country == levels(preds_dat$country)[i]],lty=vec_lty[i],col=vec_col[i],lwd=2)
  
}

#######################################
##
## Country level assessment with PMI data

coun = read.table("H:/Ellie/IRS and resistance/PMI/COUNTRY_PHI.txt",header=TRUE)
head(coun)

par(mfrow=c(1,2))
PHII = c(phiI1ALL) 
PHIB = c(phiB1ALL) 

hist(PHII,breaks=50,col=transp("red",0.6),border=NA,xlim=c(0,1),ylim=c(0,100),
     ylab="Frequency", xlab="Proportion of mosquitoes biting",
     main="Meta-analysis data",cex.lab=1.4,cex.axis=1.4,yaxt="n")
axis(2,las=2,at=seq(0,100,20),labels=seq(0,100,20), cex=1.4,cex.lab=1.4,cex.axis=1.4)
hist(PHIB,add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
abline(v=mean(PHIB,na.rm=TRUE),lty=2,lwd=2,col="blue4")
abline(v=mean(PHII,na.rm=TRUE),lty=2,lwd=2,col="darkred")
#text(mean(PHIB,na.rm=TRUE)-0.15,100,expression(paste("In bed bites ", phi[B])),cex=1.5)
#text(mean(PHII,na.rm=TRUE)-0.05,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
text(0.2,100,expression(paste("In bed bites ", phi[B])),cex=1.5,col="darkblue")
text(0.3,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")

text(0.3,95,"Mean: 0.746",cex=1.4,col="blue4",font=4)
text(0.4,45,"Mean: 0.859",cex=1.4,col="darkred",font=4)

text(0.3,90,"Median: 0.785",cex=1.4,col="blue4",font=4)
text(0.4,40,"Median: 0.891",cex=1.4,col="darkred",font=4)

summary(PHII,na.rm=TRUE)
summary(PHIB,na.rm=TRUE)


hist(coun[,6],breaks=50,col=transp("red",0.6),border=NA,xlim=c(0,1),ylim=c(0,20),
     ylab="Frequency", xlab="Proportion of mosquitoes biting",
     main="PMI data",cex.lab=1.4,cex.axis=1.4,yaxt="n")
axis(2,las=2,at=seq(0,20,5),labels=seq(0,20,5), cex=1.4,cex.lab=1.4,cex.axis=1.4)
hist(coun[,7],add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
abline(v=mean(coun[,7],na.rm=TRUE),lty=2,lwd=2,col="blue4")
abline(v=mean(coun[,6],na.rm=TRUE),lty=2,lwd=2,col="darkred")
#text(mean(PHIB,na.rm=TRUE)-0.15,100,expression(paste("In bed bites ", phi[B])),cex=1.5)
#text(mean(PHII,na.rm=TRUE)-0.05,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
text(0.2,20,expression(paste("In bed bites ", phi[B])),cex=1.5,col="darkblue")
text(0.3,10,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")

text(0.3,19,"Mean: 0.686",cex=1.4,col="blue4",font=4)
text(0.4,9,"Mean: 0.840",cex=1.4,col="darkred",font=4)

text(0.3,18,"Median: 0.715",cex=1.4,col="blue4",font=4)
text(0.4,8,"Median: 0.859",cex=1.4,col="darkred",font=4)

summary(coun[,6],na.rm=TRUE)
summary(coun[,7],na.rm=TRUE)

head(coun)
par(mfrow=c(1,1))
coun$x=(1 - coun$deltamethrin)
plot(coun[,6] ~ coun$x, bty="n", cex.lab = 1.6, cex.axis = 1.6,
     ylab=expression(paste("Behavioural resistance, ", phi[I])),
     xlab="Physiological resistance, bioassay resistance test",
     ylim=c(0,1),xlim=c(0,1),yaxt="n")
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=1.6,cex.axis=1.6)
summary.lm(lm(coun[,6] ~ coun$x))

coun2 = subset(coun, coun$sprayed == "spray" | coun$sprayed == "control")
dim(coun2)
tapply(coun2$phi_B,coun2$sprayed,median)
tapply(coun2$phi_I,coun2$sprayed,mean)
  summary(coun2$species)

MOD1 <- lmer(phi_I ~ sprayed + (1|Country),data=coun2)
MOD1temp <- lmer(phi_I ~ 1 + (1|Country),data=coun2)
anova(MOD1,MOD1temp)

MOD1 <- lmer(phi_B ~ sprayed + (1|Country),data=coun2)
MOD1temp <- lmer(phi_B ~ 1 + (1|Country),data=coun2)
anova(MOD1,MOD1temp)


se2 <- sqrt(diag(vcov(MOD1)))
tab <- cbind(Est = fixef(MOD1), LL = fixef(MOD1) - 1.96 * se2, UL = fixef(MOD1) + 1.96 * se2)
print(exp(tab),digits=3) #to get quick odds ratios for glmm - these show you how important the different levels of each factor are


sjp.setTheme(theme = "forestgrey", 
             geom.label.size = 5, 
             axis.textsize = 1.6, 
             axis.title.size = 1.9,
             title.size=2)

sjp.glmer(MOD1, y.offset = .2, show.values = TRUE, type = "eff")
# sort all predictors
sjp.glmer(MOD1,
          facet.grid = FALSE,
          sort = "sort.all",
          y.offset = .2)

# plot qq-plot of random effects: dots should be along the line
sjp.glmer(MOD1, type = "re.qq")

####################################
##
## Figure 4
par(mfrow=c(1,2))
par(mar=c(10,5,2,2))
boxplot(coun$phi_I ~ coun$Country, ylim=c(0,1.1), frame=FALSE,
        cex.lab=1.6,cex.axis=1.6,yaxt="n",xaxt="n",
        col=transp("darkred",0.5),ylab="Proportion of mosquitoes biting")
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=1.6,cex.axis=1.6)
axis(1,las=2,at=1:11,labels=unique(levels(coun$Country)),cex.lab=1.6,cex.axis=1.6)
numI = c(as.numeric(c(summary(coun$Country))))
num=1:11
for(i in 1:11){
  text(num[i],0.02,numI[i])
  }
text(11,1.1,"A",cex=2)
par(new=TRUE)
boxplot(coun$phi_B ~ coun$Country, ylim=c(0,1), frame=FALSE,
        cex.lab=1.6,cex.axis=1.6,yaxt="n",xaxt="n",
        col=transp("darkblue",0.5),ylab="Proportion of mosquitoes biting")
legend(8.5,0.4,bty="n",
       legend = c(expression(phi[I]),
                        expression(phi[B])),
       col=transp(c("darkred","darkblue"),0.4),pch=15,cex=2)

par(mar=c(5,5,2,2))
plot(coun[,6] ~ coun$x, pch="",bty="n", cex.lab = 1.6, cex.axis = 1.6,
     ylab=expression(paste("Behavioural resistance, ", phi[I])),
     xlab="Physiological (deltamethrin) resistance",
     ylim=c(0,1),xlim=c(0,1),yaxt="n")
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=1.6,cex.axis=1.6)
text(1,1,"B",cex=2)
vec_pch = 1:11
for(i in 1:11){
  points(coun[,6][coun$Country == levels(coun$Country)[i]] ~ 
           coun$x[coun$Country == levels(coun$Country)[i]],pch=vec_pch[i])
}
legend(0.15,0.3,
  legend=c(names(summary(coun$Country))),ncol=3,
       pch=vec_pch)

