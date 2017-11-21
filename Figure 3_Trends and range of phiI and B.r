###############################
##
##
## Figure 3 Histogram of data and trends

##########################
###
###
### Figure 3: Temporal trends and phi I and phi B


################################################
##
## Calculate:
## All possible combinations of phiI AND phiB

head(dat_mosq)
dat_inbed = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_sleeping_vs_time(1).csv",header=TRUE)

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
##See Beale bed data
dat_mosq = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI_phiB_rawdata.csv",header=TRUE)
dat_inbed = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Andrew Beale\\Human_sleeping_vs_time_Beale_data_added.csv",header=TRUE)
dat_indoor = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_indoor_vs_time.csv",header=TRUE)

vec_sets = seq(0,1464,24) ##This is the start of each set of data for 24 hours
vec2 = c(2:61,1) ##This is the start of each set of data for 24 hours

##Now simulate for all of the data combinations i.e. of all the mosquito populations crossed with each piece of human data
##dat_indoor[,3:11]  ## individual studies on humans indoors
##dat_inbed[,3:4]     ## individual studies on humans in bed

phiB_TESTnumALL <- array(dim=c(24,61,3),data=NA)
phiI_TESTnumALL <- phiI_TESTdenomALL <-array(dim=c(24,61,11),data=NA) 

##Calculate numerator for all of the phiB options
for(i in 1:24) {
  for(j in 1:61){
    phiB_TESTnumALL[i,j,1]   <- dat_inbed[i,733] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_TESTnumALL[i,j,2]   <- dat_inbed[i,734] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_TESTnumALL[i,j,3]   <- dat_inbed[i,739] * dat_mosq$Inside_mosq[vec_sets[j]+i]
  }
}

##Calculate numerator and denominator for all of the phiI options
for(i in 1:24) {
  for(j in 1:61){
    for(k in 1:11){
      phiI_TESTnumALL[i,j,k]   <- dat_indoor[i,k+2] * dat_mosq$Inside_mosq[vec_sets[j]+i]
      phiI_TESTdenomALL[i,j,k] <- dat_indoor[i,k+2] * dat_mosq$Inside_mosq[vec_sets[j]+i] + (1-dat_indoor[i,k+2]) * dat_mosq$Outside_mosq[vec_sets[j]+i]
      
    }
  }
}

##Calculate PHI_B for all options making an assumption that people go indoors when they go to bed
veccount = seq(0,8063,by=11)
phiB1ALL <- array(dim=c(61,8074),data=NA)
for(j in 1:61){
  for(k in 1:11){
    for(b in 1:3){
      
      phiB1ALL[j,veccount[b]+k] <- ifelse(sum(phiB_TESTnumALL[1:24,j,b]) / sum(phiI_TESTdenomALL[1:24,j,k]) < 1,
                                          sum(phiB_TESTnumALL[1:24,j,b]) / sum(phiI_TESTdenomALL[1:24,j,k]), 1)
    }
  }
}

##Calculate PHI_I for all options
phiI1ALL <- array(dim=c(61,11),data=NA)
for(k in 1:11){
  for(j in 1:61){
    phiI1ALL[j,k]     <- sum(phiI_TESTnumALL[1:24,j,k]) / sum(phiI_TESTdenomALL[1:24,j,k])
  }
}    
as.numeric(quantile(c(phiB1ALL),c(0.025,0.05,0.1,0.25,0.35,0.5,0.65,0.75,0.9,0.95,0.975),na.rm=TRUE)  )
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

par(mfrow=c(3,1))
par(mar=c(5,6,5,5))
plot(dat_mosq2[,14] ~ dat_mosq2[,4],xlab="Year",xlim=c(1998,2015),ylim=c(0,1),
     yaxt="n",bty="n",cex.lab=1.6,cex.axis=1.5,pch="",
     ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])))##phiB

quantile(c(phiI1ALL),c(0.025,0.25,0.5,0.75,0.975))  

LOW1 = c(0,0.5045155,0.8140084,0.8908540,0.9400264,0.9890247,1)

val = c(0.2,0.4,0.6,0.6,0.4,0.2)
for(i in 1:6){
  polygon(c(1998,2001,
            2001,1998),
          c(LOW1[i],LOW1[i],
            LOW1[i+1],LOW1[i+1]),
          col=transp("grey",val[i]),border = FALSE)
}
segments(x0=1998,x1=2001,y0=median(c(phiI1ALL)),y1=median(c(phiI1ALL)),lwd=2,col="grey")

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
text(2015.5,1,"A",cex=2)

legend(2002,0.38,legend=c("Kolombero Valley, Tanzania",
                          "Tokoli-Vidjinnagnimon, Benin",
                          "Lokohouè, Benin",
                          "Matched data: Geissbuhler et al. 2007",
                          "An. funestus",
                          "An. gambiae s.l.",
                          "Other Anopheles"),
       col=c("orange","darkred","red","blue","grey","grey","black"),
       ncol=2,pch=c(19,19,19,8,17,15,1),cex=1.2,bty="n")

plot(dat_mosq2[,15] ~ dat_mosq2[,4],xlab="Year",xlim=c(1998,2015),ylim=c(0,1),
     yaxt="n",bty="n",cex.lab=1.6,cex.axis=1.5,pch="",
     ylab=expression(paste("Proportion of mosquitoes biting in bed  ", phi[B])))##phiB

quantile(c(phiB1ALL),c(0.025,0.25,0.5,0.75,0.975),na.rm=TRUE)  

LOW1 = c(0,0.4315768,0.7274631,0.8482166,0.9262436,0.9913500,1)
val = c(0.2,0.4,0.6,0.6,0.4,0.2)
for(i in 1:6){
  polygon(c(1998,2001,
            2001,1998),
          c(LOW1[i],LOW1[i],
            LOW1[i+1],LOW1[i+1]),
          col=transp("grey",val[i]),border = FALSE)
}
segments(x0=1998,x1=2001,y0=median(c(phiB1ALL),na.rm=TRUE),y1=median(c(phiB1ALL),na.rm=TRUE),lwd=2,col="grey")


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
text(2015.5,1,"B",cex=2)


PHII = c(phiI1ALL) 
PHIB = c(phiB1ALL) 

hist(PHII,breaks=50,col=transp("red",0.6),border=NA,main="",xlim=c(0,1),ylim=c(0,200),
     ylab="", xlab="Proportion of mosquitoes biting",cex.lab=1.6,cex.axis=1.5,yaxt="n")
axis(2,las=2,at=seq(0,200,50),labels=seq(0,200,50), cex=1.4,cex.lab=1.6,cex.axis=1.5)
mtext(side = 2,line=3.5,"Frequency")
hist(PHIB,add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
abline(v=mean(PHIB,na.rm=TRUE),lty=2,lwd=2,col="blue4")
abline(v=mean(PHII,na.rm=TRUE),lty=2,lwd=2,col="darkred")
#text(mean(PHIB,na.rm=TRUE)-0.15,100,expression(paste("In bed bites ", phi[B])),cex=1.5)
#text(mean(PHII,na.rm=TRUE)-0.05,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
text(0.2,200,expression(paste("In bed bites ", phi[B])),cex=1.5,col="darkblue")
text(0.3,100,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")

text(0.3,185,"Mean: 0.817",cex=1.5,col="blue4",font=4)
text(0.4,85,"Mean: 0.859",cex=1.5,col="darkred",font=4)

text(0.3,170,"Median: 0.846",cex=1.5,col="blue4",font=4)
text(0.4,70,"Median: 0.891",cex=1.5,col="darkred",font=4)

text(1,200,"C",cex=2)


summary(PHII,na.rm=TRUE)
summary(PHIB,na.rm=TRUE)
