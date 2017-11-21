########################################
##
## Supplementary figures

##S1 

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

##############################
##                          ##
## Supplementary figure     ##
##                          ##
##############################

##s2
## Add in the phiI + phiB.CSV data so that the 9th column is the first distribution of phi values

DAT_Box = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI1ALL.csv")
DAT_Box = DAT_Box[with(DAT_Box, order(DAT_Box$Year)), ]
Tanzania = subset(DAT_Box,DAT_Box$Country=="Tanzania")
phiI_tan = t(Tanzania[,9:1339])
par(mfrow=c(1,2))
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
text(21,1,"A",cex=2)

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

legend(1,0.3,legend=c("Benin","Ghana","Kenya","Tanzania","Uganda"),
       col=c("aquamarine3","gold",transp(c("firebrick"),0.2),"dodgerblue","red"),
       pch=15,cex=1.4,bty="n")


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
text(21,1,"B",cex=2)
#legend(1,0.3,legend=c("Benin","Ghana","Kenya","Tanzania","Uganda"),
#       col=c("aquamarine3","gold",transp(c("firebrick"),0.2),"dodgerblue","red"),
#       pch=15,cex=1.4,bty="n")



##############################
##                          ##
## Supplementary figure     ##
##                          ##
##############################

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
phiB1ALL <- array(dim=c(61,33),data=NA)
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
PHII = c(phiI1ALL) 
PHIB = c(phiB1ALL) 


## S3
coun = read.table("H:/Ellie/IRS and resistance/PMI/COUNTRY_PHI.txt",header=TRUE)
head(coun)

par(mfrow=c(1,2))

hist(PHII,breaks=50,col=transp("red",0.6),border=NA,xlim=c(0,1),ylim=c(0,200),
     ylab="Frequency", xlab="Proportion of mosquitoes biting",
     main="Meta-analysis data",cex.lab=1.4,cex.axis=1.4,yaxt="n")
axis(2,las=2,at=seq(0,200,50),labels=seq(0,200,50), cex=1.4,cex.lab=1.4,cex.axis=1.4)
hist(PHIB,add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
abline(v=mean(PHIB,na.rm=TRUE),lty=2,lwd=2,col="blue4")
abline(v=mean(PHII,na.rm=TRUE),lty=2,lwd=2,col="darkred")
#text(mean(PHIB,na.rm=TRUE)-0.15,100,expression(paste("In bed bites ", phi[B])),cex=1.5)
#text(mean(PHII,na.rm=TRUE)-0.05,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
text(0.2,180,expression(paste("In bed bites ", phi[B])),cex=1.5,col="darkblue")
text(0.3,100,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")

text(0.3,170,"Mean: 0.817",cex=1.4,col="blue4",font=4)
text(0.4,90,"Mean: 0.859",cex=1.4,col="darkred",font=4)

text(0.3,160,"Median: 0.846",cex=1.4,col="blue4",font=4)
text(0.4,80,"Median: 0.891",cex=1.4,col="darkred",font=4)

summary(PHII,na.rm=TRUE)
summary(PHIB,na.rm=TRUE)
text(1,200,"A",cex=2)

hist(coun[,6],breaks=50,col=transp("red",0.6),border=NA,xlim=c(0,1),ylim=c(0,20),
     ylab="Frequency", xlab="Proportion of mosquitoes biting",
     main="PMI data",cex.lab=1.4,cex.axis=1.4,yaxt="n")
axis(2,las=2,at=seq(0,20,5),labels=seq(0,20,5), cex=1.4,cex.lab=1.4,cex.axis=1.4)
hist(coun[,7],add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
abline(v=mean(coun[,7],na.rm=TRUE),lty=2,lwd=2,col="blue4")
abline(v=mean(coun[,6],na.rm=TRUE),lty=2,lwd=2,col="darkred")
#text(mean(PHIB,na.rm=TRUE)-0.15,100,expression(paste("In bed bites ", phi[B])),cex=1.5)
#text(mean(PHII,na.rm=TRUE)-0.05,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
text(0.2,18,expression(paste("In bed bites ", phi[B])),cex=1.5,col="darkblue")
text(0.3,10,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")

text(0.3,17,"Mean: 0.686",cex=1.4,col="blue4",font=4)
text(0.4,9,"Mean: 0.840",cex=1.4,col="darkred",font=4)

text(0.3,16,"Median: 0.715",cex=1.4,col="blue4",font=4)
text(0.4,8,"Median: 0.859",cex=1.4,col="darkred",font=4)

summary(coun[,6],na.rm=TRUE)
summary(coun[,7],na.rm=TRUE)

text(1,20,"B",cex=2)

##############################
##                          ##
## Supplementary figure     ##
##                          ##
##############################

##s5

#################################
##
## Impact of resistance
##
TIME = 1:365
#Doma and Nasawara
seasonal_a0=	0.2862
seasonal_a1=	-0.2656
seasonal_b1=	-0.105
seasonal_a2=	-0.0908
seasonal_b2=	0.0277
seasonal_a3=	0.0929
seasonal_b3=	0.0385

ssa0 = seasonal_a0
ssa1 = seasonal_a1
ssb1 =  seasonal_b1
ssa2 = seasonal_a2
ssb2 = seasonal_b2
ssa3 = seasonal_a3
ssb3 = seasonal_b3
data = (ssa0+ssa1*cos(2*pi*TIME/365)+ssa2*cos(2*2*pi*TIME/365)+ssa3*cos(3*2*pi*TIME/365)+ssb1*sin(2*pi*TIME/365)+ssb2*sin(2*2*pi*TIME/365)+ ssb3*sin(3*2*pi*TIME/365) )
data[which(data < 0)] = 0.001
seas_mosqs = expand.grid(Doma = c(data[91:365],data[1:90])) ##Doma

#Lagos
seasonal_a0=	0.2859
seasonal_a1=	-0.2431
seasonal_b1=	0.0019
seasonal_a2=	-0.0279
seasonal_b2=	-0.0964
seasonal_a3=	-0.0271
seasonal_b3=	0.1008

ssa0 = seasonal_a0
ssa1 = seasonal_a1
ssb1 =  seasonal_b1
ssa2 = seasonal_a2
ssb2 = seasonal_b2
ssa3 = seasonal_a3
ssb3 = seasonal_b3
data = (ssa0+ssa1*cos(2*pi*TIME/365)+ssa2*cos(2*2*pi*TIME/365)+ssa3*cos(3*2*pi*TIME/365)+ssb1*sin(2*pi*TIME/365)+ssb2*sin(2*2*pi*TIME/365)+ ssb3*sin(3*2*pi*TIME/365) )
data[which(data < 0)] = 0.001
seas_mosqs[,2] = c(data[91:365],data[1:90]) ##Lagos

#Doma and Nasawara
seasonal_a0=	0.2862
seasonal_a1=	-0.2656
seasonal_b1=	-0.105
seasonal_a2=	-0.0908
seasonal_b2=	0.0277
seasonal_a3=	0.0929
seasonal_b3=	0.0385

ssa0 = seasonal_a0
ssa1 = seasonal_a1
ssb1 =  seasonal_b1
ssa2 = seasonal_a2
ssb2 = seasonal_b2
ssa3 = seasonal_a3
ssb3 = seasonal_b3
data = (ssa0+ssa1*cos(2*pi*TIME/365)+ssa2*cos(2*2*pi*TIME/365)+ssa3*cos(3*2*pi*TIME/365)+ssb1*sin(2*pi*TIME/365)+ssb2*sin(2*2*pi*TIME/365)+ ssb3*sin(3*2*pi*TIME/365) )
data[which(data < 0)] = 0.001
seas_mosqs[,3] = c(data[91:365],data[1:90]) ##Nasarawa

#Plateau
seasonal_a0=	0.2861
seasonal_a1=	-0.28
seasonal_b1=	-0.0985
seasonal_a2=	-0.081
seasonal_b2=	0.0294
seasonal_a3=	0.1069
seasonal_b3=	0.0376
ssa0 = seasonal_a0
ssa1 = seasonal_a1
ssb1 =  seasonal_b1
ssa2 = seasonal_a2
ssb2 = seasonal_b2
ssa3 = seasonal_a3
ssb3 = seasonal_b3
data = (ssa0+ssa1*cos(2*pi*TIME/365)+ssa2*cos(2*2*pi*TIME/365)+ssa3*cos(3*2*pi*TIME/365)+ssb1*sin(2*pi*TIME/365)+ssb2*sin(2*2*pi*TIME/365)+ ssb3*sin(3*2*pi*TIME/365) )
data[which(data < 0)] = 0.001
seas_mosqs[,4] = c(data[91:365],data[1:90]) ##Plateau

#Rivers
seasonal_a0=	0.2856
seasonal_a1=	-0.1812
seasonal_b1=	-0.082
seasonal_a2=	-0.0893
seasonal_b2=	0.0017
seasonal_a3=	0.0335
seasonal_b3=	0.0329

ssa0 = seasonal_a0
ssa1 = seasonal_a1
ssb1 =  seasonal_b1
ssa2 = seasonal_a2
ssb2 = seasonal_b2
ssa3 = seasonal_a3
ssb3 = seasonal_b3
data = (ssa0+ssa1*cos(2*pi*TIME/365)+ssa2*cos(2*2*pi*TIME/365)+ssa3*cos(3*2*pi*TIME/365)+ssb1*sin(2*pi*TIME/365)+ssb2*sin(2*2*pi*TIME/365)+ ssb3*sin(3*2*pi*TIME/365) )
data[which(data < 0)] = 0.001
seas_mosqs[,5] = c(data[91:365],data[1:90]) ##Rivers

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
data = (ssa0+ssa1*cos(2*pi*TIME/365)+ssa2*cos(2*2*pi*TIME/365)+ssa3*cos(3*2*pi*TIME/365)+ssb1*sin(2*pi*TIME/365)+ssb2*sin(2*2*pi*TIME/365)+ ssb3*sin(3*2*pi*TIME/365) )
data[which(data < 0)] = 0.001
seas_mosqs[,6] = c(data[91:365],data[1:90]) ##Rivers



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





## Liberia seasonal patterns of phiI and phiB
dat_lib = read.table("H:/Ellie/IRS and resistance/PMI/Liberia2013biting_behaviour.txt",header=TRUE)
head(dat_lib)


plot(data~TIME,pch="",bty="n",yaxt="n",xaxt="n",ylab="",xlab="")
lines(data~TIME,lwd=3,col="grey",lty=3)
i=3
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

##############################
##                          ##
## Supplementary figure     ##
##                          ##
##############################
#s6
par(mar=c(5,5,5,5))
par(mfrow=c(1,2))


boxplot(dat_nig$phi_i,dat_nig$phi_b,xaxt="n",yaxt="n",ylim=c(0,1),cex.lab=1.6,
        ylab="Proportion of mosquitoes biting",col=c("white",transp("blue")))
axis(1,at=c(1,2),labels=c(expression(phi[I]),expression(phi[B])),cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2),cex.lab=1.6,cex.axis=1.6)
abline(h= median(PHII),lty=2,col="grey")
abline(h= median(PHIB,na.rm=TRUE),lty=2,col="blue")

text(2.45,0.95,"A",cex=2)

boxplot(dat_lib$PHII,dat_lib$PHIB,xaxt="n",yaxt="n",ylim=c(0,1),cex.lab=1.6,
        ylab="Proportion of mosquitoes biting",col=c("white",transp("blue")))
axis(1,at=c(1,2),labels=c(expression(phi[I]),expression(phi[B])),cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2),cex.lab=1.6,cex.axis=1.6)
abline(h= median(PHII),lty=2,col="grey")
abline(h= median(PHIB,na.rm=TRUE),lty=2,col="blue")

text(2.45,0.95,"B",cex=2)




