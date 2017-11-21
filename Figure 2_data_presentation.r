###################################################
##
##
## Figure 2 Behaviour analysis

library(adegenet)

##########################
###
###
### Figure 2: Summary metadata, mosquitoes activity times, people indoors and in bed
dat_mosq = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI_phiB_rawdata.csv",header=TRUE)
dat_indoor = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_indoor_vs_time.csv",header=TRUE)
dat_inbed = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_sleeping_vs_time(1).csv",header=TRUE)

activity_mosqfunALL = function(title){
  hours_vec = c(16:24,1:15)
  mean_in_mosq = mean_out_mosq = 
    max_in_mosq = max_out_mosq = min_in_mosq = min_out_mosq = numeric(24)
  for(j in 1:24){
    mean_in_mosq[j] = mean(dat_mosq[,2][dat_mosq[,1] == hours_vec[j]])
    mean_out_mosq[j] = mean(dat_mosq[,3][dat_mosq[,1] == hours_vec[j]])
    
    max_in_mosq[j] = max(dat_mosq[,2][dat_mosq[,1] == hours_vec[j]])
    max_out_mosq[j] = max(dat_mosq[,3][dat_mosq[,1] == hours_vec[j]])
    
    min_in_mosq[j] = min(dat_mosq[,2][dat_mosq[,1] == hours_vec[j]])
    min_out_mosq[j] = min(dat_mosq[,3][dat_mosq[,1] == hours_vec[j]])
    
  }
  
  plot(mean_in_mosq~c(1:24),ylim=c(0,0.4),main=title,
       bty="n",ylab="Proportion of active mosquitoes",yaxt="n",pch="",
       xlab="Time (hours)",cex.lab=1.5,xaxt="n",las=2,cex.axis=1.5)
  axis(2,las=2,at=c(0,0.2,0.4),labels=c(0.0,0.2,0.4),cex.axis=1.4,cex=1.4)
  axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
  polygon(c(c(1:24),rev(c(1:24))),c(max_in_mosq,rev(min_in_mosq)),col=transp("blue",0.5),border=NA)
  polygon(c(c(1:24),rev(c(1:24))),c(max_out_mosq,rev(min_out_mosq)),col=transp("grey",0.5),border=NA)
  lines(mean_in_mosq~c(1:24),lwd=2,col="blue")
  lines(mean_out_mosq~c(1:24),lwd=2,lty=2)
  
}

activity_mosqfun = function(species,title,xlabs){
  hours_vec = c(16:24,1:15)
  mean_in_mosq = mean_out_mosq = 
    max_in_mosq = max_out_mosq = min_in_mosq = min_out_mosq = numeric(24)
  for(j in 1:24){
    mean_in_mosq[j] = mean(dat_mosq[,2][dat_mosq[,1] == hours_vec[j] & dat_mosq$species_cleaned == species])
    mean_out_mosq[j] = mean(dat_mosq[,3][dat_mosq[,1] == hours_vec[j] & dat_mosq$species_cleaned == species])
    
    max_in_mosq[j] = max(dat_mosq[,2][dat_mosq[,1] == hours_vec[j] & dat_mosq$species_cleaned == species])
    max_out_mosq[j] = max(dat_mosq[,3][dat_mosq[,1] == hours_vec[j] & dat_mosq$species_cleaned == species])
    
    min_in_mosq[j] = min(dat_mosq[,2][dat_mosq[,1] == hours_vec[j] & dat_mosq$species_cleaned == species])
    min_out_mosq[j] = min(dat_mosq[,3][dat_mosq[,1] == hours_vec[j] & dat_mosq$species_cleaned == species])
    
  }
  
  plot(mean_in_mosq~c(1:24),ylim=c(0,0.4),main=title,
       bty="n",ylab="Proportion of active mosquitoes",yaxt="n",pch="",
       xlab=xlabs,cex.lab=1.5,xaxt="n",las=2,cex.axis=1.5)
  axis(2,las=2,at=c(0,0.2,0.4),labels=c(0.0,0.2,0.4),cex.axis=1.4,cex=1.4)
  axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
  abline(h=0.05,lty=3,col="grey")
  abline(h=0.1,lty=3,col="grey")
  abline(h=0.15,lty=3,col="grey")
  abline(h=0.2,lty=3,col="grey")
  polygon(c(c(1:24),rev(c(1:24))),c(max_in_mosq,rev(min_in_mosq)),col=transp("blue",0.5),border=NA)
  polygon(c(c(1:24),rev(c(1:24))),c(max_out_mosq,rev(min_out_mosq)),col=transp("grey",0.5),border=NA)
  lines(mean_in_mosq~c(1:24),lwd=2,col="blue")
  lines(mean_out_mosq~c(1:24),lwd=2,lty=2)
  
}
par(mfrow=c(1,1))
activity_mosqfunALL(title = "All mosquitoes")

par(mfrow=c(3,1))
activity_mosqfun(species =  "A_funestus",
                 title = "An funestus",xlabs = "")
activity_mosqfun(species =  "A_gambiae",
                 title = "An gambiae",xlabs = "")
activity_mosqfun(species =  "A_arabiensis",
                 title = "An arabiensis",xlabs = "Time (hours)")

legend(1,0.4,legend = c("Indoor","Outdoor"),border=NA,cex=1.6,bty="n",
       col=transp(c("blue","black"),0.5),lty=c(1,2),pch=15,lwd=2)

par(mfrow=c(3,1))
par(mar=c(5,8,2,2))

plot(mean_in_mosq~c(1:24),ylim=c(0,0.5),
     bty="n",ylab="Proportion of active mosquitoes",yaxt="n",pch="",
     xlab="Time (hours)",cex.lab=1.5,xaxt="n",las=2,cex.axis=1.5)
axis(2,las=2,at=c(0,0.2,0.4),labels=c(0.0,0.2,0.4),cex.axis=1.4,cex=1.4)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
polygon(c(c(1:24),rev(c(1:24))),c(max_in_mosq,rev(min_in_mosq)),col=transp("blue",0.5),border=NA)
polygon(c(c(1:24),rev(c(1:24))),c(max_out_mosq,rev(min_out_mosq)),col=transp("grey",0.5),border=NA)
lines(mean_in_mosq~c(1:24),lwd=2,col="blue")
lines(mean_out_mosq~c(1:24),lwd=2,lty=2)

plot(mean_in_mosqf~c(1:24),ylim=c(0,0.5),
     bty="n",ylab="Proportion of active A funestus",yaxt="n",pch="",
     xlab="Time (hours)",cex.lab=1.5,xaxt="n",las=2,cex.axis=1.5)
axis(2,las=2,at=c(0,0.2,0.4),labels=c(0.0,0.2,0.4),cex.axis=1.4,cex=1.4)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
polygon(c(c(1:24),rev(c(1:24))),c(max_in_mosqf,rev(min_in_mosqf)),col=transp("blue",0.5),border=NA)
polygon(c(c(1:24),rev(c(1:24))),c(max_out_mosqf,rev(min_out_mosqf)),col=transp("grey",0.5),border=NA)
lines(mean_in_mosqf~c(1:24),lwd=2,col="blue")
lines(mean_out_mosqf~c(1:24),lwd=2,lty=2)

plot(mean_in_mosqg~c(1:24),ylim=c(0,0.5),
     bty="n",ylab="Proportion of active A gambiae sl",yaxt="n",pch="",
     xlab="Time (hours)",cex.lab=1.5,xaxt="n",las=2,cex.axis=1.5)
axis(2,las=2,at=c(0,0.2,0.4),labels=c(0.0,0.2,0.4),cex.axis=1.4,cex=1.4)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.4,cex=1.4)
polygon(c(c(1:24),rev(c(1:24))),c(max_in_mosqg,rev(min_in_mosqg)),col=transp("blue",0.5),border=NA)
polygon(c(c(1:24),rev(c(1:24))),c(max_out_mosqg,rev(min_out_mosqg)),col=transp("grey",0.5),border=NA)
lines(mean_in_mosqg~c(1:24),lwd=2,col="blue")
lines(mean_out_mosqg~c(1:24),lwd=2,lty=2)

par(mfrow=c(3,1))

veca = seq(1,nrow(dat_mosq),24)
vecb = c(veca[2:61]-1,1464)
vecc = c("red","purple","grey","aquamarine3","coral3","yellow","blue")
plot(dat_mosq[veca[1]:vecb[1],4] ~ c(1:24),ylim=c(-0.5,0.5),pch="",
     bty="n",ylab="Proportion of active mosquitoes",yaxt="n",
     xlab="Time (hours)",cex.lab=1.6,xaxt="n",las=2,cex.axis=1.6)
axis(2,las=2,at=c(-0.4,-0.2,0,0.2,0.4),labels=c(0.4,0.2,0.0,0.2,0.4),cex.axis=1.6,cex=1.6)
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

legend(15,-0.1,legend=c("An. funestus","An. gambiae","Other"),
       col=c("cyan2","red","black"),lty=1,lwd=2,cex=1.4)

plot(dat_indoor[,2]~dat_indoor[,1],pch="",bty="n",ylab="Proportion of people indoors",
     xlab="Time (hours)",cex.lab=1.6,ylim=c(0,1),xaxt="n",las=2,cex.axis=1.6)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.6,cex=1.6)
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


dat_inbed = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Andrew Beale\\Human_sleeping_vs_time_Beale_data_added.csv",header=TRUE)

plot(dat_inbed[,3]~dat_inbed[1:24,1],pch="",bty="n",ylab="Proportion of people in bed",
     xlab="Time (hours)",cex.lab=1.6,ylim=c(0,1),xaxt="n",las=2,cex.axis=1.6)
axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.6,cex=1.6)

#for(i in 3:736){
#  lines(dat_inbed[1:24,i]~dat_inbed[1:24,1],col=transp("blue",0.3),lwd=2)##Tanzania
#}
library(adegenet)
lines(dat_inbed[1:24,735]~dat_inbed[1:24,1],col="orange",lwd=2)##Tanzania
lines(dat_inbed[1:24,736]~dat_inbed[1:24,1],col="orange",lwd=2)##Tanzania
lines(dat_inbed[1:24,739]~dat_inbed[1:24,1],col=transp("blue",0.5),lwd=2)##Mozambique_Beale average data

lines(dat_inbed[,737]~dat_inbed[,1],lty=2,lwd=3,col="darkorange")##Average Tanzania
#lines(dat_inbed[,739]~dat_inbed[,1],lty=2,lwd=3,col="blue")##Average Mozambique
#polygon(c(dat_inbed[,1],rev(dat_inbed[,1])),c(dat_inbed[,740],rev(dat_inbed[,741])),border=NA,col=transp("blue",0.3))
text(24,1,"C",cex=2)
legend(16.2,0.9,legend=c("Tanzania",
                         "Burina Faso",
                         "Zambia",
                         "Kenya",
                         "Benin",
                         "Mozambique"),
       lty=1,lwd=2,cex=1.4,col=c("orange","purple","blue","aquamarine3","darkred",transp("blue",0.5)))

