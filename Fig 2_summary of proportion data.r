###############################
##
## Figure 2 Summary of proportion biting
##
####################################

##############
##
##DATA AND PROCESSING
coun = read.table("H:/Ellie/IRS and resistance/behaviour_paper/PMI/COUNTRY_PHI2.txt",header=TRUE)
names(coun)

dat_mosq1 = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiB+phiI_Beale added.csv",header=TRUE)
dat_mosq2 = subset(dat_mosq1,dat_mosq1$Study != "PMI")
dat_mosq1$source = ifelse(dat_mosq1$Study == "PMI","PMI", "published")

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

options("devEval/args/path"=file.path("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Evolution_version 1"))
devEval("tiff", name="test1", width=1200, height=800, {
  
  plot.new()
  
  
  ##Top left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
  par(new = "TRUE",  
      mar = c(8,8,3,5),
      plt = c(0.1,0.45,0.6,0.95))#,
  # major tick size and direction, < 0 means outside
  
  hist(PHII,breaks=50,col=transp("red",0.6),border=NA,xlim=c(0,1),ylim=c(0,200),
       ylab="Frequency", xlab="Proportion of mosquitoes biting",
       main="",cex.lab=1.4,cex.axis=1.4,yaxt="n")
  axis(2,las=2,at=seq(0,200,50),labels=seq(0,200,50), cex=1.4,cex.lab=1.4,cex.axis=1.4)
  hist(PHIB,add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
  abline(v=mean(PHIB,na.rm=TRUE),lty=2,lwd=2,col="blue4")
  abline(v=mean(PHII,na.rm=TRUE),lty=2,lwd=2,col="darkred")
  ###text(mean(PHIB,na.rm=TRUE)-0.15,100,expression(paste("In bed bites ", phi[B])),cex=1.5)
  ###text(mean(PHII,na.rm=TRUE)-0.05,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
  #text(0.2,180,expression(paste("In bed bites ", phi[B])),cex=1.5,col="darkblue")
  #text(0.3,100,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
  
  #text(0.3,170,"Mean: 0.817",cex=1.4,col="blue4",font=4)
  #text(0.4,90,"Mean: 0.859",cex=1.4,col="darkred",font=4)
  
  #text(0.3,160,"Median: 0.846",cex=1.4,col="blue4",font=4)
  #text(0.4,80,"Median: 0.891",cex=1.4,col="darkred",font=4)
  
  summary(PHII,na.rm=TRUE)
  summary(PHIB,na.rm=TRUE)
  text(1,200,"A",cex=1.4)
  
  ##Bottom left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
  par(new = "TRUE",  
      mar = c(8,8,6,5),
      plt = c(0.1,0.45,0.1,0.45))#,
  # major tick size and direction, < 0 means outside
  
  hist(coun[,6],breaks=50,col=transp("red",0.6),border=NA,xlim=c(0,1),ylim=c(0,20),
       ylab="Frequency", xlab="Proportion of mosquitoes biting",
       main="",cex.lab=1.4,cex.axis=1.4,yaxt="n")
  axis(2,las=2,at=seq(0,20,5),labels=seq(0,20,5), cex=1.4,cex.lab=1.4,cex.axis=1.4)
  hist(coun[,7],add=TRUE,breaks=50,col=transp("blue",0.6),border=NA)
  abline(v=mean(coun[,7],na.rm=TRUE),lty=2,lwd=2,col="blue4")
  abline(v=mean(coun[,6],na.rm=TRUE),lty=2,lwd=2,col="darkred")
  ###text(mean(PHIB,na.rm=TRUE)-0.15,100,expression(paste("In bed bites ", phi[B])),cex=1.5)
  ###text(mean(PHII,na.rm=TRUE)-0.05,50,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
  #text(0.2,18,expression(paste("In bed bites ", phi[B])),cex=1.5,col="darkblue")
  #text(0.3,10,expression(paste("Indoor bites ", phi[I])),cex=1.5,col="darkred")
  
  #text(0.3,17,"Mean: 0.750",cex=1.4,col="blue4",font=4)
  #text(0.4,9,"Mean: 0.840",cex=1.4,col="darkred",font=4)
  
  #text(0.3,16,"Median: 0.771",cex=1.4,col="blue4",font=4)
  #text(0.4,8,"Median: 0.859",cex=1.4,col="darkred",font=4)
  
  summary(coun[,6],na.rm=TRUE)
  summary(coun[,7],na.rm=TRUE)
  
  text(1,20,"B",cex=1.4)
  
  ##INSERTED plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
  par(new = "TRUE",  
      mar = c(8,8,3,5),
      plt = c(0.14,0.32,0.25,0.42))#,
  # major tick size and direction, < 0 means outside
  
  boxplot(coun$phi_I ~ coun$Country, ylim=c(0,1.1), frame=FALSE,
          cex.lab=0.8,cex.axis=1.4,yaxt="n",xaxt="n",
          col=transp("darkred",0.3),ylab="")
  axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=0.8,cex.axis=0.8)
  axis(1,las=2,at=1:11,labels=unique(levels(coun$Country)),cex.lab=0.8,cex.axis=0.8)
  numI = c(as.numeric(c(summary(coun$Country))))
  num=1:11
  for(i in 1:11){
    text(num[i],0.02,numI[i],cex=0.8)
  }
  #text(11.5,1.1,"C",cex=0.8)
  par(new=TRUE)
  boxplot(coun$phi_B ~ coun$Country, ylim=c(0,1), frame=FALSE,
          cex.lab=0.8,cex.axis=0.8,yaxt="n",xaxt="n",
          col=transp("darkblue",0.3),ylab="")
  legend(7,0.45,bty="n",
         legend = c("Indoor biting",
                    "In bed biting"),
         col=transp(c("darkred","darkblue"),0.4),pch=15,cex=0.8)
  
  ##Top right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
  par(new = "TRUE",  
      mar = c(8,8,3,5),
      plt = c(0.55,0.9,0.6,0.95))#,
  # major tick size and direction, < 0 means outside
  
  ##**RUN THROUGH MODELS IN figure_map.r first
  plot(phiI~Year,data=dat_mosq1,pch="",bty="n",ylim=c(0,1),yaxt="n",cex.main=1.8,xlim=c(2000,2015),
       cex.lab=1.4, ylab=expression(paste("Proportion of mosquitoes biting indoors  ", phi[I])))
  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.4)  
  
  ##PREDICTIONS FROM THE GLMM WITH COUNTRY AS A RANDOM EFFECT
  lines(rev(sort(datAnArab$preds)) ~ sort(datAnArab$exp1),lty=1,lwd=1)
  polygon(c(sort(datAnArab$exp1),rev(sort(datAnArab$exp1))),
          c(rev(sort(datAnArab$preds-6*se(datAnArab$preds))),
            sort(datAnArab$preds+6*se(datAnArab$preds))),
          col=transp("grey",0.2),border=NA)             
  
  ##PREDICTIONS WITH COUNTRY AS A FIXED EFFECT
  cols=transp(c("red","red","aquamarine3","aquamarine3","blue",
                "red","red","red","aquamarine3","blue",
                "red","blue","red","blue","aquamarine3",
                "aquamarine3","red","blue","aquamarine3","aquamarine3","aquamarine3"),0.5)
  pchs=rep(15,21)
  ltys=rep(1,21)
  for(i in 1:length(unique(dat_mosq1$Country))){
    #  lines(min(dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i]]):
    #          max(dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i]]), 
    #        predict(i.glm, 
    #                newdata=
    #                  data.frame(Year = min(dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i]]):max(dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i]]),
    #                             Country = rep(levels(unique(dat_mosq1$Country))[i],
    #                                           length(min(dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i]]):max(dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i]])))),
    #                type = "response"),lty=ltys[i],lwd=2,col=cols[i])
    points(dat_mosq1$phiI[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i] & dat_mosq1$source != "PMI"]~
             dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i] & dat_mosq1$source != "PMI"],col=cols[i],pch=15)
    
    points(dat_mosq1$phiI[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i] & dat_mosq1$source == "PMI"]~
             dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i] & dat_mosq1$source == "PMI"],col=cols[i],pch=17)
    
  }
  
  legend(2000,0.5,legend=c("West Africa","East Africa","Southern Africa","Central Africa"),
         col=c("red","blue","purple","aquamarine3"),pch=20,cex=1.2,bty="n")
  
  legend(2000,0.15,legend=c("Review data","PMI data"),
         col=c("grey"),pch=c(15,17),cex=1.2,bty="n")
  
  #legend(1992,0.2,legend=c(levels(unique(dat_mosq1$Country))),
  #       col=c("blue","blue","red","red","red","aquamarine3"),
  #       pch=c(13,16,12,6,11,17),ncol=2,
  #       lty=c(1,2,1,1,6,1),cex=1.4,bty="n")
  text(2015,1,"C",cex=1.4)
  
  
  ##LOWER right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
  par(new = "TRUE",  
      mar = c(8,8,3,5),
      plt = c(0.55,0.9,0.1,0.45))#,
  
  plot(phiB~Year,data=dat_mosq1,pch="",bty="n",ylim=c(0,1),yaxt="n",cex.main=1.4,xlim=c(2000,2015),
       cex.lab=1.4, ylab=expression(paste("Proportion of mosquitoes biting in bed  ", phi[B])))
  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.4)  
  
  ##PREDICTIONS FROM THE GLMM WITH COUNTRY AS A RANDOM EFFECT
  lines(rev(sort(datAnArab$preds)) ~ sort(datAnArab$exp1),lty=1,lwd=1)
  polygon(c(sort(datAnArab$exp1),rev(sort(datAnArab$exp1))),
          c(rev(sort(datAnArab$preds-6*se(datAnArab$preds))),
            sort(datAnArab$preds+6*se(datAnArab$preds))),
          col=transp("grey",0.2),border=NA)             
  
  ##PREDICTIONS WITH COUNTRY AS A FIXED EFFECT
  for(i in 1:length(unique(dat_mosq1$Country))){
    points(dat_mosq1$phiB[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i] & dat_mosq1$source != "PMI"]~
             dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i] & dat_mosq1$source != "PMI"],col=cols[i],pch=15)
    
    points(dat_mosq1$phiB[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i] & dat_mosq1$source == "PMI"]~
             dat_mosq1$Year[dat_mosq1$Country==levels(unique(dat_mosq1$Country))[i] & dat_mosq1$source == "PMI"],col=cols[i],pch=17)
    
  }
  #legend(1992,0.4,legend=c("Kenya","Tanzania","Guinea","Benin","Ghana","Zambia"),
  #       col=c("blue","blue","red","red","red","aquamarine3"),
  #       pch=c(13,16,12,6,11,17),ncol=2,
  #       lty=c(1,2,1,1,6,1),cex=1.4)
  text(2015,1,"D",cex=1.4)
})
