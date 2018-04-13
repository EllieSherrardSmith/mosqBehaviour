##Differences in cohorts
library(adegenet)
dat = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Andrew Beale\\mergedata.csv",header=TRUE)
summary(dat)

time =1:24
distribs = expand.grid(distrib1 = t(dat[1,12:35]))
for(i in 1:ncol(dat)){
  distribs[,i] =   t(dat[i,12:35])
}

plot(distribs[,1] ~time)
for(i in 1:ncol(distribs)){
  lines(distribs[,i] ~ time)
}

visual_fun = function(dat, factors1,val1,val2,time,name_of_factor){
  
  dist_1 = expand.grid(dist_1 = t(dat[1,12:35]))
  dist_2 = expand.grid(dist_2 = t(dat[1,12:35]))
  
  for(i in 1:ncol(subset(dat,factors1 == val1))){
    dist_1[,i] =   t(subset(dat,factors1 == val1)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val2))){
    dist_2[,i] =   t(subset(dat,factors1 == val2)[i,12:35])
  }
  
  plot(dist_1[,1] ~ time,xaxt="n",xlab = "Time in hours",
       yaxt="n",ylab="Proportion of people in bed",main=name_of_factor)
  axis(1,at=c(1:24),labels=c(16:24,1:15))
  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2))
  for(i in 1:ncol(dist_1)){
    lines(dist_1[,i] ~ time,col=transp("aquamarine3",0.6))
  }
  for(i in 1:ncol(dist_2)){
    lines(dist_2[,i] ~ time,col=transp("purple",0.6))
  }
}

visual_fun(dat = dat,factors1 = dat$sex,        val1 = 1, val2 =2,time = 1:24,name_of_factor="Sex")
visual_fun(dat = dat,factors1 = dat$location,   val1 = 1, val2 =2,time = 1:24,name_of_factor="Location")
visual_fun(dat = dat,factors1 = dat$mosquitonet,val1 = 0, val2 =1,time = 1:24,name_of_factor="Net use")


visual_fun2 = function(dat, factors1,val1,val2,val3,val4,val5,val6,val7,time,name_of_factor){
  
  dist_1 = expand.grid(dist_1 = t(dat[1,12:35]))
  dist_2 = expand.grid(dist_2 = t(dat[1,12:35]))
  dist_3 = expand.grid(dist_3 = t(dat[1,12:35]))
  dist_4 = expand.grid(dist_4 = t(dat[1,12:35]))
  dist_5 = expand.grid(dist_5 = t(dat[1,12:35]))
  dist_6 = expand.grid(dist_6 = t(dat[1,12:35]))
  dist_7 = expand.grid(dist_7 = t(dat[1,12:35]))

  for(i in 1:ncol(subset(dat,factors1 == val1))){
    dist_1[,i] =   t(subset(dat,factors1 == val1)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val2))){
    dist_2[,i] =   t(subset(dat,factors1 == val2)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val3))){
    dist_3[,i] =   t(subset(dat,factors1 == val3)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val4))){
    dist_4[,i] =   t(subset(dat,factors1 == val4)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val5))){
    dist_5[,i] =   t(subset(dat,factors1 == val5)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val6))){
    dist_6[,i] =   t(subset(dat,factors1 == val6)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val7))){
    dist_7[,i] =   t(subset(dat,factors1 == val7)[i,12:35])
  }

  plot(dist_1[,1] ~ time,xaxt="n",xlab = "Time in hours",
       yaxt="n",ylab="Proportion of people in bed",main=name_of_factor)
  axis(1,at=c(1:24),labels=c(16:24,1:15))
  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2))

  for(i in 1:ncol(dist_1)){
    lines(dist_1[,i] ~ time,col=transp("aquamarine3",0.6))
  }
  for(i in 1:ncol(dist_2)){
    lines(dist_2[,i] ~ time,col=transp("purple",0.6))
  }
  for(i in 1:ncol(dist_3)){
    lines(dist_3[,i] ~ time,col=transp("darkred",0.6))
  }
  for(i in 1:ncol(dist_4)){
    lines(dist_4[,i] ~ time,col=transp("orange",0.6))
  }
  for(i in 1:ncol(dist_5)){
    lines(dist_5[,i] ~ time,col=transp("blue",0.6))
  }
  for(i in 1:ncol(dist_6)){
    lines(dist_6[,i] ~ time,col=transp("green",0.6))
  }
  for(i in 1:ncol(dist_7)){
    lines(dist_7[,i] ~ time,col=transp("grey",0.6))
  }
  
}
visual_fun2(dat=dat, factors1=dat$week_evening,
            val1="Monday",
            val2="Tuesday",
            val3="Wednesday",
            val4="Thursday",
            val5="Friday",
            val6="Saturday",
            val7="Sunday",
            time=time,
            name_of_factor="Day of the week")

par(mfrow=c(2,3));par(mar=c(5,5,3,3))


check_By_vill = function(dat,pan_lab1,pan_lab2,pan_lab3){
  
  factors1=dat$week_evening
  val1="Monday"
  val2="Tuesday"
  val3="Wednesday"
  val4="Thursday"
  val5="Friday"
  val6="Saturday"
  val7="Sunday"
  dist_1 = expand.grid(dist_1 = t(dat[1,12:35]))
  dist_2 = expand.grid(dist_2 = t(dat[1,12:35]))
  dist_3 = expand.grid(dist_3 = t(dat[1,12:35]))
  dist_4 = expand.grid(dist_4 = t(dat[1,12:35]))
  dist_5 = expand.grid(dist_5 = t(dat[1,12:35]))
  dist_6 = expand.grid(dist_6 = t(dat[1,12:35]))
  dist_7 = expand.grid(dist_7 = t(dat[1,12:35]))
  
  
  for(i in 1:ncol(subset(dat,factors1 == val1))){
    dist_1[,i] =   t(subset(dat,factors1 == val1)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val2))){
    dist_2[,i] =   t(subset(dat,factors1 == val2)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val3))){
    dist_3[,i] =   t(subset(dat,factors1 == val3)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val4))){
    dist_4[,i] =   t(subset(dat,factors1 == val4)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val5))){
    dist_5[,i] =   t(subset(dat,factors1 == val5)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val6))){
    dist_6[,i] =   t(subset(dat,factors1 == val6)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val7))){
    dist_7[,i] =   t(subset(dat,factors1 == val7)[i,12:35])
  }
  
  mean_weeks = array(dim=c(24,7),data=NA)
  mean_weeks[,1] = rowMeans(dist_1)
  mean_weeks[,2] = rowMeans(dist_2)
  mean_weeks[,3] = rowMeans(dist_3)
  mean_weeks[,4] = rowMeans(dist_4)
  mean_weeks[,5] = rowMeans(dist_5,na.rm=TRUE)
  mean_weeks[,6] = rowMeans(dist_6)
  mean_weeks[,7] = rowMeans(dist_7)
  
  low_q_weeks =  array(dim=c(24,7),data=NA)
  upp_q_weeks =  array(dim=c(24,7),data=NA)
  for(i in 1:24){
    upp_q_weeks[i,1] = quantile(t(dist_1)[,i],0.975,na.rm = TRUE) 
    upp_q_weeks[i,2] = quantile(t(dist_2)[,i],0.975,na.rm = TRUE) 
    upp_q_weeks[i,3] = quantile(t(dist_3)[,i],0.975,na.rm = TRUE) 
    upp_q_weeks[i,4] = quantile(t(dist_4)[,i],0.975,na.rm = TRUE) 
    upp_q_weeks[i,5] = quantile(t(dist_5)[,i],0.975,na.rm = TRUE) 
    upp_q_weeks[i,6] = quantile(t(dist_6)[,i],0.975,na.rm = TRUE) 
    upp_q_weeks[i,7] = quantile(t(dist_7)[,i],0.975,na.rm = TRUE) 
    
    low_q_weeks[i,1] = quantile(t(dist_1)[,i],0.025,na.rm = TRUE) 
    low_q_weeks[i,2] = quantile(t(dist_2)[,i],0.025,na.rm = TRUE) 
    low_q_weeks[i,3] = quantile(t(dist_3)[,i],0.025,na.rm = TRUE) 
    low_q_weeks[i,4] = quantile(t(dist_4)[,i],0.025,na.rm = TRUE) 
    low_q_weeks[i,5] = quantile(t(dist_5)[,i],0.025,na.rm = TRUE) 
    low_q_weeks[i,6] = quantile(t(dist_6)[,i],0.025,na.rm = TRUE) 
    low_q_weeks[i,7] = quantile(t(dist_7)[,i],0.025,na.rm = TRUE) 
  }
  
  name_of_factor = "Days of the week"
  plot(mean_weeks[,1] ~ time,xaxt="n",xlab = "Time in hours",pch="",
       yaxt="n",ylab="Proportion of people in bed",main=name_of_factor)
  axis(1,at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)))
  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2))
  text(1,1,pan_lab1)
  wknd = c(rep("grey20",4),rep("orange",2),"blue")
  lintyp = 1:7
  for(i in 1:7){
    polygon(c(time,rev(time)),c(upp_q_weeks[,i],rev(low_q_weeks[,i])),border=NA,col=transp(wknd[i],0.2))
    lines(mean_weeks[,i] ~ time,lty=lintyp[i],col=wknd[i])
  }
  
  legend(16,1,legend=c(paste("Mon (n =",length(dat$week_evening[dat$week_evening == "Monday"]),")"),
                       paste("Tue (",length(dat$week_evening[dat$week_evening == "Tuesday"]),")"),
                       paste("Wed (",length(dat$week_evening[dat$week_evening == "Wednesday"]),")"),
                       paste("Thu (",length(dat$week_evening[dat$week_evening == "Thursday"]),")"),
                       paste("Fri (",length(dat$week_evening[dat$week_evening == "Friday"]),")"),
                       paste("Sat (",length(dat$week_evening[dat$week_evening == "Saturday"]),")"),
                       paste("Sun (",length(dat$week_evening[dat$week_evening == "Sunday"]),")")),
         lty=1:7,bty="n",col=wknd)
  
  
  factors1 = dat$sex
  name_of_factor = "Sex"
  val1=1
  val2=2
  dist_1 = expand.grid(dist_1 = t(dat[1,12:35]))
  dist_2 = expand.grid(dist_2 = t(dat[1,12:35]))
  for(i in 1:ncol(subset(dat,factors1 == val1))){
    dist_1[,i] =   t(subset(dat,factors1 == val1)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val2))){
    dist_2[,i] =   t(subset(dat,factors1 == val2)[i,12:35])
  }
  plot(mean_weeks[,1] ~ time,xaxt="n",xlab = "Time in hours",pch="",
       yaxt="n",ylab="Proportion of people in bed",main=name_of_factor)
  axis(1,at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)))
  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2))
  text(1,1,pan_lab2)
  mean_sex = array(dim=c(24,2),data=NA)
  mean_sex[,1] = rowMeans(dist_1,na.rm=TRUE)
  mean_sex[,2] = rowMeans(dist_2,na.rm=TRUE)
  
  
  low_q_sex =  array(dim=c(24,2),data=NA)
  upp_q_sex =  array(dim=c(24,2),data=NA)
  for(i in 1:24){
    upp_q_sex[i,1] = quantile(t(dist_1)[,i],0.975,na.rm = TRUE) 
    upp_q_sex[i,2] = quantile(t(dist_2)[,i],0.975,na.rm = TRUE) 
    low_q_sex[i,1] = quantile(t(dist_1)[,i],0.025,na.rm = TRUE) 
    low_q_sex[i,2] = quantile(t(dist_2)[,i],0.025,na.rm = TRUE) 
  }
  colsex = c("aquamarine3","purple")
  for(i in 1:2){
    lines(mean_sex[,i] ~ time,lty=lintyp[i],col=colsex[i])
    polygon(c(time,rev(time)),c(upp_q_sex[,i],rev(low_q_sex[,i])),border=NA,col=transp(colsex[i],0.5))
  }
  legend(16,1,legend=c(paste("Males (",length(dat$sex[dat$sex == "1"]),")"),
                       paste("Females (",length(dat$sex[dat$sex == "2"]),")")),
         lty=1:2,bty="n",col=colsex)
  
#  factors1 = dat$location
#  name_of_factor = "Location"
#  dist_1 = expand.grid(dist_1 = t(dat[1,12:35]))
#  dist_2 = expand.grid(dist_2 = t(dat[1,12:35]))
#  for(i in 1:ncol(subset(dat,factors1 == val1))){
#    dist_1[,i] =   t(subset(dat,factors1 == val1)[i,12:35])
#  }
#  for(i in 1:ncol(subset(dat,factors1 == val2))){
#    dist_2[,i] =   t(subset(dat,factors1 == val2)[i,12:35])
#  }
#  plot(mean_weeks[,1] ~ time,xaxt="n",xlab = "Time in hours",pch="",
#       yaxt="n",ylab="Proportion of people in bed",main=name_of_factor)
#  axis(1,at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)))
#  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2))
#  mean_loc = array(dim=c(24,2),data=NA)
#  mean_loc[,1] = rowMeans(dist_1,na.rm=TRUE)
#  mean_loc[,2] = rowMeans(dist_2,na.rm=TRUE)
#  
#  low_q_loc =  array(dim=c(24,2),data=NA)
#  upp_q_loc =  array(dim=c(24,2),data=NA)
#  for(i in 1:24){
#    upp_q_loc[i,1] = quantile(t(dist_1)[,i],0.975,na.rm = TRUE) 
#    upp_q_loc[i,2] = quantile(t(dist_2)[,i],0.975,na.rm = TRUE) 
#    low_q_loc[i,1] = quantile(t(dist_1)[,i],0.025,na.rm = TRUE) 
#    low_q_loc[i,2] = quantile(t(dist_2)[,i],0.025,na.rm = TRUE) 
#  }
#  colloc = c("orange","red")
#  for(i in 1:2){
#    lines(mean_loc[,i] ~ time,lty=lintyp[i],col=colloc[i])
#    polygon(c(time,rev(time)),c(upp_q_loc[,i],rev(low_q_loc[,i])),border=NA,col=transp(colloc[i],0.5))
#    
#  }
#  legend(16,1,legend=c(paste("Milange (",length(dat$location[dat$location == "1"]),")"),
#                       paste("Tengua (",length(dat$location[dat$location == "2"]),")")),
#         lty=1:7,bty="n",col=colloc)
  
  factors1 = dat$mosquitonet
  name_of_factor = "Net use"
  val1 = 0
  val2 = 1
  dist_1 = expand.grid(dist_1 = t(dat[1,12:35]))
  dist_2 = expand.grid(dist_2 = t(dat[1,12:35]))
  for(i in 1:ncol(subset(dat,factors1 == val1))){
    dist_1[,i] =   t(subset(dat,factors1 == val1)[i,12:35])
  }
  for(i in 1:ncol(subset(dat,factors1 == val2))){
    dist_2[,i] =   t(subset(dat,factors1 == val2)[i,12:35])
  }
  plot(mean_weeks[,1] ~ time,xaxt="n",xlab = "Time in hours",pch="",
       yaxt="n",ylab="Proportion of people in bed",main=name_of_factor)
  axis(1,at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)))
  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2))
  text(1,1,pan_lab3)
  mean_net = array(dim=c(24,2),data=NA)
  mean_net[,1] = rowMeans(dist_1,na.rm=TRUE)
  mean_net[,2] = rowMeans(dist_2,na.rm=TRUE)
  
  low_q_net =  array(dim=c(24,2),data=NA)
  upp_q_net =  array(dim=c(24,2),data=NA)
  for(i in 1:24){
    upp_q_net[i,1] = quantile(t(dist_1)[,i],0.975,na.rm = TRUE) 
    upp_q_net[i,2] = quantile(t(dist_2)[,i],0.975,na.rm = TRUE) 
    low_q_net[i,1] = quantile(t(dist_1)[,i],0.025,na.rm = TRUE) 
    low_q_net[i,2] = quantile(t(dist_2)[,i],0.025,na.rm = TRUE) 
  }
  colnet = c("red","blue")
  
  for(i in 1:2){
    lines(mean_net[,i] ~ time,lty=lintyp[i],col=colnet[i])
    polygon(c(time,rev(time)),c(upp_q_net[,i],rev(low_q_net[,i])),border=NA,col=transp(colnet[i],0.4))
    
  }
  legend(16,1,legend=c(paste("No net (",length(dat$mosquitonet[dat$mosquitonet == "0"]),")"),
                       paste("Net used (",length(dat$mosquitonet[dat$mosquitonet == "1"]),")")),
         lty=1:2,bty="n",col=colnet)
  
}

check_By_vill(dat = dat)
check_By_vill(dat = subset(dat,dat$location == "1"),
              pan_lab1="A",pan_lab2="B",pan_lab3="C")
check_By_vill(dat = subset(dat,dat$location == "2"),
              pan_lab1="D",pan_lab2="E",pan_lab3="F")
##############################################
## What are the different estimates for phiI and phiB?

dat_mosq = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI_phiB_rawdata.csv",header=TRUE)
vec_sets = seq(0,1464,24) ##This is the start of each set of data for 24 hours
vec2 = c(2:61,1) ##This is the start of each set of data for 24 hours

dat_inbed_category = cbind(mean_weeks,mean_sex,mean_loc,mean_net)

phiB_estimates <- array(dim=c(24,61,13),data=NA)
##Calculate numerator for all of the phiB options
for(i in 1:24) {
  for(j in 1:61){
    phiB_estimates[i,j,1]   <- dat_inbed_category[i,1] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,2]   <- dat_inbed_category[i,2] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,3]   <- dat_inbed_category[i,3] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,4]   <- dat_inbed_category[i,4] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,5]   <- dat_inbed_category[i,5] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,6]   <- dat_inbed_category[i,6] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,7]   <- dat_inbed_category[i,7] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,8]   <- dat_inbed_category[i,8] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,9]   <- dat_inbed_category[i,9] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,10]   <- dat_inbed_category[i,10] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,11]   <- dat_inbed_category[i,11] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,12]   <- dat_inbed_category[i,12] * dat_mosq$Inside_mosq[vec_sets[j]+i]
    phiB_estimates[i,j,13]   <- dat_inbed_category[i,13] * dat_mosq$Inside_mosq[vec_sets[j]+i]
  }
}

#Calculate the sum of the hourly estimates
dat_inbed_category2 = array(dim=c(61,13))
for(k in 1:13){
  for(j in 1:61) {
    dat_inbed_category2[j,k] = sum(phiB_estimates[1:24,j,k])
  }
}
par(mfrow=c(1,1))
boxplot(dat_inbed_category2,xact="",yaxt="",xlab="Categories",ylab="Proportion of mosquito bites in bed (numerator only)")
axis(1,at=c(1:13),labels=c("Mo","Tu","We","Th","Fr","Sa","Su","Mal","Fem","Mil","Teg","No net","Net"))


tes = expand.grid(phib_num = c(dat_inbed_category2))
tes$categ = rep(c("1Mo","2Tu","3We","4Th","5Fr","6Sa","7Su",
                     "8Mal","9Fem",
                     "10Mil","11Teg",
                     "12No net","13Net"),each=61)
tes$category = c(rep("week days",61*7),rep("sex",61*2),rep("location",61*2),rep("net use",61*2))
library(ggplot2)
# Basic box plot
p <- ggplot(tes, aes(x=categ, y=phib_num, fill=category)) + geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                             outlier.size=2,notch=TRUE)
p + stat_summary(fun.y=mean, geom="point", shape=23, size=4) + geom_jitter(shape=16, position=position_jitter(0.2))

##Calculate denominator for all of the phiB options
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

