###############################
##
##
##
####################################

library(adegenet)
library(lme4)


se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(c(x))))

##############
##
##DATA AND PROCESSING
coun = read.table("H:/Ellie/IRS and resistance/behaviour_paper/PMI/COUNTRY_PHI2.txt",header=TRUE)
dat_1phi = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Janetta\\dataset1_phi_countries.csv",header=TRUE)
dat_mosq1 = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiB+phiI_Beale added.csv",header=TRUE)
dat_mosq2 = subset(dat_mosq1,dat_mosq1$Study != "PMI")
dat_mosq1$source = ifelse(dat_mosq1$Study == "PMI","PMI", "published")

levels(dat_mosq2$Species.grouped)[1] <- "A_gambiae_sl"
levels(dat_mosq2$Species.grouped)[4] <- "A_gambiae_sl"
levels(dat_mosq2$Species.grouped)[4:7] <- "An_other"
summary(dat_mosq2$Species.grouped)


head(dat_1phi)
## re-arrange data so that we can stack it

data_all1 = data.frame(Year = dat_1phi$Year,
                       Country = dat_1phi$Country,
                       Site = dat_1phi$Site,
                       sprayed = rep("not_recorded", nrow(dat_1phi)),
                       species = dat_1phi$Species.grouped,
                       phi_I = dat_1phi$phiI,
                       phi_B = dat_1phi$phiB,
                       deltamethrin = dat_1phi$ITN_levels)
names(data_all1)
names(coun)
dat_all = rbind(coun,data_all1)

#########################################
## 
## glmm
##
mod1<-lmer(phiI~Year+Species.grouped+(1|Country), data=dat_mosq1)
chmod1<-lmer(phiI~1+Species.grouped+(1|Country), data=dat_mosq1)
anova(mod1,chmod1)
summary(mod1)
confint(mod1)
se2 <- sqrt(diag(vcov(mod1)))
tab <- cbind(Est = fixef(mod1), LL = fixef(mod1) - 1.96 * se2, UL = fixef(mod1) + 1.96 * se2)
print(exp(tab),digits=3) #to get quick odds ratios for glmm - these show you how important the different levels of each factor are

datAnArab <- data.frame(preds = predict(mod1, 
                                        data.frame(Year = dat_mosq1$Year, 
                                                         Species.grouped = dat_mosq1$Species.grouped), 
                                        type="response", re.form = NA),
                        exp1 = dat_mosq1$Year)


mod2<-lmer(phiB~Year+Species.grouped+(1|Country), data=dat_mosq1)
summary(mod2)
confint(mod2)
se2 <- sqrt(diag(vcov(mod2)))
tab <- cbind(Est = fixef(mod2), LL = fixef(mod2) - 1.96 * se2, UL = fixef(mod2) + 1.96 * se2)
print(exp(tab),digits=3) #to get quick odds ratios for glmm - these show you how important the different levels of each factor are

datAnArab2 <- data.frame(preds = predict(mod2, 
                                        data.frame(Year = dat_mosq1$Year, 
                                                   Species.grouped = dat_mosq1$Species.grouped), 
                                        type="response", re.form = NA),
                        exp1 = dat_mosq1$Year)



plot.new()

##Top left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    mar = c(8,8,3,5),
    plt = c(0.1,0.45,0.6,0.95))#,
# major tick size and direction, < 0 means outside


boxplot(dat_all$phi_I ~ dat_all$Country, ylim=c(0,1.1), frame=FALSE,
        cex.lab=0.8,cex.axis=1.4,yaxt="n",xaxt="n",
        col=transp("darkblue",0.5),ylab="",
        border="darkblue",outline=FALSE)
mtext(side=2,las=0, line = 3,"Proportion of mosquitoes biting indoors",cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=0.8,cex.axis=0.8)
axis(1,las=2,at=1:21,labels=unique(levels(dat_all$Country)),cex.lab=0.8,cex.axis=0.8)
numI = c(as.numeric(c(summary(dat_all$Country))))
num=1:length(numI)
for(i in 1:21){
  text(num[i],0.02,numI[i],cex=0.8)
}
text(21.3,1.1,"A",cex=1.4)

##Bottom left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    mar = c(8,8,6,5),
    plt = c(0.1,0.45,0.12,0.47))#,
# major tick size and direction, < 0 means outside

boxplot(dat_all$phi_B ~ dat_all$Country, ylim=c(0,1.1), frame=FALSE,
        cex.lab=0.8,cex.axis=1.4,yaxt="n",xaxt="n",
        col=transp("darkblue",0.3),ylab="",
        border="darkblue",outline=FALSE)
mtext(side=2,las=0, line = 3,"Proportion of mosquitoes biting in bed",cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=0.8,cex.axis=0.8)
axis(1,las=2,at=1:21,labels=unique(levels(dat_all$Country)),cex.lab=0.8,cex.axis=0.8)
numI = c(as.numeric(c(summary(dat_all$Country))))
num=1:length(numI)
for(i in 1:21){
  text(num[i],0.02,numI[i],cex=0.8)
}
text(21.3,1.1,"B",cex=1.4)

##Top right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    mar = c(8,8,3,5),
    plt = c(0.55,0.9,0.6,0.95))#,
# major tick size and direction, < 0 means outside

##**RUN THROUGH MODELS IN figure_map.r first
plot(phiI~Year,data=dat_mosq1,pch="",bty="n",ylim=c(0,1),yaxt="n",cex.main=1.8,xlim=c(2000,2015),
     cex.lab=1.4, ylab="Proportion of mosquitoes biting indoors")
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

legend(2008,0.25,legend=c("Review data","PMI data"),
       col=c("grey"),pch=c(15,17),cex=1.2,bty="n")

#legend(1992,0.2,legend=c(levels(unique(dat_mosq1$Country))),
#       col=c("blue","blue","red","red","red","aquamarine3"),
#       pch=c(13,16,12,6,11,17),ncol=2,
#       lty=c(1,2,1,1,6,1),cex=1.4,bty="n")
text(2015,1,"C",cex=1.4)


##LOWER right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    mar = c(8,8,3,5),
    plt = c(0.55,0.9,0.12,0.47))#,

plot(phiB~Year,data=dat_mosq1,pch="",bty="n",ylim=c(0,1),yaxt="n",cex.main=1.4,xlim=c(2000,2015),
     cex.lab=1.4, ylab="Proportion of mosquitoes biting in bed")
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.4)  

##PREDICTIONS FROM THE GLMM WITH COUNTRY AS A RANDOM EFFECT
lines(rev(sort(datAnArab2$preds)) ~ sort(datAnArab2$exp1),lty=1,lwd=1)
polygon(c(sort(datAnArab2$exp1),rev(sort(datAnArab2$exp1))),
        c(rev(sort(datAnArab2$preds-6*se(datAnArab2$preds))),
          sort(datAnArab2$preds+6*se(datAnArab2$preds))),
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
