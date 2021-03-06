##Differences in cohorts
library(adegenet)
dat = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Andrew Beale\\mergedata.csv",header=TRUE)
head(dat)

which(dat[1,12:35] == 1)[1]
for(i in 1:nrow(dat)){
  dat$time_went_to_bed[i] = sum(c(ifelse(dat[i,12] == "0", 60,ifelse(dat[i,12] == "1",0,60*dat[i,12])),
                                  ifelse(dat[i,13] == "0", 60,ifelse(dat[i,13] == "1",0,60*dat[i,13])),
                                  ifelse(dat[i,14] == "0", 60,ifelse(dat[i,14] == "1",0,60*dat[i,14])),
                                  ifelse(dat[i,15] == "0", 60,ifelse(dat[i,15] == "1",0,60*dat[i,15])),
                                  ifelse(dat[i,16] == "0", 60,ifelse(dat[i,16] == "1",0,60*dat[i,16])),
                                  ifelse(dat[i,17] == "0", 60,ifelse(dat[i,17] == "1",0,60*dat[i,17])),
                                  ifelse(dat[i,18] == "0", 60,ifelse(dat[i,18] == "1",0,60*dat[i,18])),
                                  ifelse(dat[i,19] == "0", 60,ifelse(dat[i,19] == "1",0,60*dat[i,19])),
                                  ifelse(dat[i,20] == "0", 60,ifelse(dat[i,20] == "1",0,60*dat[i,20])),
                                  ifelse(dat[i,21] == "0", 60,ifelse(dat[i,21] == "1",0,60*dat[i,21])) )  )
  dat$time_to_rise[i] = 1440 - sum(c(ifelse(dat[i,22] == "0", 60,ifelse(dat[i,22] == "1",0,60 - 60*dat[i,22])),
                                     ifelse(dat[i,23] == "0", 60,ifelse(dat[i,23] == "1",0,60 - 60*dat[i,23])),
                                     ifelse(dat[i,24] == "0", 60,ifelse(dat[i,24] == "1",0,60 - 60*dat[i,24])),
                                     ifelse(dat[i,25] == "0", 60,ifelse(dat[i,25] == "1",0,60 - 60*dat[i,25])),
                                     ifelse(dat[i,26] == "0", 60,ifelse(dat[i,26] == "1",0,60 - 60*dat[i,26])),
                                     ifelse(dat[i,27] == "0", 60,ifelse(dat[i,27] == "1",0,60 - 60*dat[i,27])),
                                     ifelse(dat[i,28] == "0", 60,ifelse(dat[i,28] == "1",0,60 - 60*dat[i,28])),
                                     ifelse(dat[i,29] == "0", 60,ifelse(dat[i,29] == "1",0,60 - 60*dat[i,29])),
                                     ifelse(dat[i,30] == "0", 60,ifelse(dat[i,30] == "1",0,60 - 60*dat[i,30])),
                                     ifelse(dat[i,31] == "0", 60,ifelse(dat[i,31] == "1",0,60 - 60*dat[i,31])),
                                     ifelse(dat[i,32] == "0", 60,ifelse(dat[i,32] == "1",0,60 - 60*dat[i,32])),
                                     ifelse(dat[i,33] == "0", 60,ifelse(dat[i,33] == "1",0,60 - 60*dat[i,33])),
                                     ifelse(dat[i,34] == "0", 60,ifelse(dat[i,34] == "1",0,60 - 60*dat[i,34])),
                                     ifelse(dat[i,35] == "0", 60,ifelse(dat[i,35] == "1",0,60 - 60*dat[i,35])) )  )
  }
dat$time_went_to_bed[480] = 189 ##check if reorder...this is the one that wakes up super early so need to adjust form 191 to 189
dat$time_to_rise[480] = 542 ##check if reorder...this is the one that wakes up super early so need to adjust form 191 to 189
hist(dat$time_went_to_bed)
hist(dat$time_to_rise)

dat$week_evening2=ifelse(dat$week_evening == "Monday",1,
                         ifelse(dat$week_evening == "Tuesday",2,
                                ifelse(dat$week_evening == "Wednesday",3,
                                       ifelse(dat$week_evening == "Thursday",4,
                                              ifelse(dat$week_evening == "Friday",5,
                                                     ifelse(dat$week_evening == "Saturday",6,7))))))

shapiro.test(dat$time_went_to_bed)
qqnorm(dat$time_went_to_bed);qqline(dat$time_went_to_bed, col = 2)
shapiro.test(dat$time_to_rise)
qqnorm(dat$time_to_rise);qqline(dat$time_to_rise, col = 2)

shapiro.test(sqrt(dat$time_went_to_bed))
qqnorm(sqrt(dat$time_went_to_bed));qqline(sqrt(dat$time_went_to_bed), col = 2)
shapiro.test(sqrt(dat$time_went_to_bed))
qqnorm(sqrt(dat$time_to_rise));qqline(sqrt(dat$time_to_rise), col = 2)

dat$age_grouped = ifelse(dat$age < 21,"a_<21",
                         ifelse(dat$age >20 & dat$age <30, "b_21-30",
                                ifelse(dat$age >=30 & dat$age <40, "c_31-40","d_>40")))

boxplot(dat$time_went_to_bed ~ dat$age_grouped,ylab="Time to bed (minutes since 16:00)")
boxplot(dat$time_went_to_bed ~ dat$location)

par(mfrow=c(4,4))
par(mar=c(5,5,2,2))
boxplot(dat$time_went_to_bed[dat$location == "1"] ~ dat$sex[dat$location == "1"],ylab="Time to bed (minutes since 16:00)",xlab="Sex",col="lightblue",cex.lab=1.3)##Milange
boxplot(dat$time_went_to_bed[dat$location == "2"] ~ dat$sex[dat$location == "2"],ylab="Time to bed (minutes since 16:00)",xlab="Sex",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1"] ~ dat$sex[dat$location == "1"],ylab="Time to rise (minutes since 16:00)",xlab="Sex",col="lightblue",cex.lab=1.3)##Milange
boxplot(dat$time_to_rise[dat$location == "2"] ~ dat$sex[dat$location == "2"],ylab="Time to rise (minutes since 16:00)",xlab="Sex",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1"] ~ dat$age_grouped[dat$location == "1"],ylab="Time to bed (minutes since 16:00)",xlab="Age group in years",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2"] ~ dat$age_grouped[dat$location == "2"],ylab="Time to bed (minutes since 16:00)",xlab="Age group in years",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1"] ~ dat$age_grouped[dat$location == "1"],ylab="Time to rise (minutes since 16:00)",xlab="Age group in years",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2"] ~ dat$age_grouped[dat$location == "2"],ylab="Time to rise (minutes since 16:00)",xlab="Age group in years",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1"] ~ dat$week_evening2[dat$location == "1"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2"] ~ dat$week_evening2[dat$location == "2"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1"] ~ dat$week_evening2[dat$location == "1"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2"] ~ dat$week_evening2[dat$location == "2"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1"] ~ dat$mosquitonet[dat$location == "1"],ylab="Time to bed (minutes since 16:00)",xlab="Net use",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2"] ~ dat$mosquitonet[dat$location == "2"],ylab="Time to bed (minutes since 16:00)",xlab="Net use",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1"] ~ dat$mosquitonet[dat$location == "1"],ylab="Time to rise (minutes since 16:00)",xlab="Net use",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2"] ~ dat$mosquitonet[dat$location == "2"],ylab="Time to rise (minutes since 16:00)",xlab="Net use",col="aquamarine2",cex.lab=1.3)


boxplot(dat$time_went_to_bed[dat$location == "1" & dat$sex == "1"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "1"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$sex == "1"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "1"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$sex == "1"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "1"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$sex == "1"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "1"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$sex == "2"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "2"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$sex == "2"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "2"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$sex == "2"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "2"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$sex == "2"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "2"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$age_grouped == "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$age_grouped == "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$age_grouped == "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$age_grouped == "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$age_grouped != "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$age_grouped != "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$age_grouped != "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$age_grouped != "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$sex == "1" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "1" & dat$age_grouped == "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$sex == "1" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "1" & dat$age_grouped == "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$sex == "1" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "1" & dat$age_grouped == "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$sex == "1" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "1" & dat$age_grouped == "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$sex == "2" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "2" & dat$age_grouped == "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$sex == "2" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "2" & dat$age_grouped == "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$sex == "2" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "2" & dat$age_grouped == "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$sex == "2" & dat$age_grouped == "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "2" & dat$age_grouped == "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$sex == "1" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "1" & dat$age_grouped != "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$sex == "1" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "1" & dat$age_grouped != "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$sex == "1" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "1" & dat$age_grouped != "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$sex == "1" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "1" & dat$age_grouped != "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$sex == "2" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "2" & dat$age_grouped != "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$sex == "2" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "2" & dat$age_grouped != "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$sex == "2" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "1" & dat$sex == "2" & dat$age_grouped != "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$sex == "2" & dat$age_grouped != "b21-30"] ~ dat$week_evening2[dat$location == "2" & dat$sex == "2" & dat$age_grouped != "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Weekdays (Mon - Sun)",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$sex == "1" & dat$age_grouped == "b21-30"] ~ dat$mosquitonet[dat$location == "1"& dat$sex == "1" & dat$age_grouped == "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Net use",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$sex == "1" & dat$age_grouped == "b21-30"] ~ dat$mosquitonet[dat$location == "2"& dat$sex == "1" & dat$age_grouped == "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Net use",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$sex == "2" & dat$age_grouped == "b21-30"] ~ dat$mosquitonet[dat$location == "1"& dat$sex == "2" & dat$age_grouped == "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Net use",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$sex == "2" & dat$age_grouped == "b21-30"] ~ dat$mosquitonet[dat$location == "2"& dat$sex == "2" & dat$age_grouped == "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Net use",col="aquamarine2",cex.lab=1.3)

boxplot(dat$time_went_to_bed[dat$location == "1" & dat$sex == "1" & dat$age_grouped != "b21-30"] ~ dat$mosquitonet[dat$location == "1"& dat$sex == "1" & dat$age_grouped != "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Net use",col="lightblue",cex.lab=1.3)
boxplot(dat$time_went_to_bed[dat$location == "2" & dat$sex == "1" & dat$age_grouped != "b21-30"] ~ dat$mosquitonet[dat$location == "2"& dat$sex == "1" & dat$age_grouped != "b21-30"],ylab="Time to bed (minutes since 16:00)",xlab="Net use",col="aquamarine2",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "1" & dat$sex == "2" & dat$age_grouped != "b21-30"] ~ dat$mosquitonet[dat$location == "1"& dat$sex == "2" & dat$age_grouped != "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Net use",col="lightblue",cex.lab=1.3)
boxplot(dat$time_to_rise[dat$location == "2" & dat$sex == "2" & dat$age_grouped != "b21-30"] ~ dat$mosquitonet[dat$location == "2"& dat$sex == "2" & dat$age_grouped != "b21-30"],ylab="Time to rise (minutes since 16:00)",xlab="Net use",col="aquamarine2",cex.lab=1.3)

dat_milange = subset(dat,dat$location == "1")
dat_tengua = subset(dat,dat$location == "2")

a = glm(time_went_to_bed ~ age_grouped*sex*location+mosquitonet,data=dat)
summary.lm(a)

a = glm(time_went_to_bed ~ age_grouped+sex+mosquitonet,data=dat_tengua)
summary.lm(a)

tapply(dat_tengua$sex[dat_tengua$age_grouped == "a<21"],dat_tengua$mosquitonet[dat_tengua$age_grouped == "a<21"],length)
tapply(dat_tengua$sex[dat_tengua$age_grouped == "b21-30"],dat_tengua$mosquitonet[dat_tengua$age_grouped == "b21-30"],length)
tapply(dat_tengua$sex[dat_tengua$age_grouped == "31-40"],dat_tengua$mosquitonet[dat_tengua$age_grouped == "31-40"],length)
tapply(dat_tengua$sex[dat_tengua$age_grouped == ">40"],dat_tengua$mosquitonet[dat_tengua$age_grouped == ">40"],length)

a = glm(time_went_to_bed ~ age_grouped*mosquitonet,data=dat_milange)
summary.lm(a)

tapply(dat_milange$sex[dat_milange$age_grouped == "a<21"],dat_milange$mosquitonet[dat_milange$age_grouped == "a<21"],length)
tapply(dat_milange$sex[dat_milange$age_grouped == "b21-30"],dat_milange$mosquitonet[dat_milange$age_grouped == "b21-30"],length)
tapply(dat_milange$sex[dat_milange$age_grouped == "31-40"],dat_milange$mosquitonet[dat_milange$age_grouped == "31-40"],length)

##Key observations
#1 there is a clear difference in the time to bed and to rise between locations
par(mfrow=c(2,2))
par(mar=c(5,5,2,2))

summary.lm(glm(dat$time_went_to_bed ~ dat$location))

boxplot(dat$time_went_to_bed ~ dat$codelocation,ylab="Time to bed (minutes since 16:00)",xlab="Location",col=c("lightblue","aquamarine2"),cex.lab=1.3)
text(1.5,600,"***",cex=2)

summary.lm(glm(dat$time_to_rise ~ dat$location))

boxplot(dat$time_to_rise ~ dat$codelocation,ylab="Time to rise (minutes since 16:00)",xlab="Location",col=c("lightblue","aquamarine2"),cex.lab=1.3)
text(1.5,1000,"***",cex=2)

#2 within villages
#2a individual effects of age, sex, net use and weekday (with individual as a random effect)
library(lme4)

a1 = lmer(time_went_to_bed ~ age_grouped + (1|code_hours),data=dat_milange,REML=TRUE)
a1a = lmer(time_went_to_bed ~ (1|code_hours),data=dat_milange,REML=TRUE)
anova(a1,a1a)
a2 = lmer(time_went_to_bed ~ sex + (1|code_hours),data=dat_milange,REML=TRUE)
a2a = lmer(time_went_to_bed ~ (1|code_hours),data=dat_milange,REML=TRUE)
anova(a2,a2a)
a3 = lmer(time_went_to_bed ~ mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
a3a = lmer(time_went_to_bed ~ (1|code_hours),data=dat_milange,REML=TRUE)
anova(a3,a3a)
a4 = lmer(time_went_to_bed ~ week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
a4a = lmer(time_went_to_bed ~ (1|code_hours),data=dat_milange,REML=TRUE)
anova(a4,a4a)

b1 = lmer(time_went_to_bed ~ age_grouped + (1|code_hours),data=dat_tengua,REML=TRUE)
b1a = lmer(time_went_to_bed ~ (1|code_hours),data=dat_tengua,REML=TRUE)
anova(b1,b1a)
b2 = lmer(time_went_to_bed ~ sex + (1|code_hours),data=dat_tengua,REML=TRUE)
b2a = lmer(time_went_to_bed ~ (1|code_hours),data=dat_tengua,REML=TRUE)
anova(b2,b2a)
b3 = lmer(time_went_to_bed ~ mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
b3a = lmer(time_went_to_bed ~ (1|code_hours),data=dat_tengua,REML=TRUE)
anova(b3,b3a)
b4 = lmer(time_went_to_bed ~ week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
b4a = lmer(time_went_to_bed ~ (1|code_hours),data=dat_tengua,REML=TRUE)
anova(b4,b4a)


#2b dual effects of age, sex, net use and weekday (with individual as a random effect)
aa1 = lmer(time_went_to_bed ~ age_grouped + sex + (1|code_hours),data=dat_milange,REML=TRUE)
aa1a = lmer(time_went_to_bed ~ age_grouped + (1|code_hours),data=dat_milange,REML=TRUE)
aa1b = lmer(time_went_to_bed ~ sex + (1|code_hours),data=dat_milange,REML=TRUE)
aa1c = lmer(time_went_to_bed ~ age_grouped * sex + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa1,aa1c)
anova(aa1,aa1a)
anova(aa1,aa1b)

aa2 = lmer(time_went_to_bed ~ age_grouped + mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa2a = lmer(time_went_to_bed ~ age_grouped + (1|code_hours),data=dat_milange,REML=TRUE)
aa2b = lmer(time_went_to_bed ~ mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa2c = lmer(time_went_to_bed ~ age_grouped * mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa2,aa2c)
anova(aa2,aa2a)
anova(aa2,aa2b)

aa3 = lmer(time_went_to_bed ~ sex + mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa3a = lmer(time_went_to_bed ~ sex + (1|code_hours),data=dat_milange,REML=TRUE)
aa3b = lmer(time_went_to_bed ~ mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa3c = lmer(time_went_to_bed ~ sex * mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa3,aa3c)
anova(aa3,aa3a)
anova(aa3,aa3b)

aa4 = lmer(time_went_to_bed ~ sex + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa4a = lmer(time_went_to_bed ~ sex + (1|code_hours),data=dat_milange,REML=TRUE)
aa4b = lmer(time_went_to_bed ~ week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa4c = lmer(time_went_to_bed ~ sex * week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa4,aa4c)
anova(aa4,aa4a)
anova(aa4,aa4b)

aa5 = lmer(time_went_to_bed ~ age_grouped + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa5a = lmer(time_went_to_bed ~ age_grouped + (1|code_hours),data=dat_milange,REML=TRUE)
aa5b = lmer(time_went_to_bed ~ week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa5c = lmer(time_went_to_bed ~ age_grouped * week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa5,aa5c)
anova(aa5,aa5a)
anova(aa5,aa5b)

aa6 = lmer(time_went_to_bed ~ mosquitonet + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa6a = lmer(time_went_to_bed ~ mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa6b = lmer(time_went_to_bed ~ week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa6c = lmer(time_went_to_bed ~ mosquitonet * week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa6,aa6c)
anova(aa6,aa6a)
anova(aa6,aa6b)


bb1 = lmer(time_went_to_bed ~ age_grouped + sex + (1|code_hours),data=dat_tengua,REML=TRUE)
bb1a = lmer(time_went_to_bed ~ age_grouped + (1|code_hours),data=dat_tengua,REML=TRUE)
bb1b = lmer(time_went_to_bed ~ sex + (1|code_hours),data=dat_tengua,REML=TRUE)
bb1c = lmer(time_went_to_bed ~ age_grouped * sex + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb1,bb1c)
anova(bb1,bb1a)
anova(bb1,bb1b)

bb2 = lmer(time_went_to_bed ~ age_grouped + mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb2a = lmer(time_went_to_bed ~ age_grouped + (1|code_hours),data=dat_tengua,REML=TRUE)
bb2b = lmer(time_went_to_bed ~ mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb2c = lmer(time_went_to_bed ~ age_grouped * mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb2,bb2c)
anova(bb2,bb2a)
anova(bb2,bb2b)

bb3 = lmer(time_went_to_bed ~ sex + mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb3a = lmer(time_went_to_bed ~ sex + (1|code_hours),data=dat_tengua,REML=TRUE)
bb3b = lmer(time_went_to_bed ~ mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb3c = lmer(time_went_to_bed ~ sex * mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb3,bb3c)
anova(bb3,bb3a)
anova(bb3,bb3b)

bb4 = lmer(time_went_to_bed ~ sex + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb4a = lmer(time_went_to_bed ~ sex + (1|code_hours),data=dat_tengua,REML=TRUE)
bb4b = lmer(time_went_to_bed ~ week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb4c = lmer(time_went_to_bed ~ sex * week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb4,bb4c)
anova(bb4,bb4a)
anova(bb4,bb4b)

bb5 = lmer(time_went_to_bed ~ age_grouped + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb5a = lmer(time_went_to_bed ~ age_grouped + (1|code_hours),data=dat_tengua,REML=TRUE)
bb5b = lmer(time_went_to_bed ~ week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb5c = lmer(time_went_to_bed ~ age_grouped * week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb5,bb5c)
anova(bb5,bb5a)
anova(bb5,bb5b)

bb6 = lmer(time_went_to_bed ~ mosquitonet + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb6a = lmer(time_went_to_bed ~ mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb6b = lmer(time_went_to_bed ~ week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb6c = lmer(time_went_to_bed ~ mosquitonet * week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb6,bb6c)
anova(bb6,bb6a)
anova(bb6,bb6b)

#3 Interactive effects of age, sex, net use and weekday (with individual as a random effect)
aaa1 = lmer(time_went_to_bed ~ age_grouped + sex + mosquitonet + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aaa1a = lmer(time_went_to_bed ~ age_grouped + sex + mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aaa1b = lmer(time_went_to_bed ~ age_grouped + sex + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aaa1c = lmer(time_went_to_bed ~ age_grouped + mosquitonet + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aaa1d = lmer(time_went_to_bed ~ sex + mosquitonet + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aaa1,aaa1a)
anova(aaa1,aaa1b)
anova(aaa1,aaa1c)
anova(aaa1,aaa1d)

bbb1 = lmer(time_went_to_bed ~ age_grouped + sex + mosquitonet + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bbb1a = lmer(time_went_to_bed ~ age_grouped + sex + mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bbb1b = lmer(time_went_to_bed ~ age_grouped + sex + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bbb1c = lmer(time_went_to_bed ~ age_grouped + mosquitonet + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bbb1d = lmer(time_went_to_bed ~ sex + mosquitonet + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bbb1,bbb1a)
anova(bbb1,bbb1b)
anova(bbb1,bbb1c)
anova(bbb1,bbb1d)

#boxplot(dat_tengua$time_went_to_bed ~ dat_tengua$sex,ylab="Time to bed (minutes since 16:00)",xlab="Sex",col="aquamarine2",cex.lab=1.3)
#text(1.5,500,"*",cex=2)
boxplot(dat_tengua$time_went_to_bed[dat_tengua$sex == "1"] ~ 
          dat_tengua$age_grouped[dat_tengua$sex == "1"],
        ylab="Time to bed (minutes since 16:00)",xaxt="n",yaxt="n",
        xlab="Age group",col="aquamarine2",cex.lab=1.3,ylim=c(0,500))
axis(1,at=c(1,2,3,4),labels=c("Under 21","21 - 30","31 - 40","Over 40"),cex.axis=1.3)
axis(2,las=2,at=seq(0,500,100),labels=seq(0,500,100),cex.axis=1.3)
text(0.8,0,"Males",cex=1.4)

boxplot(dat_tengua$time_went_to_bed[dat_tengua$sex == "2"] ~ 
          dat_tengua$age_grouped[dat_tengua$sex == "2"],
        ylab="Time to bed (minutes since 16:00)",xaxt="n",yaxt="n",
        xlab="Age group",col="aquamarine2",cex.lab=1.3,ylim=c(0,500))
axis(1,at=c(1,2,3,4),labels=c("Under 21","21 - 30","31 - 40","Over 40"),cex.axis=1.3)
axis(2,las=2,at=seq(0,500,100),labels=seq(0,500,100),cex.axis=1.3)
text(1,0,"Females",cex=1.4)


#2 within villages
#2a individual effects of age, sex, net use and weekday (with individual as a random effect)
library(lme4)

a1 = lmer(time_to_rise ~ age_grouped + (1|code_hours),data=dat_milange,REML=TRUE)
a1a = lmer(time_to_rise ~ (1|code_hours),data=dat_milange,REML=TRUE)
anova(a1,a1a)
a2 = lmer(time_to_rise ~ sex + (1|code_hours),data=dat_milange,REML=TRUE)
a2a = lmer(time_to_rise ~ (1|code_hours),data=dat_milange,REML=TRUE)
anova(a2,a2a)
a3 = lmer(time_to_rise ~ mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
a3a = lmer(time_to_rise ~ (1|code_hours),data=dat_milange,REML=TRUE)
anova(a3,a3a)
a4 = lmer(time_to_rise ~ week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
a4a = lmer(time_to_rise ~ (1|code_hours),data=dat_milange,REML=TRUE)
anova(a4,a4a)

b1 = lmer(time_to_rise ~ age_grouped + (1|code_hours),data=dat_tengua,REML=TRUE)
b1a = lmer(time_to_rise ~ (1|code_hours),data=dat_tengua,REML=TRUE)
anova(b1,b1a)
b2 = lmer(time_to_rise ~ sex + (1|code_hours),data=dat_tengua,REML=TRUE)
b2a = lmer(time_to_rise ~ (1|code_hours),data=dat_tengua,REML=TRUE)
anova(b2,b2a)
b3 = lmer(time_to_rise ~ mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
b3a = lmer(time_to_rise ~ (1|code_hours),data=dat_tengua,REML=TRUE)
anova(b3,b3a)
b4 = lmer(time_to_rise ~ week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
b4a = lmer(time_to_rise ~ (1|code_hours),data=dat_tengua,REML=TRUE)
anova(b4,b4a)


#2b dual effects of age, sex, net use and weekday (with individual as a random effect)
aa1 = lmer(time_to_rise ~ age_grouped + sex + (1|code_hours),data=dat_milange,REML=TRUE)
aa1a = lmer(time_to_rise ~ age_grouped + (1|code_hours),data=dat_milange,REML=TRUE)
aa1b = lmer(time_to_rise ~ sex + (1|code_hours),data=dat_milange,REML=TRUE)
aa1c = lmer(time_to_rise ~ age_grouped * sex + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa1,aa1c)
anova(aa1,aa1a)
anova(aa1,aa1b)

aa2 = lmer(time_to_rise ~ age_grouped + mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa2a = lmer(time_to_rise ~ age_grouped + (1|code_hours),data=dat_milange,REML=TRUE)
aa2b = lmer(time_to_rise ~ mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa2c = lmer(time_to_rise ~ age_grouped * mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa2,aa2c)
anova(aa2,aa2a)
anova(aa2,aa2b)

aa3 = lmer(time_to_rise ~ sex + mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa3a = lmer(time_to_rise ~ sex + (1|code_hours),data=dat_milange,REML=TRUE)
aa3b = lmer(time_to_rise ~ mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa3c = lmer(time_to_rise ~ sex * mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa3,aa3c)
anova(aa3,aa3a)
anova(aa3,aa3b)

aa4 = lmer(time_to_rise ~ sex + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa4a = lmer(time_to_rise ~ sex + (1|code_hours),data=dat_milange,REML=TRUE)
aa4b = lmer(time_to_rise ~ week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa4c = lmer(time_to_rise ~ sex * week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa4,aa4c)
anova(aa4,aa4a)
anova(aa4,aa4b)

aa5 = lmer(time_to_rise ~ age_grouped + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa5a = lmer(time_to_rise ~ age_grouped + (1|code_hours),data=dat_milange,REML=TRUE)
aa5b = lmer(time_to_rise ~ week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa5c = lmer(time_to_rise ~ age_grouped * week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa5,aa5c)
anova(aa5,aa5a)
anova(aa5,aa5b)

aa6 = lmer(time_to_rise ~ mosquitonet + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa6a = lmer(time_to_rise ~ mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aa6b = lmer(time_to_rise ~ week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aa6c = lmer(time_to_rise ~ mosquitonet * week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aa6,aa6c)
anova(aa6,aa6a)
anova(aa6,aa6b)


bb1 = lmer(time_to_rise ~ age_grouped + sex + (1|code_hours),data=dat_tengua,REML=TRUE)
bb1a = lmer(time_to_rise ~ age_grouped + (1|code_hours),data=dat_tengua,REML=TRUE)
bb1b = lmer(time_to_rise ~ sex + (1|code_hours),data=dat_tengua,REML=TRUE)
bb1c = lmer(time_to_rise ~ age_grouped * sex + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb1,bb1c)
anova(bb1,bb1a)
anova(bb1,bb1b)

bb2 = lmer(time_to_rise ~ age_grouped + mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb2a = lmer(time_to_rise ~ age_grouped + (1|code_hours),data=dat_tengua,REML=TRUE)
bb2b = lmer(time_to_rise ~ mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb2c = lmer(time_to_rise ~ age_grouped * mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb2,bb2c)
anova(bb2,bb2a)
anova(bb2,bb2b)

bb3 = lmer(time_to_rise ~ sex + mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb3a = lmer(time_to_rise ~ sex + (1|code_hours),data=dat_tengua,REML=TRUE)
bb3b = lmer(time_to_rise ~ mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb3c = lmer(time_to_rise ~ sex * mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb3,bb3c)
anova(bb3,bb3a)
anova(bb3,bb3b)

bb4 = lmer(time_to_rise ~ sex + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb4a = lmer(time_to_rise ~ sex + (1|code_hours),data=dat_tengua,REML=TRUE)
bb4b = lmer(time_to_rise ~ week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb4c = lmer(time_to_rise ~ sex * week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb4,bb4c)
anova(bb4,bb4a)
anova(bb4,bb4b)

bb5 = lmer(time_to_rise ~ age_grouped + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb5a = lmer(time_to_rise ~ age_grouped + (1|code_hours),data=dat_tengua,REML=TRUE)
bb5b = lmer(time_to_rise ~ week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb5c = lmer(time_to_rise ~ age_grouped * week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb5,bb5c)
anova(bb5,bb5a)
anova(bb5,bb5b)

bb6 = lmer(time_to_rise ~ mosquitonet + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb6a = lmer(time_to_rise ~ mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bb6b = lmer(time_to_rise ~ week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bb6c = lmer(time_to_rise ~ mosquitonet * week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bb6,bb6c)
anova(bb6,bb6a)
anova(bb6,bb6b)

#3 Interactive effects of age, sex, net use and weekday (with individual as a random effect)
aaa1 = lmer(time_to_rise ~ age_grouped + sex + mosquitonet + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aaa1a = lmer(time_to_rise ~ age_grouped + sex + mosquitonet + (1|code_hours),data=dat_milange,REML=TRUE)
aaa1b = lmer(time_to_rise ~ age_grouped + sex + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aaa1c = lmer(time_to_rise ~ age_grouped + mosquitonet + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
aaa1d = lmer(time_to_rise ~ sex + mosquitonet + week_evening2 + (1|code_hours),data=dat_milange,REML=TRUE)
anova(aaa1,aaa1a)
anova(aaa1,aaa1b)
anova(aaa1,aaa1c)
anova(aaa1,aaa1d)

bbb1 = lmer(time_to_rise ~ age_grouped + sex + mosquitonet + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bbb1a = lmer(time_to_rise ~ age_grouped + sex + mosquitonet + (1|code_hours),data=dat_tengua,REML=TRUE)
bbb1b = lmer(time_to_rise ~ age_grouped + sex + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bbb1c = lmer(time_to_rise ~ age_grouped + mosquitonet + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
bbb1d = lmer(time_to_rise ~ sex + mosquitonet + week_evening2 + (1|code_hours),data=dat_tengua,REML=TRUE)
anova(bbb1,bbb1a)
anova(bbb1,bbb1b)
anova(bbb1,bbb1c)
anova(bbb1,bbb1d)

##########
##
## Figure for glmms on bed time data
par(mfrow=c(2,2))
par(mar=c(5,5,2,2))

summary.lm(glm(dat$time_went_to_bed ~ dat$location))

boxplot(dat$time_went_to_bed ~ dat$codelocation,yaxt="n",ylab="Time to bed (minutes since 16:00 hrs)",xlab="Location",col=c("lightblue","aquamarine2"),cex.lab=1.3)
text(1.5,600,"***",cex=2)
text(2.5,600,"A",cex=1.6)
summary.lm(glm(dat$time_to_rise ~ dat$location))
axis(2,las=2,at=seq(0,500,100),labels=seq(0,500,100),cex.axis=1.3)

boxplot(dat$time_to_rise ~ dat$codelocation,yaxt="n",ylab="Time to rise (minutes since 16:00 hrs)",xlab="Location",col=c("lightblue","aquamarine2"),cex.lab=1.3)
text(1.5,1000,"***",cex=2)
text(2.5,1000,"B",cex=1.6)
axis(2,las=2,at=seq(600,1000,100),labels=seq(600,1000,100),cex.axis=1.3)

boxplot(dat_milange$time_to_rise[dat_milange$mosquitonet == "0" & dat_milange$age_grouped == "a_<21"],  
        dat_milange$time_to_rise[dat_milange$mosquitonet == "1" & dat_milange$age_grouped == "a_<21"], 
        
        dat_milange$time_to_rise[dat_milange$mosquitonet == "0" & dat_milange$age_grouped == "b_21-30"], 
        dat_milange$time_to_rise[dat_milange$mosquitonet == "1" & dat_milange$age_grouped == "b_21-30"], 
        
        dat_milange$time_to_rise[dat_milange$mosquitonet == "0" & dat_milange$age_grouped == "c_31-40"], 
        dat_milange$time_to_rise[dat_milange$mosquitonet == "1" & dat_milange$age_grouped == "c_31-40"], 
        
        ylab="Time to rise (minutes since 16:00 hrs)",xaxt="n",yaxt="n",
        xlab="Use of nets",col=rep(c("lightblue",transp("blue",0.6)),3),cex.lab=1.3,ylim=c(600,1000))

axis(1,at=c(1.5,3.5,5.5),labels=c("Under 21","21 to 30","31 to 40"),cex.axis=1.3)
axis(2,las=2,at=seq(600,1000,100),labels=seq(600,1000,100),cex.axis=1.3)

text(3.5,600,"Milange",cex=1.2)
legend(0.5,1000,legend=c("No net","Net"),col=c("lightblue",transp("blue",0.6)),cex=1.3,pch=15)
text(6.5,1000,"C",cex=1.6)
#boxplot(dat_tengua$time_to_rise ~ dat_tengua$sex,ylab="Time to bed (minutes since 16:00)",xlab="Sex",col="aquamarine2",cex.lab=1.3)
#text(1.5,500,"*",cex=2)
boxplot(dat_tengua$time_went_to_bed[dat_tengua$sex == "1" & dat_tengua$mosquitonet == "0"],  
        dat_tengua$time_went_to_bed[dat_tengua$sex == "1" & dat_tengua$mosquitonet == "1"], 
        dat_tengua$time_went_to_bed[dat_tengua$sex == "2" & dat_tengua$mosquitonet == "0"], 
        dat_tengua$time_went_to_bed[dat_tengua$sex == "2" & dat_tengua$mosquitonet == "1"], 
        ylab="Time to bed (minutes since 16:00 hrs)",xaxt="n",yaxt="n",
        xlab="Sex",col=rep(c("aquamarine","aquamarine3"),2),cex.lab=1.3,ylim=c(0,500))

axis(1,at=c(1.5,3.5),labels=c("Male","Female"),cex.axis=1.3)
axis(2,las=2,at=seq(0,500,100),labels=seq(0,500,100),cex.axis=1.3)

text(2.5,0,"Tengua",cex=1.2)
legend(0.5,500,legend=c("No net","Net"),col=c("aquamarine","aquamarine3"),cex=1.3,pch=15)
text(4.5,500,"D",cex=1.6)

par(mfrow=c(1,2))
boxplot(dat_tengua$time_went_to_bed[dat_tengua$sex == "1" & dat_tengua$age_grouped == "a_<21"],  
        dat_tengua$time_went_to_bed[dat_tengua$sex == "2" & dat_tengua$age_grouped == "a_<21"], 
        dat_tengua$time_went_to_bed[dat_tengua$sex == "1" & dat_tengua$age_grouped == "b_21-30"], 
        dat_tengua$time_went_to_bed[dat_tengua$sex == "2" & dat_tengua$age_grouped == "b_21-30"], 
        dat_tengua$time_went_to_bed[dat_tengua$sex == "1" & dat_tengua$age_grouped == "c_31-40"], 
        dat_tengua$time_went_to_bed[dat_tengua$sex == "2" & dat_tengua$age_grouped == "c_31-40"], 
        dat_tengua$time_went_to_bed[dat_tengua$sex == "1" & dat_tengua$age_grouped == "d_>40"], 
        dat_tengua$time_went_to_bed[dat_tengua$sex == "2" & dat_tengua$age_grouped == "d_>40"], 
        ylab="Time to bed (minutes since 16:00)",xaxt="n",yaxt="n",
        xlab="Age group",col=rep(c("aquamarine","aquamarine3"),4),cex.lab=1.3,ylim=c(0,500))

axis(1,at=c(1.5,3.5,5.5,7.5),labels=c("Under 21","21 - 30","31 - 40","Over 40"),cex.axis=1.3)
axis(2,las=2,at=seq(0,500,100),labels=seq(0,500,100),cex.axis=1.3)

text(4.5,0,"Tengua",cex=1.2)
legend(0.5,500,legend=c("Male","Female"),col=c("aquamarine","aquamarine3"),cex=1.3,pch=15)
text(8.5,500,"A",cex=1.6)


boxplot(dat_milange$time_to_rise[dat_milange$week_evening2 == "1" & dat_milange$age_grouped == "a_<21"],  
        dat_milange$time_to_rise[dat_milange$week_evening2 == "2" & dat_milange$age_grouped == "a_<21"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "3" & dat_milange$age_grouped == "a_<21"],  
        dat_milange$time_to_rise[dat_milange$week_evening2 == "4" & dat_milange$age_grouped == "a_<21"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "5" & dat_milange$age_grouped == "a_<21"],  
        dat_milange$time_to_rise[dat_milange$week_evening2 == "6" & dat_milange$age_grouped == "a_<21"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "7" & dat_milange$age_grouped == "a_<21"], 
        
        dat_milange$time_to_rise[dat_milange$week_evening2 == "1" & dat_milange$age_grouped == "b_21-30"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "2" & dat_milange$age_grouped == "b_21-30"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "3" & dat_milange$age_grouped == "b_21-30"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "4" & dat_milange$age_grouped == "b_21-30"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "5" & dat_milange$age_grouped == "b_21-30"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "6" & dat_milange$age_grouped == "b_21-30"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "7" & dat_milange$age_grouped == "b_21-30"], 
        
        dat_milange$time_to_rise[dat_milange$week_evening2 == "1" & dat_milange$age_grouped == "c_31-40"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "2" & dat_milange$age_grouped == "c_31-40"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "3" & dat_milange$age_grouped == "c_31-40"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "4" & dat_milange$age_grouped == "c_31-40"],         
        dat_milange$time_to_rise[dat_milange$week_evening2 == "5" & dat_milange$age_grouped == "c_31-40"],         
        dat_milange$time_to_rise[dat_milange$week_evening2 == "6" & dat_milange$age_grouped == "c_31-40"], 
        dat_milange$time_to_rise[dat_milange$week_evening2 == "7" & dat_milange$age_grouped == "c_31-40"], 
        
        ylab="Time to bed (minutes since 16:00)",xaxt="n",yaxt="n",
        xlab="Days of the week (Morning)",col=rep(c("grey","lightblue","blue"),each=7),cex.lab=1.3,ylim=c(600,1000))

axis(1,at=1:21,labels=c("Tuesday","","","","","","Monday","","","","","","Sunday","","","","","","Saturday","",""),srt=0.45, cex.axis=1.3)
axis(2,las=2,at=seq(600,1000,100),labels=seq(600,1000,100),cex.axis=1.3)

text(11,600,"Milange",cex=1.2)
legend(1,710,legend=c("Under 21","21 to 30","31 to 40"),col=c("grey","lightblue","blue"),cex=1.3,pch=15)
text(21.5,1000,"B",cex=1.6)



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

