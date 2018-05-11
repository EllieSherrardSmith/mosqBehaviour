##########################################
##
## Figure 1: Summary of data
##
########################################

#################
##
## FUNCTIONS
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

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(c(x))))


###########################
##
## PACKAGES
## 
library(rworldmap)
library(ggmap)
library(rworldxtra)
library(GISTools)
library(mapplots)
library(adegenet)
library(lme4)
library(sjPlot)
library(R.devices)

###########################
##
##
## DATA
dat_mosq = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI_phiB_rawdata.csv",header=TRUE)
dat_indoor = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_indoor_vs_time.csv",header=TRUE)
#dat_inbed = read.csv("C:\\Users\\esherrar\\Documents\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_sleeping_vs_time(1).csv",header=TRUE)
dat_inbed = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Andrew Beale\\Human_sleeping_vs_time_Beale_data_added.csv",header=TRUE)

options("devEval/args/path"=file.path("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Evolution_version 1"))
devEval("tiff", name="test1", width=850, height=950, {
  
  par(mfrow=c(3,2))
  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = c(-15, 50), ylim = c(-35, 40), asp = 1,col="grey95", border="grey65")
  points(36.88,-7.87,col="orange",cex=2,pch=17) ## Killeen et al 2006 ##**HUMAN BED BEHAVIOUR
  points(2.116,6.35,col="orange",cex=2, pch=17) ## Moiroux et al 2014 (in Moiroux et al 2012 too) ## HUMAN INDOOR BEHAVIOUR
  points(2.09,6.26,col="orange",cex=2, pch=17) ## Moiroux et al 2014 (in Moiroux et al 2012 too)  ## HUMAN INDOOR BEHAVIOUR
  points(39.23,-6.81,col="orange",cex=2,pch=17) ## Geissbuhler et al. 1997 ##**HUMAN BED BEHAVIOUR ##**HUMAN INDOOR BEHAVIOUR 
  points(36.88,-16.87,col="orange",cex=2,pch=17) ## Beale et al. new data ##**HUMAN BED BEHAVIOUR
  
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
  
  vec_x=c(2.005,1.264,0.996,1.380651,1.692195,1.549544,##Benin PMI
          23.589103,25.140485,26.953296,15.410600,22.317932,##DRC PMI
          39.979419,36.723155,##Ethiopia PMI
          -0.1,-0.466667,-1.066667,-0.840453,##Ghana PMI
          -9.560459,-10.383276,-10.567113,##Liberia PMI
          49.084282,49.049908,49.443827,47.376351,49.964194,47.245355,49.198794,47.245540,45.314492,47.806564,47.671266,##Madagascar PMI
          -7.935552,-8.076123,-5.739164,-6.246983,-6.829816,##Mali PMI
          36.976931,35.902722,37.735561,##Mozambique PMI
          8.357755,8.536976,7.537840,9.329697,7.045966,5.223733,3.336108,##Nigeria PMI
          30.134943,29.842858,30.673796,30.519516,30.326772,##Rwanda PMI
          32.668517)##Zimbabwe PMI
  vec_y=c(10.231,10.620,10.729,10.3058,10.333264,9.836188,
          -3.521728,-5.312402,-10.94137,-4.408671,-6.024696,##DRC PMI
          7.011792,7.601335,##Ethiopia PMI
          10.516667,9.4,9.43333,9.408361,##Ghana PMI
          7.013486,6.890101,6.417365,##Liberia PMI
          -18.816232,-17.940711,-17.470061,-20.241440,-13.391624,-20.533213,-17.469472,-21.204418,-24.227366,-22.913950,-23.198850,##Madagascar PMI
          13.226419,12.752416,12.931088,13.428632,13.072381,##Mali PMI
          -16.850862,-17.051956,-17.158683,##Mozambique PMI
          8.398323,8.691820,6.441960,9.157546,5.017755,13.035559,6.524784,##Nigeria PMI
          -2.247634,-2.588116,-2.283779,-2.175292,-1.293251,##Rwanda PMI
          -19.207350)##Zimbabwe PMI
  for(i in 1:length(vec_x)){
    points(vec_x[i],vec_y[i],col=transp("blue",0.4),cex=1.2, pch=15)## PMI
  }
  
  
  
#  legend(-55,-5,legend=c("Mosquito biting behaviour",
#                         "Human indoor behaviour",
#                         "Human bed behaviour",
#                         "PMI mosquito data"),
#         col=c("purple","aquamarine3","orange",transp("blue")),
#         pch=c(22,20,17,15),cex=1.2,bty="n")
  text(70,38,"A",cex=2)
  
  
  
  ###row 1 col 2
  activity_mosqfun(species =  "A_funestus",
                   title = "An funestus",xlabs = "")
  text(24,0.4,"D",cex=2)
  
  
  ###row 2 col 1
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
  
  
  ###row 2 col 2
  activity_mosqfun(species =  "A_gambiae",
                   title = "An gambiae",xlabs = "")
  text(24,0.4,"E",cex=2)
  
  
  ###row 3 col 1
  plot(dat_inbed[,3]~dat_inbed[1:24,1],pch="",bty="n",ylab="Proportion of people in bed",
       xlab="Time (hours)",cex.lab=1.6,ylim=c(0,1),xaxt="n",las=2,cex.axis=1.6)
  axis(1,las=0, at=seq(1,24,2),labels=c(seq(16,24,2),seq(2,15,2)),cex.axis=1.6,cex=1.6)
  
  #for(i in 3:736){
  #  lines(dat_inbed[1:24,i]~dat_inbed[1:24,1],col=transp("blue",0.3),lwd=2)##Tanzania
  #}
  
  lines(dat_inbed[1:24,735]~dat_inbed[1:24,1],col="orange",lwd=2)##Tanzania
  lines(dat_inbed[1:24,736]~dat_inbed[1:24,1],col="orange",lwd=2)##Tanzania
  lines(dat_inbed[1:24,739]~dat_inbed[1:24,1],col=transp("blue",0.5),lwd=2)##Mozambique_Beale average data
  
  lines(dat_inbed[1:24,738]~dat_inbed[1:24,1],lty=2,lwd=3,col="black")##Average Tanzania
  #lines(dat_inbed[,739]~dat_inbed[,1],lty=2,lwd=3,col="blue")##Average Mozambique
  #polygon(c(dat_inbed[,1],rev(dat_inbed[,1])),c(dat_inbed[,740],rev(dat_inbed[,741])),border=NA,col=transp("blue",0.3))
  text(24,1,"C",cex=2)
#  legend(16.2,0.9,legend=c("Tanzania",
#                           "Burkina Faso",
#                           "Zambia",
#                           "Kenya",
#                           "Benin",
#                           "Mozambique"),
#         lty=1,lwd=2,cex=1.4,bty = "n",
#         col=c("orange","purple","blue","aquamarine3","darkred",transp("blue",0.5)))
  
  
  ###row 3 col 2
  activity_mosqfun(species =  "A_arabiensis",
                   title = "An arabiensis",xlabs = "Time (hours)")
  text(24,0.4,"F",cex=2)
  legend(15,0.35,legend = c("Indoors",
                            "Outdoors"),
         pch=15,lwd=2,cex=1.4,bty = "n",lty=c(1,2),
         col=transp(c("blue","black")))
  
})


##########################################
## At what hour are 50% of the population:

##Indoors at night
y1 = 0.402854545 ##8pm
y2 = 0.707718182 ##9pm (these are the two hours sandwiching 50% of the people)
gradient_indoor = dat_indoor[6,14] - dat_indoor[5,14]
intercept_indoor = 0.402854545 - gradient_indoor
(0.5 - intercept_indoor)/gradient_indoor ##7pm + this much time i.e. 60 mins * this... 20:19pm

##In bed at night
y1 = 0.4160733 ##9pm
y2 = 0.6486785 ##10pm (these are the two hours sandwiching 50% of the people)
gradient_indoor = dat_inbed[7,738] - dat_inbed[6,738]
intercept_indoor = 0.4160733 - gradient_indoor
(0.5 - intercept_indoor)/gradient_indoor ##7pm + this much time i.e. 60 mins * this... 20:19pm


##Outdoors in the morning
y1 = 0.809327273 ##5am
y2 = 0.203127273 ##6am
gradient_indoor = dat_indoor[15,14] - dat_indoor[14,14] 
intercept_indoor = 0.809327273 - gradient_indoor
(0.5 - intercept_indoor)/gradient_indoor ##7pm + this much time i.e. 60 mins * this... 05:30pm
