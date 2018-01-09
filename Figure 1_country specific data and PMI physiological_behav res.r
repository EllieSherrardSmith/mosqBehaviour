## Figure 1

library(rworldmap)
library(ggmap)
library(rworldxtra)
library(GISTools)
library(mapplots)
library(adegenet)
library(lme4)
library(sjPlot)

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(c(x))))

par(mfrow=c(3,1))
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



legend(-45,-5,legend=c("Mosquito biting behaviour",
                       "Human indoor behaviour",
                       "Human bed behaviour",
                       "PMI mosquito data"),
       col=c("purple","aquamarine3","orange",transp("blue")),
       pch=c(22,20,17,15),cex=1.2,bty="n")
text(95,40,"A",cex=2)



coun = read.table("H:/Ellie/IRS and resistance/behaviour_paper/PMI/COUNTRY_PHI.txt",header=TRUE)
head(coun)



par(mar=c(10,5,1,2))
boxplot(coun$phi_I ~ coun$Country, ylim=c(0,1.1), frame=FALSE,
        cex.lab=1.5,cex.axis=1.6,yaxt="n",xaxt="n",
        col=transp("darkred",0.5),ylab="Proportion of mosquitoes biting")
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=1.6,cex.axis=1.6)
axis(1,las=2,at=1:11,labels=unique(levels(coun$Country)),cex.lab=1.6,cex.axis=1.6)
numI = c(as.numeric(c(summary(coun$Country))))
num=1:11
for(i in 1:11){
  text(num[i],0.02,numI[i])
}
text(11.5,1.1,"B",cex=2)
par(new=TRUE)
boxplot(coun$phi_B ~ coun$Country, ylim=c(0,1), frame=FALSE,
        cex.lab=1.6,cex.axis=1.6,yaxt="n",xaxt="n",
        col=transp("darkblue",0.5),ylab="")
legend(8.8,0.45,bty="n",
       legend = c(expression(phi[I]),
                  expression(phi[B])),
       col=transp(c("darkred","darkblue"),0.4),pch=15,cex=2)

par(mar=c(5,5,2,2))
coun$x=(1 - coun$deltamethrin)
plot(coun[,6] ~ coun$x, pch="",bty="n", cex.lab = 1.6, cex.axis = 1.6,
     ylab=expression(paste("Behavioural resistance, ", phi[I])),
     xlab="Pyrethroid resistance test (% survival)",
     ylim=c(0,1),xlim=c(0,1),yaxt="n")
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=1.6,cex.axis=1.6)
text(1,1,"C",cex=2)
vec_pch = 1:11
for(i in 1:11){
  points(coun[,6][coun$Country == levels(coun$Country)[i]] ~ 
           coun$x[coun$Country == levels(coun$Country)[i]],pch=vec_pch[i])
}
legend(0.15,0.4,
       legend=c(names(summary(coun$Country))),ncol=3,
       pch=vec_pch,cex=1.2)

