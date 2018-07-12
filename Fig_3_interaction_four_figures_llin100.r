counts = 1:126
sites = seq(1,126,21)
data1_no_res = read.table(paste("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1",sites[1],"0.txt",sep="_"),header=TRUE)
head(data1_no_res)


clinical_Cases_no_intervention = array(dim=c(1301,21,6))#Number of time points : resistance seq(0,1,0.05) : phi c(0.4,0.5964,0.814,0.8909,0.94,0.9832) 
clinical_Cases_LLIN100 = clinical_Cases_LLIN80 = 
  clinical_Cases_LLIN80_Pirs80 = clinical_Cases_LLIN80_Airs80 = array(dim=c(1301,21,6))

for(i in 1:21){
  clinical_Cases_no_intervention[,i,4]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i,4]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i,4]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i,4] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i,4] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
}
for(i in 22:42){
  clinical_Cases_no_intervention[,i-21,2]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-21,2]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-21,2]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-21,2] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-21,2] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
}
for(i in 43:63){
  clinical_Cases_no_intervention[,i-42,3]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-42,3]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-42,3]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-42,3] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-42,3] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
}
for(i in 64:84){
  clinical_Cases_no_intervention[,i-63,5]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-63,5]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-63,5]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-63,5] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-63,5] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
}
for(i in 85:105){
  clinical_Cases_no_intervention[,i-84,6]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-84,6]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-84,6]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-84,6] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-84,6] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
}
for(i in 106:126){
  clinical_Cases_no_intervention[,i-105,1]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-105,1]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-105,1]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-105,1] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-105,1] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
}

reduction_next3years_LLIN100 = reduction_next3years_LLIN80 = reduction_next3years_LLIN80_pyr80 = reduction_next3years_LLIN80_act80 = array(dim=c(52*3,21,6) )

for(i in 1:6){
    reduction_next3years_LLIN100[,,i]       = (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN100[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]
    reduction_next3years_LLIN80[,,i]       =  (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN80[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]
    reduction_next3years_LLIN80_pyr80[,,i] =  (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN80_Pirs80[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]
    reduction_next3years_LLIN80_act80[,,i] = (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN80_Airs80[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]
}



mean_reduction_n3y_LLIN100 = mean_reduction_n3y_LLIN80 = mean_reduction_n3y_LLIN80_pyr80 = mean_reduction_n3y_LLIN80_act80 = array(dim=c(21,6))

for(i in 1:6){
  for(j in 1:21){
    mean_reduction_n3y_LLIN100[j,i] = mean(reduction_next3years_LLIN100[,j,i],na.rm=TRUE)
    mean_reduction_n3y_LLIN80[j,i] =  mean(reduction_next3years_LLIN80[,j,i],na.rm=TRUE)
    mean_reduction_n3y_LLIN80_pyr80[j,i] =  mean(reduction_next3years_LLIN80_pyr80[,j,i],na.rm=TRUE)
    mean_reduction_n3y_LLIN80_act80[j,i] =  mean(reduction_next3years_LLIN80_act80[,j,i],na.rm=TRUE)
  }
}


zf1 = mean_reduction_n3y_LLIN100 * 100
zf2 = mean_reduction_n3y_LLIN80 * 100
zf3 = mean_reduction_n3y_LLIN80_pyr80 * 100
zf4 = mean_reduction_n3y_LLIN80_act80 * 100


x = rep(seq(0,1,by=0.05),5)
y = rep(c(0.4,0.5964,0.814,0.8909,0.94,0.9832),each=21)

xcoords = unique(x)
ycoords = unique(y)

surface.mat1 = matrix(zf1,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat2 = matrix(zf2,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat3 = matrix(zf3,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat4 = matrix(zf4,ncol=length(ycoords),nrow=length(xcoords),byrow=F)


matrix_funs = function(surface.matrix,minimum_val,maximum_val,upps,uni,levs,colschoice,cols_conts){
  filled.contour3(xcoords,
                  ycoords,
                  surface.matrix,
                  color=colschoice,
                  plot.axes = { axis(2, at = seq(0.4, 1, by = 0.1), seq(0.4, 1, by = 0.1),las=2)
                    axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, 100, by = 20)) },
                  xlim = c(min(xcoords),max(xcoords)),
                  ylim = c(min(ycoords),max(ycoords)),
                  zlim = c(minimum_val,maximum_val))
  
  # the contour part - draw iso-lines
  contour(xcoords,
          ycoords,
          surface.matrix,
          color=blue2green,
          xlab = "",
          ylab = "",
          nlevels = levs, levels = seq(0,upps,by=uni),
          xlim = c(min(xcoords),max(xcoords)),
          ylim = c(min(ycoords),max(ycoords)),
          zlim = c(minimum_val,maximum_val),
          add=TRUE,                 # add the contour plot to filled-contour,
          #thus making an overlay
          col = cols_conts         # color of overlay-lines
  )
}
################################
##
## Figure 5
plot.new()
par(mar=c(4,5.5,2,2))
# major tick size and direction, < 0 means outside

############################################################
##
##PANEL A efficacy against clinical cases vs indoor biting
##
##
############################################################

##Top left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.1,0.45,0.55,0.95)  )                 # major tick size and direction, < 0 means outside


plot(zf1[1,] ~ behavioural_resistance,col="blue",lwd=2,
     ylab="",yaxt="n",xaxt="n",bty="n",
     xlab=expression(paste("Proportion of mosquito bites taken indoors, ", phi[I])),line=2.3,
     pch="",ylim=c(0,1),xlim=c(0,1),cex.lab=1)
axis(1,at=seq(0,1,0.2),labels=seq(0,1,0.20),cex.lab=1)
mtext(side=2,line = 3,las=0,cex=1, "Efficacy against clinical cases in children under 5 (%)")
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1)
for(i in 1){lines(sort(mean_reduction_n3y_LLIN100[i,]) ~ ycoords,col="darkblue",lwd=2)}##0% phys reistance
for(i in 11){lines(sort(mean_reduction_n3y_LLIN100[i,]) ~ ycoords,lty=2,col="darkblue",lwd=2)}##50%
for(i in 21){lines(sort(mean_reduction_n3y_LLIN100[i,]) ~ ycoords,lty=3,col="darkblue",lwd=2)}##100%

for(i in 1){lines(sort(mean_reduction_n3y_LLIN80[i,]) ~ ycoords,col="blue",lwd=2)}##0% phys reistance
for(i in 11){lines(sort(mean_reduction_n3y_LLIN80[i,]) ~ ycoords,lty=2,col="blue",lwd=2)}##50%
for(i in 21){lines(sort(mean_reduction_n3y_LLIN80[i,]) ~ ycoords,lty=3,col="blue",lwd=2)}##100%

for(i in 1){lines(sort(mean_reduction_n3y_LLIN80_pyr80[i,]) ~ ycoords,col="darkred",lwd=2)}##0% phys reistance
for(i in 11){lines(sort(mean_reduction_n3y_LLIN80_pyr80[i,]) ~ ycoords,lty=2,col="darkred",lwd=2)}##50%
for(i in 21){lines(sort(mean_reduction_n3y_LLIN80_pyr80[i,]) ~ ycoords,lty=3,col="darkred",lwd=2)}##100%

for(i in 1){lines(sort(mean_reduction_n3y_LLIN80_act80[i,]) ~ ycoords,col="red",lwd=2)}##0% phys reistance
for(i in 11){lines(sort(mean_reduction_n3y_LLIN80_act80[i,]) ~ ycoords,lty=2,col="red",lwd=2)}##50%
for(i in 21){lines(sort(mean_reduction_n3y_LLIN80_act80[i,]) ~ ycoords,lty=3,col="red",lwd=2)}##100%
text(0.95,1,"A",cex=1.2)

legend(0.01,0.35,title = "Survival at bioassay",
       legend = c("0%","50%","100%"),
       lty=c(1,2,3),lwd=2,cex=1,bty="n",col="blue")
legend(0.01,1,title = "Intervention",
       legend = c("80% net + pyrethroid IRS","80% net + long-lasting IRS",
                  "100% net cover","80% net cover"),
       lty=1,lwd=2,cex=1,bty="n",col=c("darkred","red","darkblue","blue"),ncol=1)

############################################################
##
##PANEL B efficacy against clinical cases vs pyrethroid resistance
##
##
############################################################

##Top right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.55,0.93,0.55,0.95)  )                 # major tick size and direction, < 0 means outside

##Efficacy plot
plot(mean_reduction_n3y_LLIN100[,1] ~ xcoords,ylim=c(0,1),lty=1,pch="",line=2.3,
     yaxt="n",ylab="",frame=FALSE,
     xaxt="n",xlab="Mosquito survival at bioassay (%)",cex.lab=1,cex.axis=1)
axis(1,at=seq(0,1,by=0.2),labels=seq(0,100,by=20),cex.lab=1,cex.axis=1)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1,cex.axis=1)
mtext(side=2,line = 3,las=0,cex=1,  "Efficacy against clinical cases in children under 5 (%)")

lines(mean_reduction_n3y_LLIN100[,1] ~ xcoords,lty=3,lwd=2,col="darkblue")
lines(mean_reduction_n3y_LLIN80[,1] ~ xcoords,lty=3,lwd=2,col="blue")
lines(mean_reduction_n3y_LLIN80_pyr80[,1] ~ xcoords,lty=3,lwd=2,col="darkred")
lines(mean_reduction_n3y_LLIN80_act80[,1] ~ xcoords,lty=3,lwd=2,col="red")

lines(mean_reduction_n3y_LLIN100[,4] ~ xcoords,lty=1,lwd=2,col="darkblue")
lines(mean_reduction_n3y_LLIN80[,4] ~ xcoords,lty=1,lwd=2,col="blue")
lines(mean_reduction_n3y_LLIN80_pyr80[,4] ~ xcoords,lty=1,lwd=2,col="darkred")
lines(mean_reduction_n3y_LLIN80_act80[,4] ~ xcoords,lty=1,lwd=2,col="red")
#
lines(mean_reduction_n3y_LLIN100[,6] ~ xcoords,lty=2,lwd=2,col="darkblue")
lines(mean_reduction_n3y_LLIN80[,6] ~ xcoords,lty=2,lwd=2,col="blue")
lines(mean_reduction_n3y_LLIN80_pyr80[,6] ~ xcoords,lty=2,lwd=2,col="darkred")
lines(mean_reduction_n3y_LLIN80_act80[,6] ~ xcoords,lty=2,lwd=2,col="red")

legend(0.01,0.35,title = "Degree of indoor biting",
       legend = c("High (0.98)","Medium (0.89)","Low (0.4)"),
       lty=c(1,2,3),lwd=2,cex=1,bty="n",col="blue",ncol=1)

text(1,1,"B",cex=1.2)

############################################################
##
##PANEL C biting behaviour vs pyrethroid resistance
##
##
############################################################

##Top right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.1,0.45,0.1,0.45)  )                 # major tick size and direction, < 0 means outside

##Perennial setting: LLIN at 50% cover No resistance

coun = read.table("H:/Ellie/IRS and resistance/behaviour_paper/PMI/COUNTRY_PHI.txt",header=TRUE)
head(coun)
coun$x=(1 - coun$deltamethrin)
plot(coun[,6] ~ coun$x, bty="n", cex.lab = 1, cex.axis = 1,line=2.3,
     ylab="",
     xlab="Mosquito survival at bioassay (%)",pch="",
     ylim=c(0,1),xlim=c(0,1),yaxt="n",xaxt="n")
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,1,0.2),cex.lab=1,cex.axis=1)
axis(1,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1,cex.axis=1)
mtext(side=2,line=3,las=0,cex=1,expression(paste("Proportion of mosquito bites taken indoors, ", phi[I])))
vec_pch = 1:11
for(i in 1:11){
  points(coun[,6][coun$Country == levels(coun$Country)[i]] ~ 
           coun$x[coun$Country == levels(coun$Country)[i]],pch=vec_pch[i])
}
legend(0.15,0.3,
       legend=c(names(summary(coun$Country))),ncol=3,
       pch=vec_pch,cex=1)
text(0.95,1,"C",cex=1.2)

############################################################
##
##PANEL D Matrix plot for biting behaviour vs pyrethroid resistance
##
##
############################################################


##mid left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.55,0.9,0.1,0.45)  )                 # major tick size and direction, < 0 means outside


matrix_funs(surface.mat1,min(surface.mat1),max(surface.mat1),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"D",cex=1.2,col="darkblue")

######################################################################
#Add a legend:
par(new = "TRUE",
    plt = c(0.93,0.95,0.18,0.4),   # define plot region for legend
    las = 1,
    cex.axis = 1)
#
filled.legend(
  xcoords,
  ycoords,
  surface.mat1,
  color = heat.colors,
  plot.title = "Efficacy (%)",
  xlab = "",
  ylab = "",
  xlim = c(0,100),
  ylim = c(0.4,1),
  zlim = c(min(surface.mat1),max(surface.mat1)))

#Add some figure labels
par(xpd=NA,cex = 1.1)
text(x = -21.6,y = 34,expression(paste("Proportion of mosquito bites taken indoors, ", phi[I])),srt = 90,cex = 0.9)
text(x = -10,y = -26,"Mosquito survival at bioassay (%)",cex = 0.9)

text(x = -16,y = 75,"100% LLIN cover",cex = 0.9)
text(x = -10,y = 82.8,"Efficacy against clinical cases in children under 5 (%)",cex = 0.9)

###############################
##
## Supplement figure 2 X 3
###############################
##
## Supplement showing the impact in other scenarios and for prevalence and at high transmission

##First work out same for prevalence
counts = 1:126
sites = seq(1,126,21)
data1_no_res = read.table(paste("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1",sites[1],"0.txt",sep="_"),header=TRUE)
data2_no_res = read.table(paste("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None2",sites[1],"0.txt",sep="_"),header=TRUE)
head(data1_no_res)


prev_no_intervention = prev_no_interventionHIGH = clinical_Cases_no_interventionHIGH = array(dim=c(1301,21,6))#Number of time points : resistance seq(0,1,0.05) : phi c(0.4,0.5964,0.814,0.8909,0.94,0.9832) 
prev_LLIN100 = prev_LLIN80 = prev_LLIN50 = clinical_Cases_LLIN50 = 
  prev_LLIN80_Pirs80 = prev_LLIN80_Airs80 = 
  prev_LLIN100HIGH = clinical_Cases_LLIN100HIGH = array(dim=c(1301,21,6))

for(i in 1:21){
  clinical_Cases_no_intervention[,i,4]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_no_interventionHIGH[,i,4]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i,4]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100HIGH[,i,4]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i,4]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN50[,i,4]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i,4] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i,4] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5

  prev_no_intervention[,i,4]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_no_interventionHIGH[,i,4]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100[,i,4]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100HIGH[,i,4]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80[,i,4]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN50[,i,4]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Pirs80[,i,4] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Airs80[,i,4] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  
}
for(i in 22:42){
  clinical_Cases_no_intervention[,i-21,2]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_no_interventionHIGH[,i-21,2]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-21,2]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100HIGH[,i-21,2]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-21,2]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN50[,i-21,2]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-21,2] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-21,2] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
  prev_no_intervention[,i-21,2]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_no_interventionHIGH[,i-21,2]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100[,i-21,2]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100HIGH[,i-21,2]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80[,i-21,2]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN50[,i-21,2]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Pirs80[,i-21,2] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Airs80[,i-21,2] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  
}
for(i in 43:63){
  clinical_Cases_no_intervention[,i-42,3]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_no_interventionHIGH[,i-42,3]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-42,3]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100HIGH[,i-42,3]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-42,3]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN50[,i-42,3]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-42,3] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-42,3] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
  prev_no_intervention[,i-42,3]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_no_interventionHIGH[,i-42,3]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100[,i-42,3]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100HIGH[,i-42,3]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80[,i-42,3]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN50[,i-42,3]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Pirs80[,i-42,3] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Airs80[,i-42,3] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  
}
for(i in 64:84){
  clinical_Cases_no_intervention[,i-63,5]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_no_interventionHIGH[,i-63,5]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-63,5]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100HIGH[,i-63,5]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-63,5]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN50[,i-63,5]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-63,5] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-63,5] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
  prev_no_intervention[,i-63,5]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_no_interventionHIGH[,i-63,5]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100[,i-63,5]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100HIGH[,i-63,5]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80[,i-63,5]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN50[,i-63,5]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Pirs80[,i-63,5] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Airs80[,i-63,5] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
}
for(i in 85:105){
  clinical_Cases_no_intervention[,i-84,6]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_no_interventionHIGH[,i-84,6]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-84,6]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100HIGH[,i-84,6]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-84,6]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN50[,i-84,6]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-84,6] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-84,6] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
  prev_no_intervention[,i-84,6]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_no_interventionHIGH[,i-84,6]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100[,i-84,6]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100HIGH[,i-84,6]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80[,i-84,6]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN50[,i-84,6]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Pirs80[,i-84,6] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Airs80[,i-84,6] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  
}
for(i in 106:126){
  clinical_Cases_no_intervention[,i-105,1]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_no_interventionHIGH[,i-105,1]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100[,i-105,1]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN100HIGH[,i-105,1]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80[,i-105,1]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN50[,i-105,1]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Pirs80[,i-105,1] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  clinical_Cases_LLIN80_Airs80[,i-105,1] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$clin_inc_0_5
  
  prev_no_intervention[,i-105,1]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_no_interventionHIGH[,i-105,1]=read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100[,i-105,1]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100/draw_0/behav_phys_resistance_LLIN100_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN100HIGH[,i-105,1]       = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN100HIGH/draw_0/behav_phys_resistance_LLIN100HIGH_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80[,i-105,1]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80/draw_0/behav_phys_resistance_LLIN80_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN50[,i-105,1]        = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN50/draw_0/behav_phys_resistance_LLIN50_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Pirs80[,i-105,1] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80pyr/draw_0/behav_phys_resistance_LLIN80_irs80pyr_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
  prev_LLIN80_Airs80[,i-105,1] = read.table(paste0("K:/Ellies_output_folder/behav_phys_resistance_LLIN80_irs80act/draw_0/behav_phys_resistance_LLIN80_irs80act_",counts[i],'_0.txt'),header=TRUE)$prev_2_10
}

reduction_next3years_LLIN100 = reduction_next3years_LLIN80 = reduction_next3years_LLIN50 =
  reduction_next3years_LLIN80_pyr80 = reduction_next3years_LLIN80_act80 = 
  reduction_next3years_LLIN100high = array(dim=c(52*3,21,6) )
reductPREV_3years_LLIN100 = reductPREV_3years_LLIN80 = reductPREV_3years_LLIN50 = 
  reductPREV_3years_LLIN80_pyr80 = reductPREV_3years_LLIN80_act80 = 
  reductPREV_3years_LLIN100high = array(dim=c(21,6) )

for(i in 1:6){
  reduction_next3years_LLIN100[,,i]       = (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN100[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]
  reduction_next3years_LLIN100high[,,i]       = (clinical_Cases_no_interventionHIGH[262:417,,i] - clinical_Cases_LLIN100HIGH[262:417,,i])/clinical_Cases_no_interventionHIGH[262:417,,i]
  reduction_next3years_LLIN80[,,i]       =  (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN80[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]
  reduction_next3years_LLIN50[,,i]       =  (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN50[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]
  reduction_next3years_LLIN80_pyr80[,,i] =  (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN80_Pirs80[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]
  reduction_next3years_LLIN80_act80[,,i] = (clinical_Cases_no_intervention[262:417,,i] - clinical_Cases_LLIN80_Airs80[262:417,,i])/clinical_Cases_no_intervention[262:417,,i]

  reductPREV_3years_LLIN100[,i]       = (prev_no_intervention[418,,i] - prev_LLIN100[418,,i])/prev_no_intervention[418,,i]
  reductPREV_3years_LLIN80[,i]       = (prev_no_intervention[418,,i] - prev_LLIN80[418,,i])/prev_no_intervention[418,,i]
  reductPREV_3years_LLIN50[,i]       = (prev_no_intervention[418,,i] - prev_LLIN80[418,,i])/prev_no_intervention[418,,i]
  reductPREV_3years_LLIN80_pyr80[,i]       = (prev_no_intervention[418,,i] - prev_LLIN80_Pirs80[418,,i])/prev_no_intervention[418,,i]
  reductPREV_3years_LLIN80_act80[,i]       = (prev_no_intervention[418,,i] - prev_LLIN80_Airs80[418,,i])/prev_no_intervention[418,,i] 
  reductPREV_3years_LLIN100high[,i]       = (prev_no_interventionHIGH[418,,i] - prev_LLIN100HIGH[418,,i])/prev_no_interventionHIGH[418,,i] 
  }



mean_reduction_n3y_LLIN100 = mean_reduction_n3y_LLIN100high = mean_reduction_n3y_LLIN80 = mean_reduction_n3y_LLIN50 = mean_reduction_n3y_LLIN80_pyr80 = mean_reduction_n3y_LLIN80_act80 = array(dim=c(21,6))

for(i in 1:6){
  for(j in 1:21){
    mean_reduction_n3y_LLIN100[j,i] = mean(reduction_next3years_LLIN100[,j,i],na.rm=TRUE)
    mean_reduction_n3y_LLIN100high[j,i] = mean(reduction_next3years_LLIN100high[,j,i],na.rm=TRUE)
    mean_reduction_n3y_LLIN80[j,i] =  mean(reduction_next3years_LLIN80[,j,i],na.rm=TRUE)
    mean_reduction_n3y_LLIN50[j,i] =  mean(reduction_next3years_LLIN50[,j,i],na.rm=TRUE)
    mean_reduction_n3y_LLIN80_pyr80[j,i] =  mean(reduction_next3years_LLIN80_pyr80[,j,i],na.rm=TRUE)
    mean_reduction_n3y_LLIN80_act80[j,i] =  mean(reduction_next3years_LLIN80_act80[,j,i],na.rm=TRUE)
  }
}


zf1 = mean_reduction_n3y_LLIN100 * 100
zf2 = mean_reduction_n3y_LLIN80 * 100
zf3 = mean_reduction_n3y_LLIN80_pyr80 * 100
zf4 = mean_reduction_n3y_LLIN80_act80 * 100
zf5 = mean_reduction_n3y_LLIN50 * 100
zf6 = mean_reduction_n3y_LLIN100high * 100

z1 = reductPREV_3years_LLIN50 * 100
z2 = reductPREV_3years_LLIN80_act80 * 100
z3 = reductPREV_3years_LLIN100high * 100

x = rep(seq(0,1,by=0.05),5)
y = rep(c(0.4,0.5964,0.814,0.8909,0.94,0.9832),each=21)

xcoords = unique(x)
ycoords = unique(y)

##For supplement we want LLIN at 50%, LLIN 80% + 80% Actellic, and LLIN 100% with high transmission
surface.mat5 = matrix(zf5,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat5[4,4] = 50.8
surface.mat4 = matrix(zf4,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat6 = matrix(zf6,ncol=length(ycoords),nrow=length(xcoords),byrow=F)

surf.mat5 = matrix(z1,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surf.mat4 = matrix(z2,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surf.mat6 = matrix(z3,ncol=length(ycoords),nrow=length(xcoords),byrow=F)

plot.new()
par(mar=c(4,5.5,2,2))
# major tick size and direction, < 0 means outside

############################################################
##
##PANEL A efficacy against clinical cases vs indoor biting
##
##
############################################################

par(new = "TRUE",  
    plt = c(0.09,0.32,0.1,0.45)  )                 # major tick size and direction, < 0 means outside
matrix_funs(surface.mat5,min(surf.mat6),max(surf.mat4),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"D",cex=1.2,col="darkblue")

par(new = "TRUE",  
    plt = c(0.37,0.60,0.1,0.45)  )                 # major tick size and direction, < 0 means outside
matrix_funs(surface.mat4,min(surf.mat6),max(surf.mat4),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"E",cex=1.2,col="darkblue")

par(new = "TRUE",  
    plt = c(0.65,0.88,0.1,0.45)  )                 # major tick size and direction, < 0 means outside
matrix_funs(surface.mat6,min(surf.mat6),max(surf.mat4),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"F",cex=1.2,col="darkblue")

par(new = "TRUE",  
    plt = c(0.09,0.32,0.55,0.9)  )                 # major tick size and direction, < 0 means outside
matrix_funs(surf.mat5,min(surf.mat6),max(surf.mat4),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"A",cex=1.2,col="darkblue")

par(new = "TRUE",  
    plt = c(0.37,0.6,0.55,0.9)  )                 # major tick size and direction, < 0 means outside
matrix_funs(surf.mat4,min(surf.mat6),max(surf.mat4),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"B",cex=1.2,col="darkblue")

par(new = "TRUE",  
    plt = c(0.65,0.88,0.55,0.9)  )                 # major tick size and direction, < 0 means outside
matrix_funs(surf.mat6,min(surf.mat6),max(surf.mat4),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"C",cex=1.2,col="darkblue")

#Add a legend:
par(new = "TRUE",
    plt = c(0.93,0.95,0.18,0.82),   # define plot region for legend
    las = 1,
    cex.axis = 1)
#
filled.legend(
  xcoords,
  ycoords,
  surface.mat4,
  color = heat.colors,
  plot.title = "Efficacy (%)",
  xlab = "",
  ylab = "",
  xlim = c(0,100),
  ylim = c(0.5,1),
  zlim = c(min(surf.mat6),max(surf.mat4)))

#Add some figure labels
par(xpd=NA,cex = 1.1)
text(x = -45,y = 48,expression(paste("Proportion of mosquito bites taken indoors, ", phi[I])),srt = 90,cex = 0.9)
text(x = -21.8,y = -25,"Mosquito survival at bioassay (%)",cex = 0.9)

text(x = -36,y = 118,"50% LLIN cover",cex=0.9)
text(x = -22.2,y = 118,"80% LLIN cover + long-lasting IRS",cex = 0.9)
text(x = -8,y = 118,"100% LLIN cover",cex = .9)

text(x = -29,y = 123,"Moderate transmission (30% prevalence 2- 10 years)",cex = 0.9)
text(x = -8,y = 123,"High transmission (75% prevalence 2- 10 years)",cex = 0.9)

text(x = -46,y = 85,"Efficacy against prevalence in 2 - 10-yrs (%)",cex = 0.9,srt=90)

text(x = -46,y = 12,"Efficacy against clinical cases U5 (%)",cex = 0.9,srt=90)
