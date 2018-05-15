############################################
##
## Figure 4 (maps of percentage increase in clinical cases)
##
##

library(ggplot2)
library(gplots)
library(colorRamps)
library(adegenet)

##
## the test run
base_default = read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_258_0.txt'),header=TRUE)
base_5pcreds = read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_258_0.txt'),header=TRUE)

plot(base_default$clin_inc_all ~ base_default$year,ylim=c(0,max(base_5pcreds$clin_inc_all)))
lines(base_5pcreds$clin_inc_all ~ base_5pcreds$year,col="blue")
##
## the cases
site_temp = read.csv("F:\\Intervention_coverage.csv",header=TRUE)
site = c(site_temp[1:601,1])
pop_temp = read.csv("F:\\populations.csv",header=TRUE)

bas0 = bas5 = bas10 = array(dim=c(length(site),3))


percentage_increase_in_cases = array(dim=c(length(site),3))
percentage_increase_in_cases[,1] = site

absolute_increase_cases = array(dim=c(length(site),3))
absolute_increase_cases[,1] = site

for(i in 1:length(site)){
    bas0[i,]= c(read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$year == 1] * pop_temp$par_2017[pop_temp$DIDE_CODE == site[i]],
                read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$year == 2] * pop_temp$par_2018[pop_temp$DIDE_CODE == site[i]],
                read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$year == 3] * pop_temp$par_2019[pop_temp$DIDE_CODE == site[i]])
   
    bas5[i,]= c(read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$year == 1] * pop_temp$par_2017[pop_temp$DIDE_CODE == site[i]],
                read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$year == 2] * pop_temp$par_2018[pop_temp$DIDE_CODE == site[i]],
                read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$year == 3] * pop_temp$par_2019[pop_temp$DIDE_CODE == site[i]])
    
    bas10[i,]= c(read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$year == 1] * pop_temp$par_2017[pop_temp$DIDE_CODE == site[i]],
                 read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$year == 2] * pop_temp$par_2018[pop_temp$DIDE_CODE == site[i]],
                 read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$clin_inc_all[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$year == 3] * pop_temp$par_2019[pop_temp$DIDE_CODE == site[i]])
    
    
    percentage_increase_in_cases[i,2] =   mean(ifelse(bas0[i,1] > 0, c((bas5[i,1] - bas0[i,1])/bas0[i,1],(bas5[i,2] - bas0[i,2])/bas0[i,2], (bas5[i,3] - bas0[i,3])/bas0[i,3]),9999))
    percentage_increase_in_cases[i,3] =   mean(ifelse(bas0[i,1] > 0, c((bas10[i,1] - bas0[i,1])/bas0[i,1],(bas10[i,2] - bas0[i,2])/bas0[i,2], (bas10[i,3] - bas0[i,3])/bas0[i,3]),9999))
    
    absolute_increase_cases[i,2] =   sum(c((bas5[i,1] - bas0[i,1]),(bas5[i,2] - bas0[i,2]), (bas5[i,3] - bas0[i,3])))
    absolute_increase_cases[i,3] =   sum(c((bas10[i,1] - bas0[i,1]),(bas10[i,2] - bas0[i,2]), (bas10[i,3] - bas0[i,3])))
}

prev = array(dim=c(length(site),10))
prev[,1] = site

percentage_increase_in_prev=array(dim=c(length(site),7))
percentage_increase_in_prev[,1]= site

for(i in 1:length(site)){
  prev[i,2]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$year == 1]
  prev[i,3]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$year == 2]
  prev[i,4]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_', site[i], '_0.txt'),header=TRUE)$year == 3]
                                                                          
  prev[i,5]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$year == 1]
  prev[i,6]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$year == 2]
  prev[i,7]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_', site[i], '_0.txt'),header=TRUE)$year == 3]
  
  prev[i,8]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$year == 1]
  prev[i,9]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$year == 2]
  prev[i,10]= read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$prev_2_10[read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_10pc/draw_0/Behaviour_paper_phi_10pc_', site[i], '_0.txt'),header=TRUE)$year == 3]
  
} 

percentage_increase_in_prev[,2:4] =   100*c(prev[,5] - prev[,2],prev[,6] - prev[,3],prev[,7] - prev[,4])
percentage_increase_in_prev[,5:7] =   100*c(prev[,8] - prev[,2],prev[,9] - prev[,3],prev[,10] - prev[,4])  



################################
##
## Mapping
########
## Mapping

## Run everything in Set up instructions, eg 
#install.packages("drat")
#drat::addRepo("malaria", "file:///f:/drat") ##mapping malaria drive to F:/
#install.packages("MalariaMap")
## load the relevent libraries
library(adegenet)
library(MalariaMap)
library(latticeExtra)
library(sp)
require(maptools)

simple_map2= function (data, z, breaks = seq(0, 1, length.out = 11), labs, ...) 
{
  M1 <- subset_world(...)
  M2 <- sp::merge(M1, data, by = "DIDE_CODE")
  sp::spplot(M2[z], at = breaks, col.regions = brewer.pal(9,"YlOrRd"), labels=labs)#YlGnBu
}

simple_map3= function (data, data_U, z, labs, seq1, seq2, seq3,  seq4,  seq5,  seq6,  seq7,  seq8,  seq9,...) 
{
  M1 <- subset_world(level = "Admin", Continent = "Africa")
  M2 <- sp::merge(M1, data, by = "DIDE_CODE")
  
  M3 = subset_world(level = "Country", Continent = "Africa")
  M4 <- sp::merge(M3, data_U, by = "ISO")
  
  b = spplot(M4[z], at = seq(seq1, seq9, length.out = 9),fill=FALSE, col="black",lwd=1.5,names.attr = labs,
             main = "Percentage increase in prevalence (Year 3)")
  spplot(M2[z], names.attr = labs, main = "Percentage increase in prevalence (Year 3)", at = c(seq1, seq2, seq3,  seq4,  seq5,  seq6,  seq7,  seq8,  seq9), col="grey",col.regions = brewer.pal(9,"YlOrRd"),
         colorkey =  list(space = "right", height = 0.8)) + b
  
  
}


populations = read.csv("F:/populations.csv",header=TRUE)

head(populations)
trRes = expand.grid(col1=c(1:601))
for(i in 1:601){
  trRes[i,1] = percentage_increase_in_cases[i,2] ##  percentage increase in cases with 5% more outdoor biting
  trRes[i,2] = percentage_increase_in_cases[i,3] ##  percentage increase in cases with 10% more outdoor biting
}
trRes[,1] = ifelse(trRes[,1] > 1,1.1,trRes[,1])
trRes[,2] = ifelse(trRes[,2] > 1,1.1,trRes[,2])

input=read.csv("F:/Copy of Intervention_coverage.csv",header=TRUE)
trRes[,3] = input$ISO
trRes[,4] = input$DIDE_CODE

colnames(trRes) = c("percentage_5",
                    "percentage_10",
                    "ISO","DIDE_CODE")
trRes[ trRes == 9999 ] <- 1.1

##Work out how best to split the data
#trRes2 = trRes[apply(trRes, 1, function(x) all(is.finite(x))), ]
quantile(c(trRes[,1],trRes[,2]),c(0.00001,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.9999),na.rm=TRUE)

#trRes_RED5 <- trRes[!is.infinite(trRes[,1]),]
trRes[,1] = trRes[,1]*100
trRes[,2] = trRes[,2]*100
trRes_U =  trRes[!duplicated(trRes[,c('ISO')]),]

require(gridExtra)

plot.new()
par(new = "TRUE",
    plt = c(0.1,0.9,0.1,0.3),
    las = 1,
    cex.axis = 1)

obj1 = simple_map3(data=trRes, data_U = trRes_U, 
                   z=c("percentage_5",
                       "percentage_10"),labs = c("5% increase in outdoor biting","10% increase in outdoor biting"),
                   seq1=0, seq2=0, seq3=4.7,  seq4=10.8,  seq5=19.05,
                   seq6=33.8,  seq7=45.6, seq8=54.4,  seq9 = 110.1,
                   names.attr = c("5% increase in outdoor biting","10% increase in outdoor biting"),as.table = TRUE,
                   main = "Percentage increase in cases per person per year") ##Fig 3 panel G 
obj1

#simple_map2(trRes,
#            z=c("percentage_5"),seq(-0.727, 1.5,length=11),
#            labs = c("5% increase in outdoor biting"))
sum(absolute_increase_cases[,2])
sum(absolute_increase_cases[,3])


##########
## Prev
trRes2 = expand.grid(col1=c(1:601))
for(i in 1:601){
  trRes2[i,1] = percentage_increase_in_prev[i,4] ##  percentage increase in cases with 5% more outdoor biting
  trRes2[i,2] = percentage_increase_in_prev[i,7] ##  percentage increase in cases with 10% more outdoor biting
}

input=read.csv("F:/Copy of Intervention_coverage.csv",header=TRUE)
trRes2[,3] = input$ISO
trRes2[,4] = input$DIDE_CODE

colnames(trRes2) = c("percentage_5",
                    "percentage_10",
                    "ISO","DIDE_CODE")


##Work out how best to split the data
#trRes2 = trRes[apply(trRes, 1, function(x) all(is.finite(x))), ]
quantile(c(trRes2[,1],trRes2[,2]),c(0.00001,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.9999),na.rm=TRUE)
trRes2_U =  trRes2[!duplicated(trRes2[,c('ISO')]),]

obj2 = simple_map3(data=trRes2, data_U = trRes2_U, 
                   z=c("percentage_5",
                       "percentage_10"),labs = c("5% increase in outdoor biting","10% increase in outdoor biting"),
                   seq1=0, seq2=0.003, seq3=0.037,  seq4=0.302,  seq5=2.07,
                   seq6=4.037,  seq7=6.677, seq8=7.487,  seq9 = 8.9,
                   names.attr = c("5% increase in outdoor biting","10% increase in outdoor biting"),as.table = TRUE,
                   main = "Percentage increase in prevalence (Year 3)") ##Fig 3 panel G 
obj2
