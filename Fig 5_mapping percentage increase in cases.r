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
base_default = read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_default/draw_0/Behaviour_paper_phi_default_317_0.txt'),header=TRUE)
base_5pcreds = read.table(paste0('F:/Ellies_output_folder/Behaviour_paper_phi_5pc/draw_0/Behaviour_paper_phi_5pc_317_0.txt'),header=TRUE)

plot(base_default$clin_inc_all ~ base_default$year,ylim=c(0,max(base_5pcreds$clin_inc_all)))
lines(base_5pcreds$clin_inc_all ~ base_5pcreds$year,col="blue")
##
## the cases
site_temp = read.csv("H:\\Ellie\\Rprojects\\Malaria\\Intervention_coverage.csv",header=TRUE)
site = c(site_temp[1:601,1])
pop_temp = read.csv("H:\\Ellie\\Rprojects\\Malaria\\populations.csv",header=TRUE)

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
    
    
    percentage_increase_in_cases[i,2] =   mean(c((bas5[i,1] - bas0[i,1])/bas0[i,1],(bas5[i,2] - bas0[i,2])/bas0[i,2], (bas5[i,3] - bas0[i,3])/bas0[i,3]))
    percentage_increase_in_cases[i,3] =   mean(c((bas10[i,1] - bas0[i,1])/bas0[i,1],(bas10[i,2] - bas0[i,2])/bas0[i,2], (bas10[i,3] - bas0[i,3])/bas0[i,3]))
    
    absolute_increase_cases[i,2] =   sum(c((bas5[i,1] - bas0[i,1]),(bas5[i,2] - bas0[i,2]), (bas5[i,3] - bas0[i,3])))
    absolute_increase_cases[i,3] =   sum(c((bas10[i,1] - bas0[i,1]),(bas10[i,2] - bas0[i,2]), (bas10[i,3] - bas0[i,3])))
}

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
             main = "Cases averted per 1000 people per year with 50% pyrethroid resistance")
  spplot(M2[z], names.attr = labs, main = "Cases averted per 1000 people per year with 50% pyrethroid resistance", at = c(seq1, seq2, seq3,  seq4,  seq5,  seq6,  seq7,  seq8,  seq9), col="grey",col.regions = brewer.pal(9,"RdBu"),
         colorkey =  list(space = "right", height = 0.8)) + b
  
  
}


populations = read.csv("F:/populations.csv",header=TRUE)

head(populations)
trRes = expand.grid(col1=c(1:601))
for(i in 1:601){
  trRes[i,1] = percentage_increase_in_cases[i,2] ##  percentage increase in cases with 5% more outdoor biting
  trRes[i,2] = percentage_increase_in_cases[i,3] ##  percentage increase in cases with 10% more outdoor biting
}

##Work out how best to split the data
quantile(c(trRes[,1],trRes[,2]),c(0.00001,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.9999))

input=read.csv("F:/Copy of Intervention_coverage.csv",header=TRUE)
trRes[,10] = input$ISO
trRes[,11] = input$DIDE_CODE

colnames(trRes) = c("percentage_5",
                    "percentage_10",
                    "ISO","DIDE_CODE")

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
                   seq1=, seq2=, seq3=,  seq4=,  seq5=,  seq6=,  seq7=,  seq8=,  seq9 = ,
                   names.attr = c("5% increase in outdoor biting","10% increase in outdoor biting"),as.table = TRUE,
                   main = "Percentage increase in cases per person per year") ##Fig 3 panel G 
obj1

