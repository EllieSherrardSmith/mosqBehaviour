###########################
##
## Fig 6 Map showing concordance of residuals from map for predicted vs observed cases / prev
## and corresponding phi I estimates 
##
###########################


library(ggplot2)
library(gplots)
library(colorRamps)
library(adegenet)

##
## load the relevent libraries
library(adegenet)
library(MalariaMap)
library(latticeExtra)
library(sp)
require(maptools)


##oPTION 1 Country level
phiIs = read.csv("B:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiB+phiI_Beale added.csv",header=TRUE)

phiI = aggregate(phiIs[, 2:3], list(phiIs$ISO), mean)
colnames(phiI) = c("ISO","Bed_biting","Indoor_biting")

quantile(phiI$Indoor_biting,c(0,0.1,0.02,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.975,1),na.rm = TRUE)

plot.new()
par(new = "TRUE",
    plt = c(0.1,0.9,0.1,0.3),
    las = 1,
    cex.axis = 1)

simple_map = function (data, z, breaks = seq(0, 1, length.out = 11), labs, ...) 
{
  M1 <- subset_world(...)
  M2 <- sp::merge(M1, data, by = "ISO")
  sp::spplot(M2[z], at = breaks, col.regions = brewer.pal(9,"YlGnBu"), labels=labs)#YlOrRd
}
simple_mapd = function (data, z, seq1, seq2, seq3,  seq4,  seq5,  seq6,  seq7,  seq8,  seq9,...) 
{
  M1 <- subset_world(level = "Admin", Continent = "Africa")
  M2 <- sp::merge(M1, data, by = "ISO")
  
  spplot(M2[z], names.attr = "Proportion of mosquito bites received indoors", 
         main = "Residual transmission", 
         at = c(seq1, seq2, seq3,  seq4,  seq5,  seq6,  seq7,  seq8,  seq9), col="grey",col.regions = brewer.pal(9,"YlGnBu"),
         colorkey =  list(space = "right", height = 0.8)) 
  
  
}

obj1 = simple_mapd(data=phiI, z="Indoor_biting",
                   0.699, 0.703, 0.846, 0.851, 0.864, 0.874, 0.884, 0.908, 0.923, 0.989) ##Fig 3 panel G 
obj1

#################
## Option 2
phiIs = read.csv("B:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiB+phiI_Beale added.csv",header=TRUE)
dat_temp = data.frame(ISO = phiIs$ISO,DIDE_CODE = phiIs$DIDE_CODE,PHI_I = phiIs$phiI,PHI_B = phiIs$phiB)
dat = dat_temp[1:174,]

dat2 = aggregate(dat[, 3:4], list(dat$DIDE_CODE,dat$ISO), mean)
colnames(dat2) = c("DIDE_CODE","ISO","PHI_I","PHI_B")

input=read.csv("B:\\Ellie\\Rprojects\\Malaria\\Copy of Intervention_coverage.csv",header=TRUE)
head(input)
isos = unique(input$ISO)

isos_temp = data.frame(DIDE_CODE = NA, ISO = isos,PHI_I = NA,PHI_B = NA)

dat3 = rbind(dat2,isos_temp)

dat_U =  dat3[!duplicated(dat3[,c('ISO')]),]
M1 <- subset_world(level = "Admin", Continent = "Africa")
M2 <- sp::merge(M1, dat2, by = "DIDE_CODE")

M3 = subset_world(level = "Country", Continent = "Africa")
M4 <- sp::merge(M3, dat_U, by = "ISO")
quantile(phiIs$phiI,c(0,0.1,0.02,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.975,1),na.rm = TRUE)

b = spplot(M4["PHI_I"], at = seq(0.2539, 0.9899, length.out = 9),fill=FALSE, col="black",lwd=1.5,
       main = "Residual transmission (Proportion of mosquito bites received indoors)")
spplot(M2["PHI_I"], main = "Residual transmission (Proportion of mosquito bites received indoors)", at = seq(0.2539, 0.9899, length.out = 9), 
       col="grey",col.regions = brewer.pal(9,"YlGnBu"),colorkey =  list(space = "right", height = 0.8)) + b

