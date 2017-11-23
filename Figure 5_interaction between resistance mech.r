#######################
##                   ##
## Figure 5          ##
##                   ##
#######################
######################

## Functions
filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...)   {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    # further modified by Carey McGilliard and Bridget Ferris
    # to allow multiple plots on one page
    
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    # on.exit(par(par.orig))
    # w <- (3 + mar.orig[2]) * par("csi") * 2.54
    # par(las = las)
    # mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }


filled.legend <-function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                                       length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
                          ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
                          levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
                          col = color.palette(length(levels) - 1), plot.title, plot.axes, 
                          key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
                          axes = TRUE, frame.plot = axes, ...){
  # modification of filled.contour by Carey McGilliard and Bridget Ferris
  # designed to just plot the legend
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  #  on.exit(par(par.orig))
  #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  #  par(las = las)
  #  mar <- mar.orig
  #  mar[4L] <- mar[2L]
  #  mar[2L] <- 1
  #  par(mar = mar)
  # plot.new()
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
              yaxs = "i")
  rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
  if (missing(key.axes)) {
    if (axes) 
      axis(4)
  }
  else key.axes
  box()
}


heat.colors_Rev = function (n, alpha = 1) {
  if ((n <- as.integer(n[1L])) > 0) {
    j <- n%/%4
    i <- n - j
    rev(c(rainbow(i, start = 0, end = 1/6, alpha = alpha), if (j > 
                                                               0) hsv(h = 1/6, s = seq.int(from = 1 - 1/(2 * j), 
                                                                                           to = 1/(2 * j), length.out = j), v = 1, alpha = alpha)))
  }
  else character()
}

## Run the model with phiI and phiB 
## for quantiles 25% 50% and 75% 
dat_mosq = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Janetta\\phiI_phiB_rawdata.csv",header=TRUE)
dat_inbed = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Andrew Beale\\Human_sleeping_vs_time_Beale_data_added.csv",header=TRUE)
dat_indoor = read.csv("H:\\Ellie\\IRS and resistance\\behaviour_paper\\Data from Janetta\\Human_indoor_vs_time.csv",header=TRUE)

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
as.numeric(quantile(c(phiB1ALL),c(0.05,0.25,0.5,0.75,0.95),na.rm=TRUE)  )
as.numeric(quantile(c(phiI1ALL),c(0.05,0.25,0.5,0.75,0.95))  )

## also do 5% ... 95% quantiles to estimate relationship between prevalence 2-12 and phiI and phiB
library(MalariaLaunchR)

phi_function<-function(site,
                       
                       ITN, IRS,
                       
                       phi_I,phi_B,
                       
                       irs_decay_mort1_1, irs_decay_mort2_1,irs_decay_succ1_1,
                       irs_decay_succ2_1,	irs_decay_det1_1,	irs_decay_det2_1,
                       itn_repel_fun_1,	itn_repel_gamb_ss_1,	itn_repel_arab_1,
                       itn_kill_fun_1,	itn_kill_gamb_ss_1,	itn_kill_arab_1,
                       itn_half_life,
                       
                       run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/behaviour_sites/Site_Perennial_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'output_type 0 itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'fun_Q_in',phi_I,'fun_Q_bed',phi_B,
                    'arab_Q_in',phi_I,'arab_Q_bed',phi_B,
                    'gamb_ss_Q_in',phi_I,'gamb_ss_Q_bed',phi_B,
                    'add change_irs 1 change_irs_time 0',  
                    'irs_decay_mort1_1', irs_decay_mort1_1,	'irs_decay_mort2_1', irs_decay_mort2_1,	'irs_decay_succ1_1', irs_decay_succ1_1,
                    'irs_decay_succ2_1', irs_decay_succ2_1,	'irs_decay_det1_1', irs_decay_det1_1,	'irs_decay_det2_1', irs_decay_det2_1,
                    'add change_itn 1 change_itn_time 0',
                    'itn_repel_fun_1', itn_repel_fun_1, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_1, 'itn_repel_arab_1', itn_repel_arab_1,
                    'itn_kill_fun_1', itn_kill_fun_1, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_1, 'itn_kill_arab_1', itn_kill_arab_1,
                    'itn_half_life_1', itn_half_life,'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_'),
                 OutputRoot = paste0("F:/Ellies_output_folder/", Run_name, '/draw_', draw),
                 Options=Options,
                 Exe = "F:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="F:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('behaviour_sites/Site_Perennial_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}
##RUN ON CLUSTER...
#inp_phi = read.csv("F:\\behavioural_resistance_llin50.csv",header=TRUE)
#for(i in 1:105){
#  phi_function(site=inp_phi[i,1], 
#                
#                ITN=inp_phi[i,2], IRS=inp_phi[i,3],
#                
#                phi_I=inp_phi[i,4],phi_B=inp_phi[i,5],
#                irs_decay_mort1_1=inp_phi[i,6], irs_decay_mort2_1=inp_phi[i,7], 
#                irs_decay_succ1_1=inp_phi[i,8], irs_decay_succ2_1=inp_phi[i,9], 
#                irs_decay_det1_1=inp_phi[i,10], irs_decay_det2_1=inp_phi[i,11],
#                itn_repel_fun_1=inp_phi[i,12], itn_repel_gamb_ss_1=inp_phi[i,13], itn_repel_arab_1=inp_phi[i,14], 
#                itn_kill_fun_1=inp_phi[i,15], itn_kill_gamb_ss_1=inp_phi[i,16], itn_kill_arab_1=inp_phi[i,17],
#                
#               itn_half_life = inp_phi[i,18],
#                run_name="behav_phys_resistance")
#  
#}
sites = seq(1,105,21)
data1_no_res = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance/draw_0/behav_phys_resistance",sites[1],"0.txt",sep="_"),header=TRUE)
head(data1_no_res)

data1_res = data2_res = data3_res = data4_res = array(dim=c(nrow(data1_no_res),22,5))
sites_init = sites-1
res_val = 1:21
for(j in 1:5){
  for(i in 1:21){
    data1_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance/draw_0/behav_phys_resistance",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data1_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance/draw_0/behav_phys_resistance",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
    data2_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance2/draw_0/behav_phys_resistance2",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data2_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance2/draw_0/behav_phys_resistance2",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
    data3_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance3/draw_0/behav_phys_resistance3",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data3_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance3/draw_0/behav_phys_resistance3",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
    data4_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance4/draw_0/behav_phys_resistance4",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data4_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance4/draw_0/behav_phys_resistance4",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
      } 
}


##Perennial setting: LLIN at 50% cover No resistance
plot(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,pch="",xlim=c(-1,5),ylim=c(0,0.4),
     main="Perennial setting: 50% LLIN cover",
     ylab="Prevalence in 2 - 10 years (%)",yaxt="n",xlab="Time in years",cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),cex.lab=1.6,cex.axis=1.6)
lines(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,lty=1,col="blue")
lines(data1_res[,2,1] ~  data1_res[,1,1],lty=1,col="blue")#median
lines(data1_res[,2,2] ~  data1_res[,1,2],lty=3,col="blue")
lines(data1_res[,2,5] ~  data1_res[,1,5],lty=3,col="blue")
lines(data1_res[,2,3] ~  data1_res[,1,3],lty=2,col="blue")
lines(data1_res[,2,4] ~  data1_res[,1,4],lty=2,col="blue")

##Perennial setting: 50% LLIN and 80% IRS cover No resistance
plot(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,pch="",xlim=c(-1,5),ylim=c(0,0.4),
     main="Perennial setting: 50% LLIN and 80% IRS cover",
     ylab="Prevalence in 2 - 10 years (%)",yaxt="n",xlab="Time in years",cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),cex.lab=1.6,cex.axis=1.6)
lines(data2_res[,2,1] ~  data2_res[,1,1],lty=1,col="blue")#median
lines(data2_res[,2,2] ~  data2_res[,1,2],lty=3,col="blue")
lines(data2_res[,2,5] ~  data2_res[,1,5],lty=3,col="blue")
lines(data2_res[,2,3] ~  data2_res[,1,3],lty=2,col="blue")
lines(data2_res[,2,4] ~  data2_res[,1,4],lty=2,col="blue")

##Seasonal setting: LLIN at 50% cover No resistance
plot(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,pch="",xlim=c(-1,5),ylim=c(0,0.4),
     main="Seasonal setting: 50% LLIN",
     ylab="Prevalence in 2 - 10 years (%)",yaxt="n",xlab="Time in years",cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),cex.lab=1.6,cex.axis=1.6)
lines(data3_res[,2,1] ~  data3_res[,1,1],lty=1,col="blue")#median
lines(data3_res[,2,2] ~  data3_res[,1,2],lty=3,col="blue")
lines(data3_res[,2,5] ~  data3_res[,1,5],lty=3,col="blue")
lines(data3_res[,2,3] ~  data3_res[,1,3],lty=2,col="blue")
lines(data3_res[,2,4] ~  data3_res[,1,4],lty=2,col="blue")

##Seasonal setting: 50% LLIN and 80% IRS cover No resistance
plot(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,pch="",xlim=c(-1,5),ylim=c(0,0.4),
     main="Seasonal setting: 50% LLIN and 80% IRS cover",
     ylab="Prevalence in 2 - 10 years (%)",yaxt="n",xlab="Time in years",cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),cex.lab=1.6,cex.axis=1.6)
lines(data4_res[,2,1] ~  data4_res[,1,1],lty=1,col="blue")#median
lines(data4_res[,2,2] ~  data4_res[,1,2],lty=3,col="blue")
lines(data4_res[,2,5] ~  data4_res[,1,5],lty=3,col="blue")
lines(data4_res[,2,3] ~  data4_res[,1,3],lty=2,col="blue")
lines(data4_res[,2,4] ~  data4_res[,1,4],lty=2,col="blue")



prevs_peren_nets = prevs_peren_nets_irs = prevs_seas_nets = prevs_seas_nets_irs = array(dim=c(21,5))
for(j in 1:5){
  for(i in 2:22){
    prevs_peren_nets[i-1,j] = data1_res[,i,j][data1_res[,1,j] == 3] ##This is prevalence for each scenario
    prevs_peren_nets_irs[i-1,j] = data2_res[,i,j][data1_res[,1,j] == 3] ##This is prevalence for each scenario
    prevs_seas_nets[i-1,j] = data3_res[,i,j][data1_res[,1,j] == 3] ##This is prevalence for each scenario
    prevs_seas_nets_irs[i-1,j] = data4_res[,i,j][data1_res[,1,j] == 3] ##This is prevalence for each scenario
  }}

data1peren_LLIN = data.frame(prevs = c(prevs_peren_nets[,2:3],prevs_peren_nets[,1],prevs_peren_nets[,4:5]),
                             resistance = rep(seq(0,1,by=0.05),5),
                             behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))
data1peren_LLIN_IRS = data.frame(prevs = c(prevs_peren_nets_irs[,2:3],prevs_peren_nets_irs[,1],prevs_peren_nets_irs[,4:5]),
                             resistance = rep(seq(0,1,by=0.05),5),
                             behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))

data1seas_LLIN = data.frame(prevs = c(prevs_seas_nets[,2:3],prevs_seas_nets[,1],prevs_seas_nets[,4:5]),
                             resistance = rep(seq(0,1,by=0.05),5),
                             behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))
data1seas_LLIN_IRS = data.frame(prevs = c(prevs_seas_nets_irs[,2:3],prevs_seas_nets_irs[,1],prevs_seas_nets_irs[,4:5]),
                             resistance = rep(seq(0,1,by=0.05),5),
                             behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))

library(gplots)
library(colorRamps)
z1 = data1peren_LLIN$prevs
z2 = data1peren_LLIN_IRS$prevs
z3 = data1seas_LLIN$prevs
z4 = data1seas_LLIN_IRS$prevs

x = rep(seq(0,1,by=0.05),5)
y = rep(c(0.05,0.25,0.5,0.75,0.95),each=21)

xcoords = unique(x)
ycoords = unique(y)

surface.matrix1 = matrix(z1,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.matrix2 = matrix(z2,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.matrix3 = matrix(z3,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.matrix4 = matrix(z4,ncol=length(ycoords),nrow=length(xcoords),byrow=F)

matrix_funs = function(surface.matrix,minimum_val,maximum_val){
  filled.contour3(xcoords,
                  ycoords,
                  surface.matrix,
                  color=blue2green,
                  plot.axes = { axis(2, at = seq(0, 1, by = 0.1), seq(0, 1, by = 0.1))
                    axis(1, at = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) },
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
          nlevels = 6, levels = seq(0,0.3,by=0.05),
          xlim = c(min(xcoords),max(xcoords)),
          ylim = c(min(ycoords),max(ycoords)),
          zlim = c(minimum_val,maximum_val),
          add=TRUE,                 # add the contour plot to filled-contour,
          #thus making an overlay
          col = "gold3"         # color of overlay-lines
  )
}
matrix_funs(surface.matrix1,min(surface.matrix4),max(surface.matrix3))
matrix_funs(surface.matrix2,min(surface.matrix4),max(surface.matrix3))
matrix_funs(surface.matrix3,min(surface.matrix4),max(surface.matrix3))
matrix_funs(surface.matrix4,min(surface.matrix4),max(surface.matrix3))

