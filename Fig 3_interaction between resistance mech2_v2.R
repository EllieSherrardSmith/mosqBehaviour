#######################
##                   ##
## Figure 5          ##
##                   ##
#######################
######################

library(gplots)
library(colorRamps)
library(adegenet)

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
  site_file<-read.table(paste0('P:/Ellies_cool_model_folder2/model_files/sites/behaviour_sites/Site_Perennial_', site, '.txt'))
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
                 OutputRoot = paste0("P:/Ellies_output_folder/", Run_name, '/draw_', draw),
                 Options=Options,
                 Exe = "P:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="P:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('behaviour_sites/Site_Perennial_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}

inp_phi = read.csv("F:\\behavioural_resistance_llin50_and_irs80_actellic.csv",header=TRUE)
for(i in 1:105){
  phi_function(site=inp_phi[i,1], 
                
                ITN=inp_phi[i,2], IRS=inp_phi[i,3],
                
                phi_I=inp_phi[i,4],phi_B=inp_phi[i,5],
                irs_decay_mort1_1=inp_phi[i,6], irs_decay_mort2_1=inp_phi[i,7], 
                irs_decay_succ1_1=inp_phi[i,8], irs_decay_succ2_1=inp_phi[i,9], 
                irs_decay_det1_1=inp_phi[i,10], irs_decay_det2_1=inp_phi[i,11],
                itn_repel_fun_1=inp_phi[i,12], itn_repel_gamb_ss_1=inp_phi[i,13], itn_repel_arab_1=inp_phi[i,14], 
                itn_kill_fun_1=inp_phi[i,15], itn_kill_gamb_ss_1=inp_phi[i,16], itn_kill_arab_1=inp_phi[i,17],
                
                itn_half_life = inp_phi[i,18],
                run_name="behav_phys_resistance5b")
  
}

phi_function2<-function(site,
                       
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
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/behaviour_sites/seasonal/Site_Highly_Seasonal_', site, '.txt'))
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
                 Site=paste0('behaviour_sites/seasonal/Site_Highly_Seasonal_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}
##RUN ON CLUSTER...
inp_phi = read.csv("F:\\behavioural_resistance_llin50_and_irs80_actellic.csv",header=TRUE)
for(i in 53:105){
  phi_function2(site=inp_phi[i,1], 
                
                ITN=inp_phi[i,2], IRS=inp_phi[i,3],
                
                phi_I=inp_phi[i,4],phi_B=inp_phi[i,5],
                irs_decay_mort1_1=inp_phi[i,6], irs_decay_mort2_1=inp_phi[i,7], 
                irs_decay_succ1_1=inp_phi[i,8], irs_decay_succ2_1=inp_phi[i,9], 
                irs_decay_det1_1=inp_phi[i,10], irs_decay_det2_1=inp_phi[i,11],
                itn_repel_fun_1=inp_phi[i,12], itn_repel_gamb_ss_1=inp_phi[i,13], itn_repel_arab_1=inp_phi[i,14], 
                itn_kill_fun_1=inp_phi[i,15], itn_kill_gamb_ss_1=inp_phi[i,16], itn_kill_arab_1=inp_phi[i,17],
                
               itn_half_life = inp_phi[i,18],
                run_name="behav_phys_resistance6b")
  
}


sites = seq(1,105,21)
data1_no_res = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance/draw_0/behav_phys_resistance",sites[1],"0.txt",sep="_"),header=TRUE)
head(data1_no_res)

data0p_res = data0s_res = data1_res = data2_res = data3_res = data4_res = data5_res = data6_res = array(dim=c(nrow(data1_no_res),22,5))
sites_init = sites-1
res_val = 1:21
for(j in 1:5){
  for(i in 1:21){
    data0p_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data0p_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance_None1/draw_0/behav_phys_resistance_None1",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]

    data0s_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data0s_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance_None2/draw_0/behav_phys_resistance_None2",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]

    data1_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance/draw_0/behav_phys_resistance",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data1_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance/draw_0/behav_phys_resistance",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
    data2_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance2/draw_0/behav_phys_resistance2",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data2_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance2/draw_0/behav_phys_resistance2",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
    data3_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance3/draw_0/behav_phys_resistance3",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data3_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance3/draw_0/behav_phys_resistance3",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
    data4_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance4/draw_0/behav_phys_resistance4",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data4_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance4/draw_0/behav_phys_resistance4",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
    data5_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance5b/draw_0/behav_phys_resistance5b",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data5_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance5b/draw_0/behav_phys_resistance5b",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
    data6_res[,1,j] =  read.table(paste("F:/Ellies_output_folder/behav_phys_resistance6b/draw_0/behav_phys_resistance6b",sites_init[j]+res_val[1],"0.txt",sep="_"),header=TRUE)[,1]
    data6_res[,i+1,j] = read.table(paste("F:/Ellies_output_folder/behav_phys_resistance6b/draw_0/behav_phys_resistance6b",sites_init[j]+res_val[i],"0.txt",sep="_"),header=TRUE)[,3]
    
  } 
}

##Now work out the efficacy with pyrethroid resistance increase
Efficacy_peren_LLIN50 = Efficacy_peren_LLIN50IRS80 = 
  Efficacy_seas_LLIN50 = Efficacy_seas_LLIN50IRS80 = 
  Efficacy_peren_LLIN50IRS80act = Efficacy_seas_LLIN50IRS80act = array(dim=c(21,5))
for(j in 1:5){
  for(i in 1:21){
    Efficacy_peren_LLIN50[i,j] = (data0p_res[,i+1,j][data0p_res[,1,j] == 3] - 
                                    data1_res[,i+1,j][data0p_res[,1,j] == 3]) / data0p_res[,i+1,j][data0p_res[,1,j] == 3]
    Efficacy_peren_LLIN50IRS80[i,j] = (data0p_res[,i+1,j][data0p_res[,1,j] == 3] - 
                                    data2_res[,i+1,j][data0p_res[,1,j] == 3]) / data0p_res[,i+1,j][data0p_res[,1,j] == 3]
    Efficacy_seas_LLIN50[i,j] = (data0p_res[,i+1,j][data0p_res[,1,j] == 3] - 
                                    data3_res[,i+1,j][data0p_res[,1,j] == 3]) / data0p_res[,i+1,j][data0p_res[,1,j] == 3]
    Efficacy_seas_LLIN50IRS80[i,j] = (data0p_res[,i+1,j][data0p_res[,1,j] == 3] - 
                                    data4_res[,i+1,j][data0p_res[,1,j] == 3]) / data0p_res[,i+1,j][data0p_res[,1,j] == 3]
    Efficacy_peren_LLIN50IRS80act[i,j] = (data0p_res[,i+1,j][data0p_res[,1,j] == 3] - 
                                   data5_res[,i+1,j][data0p_res[,1,j] == 3]) / data0p_res[,i+1,j][data0p_res[,1,j] == 3]
    Efficacy_seas_LLIN50IRS80act[i,j] = (data0p_res[,i+1,j][data0p_res[,1,j] == 3] - 
                                        data6_res[,i+1,j][data0p_res[,1,j] == 3]) / data0p_res[,i+1,j][data0p_res[,1,j] == 3]
    
    
        }
}

Eff1peren_LLIN = data.frame(eff = c(Efficacy_peren_LLIN50[,2:3],Efficacy_peren_LLIN50[,1],Efficacy_peren_LLIN50[,4:5]),
                             resistance = rep(seq(0,1,by=0.05),5),
                             behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))
Eff1peren_LLIN_IRS = data.frame(eff = c(Efficacy_peren_LLIN50IRS80[,2:3],Efficacy_peren_LLIN50IRS80[,1],Efficacy_peren_LLIN50IRS80[,4:5]),
                                 resistance = rep(seq(0,1,by=0.05),5),
                                 behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))

Eff1seas_LLIN = data.frame(eff = c(Efficacy_seas_LLIN50[,2:3],Efficacy_seas_LLIN50[,1],Efficacy_seas_LLIN50[,4:5]),
                            resistance = rep(seq(0,1,by=0.05),5),
                            behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))
Eff1seas_LLIN_IRS = data.frame(eff = c(Efficacy_seas_LLIN50IRS80[,2:3],Efficacy_seas_LLIN50IRS80[,1],Efficacy_seas_LLIN50IRS80[,4:5]),
                                resistance = rep(seq(0,1,by=0.05),5),
                                behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))
Eff1peren_LLIN_IRSact = data.frame(eff = c(Efficacy_peren_LLIN50IRS80act[,2:3],Efficacy_peren_LLIN50IRS80act[,1],Efficacy_peren_LLIN50IRS80act[,4:5]),
                               resistance = rep(seq(0,1,by=0.05),5),
                               behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))
Eff1seas_LLIN_IRSact = data.frame(eff = c(Efficacy_seas_LLIN50IRS80act[,2:3],Efficacy_seas_LLIN50IRS80act[,1],Efficacy_seas_LLIN50IRS80act[,4:5]),
                               resistance = rep(seq(0,1,by=0.05),5),
                               behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))

resistance = c(0.01,seq(0.05,1,by=0.05))

prevs_peren_nets = prevs_peren_nets_irs = prevs_seas_nets = prevs_seas_nets_irs = 
  prevs_peren_nets_irsact = prevs_seas_nets_irsact = array(dim=c(21,5))
for(j in 1:5){
  for(i in 2:22){
    prevs_peren_nets[i-1,j] = data1_res[,i,j][data1_res[,1,j] == 3] ##This is prevalence for each scenario
    prevs_peren_nets_irs[i-1,j] = data2_res[,i,j][data1_res[,1,j] == 3] ##This is prevalence for each scenario
    prevs_seas_nets[i-1,j] = data3_res[,i,j][data1_res[,1,j] == 3] ##This is prevalence for each scenario
    prevs_seas_nets_irs[i-1,j] = data4_res[,i,j][data1_res[,1,j] == 3] ##This is prevalence for each scenario
    prevs_peren_nets_irsact[i-1,j] = data5_res[,i,j][data5_res[,1,j] == 3] ##This is prevalence for each scenario
    prevs_seas_nets_irsact[i-1,j] = data6_res[,i,j][data6_res[,1,j] == 3] ##This is prevalence for each scenario
    
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

data1peren_LLIN_IRSact = data.frame(prevs = c(prevs_peren_nets_irsact[,2:3],prevs_peren_nets_irsact[,1],prevs_peren_nets_irsact[,4:5]),
                                 resistance = rep(seq(0,1,by=0.05),5),
                                 behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))
data1seas_LLIN_IRSact = data.frame(prevs = c(prevs_seas_nets_irsact[,2:3],prevs_seas_nets_irsact[,1],prevs_seas_nets_irsact[,4:5]),
                                resistance = rep(seq(0,1,by=0.05),5),
                                behaviour = rep(c(0.05,0.25,0.5,0.75,0.95),each=21))

zf1 = Eff1peren_LLIN$eff * 100
zf2 = Eff1peren_LLIN_IRS$eff * 100
zf3 = Eff1seas_LLIN$eff * 100
zf4 = Eff1seas_LLIN_IRS$eff * 100
zf5 = Eff1peren_LLIN_IRSact$eff * 100
zf6 = Eff1seas_LLIN_IRSact$eff * 100

z1 = data1peren_LLIN$prevs
z2 = data1peren_LLIN_IRS$prevs
z3 = data1seas_LLIN$prevs
z4 = data1seas_LLIN_IRS$prevs
z5 = data1peren_LLIN_IRSact$prevs
z6 = data1seas_LLIN_IRSact$prevs

x = rep(seq(0,1,by=0.05),5)
y = rep(c(0.5964,0.814,0.8909,0.94,0.9832),each=21)

xcoords = unique(x)
ycoords = unique(y)

surface.matrix1 = matrix(z1,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.matrix2 = matrix(z2,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.matrix3 = matrix(z3,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.matrix4 = matrix(z4,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.matrix5 = matrix(z5,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.matrix6 = matrix(z6,ncol=length(ycoords),nrow=length(xcoords),byrow=F)

surface.mat1 = matrix(zf1,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat2 = matrix(zf2,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat3 = matrix(zf3,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat4 = matrix(zf4,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat5 = matrix(zf5,ncol=length(ycoords),nrow=length(xcoords),byrow=F)
surface.mat6 = matrix(zf6,ncol=length(ycoords),nrow=length(xcoords),byrow=F)


matrix_funs = function(surface.matrix,minimum_val,maximum_val,upps,uni,levs,colschoice,cols_conts){
  filled.contour3(xcoords,
                  ycoords,
                  surface.matrix,
                  color=colschoice,
                  plot.axes = { axis(2, at = seq(0.5, 1, by = 0.1), seq(0.5, 1, by = 0.1),las=2)
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

##Top left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.06,0.31,0.7,0.95))#,
                # major tick size and direction, < 0 means outside

##Perennial setting: LLIN at 50% cover No resistance

plot(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,pch="",xlim=c(-0.2,5),ylim=c(0,0.4),
     main="",frame=FALSE,
     ylab="",yaxt="n",xlab="",cex.lab=1,cex.axis=1)
mtext(side=1,line=2.3,"Time in years")
mtext(side=2,las=0,line=2.3,"Prevalence in 2 - 10 years (%)")
axis(2,las=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),cex.lab=1.6,cex.axis=1)
lines(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,lty=1,col="blue",xlim=c(-0.2,5))
#
polygon(c(data1_res[,1,1],rev(data1_res[,1,1])),c(data1_res[,2,2],rev(data1_res[,2,5])),border=NA,col=transp("blue",0.4))
polygon(c(data1_res[,1,1],rev(data1_res[,1,1])),c(data1_res[,2,3],rev(data1_res[,2,4])),border=NA,col=transp("blue",0.4))
lines(data1_res[,2,1] ~  data1_res[,1,1],lty=1,col="blue",lwd=2)#median

polygon(c(data5_res[,1,1],rev(data5_res[,1,1])),c(data5_res[,2,2],rev(data5_res[,2,5])),border=NA,col=transp("darkred",0.4))
polygon(c(data5_res[,1,1],rev(data5_res[,1,1])),c(data5_res[,2,3],rev(data5_res[,2,4])),border=NA,col=transp("darkred",0.4))
lines(data5_res[,2,1] ~  data5_res[,1,1],lty=2,col="darkred",lwd=2)#median

abline(v=0,lty=2)
segments(x0=3,x1=3,y0=0,y1=0.2,lty=2)
text(0.2,0.01,"a")
text(2.8,0.01,"b")
text(5,0.4,"A",cex=1.2)

legend(1.5,0.38,title = "Perennial setting",legend = c("LLIN 50%","LLIN 50% + IRS 80%"),
       lty=c(1,2),cex=1,bty="n",col=c("blue","darkred"),pch=15)


##Top right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.38,0.6,0.7,0.95)  )                 # major tick size and direction, < 0 means outside

##Efficacy plot
plot(Efficacy_peren_LLIN50[,1] ~ resistance,ylim=c(0,1),lty=1,pch="",line=2.3,
     yaxt="n",ylab="Efficacy of indoor vector intervention (%)",frame=FALSE,
     xaxt="n",xlab="Mosquito survival at bioassay (%)",cex.lab=1,cex.axis=1)
axis(1,at=seq(0,1,by=0.2),labels=seq(0,100,by=20),cex.lab=1,cex.axis=1)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1,cex.axis=1)

veca = 2:3
vecb = 5:4
for(i in 1:2){
  polygon(c(resistance,rev(resistance)),
          c(Efficacy_peren_LLIN50[,veca[i]],rev(Efficacy_peren_LLIN50[,vecb[i]])),
          border=NA,col=transp("blue",0.4))
  polygon(c(resistance,rev(resistance)),
          c(Efficacy_peren_LLIN50IRS80act[,veca[i]],rev(Efficacy_peren_LLIN50IRS80act[,vecb[i]])),
          border=NA,col=transp("darkred",0.4))
  
}
lines(Efficacy_peren_LLIN50[,1] ~ resistance,lty=1,lwd=2,col="blue")
lines(Efficacy_peren_LLIN50IRS80act[,1] ~ resistance,lty=2,lwd=2,col="darkred")


text(1,1,"B",cex=1.2)

##Top right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.68,0.93,0.7,0.95)  )                 # major tick size and direction, < 0 means outside


behavioural_resistance = unique(read.csv("F:\\behavioural_resistance_llin50.csv",header=TRUE)$phi_I)
plot(Efficacy_peren_LLIN50[1,] ~ behavioural_resistance,col="blue",lwd=2,
     ylab="Efficacy of indoor intervention (%)",yaxt="n",xaxt="n",bty="n",
     xlab="Proportion of mosquito bites indoors",line=2.3,
     pch="",ylim=c(0,1),xlim=c(0,1),cex.lab=1)
axis(1,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1)
for(i in 1){lines(sort(Efficacy_peren_LLIN50[i,]) ~ sort(behavioural_resistance),col="blue",lwd=2)}##0% phys reistance
for(i in 11){lines(sort(Efficacy_peren_LLIN50[i,]) ~ sort(behavioural_resistance),lty=2,col="blue",lwd=2)}##50%
for(i in 21){lines(sort(Efficacy_peren_LLIN50[i,]) ~ sort(behavioural_resistance),lty=3,col="blue",lwd=2)}##100%

for(i in 1){lines(sort(Efficacy_peren_LLIN50IRS80act[i,]) ~ sort(behavioural_resistance),col="darkred",lwd=2)}##0% phys reistance
for(i in 11){lines(sort(Efficacy_peren_LLIN50IRS80act[i,]) ~ sort(behavioural_resistance),lty=2,col="darkred",lwd=2)}##50%
for(i in 21){lines(sort(Efficacy_peren_LLIN50IRS80act[i,]) ~ sort(behavioural_resistance),lty=3,col="darkred",lwd=2)}##100%

text(0.95,0.95,"C",cex=1.2)

legend(0.04,0.9,title = "Survival at bioassay",
       legend = c("0%","50%","100%"),
       lty=c(1,2,3),lwd=2,cex=1,bty="n",col="blue")



##mid left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.06,0.31,0.15,0.5)  )                 # major tick size and direction, < 0 means outside

surface.matrix1pc = 100*surface.matrix1
surface.matrix2pc = 100*surface.matrix2
surface.matrix3pc = 100*surface.matrix3
surface.matrix4pc = 100*surface.matrix4

matrix_funs(surface.mat1,min(surface.mat1),max(surface.mat6),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"D",cex=1.2,col="darkblue")

##mid right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.36,0.61,0.15,0.5)  )                 # major tick size and direction, < 0 means outside

matrix_funs(surface.mat5,min(surface.mat1),max(surface.mat6),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"E",cex=1.2,col="darkblue")

##mid right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.66,0.91,0.15,0.5)  )                 # major tick size and direction, < 0 means outside

matrix_funs(surface.mat6,min(surface.mat1),max(surface.mat6),
            upps=100,uni = 10,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"F",cex=1.2,col="darkblue")
######################################################################
#Add a legend:
par(new = "TRUE",
    plt = c(0.93,0.95,0.18,0.47),   # define plot region for legend
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
  zlim = c(0,100))

#Add some figure labels
par(xpd=NA,cex = 1.1)
text(x = -46,y = 40,expression(paste("Proportion of mosquito bites taken indoors, ", phi[I])),srt = 90,cex = 1)
text(x = -22,y = -40,"Mosquito survival at bioassay (%)",cex = 1)

text(x = -37,y = 130,"Perennial setting",cex = 1)
text(x = -22,y = 130,"Perennial setting",cex = 1)
text(x = -7,y = 130,"Seasonal setting",cex = 1)

text(x = -15,y = 120,"LLIN 50% cover + Long-lasting IRS 80% cover",cex = 1)
text(x = -37,y = 120,"LLIN 50% cover",cex = 1)

###############################
###################
########
#
##Perennial setting: LLIN at 50% cover No resistance

plot(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,pch="",xlim=c(-0.2,5),ylim=c(0,0.4),
     main="",frame=FALSE,
     ylab="Prevalence in 2 - 10 years (%)",yaxt="n",xlab="",cex.lab=1,cex.axis=1)
mtext(side=1,line=2.3,"Time in years")
axis(2,las=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),cex.lab=1.6,cex.axis=1)
lines(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,lty=1,col="blue",xlim=c(-0.2,5))
#
polygon(c(data1_res[,1,1],rev(data1_res[,1,1])),c(data1_res[,2,2],rev(data1_res[,2,5])),border=NA,col=transp("blue",0.4))
polygon(c(data1_res[,1,1],rev(data1_res[,1,1])),c(data1_res[,2,3],rev(data1_res[,2,4])),border=NA,col=transp("blue",0.4))
lines(data1_res[,2,1] ~  data1_res[,1,1],lty=1,col="blue",lwd=2)#median

polygon(c(data5_res[,1,1],rev(data5_res[,1,1])),c(data5_res[,2,2],rev(data5_res[,2,5])),border=NA,col=transp("darkred",0.4))
polygon(c(data5_res[,1,1],rev(data5_res[,1,1])),c(data5_res[,2,3],rev(data5_res[,2,4])),border=NA,col=transp("darkred",0.4))
lines(data5_res[,2,1] ~  data5_res[,1,1],lty=2,col="darkred",lwd=2)#median

polygon(c(data6_res[,1,1],rev(data6_res[,1,1])),c(data6_res[,2,2],rev(data6_res[,2,5])),border=NA,col=transp("orange",0.4))
polygon(c(data6_res[,1,1],rev(data6_res[,1,1])),c(data6_res[,2,3],rev(data6_res[,2,4])),border=NA,col=transp("orange",0.4))
lines(data6_res[,2,1] ~  data6_res[,1,1],lty=2,col="orange",lwd=2)#median
abline(v=0,lty=2)
segments(x0=3,x1=3,y0=0,y1=0.2,lty=2)
text(0.2,0.01,"a")
text(3.2,0.01,"b")

########
####################
#############################
################################
##
## Figure 6
plot.new()

##Top left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.1,0.45,0.6,0.9))#,
# major tick size and direction, < 0 means outside

plot(Efficacy_peren_LLIN50[,1] ~ resistance,ylim=c(0,1),lty=1,pch="",line=2.3,
     yaxt="n",ylab="Efficacy of indoor vector intervention (%)",frame=FALSE,
     main = "Perennial setting",cex.main=1.2,
     xaxt="n",xlab="Mosquito survival at bioassay (%)",cex.lab=1,cex.axis=1)
axis(1,at=seq(0,1,by=0.2),labels=seq(0,100,by=20),cex.lab=1,cex.axis=1)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1,cex.axis=1)

veca = 2:3
vecb = 5:4
for(i in 1:2){
  polygon(c(resistance,rev(resistance)),
          c(Efficacy_peren_LLIN50[,veca[i]],rev(Efficacy_peren_LLIN50[,vecb[i]])),
          border=NA,col=transp("blue",0.4))
  polygon(c(resistance,rev(resistance)),
          c(Efficacy_peren_LLIN50IRS80act[,veca[i]],rev(Efficacy_peren_LLIN50IRS80act[,vecb[i]])),
          border=NA,col=transp("darkred",0.4))
  
}
lines(Efficacy_peren_LLIN50[,1] ~ resistance,lty=1,lwd=2,col="blue")
lines(Efficacy_peren_LLIN50IRS80act[,1] ~ resistance,lty=2,lwd=2,col="darkred")

text(1,1,"A",cex=1.2)


##Top right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.55,0.9,0.6,0.9)  )                 # major tick size and direction, < 0 means outside

##Efficacy plot
plot(Efficacy_peren_LLIN50[,1] ~ resistance,ylim=c(0,1),lty=1,pch="",line=2.3,
     yaxt="n",ylab="Efficacy of indoor vector intervention (%)",frame=FALSE,
     main = "Highly seasonal setting",
     xaxt="n",xlab="Mosquito survival at bioassay (%)",cex.lab=1,cex.axis=1)
axis(1,at=seq(0,1,by=0.2),labels=seq(0,100,by=20),cex.lab=1,cex.axis=1)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1,cex.axis=1)

veca = 2:3
vecb = 5:4
for(i in 1:2){
  polygon(c(resistance,rev(resistance)),
          c(Efficacy_seas_LLIN50[,veca[i]],rev(Efficacy_seas_LLIN50[,vecb[i]])),
          border=NA,col=transp("blue",0.4))
  polygon(c(resistance,rev(resistance)),
          c(Efficacy_seas_LLIN50IRS80act[,veca[i]],rev(Efficacy_seas_LLIN50IRS80act[,vecb[i]])),
          border=NA,col=transp("orange",0.4))
  
}
lines(Efficacy_seas_LLIN50[,1] ~ resistance,lty=1,lwd=2,col="blue")
lines(Efficacy_seas_LLIN50IRS80act[,1] ~ resistance,lty=2,lwd=2,col="orange")


text(1,1,"B",cex=1.2)

##mid left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.1,0.45,0.15,0.5)  )                 # major tick size and direction, < 0 means outside

matrix_funs(surface.mat5,min(surface.mat5),max(surface.mat6),
            upps=1,uni = 0.1,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"C",cex=1.2,col="darkblue")

##mid right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.55,0.9,0.15,0.5)  )                 # major tick size and direction, < 0 means outside

matrix_funs(surface.mat6,min(surface.mat5),max(surface.mat6),
            upps=1,uni = 0.1,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"D",cex=1.2,col="darkblue")

######################################################################
#Add a legend:
#Add a legend:
par(new = "TRUE",
    plt = c(0.91,0.93,0.20,0.45),   # define plot region for legend
    las = 1,
    cex.axis = 1)
#
filled.legend(
  xcoords,
  ycoords,
  surface.mat4,
  color = heat.colors,
  plot.title = "Prevalence in 2 - 10 years",
  xlab = "",
  ylab = "",
  xlim = c(0,100),
  ylim = c(0.5,1),
  zlim = c(min(surface.mat5),max(surface.mat6)))

#Add some figure labels
par(xpd=NA,cex = 1.1)
text(x = -44,y = 0.7,expression(paste("Proportion of mosquito bites taken indoors, ", phi[I])),srt = 90,cex = 1)
text(x = -20,y = 0.35,"Mosquito survival at bioassay (%)",cex = 1)

text(x = -20,y = 1.5,"Long-lasting non-pyrethroid IRS",cex = 1)


###########################
##
## Supplementary figure Seasonal setting

plot.new()

##Top left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.1,0.45,0.7,0.95))#,
# major tick size and direction, < 0 means outside

##Perennial setting: LLIN at 50% cover No resistance

plot(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,pch="",xlim=c(-0.2,5),ylim=c(0,0.4),
     main="",frame=FALSE,
     ylab="Prevalence in 2 - 10 years (%)",yaxt="n",xlab="",cex.lab=1,cex.axis=1)
mtext(side=1,line=2.3,"Time in years")
axis(2,las=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),cex.lab=1.6,cex.axis=1)
#lines(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,lty=1,col="blue",xlim=c(-0.2,5))
#
polygon(c(data3_res[,1,1],rev(data3_res[,1,1])),c(data3_res[,2,2],rev(data3_res[,2,5])),border=NA,col=transp("blue",0.4))
polygon(c(data3_res[,1,1],rev(data3_res[,1,1])),c(data3_res[,2,3],rev(data3_res[,2,4])),border=NA,col=transp("blue",0.4))
lines(data3_res[,2,1] ~  data3_res[,1,1],lty=1,col="blue",lwd=2)#median

polygon(c(data4_res[,1,1],rev(data4_res[,1,1])),c(data4_res[,2,2],rev(data4_res[,2,5])),border=NA,col=transp("darkred",0.4))
polygon(c(data4_res[,1,1],rev(data4_res[,1,1])),c(data4_res[,2,3],rev(data4_res[,2,4])),border=NA,col=transp("darkred",0.4))
lines(data4_res[,2,1] ~  data4_res[,1,1],lty=2,col="darkred",lwd=2)#median

abline(v=0,lty=2)
segments(x0=3,x1=3,y0=0,y1=0.2,lty=2)
text(0.2,0.01,"a")
text(3.2,0.01,"b")
text(5,0.4,"A",cex=1.2)

legend(1.4,0.38,title = "Seasonal setting",legend = c("LLIN 50%","LLIN 50% + IRS 80%"),
       lty=c(1,2),cex=1,bty="n",col=c("blue","darkred"),pch=15)


##Top right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.55,0.9,0.7,0.95)  )                 # major tick size and direction, < 0 means outside

##Efficacy plot
plot(Efficacy_seas_LLIN50[,1] ~ resistance,ylim=c(0,1),lty=1,pch="",line=2.3,
     yaxt="n",ylab="Efficacy of indoor vector intervention (%)",frame=FALSE,
     xaxt="n",xlab="Mosquito survival at bioassay (%)",cex.lab=1,cex.axis=1)
axis(1,at=seq(0,1,by=0.2),labels=seq(0,100,by=20),cex.lab=1,cex.axis=1)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1,cex.axis=1)

veca = 2:3
vecb = 5:4
for(i in 1:2){
  polygon(c(resistance,rev(resistance)),
          c(Efficacy_seas_LLIN50[,veca[i]],rev(Efficacy_seas_LLIN50[,vecb[i]])),
          border=NA,col=transp("blue",0.4))
  polygon(c(resistance,rev(resistance)),
          c(Efficacy_seas_LLIN50IRS80[,veca[i]],rev(Efficacy_seas_LLIN50IRS80[,vecb[i]])),
          border=NA,col=transp("darkred",0.4))
  
}
lines(Efficacy_seas_LLIN50[,1] ~ resistance,lty=1,lwd=2,col="blue")
lines(Efficacy_seas_LLIN50IRS80[,1] ~ resistance,lty=2,lwd=2,col="darkred")


text(1,1,"B",cex=1.2)

##mid left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.1,0.45,0.38,0.6)  )                 # major tick size and direction, < 0 means outside

surface.matrix1pc = 100*surface.matrix1
surface.matrix2pc = 100*surface.matrix2
surface.matrix3pc = 100*surface.matrix3
surface.matrix4pc = 100*surface.matrix4
matrix_funs(surface.matrix3pc,min(surface.matrix4pc),max(surface.matrix3pc),
            upps=30,uni = 5,levs = 6,colschoice = blue2green,cols_conts="gold3")
text(0.95,0.95,"C",cex=1.2)

##mid right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.55,0.9,0.38,0.6)  )                 # major tick size and direction, < 0 means outside

matrix_funs(surface.matrix4pc,min(surface.matrix4pc),max(surface.matrix3pc),
            upps=30,uni = 5,levs = 6,colschoice = blue2green,cols_conts="gold3")
text(0.95,0.95,"D",cex=1.2)

##mid left plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.1,0.45,0.1,0.33)  )                 # major tick size and direction, < 0 means outside

matrix_funs(surface.mat3,min(surface.mat3),max(surface.mat4),
            upps=1,uni = 0.1,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"E",cex=1.2,col="darkblue")

##mid right plot: Explanation figure, Pervalence estimate given different phiI and phiB estimates
par(new = "TRUE",  
    plt = c(0.55,0.9,0.1,0.33)  )                 # major tick size and direction, < 0 means outside

matrix_funs(surface.mat4,min(surface.mat3),max(surface.mat4),
            upps=1,uni = 0.1,levs = 11,colschoice = heat.colors,cols_conts="darkblue")
text(0.95,0.95,"F",cex=1.2,col="darkblue")

######################################################################
#Add a legend:
par(new = "TRUE",
    plt = c(0.91,0.93,0.4,0.55),   # define plot region for legend
    las = 1,
    cex.axis = 1)
#
filled.legend(
  xcoords,
  ycoords,
  surface.matrix1,
  color = blue2green,
  plot.title = "Prevalence in 2 - 10 years",
  xlab = "",
  ylab = "",
  xlim = c(0,1),
  ylim = c(0,1),
  zlim = c(0,0.3))

#Add a legend:
par(new = "TRUE",
    plt = c(0.91,0.93,0.12,0.28),   # define plot region for legend
    las = 1,
    cex.axis = 1)
#
filled.legend(
  xcoords,
  ycoords,
  surface.mat4,
  color = heat.colors,
  plot.title = "Prevalence in 2 - 10 years",
  xlab = "",
  ylab = "",
  xlim = c(0,1),
  ylim = c(0,1),
  zlim = c(0,0.9))

#Add some figure labels
par(xpd=NA,cex = 1.1)
text(x = -44,y = 1.2,expression(paste("Proportion of mosquito bites taken indoors, ", phi[I])),srt = 90,cex = 1)
text(x = -20,y = -0.4,"Mosquito survival at bioassay (%)",cex = 1)

text(x = -20,y = 4.85,"Seasonal setting",cex = 1)

text(x = -9,y = 2.8,"LLIN 50% + IRS 80% cover",cex = 1)
text(x = -32,y = 2.8,"LLIN 50% cover",cex = 1)

#############
############
###########
##########


##Perennial setting: 50% LLIN and 80% IRS cover No resistance
plot(data1_no_res$prev_2_10_smooth ~  data1_no_res$year,pch="",xlim=c(-1,5),ylim=c(0,0.4),
     main="Perennial setting: 50% LLIN and 80% IRS cover",
     ylab="Prevalence in 2 - 10 years (%)",yaxt="n",xlab="Time in years",cex.lab=1,cex.axis=1)
axis(2,las=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),cex.lab=1,cex.axis=1)
lines(data2_res[,2,1] ~  data2_res[,1,1],lty=1,col="darkred",lwd=2)#median
lines(data2_res[,2,2] ~  data2_res[,1,2],lty=3,col="darkred")
lines(data2_res[,2,5] ~  data2_res[,1,5],lty=3,col="darkred")
lines(data2_res[,2,3] ~  data2_res[,1,3],lty=2,col="darkred")
lines(data2_res[,2,4] ~  data2_res[,1,4],lty=2,col="darkred")




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





matrix_funs(surface.matrix1,min(surface.matrix4),max(surface.matrix3),upps=0.3,uni = 0.05,levs = 6)
matrix_funs(surface.matrix2,min(surface.matrix4),max(surface.matrix3),upps=0.3,uni = 0.05,levs = 6)
matrix_funs(surface.matrix3,min(surface.matrix4),max(surface.matrix3),upps=0.3,uni = 0.05,levs = 6)
matrix_funs(surface.matrix4,min(surface.matrix4),max(surface.matrix3),upps=0.3,uni = 0.05,levs = 6)

matrix_funs(surface.mat1,min(surface.mat3),max(surface.mat4),upps=1,uni = 0.1,levs = 11)
matrix_funs(surface.mat2,min(surface.mat3),max(surface.mat4),upps=1,uni = 0.1,levs = 11)
matrix_funs(surface.mat3,min(surface.mat3),max(surface.mat4),upps=1,uni = 0.1,levs = 11)
matrix_funs(surface.mat4,min(surface.mat3),max(surface.mat4),upps=1,uni = 0.1,levs = 11)
