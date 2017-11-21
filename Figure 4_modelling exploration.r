###############################
##
## Figure 5

## Taking 
PHII = c(phiI1ALL) 
PHIB = c(phiB1ALL) 
##from Figure 3 _Trends and range of phiI and phiB

##Sensitivity analysis of phiI and phiB at different levels of pyrethroid physiological resistance.

1. 

######################
## Run the model with phiI and phiB 
## for quantiles 25% 50% and 75% 

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
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/seasonal_sites/Site_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'output_type 0 itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    
                    'fun_Q_in',phi_I,'fun_Q_bed',phi_B,
                    'arab_Q_in',phi_I,'arab_Q_bed',phi_B,
                    'gamb_ss_Q_in',phi_I,'gamb_ss_Q_bed',phi_B,
                    
                    'add change_irs 1 change_irs_time 0 change_itn 1 change_itn_time 0',  
                    'irs_decay_mort1_1', irs_decay_mort1_1,	'irs_decay_mort2_1', irs_decay_mort2_1,	'irs_decay_succ1_1', irs_decay_succ1_1,
                    'irs_decay_succ2_1', irs_decay_succ2_1,	'irs_decay_det1_1', irs_decay_det1_1,	'irs_decay_det2_1', irs_decay_det2_1,
                    'itn_repel_fun_1', itn_repel_fun_1, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_1, 'itn_repel_arab_1', itn_repel_arab_1,
                    'itn_kill_fun_1', itn_kill_fun_1, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_1, 'itn_kill_arab_1', itn_kill_arab_1,
                    'itn_half_life_1', itn_half_life,
                    
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_'),
                 OutputRoot = paste0("F:/Ellies_output_folder/", Run_name, '/draw_', draw),
                 Options=Options,
                 Exe = "F:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="F:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('seasonal_sites/Site_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}


## ON COMP
#inp_phi = read.csv("F:/phiI_B2_estimates_res0_cov80.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res0_cov80_LLINonly.csv",header=TRUE)

#inp_phi = read.csv("F:/phiI_B2_estimates_res0_cov20.csv",header=TRUE)
inp_phi = read.csv("F:/phiI_B2_estimates_res0_cov20_LLINonly.csv",header=TRUE)

#inp_phi = read.csv("F:/phiI_B2_estimates_res0_cov50.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res0_cov50_LLINonly.csv",header=TRUE)

#inp_phi = read.csv("F:/phiI_B2_estimates_res50_cov80.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res50_cov80_LLINonly.csv",header=TRUE)
###inp_phi = read.csv("F:/phiI_B2_estimates_noVecInterventions.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res20_cov50_LLINonly.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res20_cov50.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res40_cov50_LLINonly.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res40_cov50.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res60_cov50_LLINonly.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res60_cov50.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res80_cov50_LLINonly.csv",header=TRUE)
#inp_phi = read.csv("F:/phiI_B2_estimates_res80_cov50.csv",header=TRUE)

#for(i in 1:3){
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
#                run_name="phi")
#  
#}

phi_res0_prev = 
  
  phi_res0_prev_irs80_nets80 = 
  phi_res0_prev_nets80 = 
  phi_res0_prev_irs50_nets50 = 
  phi_res0_prev_nets50 = 
  phi_res0_prev_irs20_nets20 = 
  phi_res0_prev_nets20 = 
  
  phi_res20_prev_irs50_nets50 = 
  phi_res20_prev_nets50 = 
  phi_res40_prev_irs50_nets50 = 
  phi_res40_prev_nets50 = 
  phi_res60_prev_irs50_nets50 = 
  phi_res60_prev_nets50 = 
  phi_res80_prev_irs50_nets50 = 
  phi_res80_prev_nets50 = 
  
  phi_res50_prev_irs80_nets80 = 
  phi_res50_prev_nets80 = 
  phi_res50_prev_irs50_nets50 = 
  phi_res50_prev_nets50 = 
  phi_res50_prev_irs20_nets20 = 
  phi_res50_prev_nets20 = 
  
  phi_res100_prev_irs80_nets80 = 
  phi_res100_prev_nets80 = 
  phi_res100_prev_irs50_nets50 = 
  phi_res100_prev_nets50 = 
  phi_res100_prev_irs20_nets20 = 
  phi_res100_prev_nets20 = 
  
  expand.grid(time = seq(-5,20,length=1301))
for(i in 1:44){
  phi_res0_prev[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_noVecInterventions/draw_0/draw_0_noVecInterventions",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
  phi_res0_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_IRS50andLLIN50/draw_0/draw_0_res0_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res0_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_LLIN50/draw_0/draw_0_res0_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res20_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res20_IRS50andLLIN50/draw_0/draw_0_res20_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res20_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res20_LLIN50/draw_0/draw_0_res20_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res40_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res40_IRS50andLLIN50/draw_0/draw_0_res40_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res40_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res40_LLIN50/draw_0/draw_0_res40_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_IRS50andLLIN50/draw_0/draw_0_res50_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_LLIN50/draw_0/draw_0_res50_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res60_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res60_IRS50andLLIN50/draw_0/draw_0_res60_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res60_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res60_LLIN50/draw_0/draw_0_res60_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res80_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res80_IRS50andLLIN50/draw_0/draw_0_res80_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res80_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res80_LLIN50/draw_0/draw_0_res80_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_irs50_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_IRS50andLLIN50/draw_0/draw_0_res100_IRS50andLLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_nets50[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_LLIN50/draw_0/draw_0_res100_LLIN50",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
  phi_res0_prev_irs80_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_IRS80andLLIN80/draw_0/draw_0_res0_IRS80andLLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res0_prev_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_LLIN80/draw_0/draw_0_res0_LLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res0_prev_irs20_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_IRS20andLLIN20/draw_0/draw_0_res0_IRS20andLLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res0_prev_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res0_LLIN20/draw_0/draw_0_res0_LLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
  phi_res50_prev_irs80_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_IRS80andLLIN80/draw_0/draw_0_res50_IRS80andLLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_LLIN80/draw_0/draw_0_res50_LLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_irs20_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_IRS20andLLIN20/draw_0/draw_0_res50_IRS20andLLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res50_prev_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res50_LLIN20/draw_0/draw_0_res50_LLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
  phi_res100_prev_irs80_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_IRS80andLLIN80/draw_0/draw_0_res100_IRS80andLLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_nets80[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_LLIN80/draw_0/draw_0_res100_LLIN80",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_irs20_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_IRS20andLLIN20/draw_0/draw_0_res100_IRS20andLLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  phi_res100_prev_nets20[,i+1] <- read.table(paste("F:/Ellies_output_folder/phi/draw_0_res100_LLIN20/draw_0/draw_0_res100_LLIN20",inp_phi[i,1],"0.txt",sep="_"),header=TRUE)[,2]
  
}
##Estimating lost efficacy
##No resistance comparison for each season with IRS only are columns 2 to 5
##20, 50, 80 and 100 are cols 10-13, 18-21, 26-29, 34-37

vec_hs = seq(2,42,by=4)
vec_pe = vec_hs + 1
vec_se = vec_hs + 2
vec_bi = vec_hs + 3

vec_s = vec_pe
prev_res0 = prev_res0_irs20_nets20 = prev_res50_irs20_nets20 =  prev_res100_irs20_nets20 =  
  prev_res0_irs50_nets50 = prev_res50_irs50_nets50 =  prev_res100_irs50_nets50 =  
  prev_res0_irs80_nets80 = prev_res50_irs80_nets80 =  prev_res100_irs80_nets80 =  
  prev_res0_nets20 = prev_res50_nets20 = prev_res100_nets20 =  
  prev_res0_nets50 = prev_res50_nets50 = prev_res100_nets50 =   
  prev_res0_nets80 = prev_res50_nets80 = prev_res100_nets80 =  
  prev_res20_irs50_nets50 = prev_res40_irs50_nets50 = prev_res50_irs50_nets50 =
  prev_res60_irs50_nets50 = prev_res80_irs50_nets50 = prev_res100_irs50_nets50 =
  prev_res20_nets50 = prev_res40_nets50 = prev_res50_nets50 =
  prev_res60_nets50 = prev_res80_nets50 = prev_res100_nets50 = numeric(11)

for(i in 1:11){
  prev_res0[i]  = phi_res0_prev[572,vec_s[i]]
  
  prev_res0_irs20_nets20[i]  = phi_res0_prev_irs20_nets20[572,vec_s[i]]
  prev_res50_irs20_nets20[i] = phi_res50_prev_irs20_nets20[572,vec_s[i]]
  prev_res100_irs20_nets20[i] = phi_res100_prev_irs20_nets20[572,vec_s[i]]
  
  prev_res0_irs50_nets50[i]  = phi_res0_prev_irs50_nets50[572,vec_s[i]]
  prev_res50_irs50_nets50[i] = phi_res50_prev_irs50_nets50[572,vec_s[i]]
  prev_res100_irs50_nets50[i] = phi_res100_prev_irs50_nets50[572,vec_s[i]]
  
  prev_res0_irs80_nets80[i]  = phi_res0_prev_irs80_nets80[572,vec_s[i]]
  prev_res50_irs80_nets80[i] = phi_res50_prev_irs80_nets80[572,vec_s[i]]
  prev_res100_irs80_nets80[i] = phi_res100_prev_irs80_nets80[572,vec_s[i]]
  
  prev_res0_nets20[i]  = phi_res0_prev_nets20[572,vec_s[i]]
  prev_res50_nets20[i] = phi_res50_prev_nets20[572,vec_s[i]]
  prev_res100_nets20[i] = phi_res100_prev_nets20[572,vec_s[i]]
  
  prev_res0_nets50[i]  = phi_res0_prev_nets50[572,vec_s[i]]
  prev_res50_nets50[i] = phi_res50_prev_nets50[572,vec_s[i]]
  prev_res100_nets50[i] = phi_res100_prev_nets50[572,vec_s[i]]
  
  prev_res0_nets80[i]  = phi_res0_prev_nets80[572,vec_s[i]]
  prev_res50_nets80[i] = phi_res50_prev_nets80[572,vec_s[i]]
  prev_res100_nets80[i] = phi_res100_prev_nets80[572,vec_s[i]]
  
  prev_res20_irs50_nets50[i] =phi_res20_prev_irs50_nets50[572,vec_s[i]]
  prev_res40_irs50_nets50[i] =phi_res40_prev_irs50_nets50[572,vec_s[i]]
  prev_res60_irs50_nets50[i] =phi_res60_prev_irs50_nets50[572,vec_s[i]]
  prev_res80_irs50_nets50[i] =phi_res80_prev_irs50_nets50[572,vec_s[i]]
  
  prev_res20_nets50[i] =phi_res20_prev_nets50[572,vec_s[i]]
  prev_res40_nets50[i] =phi_res40_prev_nets50[572,vec_s[i]]
  prev_res60_nets50[i] =phi_res60_prev_nets50[572,vec_s[i]]
  prev_res80_nets50[i] =phi_res80_prev_nets50[572,vec_s[i]]
}

phiI = unique(inp_phi$phi_I)
phiB = unique(inp_phi$phi_B)

dat_sums = expand.grid(year=rep(6,209))
dat_sums[,2] = c(prev_res0,
                 prev_res0_irs20_nets20,prev_res50_irs20_nets20,prev_res100_irs20_nets20,
                 prev_res0_nets20,prev_res50_nets20,prev_res100_nets20,
                 prev_res0_irs50_nets50,prev_res50_irs50_nets50,prev_res100_irs50_nets50,
                 prev_res0_nets50,prev_res50_nets50,prev_res100_nets50,
                 prev_res0_irs80_nets80,prev_res50_irs80_nets80,prev_res100_irs80_nets80,
                 prev_res0_nets80,prev_res50_nets80,prev_res100_nets80)
dat_sums[,3] = rep(phiI,19)
dat_sums[,4] = rep(phiB,19)
dat_sums[,5] = c(rep(0,length(phiI)),rep(c(rep(0,length(phiI)),rep(50,length(phiI)),rep(100,length(phiI))),6)) # Resistance
dat_sums[,6] = c(rep(0,length(phiI)),
                 c(rep(20,3*length(phiI)),rep(0,3*length(phiI)),
                   rep(50,3*length(phiI)),rep(0,3*length(phiI)),
                   rep(80,3*length(phiI)),rep(0,3*length(phiI))) ) # IRS cover
dat_sums[,7] = c(rep(0,length(phiI)),rep(20,6*length(phiI)),rep(50,6*length(phiI)),rep(80,6*length(phiI)) ) # net cover
colnames(dat_sums) = c("Year","Prevalence_2_10","phiI","phiB","Resistance","IRS_cov","LLIN_cov")
head(dat_sums)
