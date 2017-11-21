
library(deSolve)
library(lhs)

proc.time()

###########################################
## Read in the data from female control group 

ATSB_exp <- read.csv("ATSB_female_experiment.csv")

times_exp <- ATSB_exp[,1]
mosq_exp <- rowSums(ATSB_exp[,2:7])
mark_exp <- ATSB_exp[,8]
unmark_exp <- mosq_exp - mark_exp

gono_exp <- read.csv("gono_exp.csv")

gono_t_exp <- gono_exp[,1]
gono_n_exp <- gono_exp[,2]

gono_exp <- gono_exp[,3:11]


###########################################
## Read in the data from female control group 

ATSB_con <- read.csv("ATSB_female_control.csv")

times_con <- ATSB_con[,1]
mosq_con <- rowSums(ATSB_con[,2:7])
mark_con <- ATSB_con[,8]
unmark_con <- mosq_con - mark_con

gono_con <- read.csv("gono_con.csv")

gono_t_con <- gono_con[,1]
gono_n_con <- gono_con[,2]

gono_con <- gono_con[,3:11]


###########################################
###########################################
## Plot and inspect the raw data


#######################################
## PANEL 1: controls

par(mfrow=c(1,2))

plot(x=times_con, y=unmark_con, type='l', ylim=c(0,max(mosq_con)),
     xlab="time (days)", ylab="mosquito catch", 
     col="forestgreen", lwd=2 )

points(x=times_con, y=mark_con, type='l',
       col="dodgerblue", lwd=2)

points(x=times_con, y=mosq_con, type='l',
       col="black", lwd=2)

points(x=c(8,8), y=c(-1000,1000), type='l', lty="longdash" )

legend(x="topright", fill=c("black", "forestgreen", "dodgerblue"),
       legend=c("total mosquito catch", "unmarked mosquito catch", "marked mosquito catch"))


#######################################
## PANEL 2: experiment

plot(x=times_exp, y=unmark_exp, type='l', ylim=c(0,max(mosq_exp)),
     xlab="time (days)", ylab="mosquito catch", 
     col="forestgreen", lwd=2 )

points(x=times_exp, y=mark_exp, type='l',
       col="dodgerblue", lwd=2)

points(x=times_exp, y=mosq_exp, type='l',
       col="black", lwd=2)

points(x=c(8,8), y=c(-1000,1000), type='l', lty="longdash" )

legend(x="topright", fill=c("black", "forestgreen", "dodgerblue"),
       legend=c("total mosquito catch", "unmarked mosquito catch", "marked mosquito catch"))


#####################################
#####################################
##        ##                       ##
##   ##   ##  #     # ##    #####  ##
##  ###   ##  ##   ## ##    ##     ##
##   ##   ##  ####### ##    ####   ##
##   ##   ##  ## # ## ##    ##     ##
##  ####  ##  ##   ## ##### #####  ##
##        ##                       ##
#####################################
#####################################

library(lhs)
N_lhs <- 1

#############################################
## Create a function that does the following:
## 1. Calculates the equilibrium age-grading distibution
## 2. Sets up a system of differential equations for the sugar bait model
## 3. Calculates predicted mosquito numbers on the same days where we have observations
## 4. Calculates the prior likelihood for muf (healthy female mosquito death rate)  
## 7. Likelihood of model fit to the control data
## 8. Likelihood of model fit to the experiment data
##
## 9. Put the five functions together



step <- 1

ode_t <- seq(from=0, to=40, by=step) 


##################################################
##                                              ##
## MODEL 1: linear change in sugar feeding rate ##
##                                              ##
##################################################

mod_gono1 <- function( params ){
  
  Nf_con <- params[1]
  Nf_exp <- params[2]
  sf_con <- params[3]
  sf_exp <- params[4]
  muf    <- params[5]
  muf_A  <- params[6]
  r_con  <- params[7] 
  r_exp  <- params[8] 
  delta  <- params[9]
  m      <- params[10]
  
  #####################################
  ## First we need to calculate the equilibrium distribution of
  ## mosquitoes broken down by gonotrophic cycle pre-intervention
  
  Uf_con_start <- Nf_con*exp( -(0:7)*muf/delta )*( 1-exp(-muf/delta) )
  Uf_con_start <- c( Uf_con_start, Nf_con*exp(-8*muf/delta) )
  
  Mf_con_start <- rep(0, 9)
  
  Uf_exp_start <- Nf_exp*exp( -(0:7)*muf/delta )*( 1-exp(-muf/delta) )
  Uf_exp_start <- c( Uf_exp_start, Nf_exp*exp(-8*muf/delta) )
  
  Mf_exp_start <- rep(0, 9)
  
  start <- c(Uf_con = Uf_con_start,
             Mf_con = Mf_con_start,
             Uf_exp = Uf_exp_start,
             Mf_exp = Mf_exp_start )
  
  
  ###################################################
  ## Define mosquito life cycle model with gonotrophic cycle
  ## number tracked within it
  
  par_ode <- c(Nf_con = Nf_con, Nf_exp = Nf_exp, sf_con = sf_con, sf_exp = sf_exp,
               muf = muf, muf_A = muf_A, r_con = r_con, r_exp = r_exp, delta = delta, m = m)
  
  gono.model <- function(t, x, par_ode){
    with(as.list(par_ode),{
      
      
      #####################################
      ## Declare variables
      
      dUf_con   <- rep(0, 9)
      dMf_con   <- rep(0, 9)
      
      dUf_exp   <- rep(0, 9)
      dMf_exp   <- rep(0, 9)
      
      Uf_con <- x[1:9]
      Mf_con <- x[10:18]
      
      Uf_exp <- x[19:27]
      Mf_exp <- x[28:36]
      
      
      #########################################
      ## Differential equations
      
      sf_con_t <- sf_con*(1 + (0:8)*m )
      sf_exp_t <- sf_exp*(1 + (0:8)*m )
      
      if( t < 8 ){ 
        sf_con_t <- rep(0, 9)
        sf_exp_t <- rep(0, 9) 
      }
      
      dUf_con[1] <- muf*Nf_con - (sf_con_t[1] + muf + delta)*Uf_con[1]
      dMf_con[1] <- sf_con_t[1]*Uf_con[1] - (muf + delta)*Mf_con[1]
      
      dUf_con[2:8] <- delta*Uf_con[(2:8)-1] - (sf_con_t[2:8] + muf + delta)*Uf_con[2:8]
      dMf_con[2:8] <- delta*Mf_con[(2:8)-1] + sf_con_t[2:8]*Uf_con[2:8] - (muf + delta)*Mf_con[2:8]
      
      dUf_con[9] <- delta*Uf_con[8] - (sf_con_t[9] + muf)*Uf_con[9]
      dMf_con[9] <- delta*Mf_con[8] + sf_con_t[9]*Uf_con[9] - muf*Mf_con[9] 
      
      
      dUf_exp[1] <- muf*Nf_exp - (sf_exp_t[1] + muf + delta)*Uf_exp[1]
      dMf_exp[1] <- sf_exp_t[1]*Uf_exp[1] - (muf_A + delta)*Mf_exp[1]
      
      dUf_exp[2:8] <- delta*Uf_exp[(2:8)-1] - (sf_exp_t[2:8] + muf + delta)*Uf_exp[2:8]
      dMf_exp[2:8] <- delta*Mf_exp[(2:8)-1] + sf_exp_t[2:8]*Uf_exp[2:8] - (muf_A + delta)*Mf_exp[2:8]
      
      dUf_exp[9] <- delta*Uf_exp[8] - (sf_exp_t[9] + muf)*Uf_exp[9]
      dMf_exp[9] <- delta*Mf_exp[8] + sf_exp_t[9]*Uf_exp[9] - muf_A*Mf_exp[9]
      
      #####################################
      ## Return output in list format
      
      list( c(dUf_con, dMf_con, dUf_exp, dMf_exp))
      
    })
  }
  
  
  #################################################
  ## Solve the set of differential equations and 
  ## format output                               
  
  gono.out <- as.data.frame(lsoda(y=start, times=ode_t, func=gono.model, parm=par_ode))
  
  data.index <- rep(NA, length(times_con))
  for(i in 1:length(data.index)){
    data.index[i] <- which( gono.out[,1]==times_con[i] )
  }
  
  gono.out <- gono.out[data.index,]
  gono.out <- as.matrix(gono.out)
  
  
  zero.index <- which(gono.out < 0.001)
  
  if( length(zero.index) > 0 ){
    gono.out[zero.index] <- 0.001
  }
  
  
  mod_unmark_con <- rowSums(gono.out[,2:10])
  mod_mark_con <- rowSums(gono.out[,11:19])
  mod_mark_con <- mod_mark_con[-which(times_con<=8)]
  
  mod_unmark_exp <- rowSums(gono.out[,20:28])
  mod_mark_exp <- rowSums(gono.out[,29:37])
  mod_mark_exp <- mod_mark_exp[-which(times_con<=8)]
  
  
  ##############################
  ## Prior distribution on muf (healthy female mosquito death rate)
  
  mean_muf <- 0.1
  sd_muf   <- 0.01
  
  muf_prior <- -0.5*log(2*pi*sd_muf^2) - 0.5*( (muf-mean_muf)/sd_muf )^2
  
  
  ##############################
  ## Prior distribution on delta (gonotrophic cycle rate)
  
  mean_delta <- 0.33
  sd_delta   <- 0.03
  
  delta_prior <- -0.5*log(2*pi*sd_delta^2) - 0.5*( (delta-mean_delta)/sd_delta )^2
  
  
  ####################################
  ## Negative binomial likelihood
  
  unmark_con_data <- unmark_con
  mark_con_data <- mark_con[-which(times_con<=8)]
  
  unmark_exp_data <- unmark_exp
  mark_exp_data <- mark_exp[-which(times_con<=8)]
  
  pUf_con <- mod_unmark_con/(mod_unmark_con+r_con)
  pMf_con <- mod_mark_con/(mod_mark_con+r_con)
  
  pUf_exp <- mod_unmark_exp/(mod_unmark_exp+r_exp)
  pMf_exp <- mod_mark_exp/(mod_mark_exp+r_exp)
  
  like_NB <-  sum( lchoose(mark_con_data+r_con-1, mark_con_data) + r_con*log(1-pMf_con) + mark_con_data*log(pMf_con) ) + 
    sum( lchoose(unmark_con_data+r_con-1, unmark_con_data) + r_con*log(1-pUf_con) + unmark_con_data*log(pUf_con) ) +
    sum( lchoose(mark_exp_data+r_exp-1, mark_exp_data) + r_exp*log(1-pMf_exp) + mark_exp_data*log(pMf_exp) ) + 
    sum( lchoose(unmark_exp_data+r_exp-1, unmark_exp_data) + r_exp*log(1-pUf_exp) + unmark_exp_data*log(pUf_exp) )
  
  ##############################
  ## Multinomial likelihood
  
  gono_pred_con <- matrix(NA, nrow=nrow(gono_con), ncol=ncol(gono_con))
  gono_pred_exp <- matrix(NA, nrow=nrow(gono_exp), ncol=ncol(gono_exp))
  
  for(i in 1:length(gono_t_exp)){
    temp_con <- gono.out[which(gono.out[,1]==gono_t_con[i]),2:19]	
    temp_con <- temp_con[1:9] + temp_con[10:18]
    
    gono_pred_con[i,] <- as.numeric(temp_con)
    
    temp_exp <- gono.out[which(gono.out[,1]==gono_t_exp[i]),20:37]	
    temp_exp <- temp_exp[1:9] + temp_exp[10:18]
    
    gono_pred_exp[i,] <- as.numeric(temp_exp)
  }
  
  for(i in 1:nrow(gono_pred_con)){
    gono_pred_con[i,] <- gono_pred_con[i,]/sum(gono_pred_con[i,])
    gono_pred_exp[i,] <- gono_pred_exp[i,]/sum(gono_pred_exp[i,])
  }
  
  like_ML <- 0
  
  for(i in 1:nrow(gono_pred_con)){
    like_ML <- like_ML + lfactorial(gono_n_con[i]) + sum( gono_con[i,]*log(gono_pred_con[i,]) - lfactorial(gono_con[i,]) )
    + lfactorial(gono_n_exp[i]) + sum( gono_exp[i,]*log(gono_pred_exp[i,]) - lfactorial(gono_exp[i,]) )
  }
  
  total_like <- muf_prior +  delta_prior + like_NB + like_ML
  
  -total_like
}


############################################################################
## Use the optimisation routine constrOptim to search the constrained
## parameter space described above for the set of parameters that provides
## the best fit to the data. The algorithm is run N_lhs times to make
## sure that the real optimum is found.

########################################
# Bounds for constrained optimisation

lower <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, -1/8 )
upper <- c(10000, 10000, 100, 100, 100, 100, 100, 100, 100, 1)

ui <- rbind( diag(10), -diag(10) )
ci <- c( lower, -upper ) 

theta <- randomLHS(N_lhs,10)
theta <- t(lower + t(theta)*(upper-lower))

MLE_max <- 10000000


for(j in 1:N_lhs){
  MLE_gono1 <- constrOptim(theta=theta[j,], f=mod_gono1, grad=NULL, ui=ui, ci=ci)
  
  if( MLE_gono1$value < MLE_max ){ 
    best_gono1 <- MLE_gono1
    MLE_max   <- MLE_gono1$value
  }
  
}




##########################################################
##                                                      ##
## MODEL 2: multiplicative change in sugar feeding rate ##
##                                                      ##
##########################################################

mod_gono2 <- function( params ){
  
  Nf_con <- params[1]
  Nf_exp <- params[2]
  sf_con <- params[3]
  sf_exp <- params[4]
  muf    <- params[5]
  muf_A  <- params[6]
  r_con  <- params[7] 
  r_exp  <- params[8] 
  delta  <- params[9]
  m      <- params[10]
  
  #####################################
  ## First we need to calculate the equilibrium distribution of
  ## mosquitoes broken down by gonotrophic cycle pre-intervention
  
  Uf_con_start <- Nf_con*exp( -(0:7)*muf/delta )*( 1-exp(-muf/delta) )
  Uf_con_start <- c( Uf_con_start, Nf_con*exp(-8*muf/delta) )
  
  Mf_con_start <- rep(0, 9)
  
  Uf_exp_start <- Nf_exp*exp( -(0:7)*muf/delta )*( 1-exp(-muf/delta) )
  Uf_exp_start <- c( Uf_exp_start, Nf_exp*exp(-8*muf/delta) )
  
  Mf_exp_start <- rep(0, 9)
  
  start <- c(Uf_con = Uf_con_start,
             Mf_con = Mf_con_start,
             Uf_exp = Uf_exp_start,
             Mf_exp = Mf_exp_start )
  
  
  ###################################################
  ## Define mosquito life cycle model with gonotrophic cycle
  ## number tracked within it
  
  par_ode <- c(Nf_con = Nf_con, Nf_exp = Nf_exp, sf_con = sf_con, sf_exp = sf_exp,
               muf = muf, muf_A = muf_A, r_con = r_con, r_exp = r_exp, delta = delta, m = m)
  
  gono.model <- function(t, x, par_ode){
    with(as.list(par_ode),{
      
      
      #####################################
      ## Declare variables
      
      dUf_con   <- rep(0, 9)
      dMf_con   <- rep(0, 9)
      
      dUf_exp   <- rep(0, 9)
      dMf_exp   <- rep(0, 9)
      
      Uf_con <- x[1:9]
      Mf_con <- x[10:18]
      
      Uf_exp <- x[19:27]
      Mf_exp <- x[28:36]
      
      
      #########################################
      ## Differential equations
      
      sf_con_t <- sf_con*( m^(0:8) )
      sf_exp_t <- sf_exp*( m^(0:8) )
      
      if( t < 8 ){ 
        sf_con_t <- rep(0, 9)
        sf_exp_t <- rep(0, 9) 
      }
      
      dUf_con[1] <- muf*Nf_con - (sf_con_t[1] + muf + delta)*Uf_con[1]
      dMf_con[1] <- sf_con_t[1]*Uf_con[1] - (muf + delta)*Mf_con[1]
      
      dUf_con[2:8] <- delta*Uf_con[(2:8)-1] - (sf_con_t[2:8] + muf + delta)*Uf_con[2:8]
      dMf_con[2:8] <- delta*Mf_con[(2:8)-1] + sf_con_t[2:8]*Uf_con[2:8] - (muf + delta)*Mf_con[2:8]
      
      dUf_con[9] <- delta*Uf_con[8] - (sf_con_t[9] + muf)*Uf_con[9]
      dMf_con[9] <- delta*Mf_con[8] + sf_con_t[9]*Uf_con[9] - muf*Mf_con[9] 
      
      
      dUf_exp[1] <- muf*Nf_exp - (sf_exp_t[1] + muf + delta)*Uf_exp[1]
      dMf_exp[1] <- sf_exp_t[1]*Uf_exp[1] - (muf_A + delta)*Mf_exp[1]
      
      dUf_exp[2:8] <- delta*Uf_exp[(2:8)-1] - (sf_exp_t[2:8] + muf + delta)*Uf_exp[2:8]
      dMf_exp[2:8] <- delta*Mf_exp[(2:8)-1] + sf_exp_t[2:8]*Uf_exp[2:8] - (muf_A + delta)*Mf_exp[2:8]
      
      dUf_exp[9] <- delta*Uf_exp[8] - (sf_exp_t[9] + muf)*Uf_exp[9]
      dMf_exp[9] <- delta*Mf_exp[8] + sf_exp_t[9]*Uf_exp[9] - muf_A*Mf_exp[9]
      
      #####################################
      ## Return output in list format
      
      list( c(dUf_con, dMf_con, dUf_exp, dMf_exp))
      
    })
  }
  
  
  #################################################
  ## Solve the set of differential equations and 
  ## format output                               
  
  gono.out <- as.data.frame(lsoda(y=start, times=ode_t, func=gono.model, parm=par_ode))
  
  data.index <- rep(NA, length(times_con))
  for(i in 1:length(data.index)){
    data.index[i] <- which( gono.out[,1]==times_con[i] )
  }
  
  gono.out <- gono.out[data.index,]
  gono.out <- as.matrix(gono.out)
  
  
  zero.index <- which(gono.out < 0.001)
  
  if( length(zero.index) > 0 ){
    gono.out[zero.index] <- 0.001
  }
  
  
  mod_unmark_con <- rowSums(gono.out[,2:10])
  mod_mark_con <- rowSums(gono.out[,11:19])
  mod_mark_con <- mod_mark_con[-which(times_con<=8)]
  
  mod_unmark_exp <- rowSums(gono.out[,20:28])
  mod_mark_exp <- rowSums(gono.out[,29:37])
  mod_mark_exp <- mod_mark_exp[-which(times_con<=8)]
  
  
  ##############################
  ## Prior distribution on muf (healthy female mosquito death rate)
  
  mean_muf <- 0.1
  sd_muf   <- 0.01
  
  muf_prior <- -0.5*log(2*pi*sd_muf^2) - 0.5*( (muf-mean_muf)/sd_muf )^2
  
  
  ##############################
  ## Prior distribution on delta (gonotrophic cycle rate)
  
  mean_delta <- 0.33
  sd_delta   <- 0.03
  
  delta_prior <- -0.5*log(2*pi*sd_delta^2) - 0.5*( (delta-mean_delta)/sd_delta )^2
  
  
  ####################################
  ## Negative binomial likelihood
  
  unmark_con_data <- unmark_con
  mark_con_data <- mark_con[-which(times_con<=8)]
  
  unmark_exp_data <- unmark_exp
  mark_exp_data <- mark_exp[-which(times_con<=8)]
  
  pUf_con <- mod_unmark_con/(mod_unmark_con+r_con)
  pMf_con <- mod_mark_con/(mod_mark_con+r_con)
  
  pUf_exp <- mod_unmark_exp/(mod_unmark_exp+r_exp)
  pMf_exp <- mod_mark_exp/(mod_mark_exp+r_exp)
  
  like_NB <-  sum( lchoose(mark_con_data+r_con-1, mark_con_data) + r_con*log(1-pMf_con) + mark_con_data*log(pMf_con) ) + 
    sum( lchoose(unmark_con_data+r_con-1, unmark_con_data) + r_con*log(1-pUf_con) + unmark_con_data*log(pUf_con) ) +
    sum( lchoose(mark_exp_data+r_exp-1, mark_exp_data) + r_exp*log(1-pMf_exp) + mark_exp_data*log(pMf_exp) ) + 
    sum( lchoose(unmark_exp_data+r_exp-1, unmark_exp_data) + r_exp*log(1-pUf_exp) + unmark_exp_data*log(pUf_exp) )
  
  ##############################
  ## Multinomial likelihood
  
  gono_pred_con <- matrix(NA, nrow=nrow(gono_con), ncol=ncol(gono_con))
  gono_pred_exp <- matrix(NA, nrow=nrow(gono_exp), ncol=ncol(gono_exp))
  
  for(i in 1:length(gono_t_exp)){
    temp_con <- gono.out[which(gono.out[,1]==gono_t_con[i]),2:19]	
    temp_con <- temp_con[1:9] + temp_con[10:18]
    
    gono_pred_con[i,] <- as.numeric(temp_con)
    
    temp_exp <- gono.out[which(gono.out[,1]==gono_t_exp[i]),20:37]	
    temp_exp <- temp_exp[1:9] + temp_exp[10:18]
    
    gono_pred_exp[i,] <- as.numeric(temp_exp)
  }
  
  for(i in 1:nrow(gono_pred_con)){
    gono_pred_con[i,] <- gono_pred_con[i,]/sum(gono_pred_con[i,])
    gono_pred_exp[i,] <- gono_pred_exp[i,]/sum(gono_pred_exp[i,])
  }
  
  like_ML <- 0
  
  for(i in 1:nrow(gono_pred_con)){
    like_ML <- like_ML + lfactorial(gono_n_con[i]) + sum( gono_con[i,]*log(gono_pred_con[i,]) - lfactorial(gono_con[i,]) )
    + lfactorial(gono_n_exp[i]) + sum( gono_exp[i,]*log(gono_pred_exp[i,]) - lfactorial(gono_exp[i,]) )
  }
  
  total_like <- muf_prior +  delta_prior + like_NB + like_ML
  
  -total_like
}


############################################################################
## Use the optimisation routine constrOptim to search the constrained
## parameter space described above for the set of parameters that provides
## the best fit to the data. The algorithm is run N_lhs times to make
## sure that the real optimum is found.

########################################
# Bounds for constrained optimisation

lower <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
upper <- c(10000, 10000, 100, 100, 100, 100, 100, 100, 100, 1.5)

ui <- rbind( diag(10), -diag(10) )
ci <- c( lower, -upper ) 

theta <- randomLHS(N_lhs,10)
theta <- t(lower + t(theta)*(upper-lower))

MLE_max <- 10000000


for(j in 1:N_lhs){
  MLE_gono2 <- constrOptim(theta=theta[j,], f=mod_gono2, grad=NULL, ui=ui, ci=ci)
  
  if( MLE_gono2$value < MLE_max ){ 
    best_gono2 <- MLE_gono2
    MLE_max   <- MLE_gono2$value
  }
  
}

##########################################################
##                                                      ##
## MODEL 3: change in sugar feeding rate after 3 cycles ##
##                                                      ##
##########################################################

mod_gono3 <- function( params ){
  
  Nf_con <- params[1]
  Nf_exp <- params[2]
  sf_con <- params[3]
  sf_exp <- params[4]
  muf    <- params[5]
  muf_A  <- params[6]
  r_con  <- params[7] 
  r_exp  <- params[8] 
  delta  <- params[9]
  m      <- params[10]
  
  #####################################
  ## First we need to calculate the equilibrium distribution of
  ## mosquitoes broken down by gonotrophic cycle pre-intervention
  
  Uf_con_start <- Nf_con*exp( -(0:7)*muf/delta )*( 1-exp(-muf/delta) )
  Uf_con_start <- c( Uf_con_start, Nf_con*exp(-8*muf/delta) )
  
  Mf_con_start <- rep(0, 9)
  
  Uf_exp_start <- Nf_exp*exp( -(0:7)*muf/delta )*( 1-exp(-muf/delta) )
  Uf_exp_start <- c( Uf_exp_start, Nf_exp*exp(-8*muf/delta) )
  
  Mf_exp_start <- rep(0, 9)
  
  start <- c(Uf_con = Uf_con_start,
             Mf_con = Mf_con_start,
             Uf_exp = Uf_exp_start,
             Mf_exp = Mf_exp_start )
  
  
  ###################################################
  ## Define mosquito life cycle model with gonotrophic cycle
  ## number tracked within it
  
  par_ode <- c(Nf_con = Nf_con, Nf_exp = Nf_exp, sf_con = sf_con, sf_exp = sf_exp,
               muf = muf, muf_A = muf_A, r_con = r_con, r_exp = r_exp, delta = delta, m = m)
  
  gono.model <- function(t, x, par_ode){
    with(as.list(par_ode),{
      
      
      #####################################
      ## Declare variables
      
      dUf_con   <- rep(0, 9)
      dMf_con   <- rep(0, 9)
      
      dUf_exp   <- rep(0, 9)
      dMf_exp   <- rep(0, 9)
      
      Uf_con <- x[1:9]
      Mf_con <- x[10:18]
      
      Uf_exp <- x[19:27]
      Mf_exp <- x[28:36]
      
      
      #########################################
      ## Differential equations
      
      sf_con_t <- sf_con*c( rep(1,3), rep(m,6) )
      sf_exp_t <- sf_exp*c( rep(1,3), rep(m,6) )
      
      if( t < 8 ){ 
        sf_con_t <- rep(0, 9)
        sf_exp_t <- rep(0, 9) 
      }
      
      dUf_con[1] <- muf*Nf_con - (sf_con_t[1] + muf + delta)*Uf_con[1]
      dMf_con[1] <- sf_con_t[1]*Uf_con[1] - (muf + delta)*Mf_con[1]
      
      dUf_con[2:8] <- delta*Uf_con[(2:8)-1] - (sf_con_t[2:8] + muf + delta)*Uf_con[2:8]
      dMf_con[2:8] <- delta*Mf_con[(2:8)-1] + sf_con_t[2:8]*Uf_con[2:8] - (muf + delta)*Mf_con[2:8]
      
      dUf_con[9] <- delta*Uf_con[8] - (sf_con_t[9] + muf)*Uf_con[9]
      dMf_con[9] <- delta*Mf_con[8] + sf_con_t[9]*Uf_con[9] - muf*Mf_con[9] 
      
      
      dUf_exp[1] <- muf*Nf_exp - (sf_exp_t[1] + muf + delta)*Uf_exp[1]
      dMf_exp[1] <- sf_exp_t[1]*Uf_exp[1] - (muf_A + delta)*Mf_exp[1]
      
      dUf_exp[2:8] <- delta*Uf_exp[(2:8)-1] - (sf_exp_t[2:8] + muf + delta)*Uf_exp[2:8]
      dMf_exp[2:8] <- delta*Mf_exp[(2:8)-1] + sf_exp_t[2:8]*Uf_exp[2:8] - (muf_A + delta)*Mf_exp[2:8]
      
      dUf_exp[9] <- delta*Uf_exp[8] - (sf_exp_t[9] + muf)*Uf_exp[9]
      dMf_exp[9] <- delta*Mf_exp[8] + sf_exp_t[9]*Uf_exp[9] - muf_A*Mf_exp[9]
      
      #####################################
      ## Return output in list format
      
      list( c(dUf_con, dMf_con, dUf_exp, dMf_exp))
      
    })
  }
  
  
  #################################################
  ## Solve the set of differential equations and 
  ## format output                               
  
  gono.out <- as.data.frame(lsoda(y=start, times=ode_t, func=gono.model, parm=par_ode))
  
  data.index <- rep(NA, length(times_con))
  for(i in 1:length(data.index)){
    data.index[i] <- which( gono.out[,1]==times_con[i] )
  }
  
  gono.out <- gono.out[data.index,]
  gono.out <- as.matrix(gono.out)
  
  
  zero.index <- which(gono.out < 0.001)
  
  if( length(zero.index) > 0 ){
    gono.out[zero.index] <- 0.001
  }
  
  
  mod_unmark_con <- rowSums(gono.out[,2:10])
  mod_mark_con <- rowSums(gono.out[,11:19])
  mod_mark_con <- mod_mark_con[-which(times_con<=8)]
  
  mod_unmark_exp <- rowSums(gono.out[,20:28])
  mod_mark_exp <- rowSums(gono.out[,29:37])
  mod_mark_exp <- mod_mark_exp[-which(times_con<=8)]
  
  
  ##############################
  ## Prior distribution on muf (healthy female mosquito death rate)
  
  mean_muf <- 0.1
  sd_muf   <- 0.01
  
  muf_prior <- -0.5*log(2*pi*sd_muf^2) - 0.5*( (muf-mean_muf)/sd_muf )^2
  
  
  ##############################
  ## Prior distribution on delta (gonotrophic cycle rate)
  
  mean_delta <- 0.33
  sd_delta   <- 0.03
  
  delta_prior <- -0.5*log(2*pi*sd_delta^2) - 0.5*( (delta-mean_delta)/sd_delta )^2
  
  
  ####################################
  ## Negative binomial likelihood
  
  unmark_con_data <- unmark_con
  mark_con_data <- mark_con[-which(times_con<=8)]
  
  unmark_exp_data <- unmark_exp
  mark_exp_data <- mark_exp[-which(times_con<=8)]
  
  pUf_con <- mod_unmark_con/(mod_unmark_con+r_con)
  pMf_con <- mod_mark_con/(mod_mark_con+r_con)
  
  pUf_exp <- mod_unmark_exp/(mod_unmark_exp+r_exp)
  pMf_exp <- mod_mark_exp/(mod_mark_exp+r_exp)
  
  like_NB <-  sum( lchoose(mark_con_data+r_con-1, mark_con_data) + r_con*log(1-pMf_con) + mark_con_data*log(pMf_con) ) + 
    sum( lchoose(unmark_con_data+r_con-1, unmark_con_data) + r_con*log(1-pUf_con) + unmark_con_data*log(pUf_con) ) +
    sum( lchoose(mark_exp_data+r_exp-1, mark_exp_data) + r_exp*log(1-pMf_exp) + mark_exp_data*log(pMf_exp) ) + 
    sum( lchoose(unmark_exp_data+r_exp-1, unmark_exp_data) + r_exp*log(1-pUf_exp) + unmark_exp_data*log(pUf_exp) )
  
  ##############################
  ## Multinomial likelihood
  
  gono_pred_con <- matrix(NA, nrow=nrow(gono_con), ncol=ncol(gono_con))
  gono_pred_exp <- matrix(NA, nrow=nrow(gono_exp), ncol=ncol(gono_exp))
  
  for(i in 1:length(gono_t_exp)){
    temp_con <- gono.out[which(gono.out[,1]==gono_t_con[i]),2:19]	
    temp_con <- temp_con[1:9] + temp_con[10:18]
    
    gono_pred_con[i,] <- as.numeric(temp_con)
    
    temp_exp <- gono.out[which(gono.out[,1]==gono_t_exp[i]),20:37]	
    temp_exp <- temp_exp[1:9] + temp_exp[10:18]
    
    gono_pred_exp[i,] <- as.numeric(temp_exp)
  }
  
  for(i in 1:nrow(gono_pred_con)){
    gono_pred_con[i,] <- gono_pred_con[i,]/sum(gono_pred_con[i,])
    gono_pred_exp[i,] <- gono_pred_exp[i,]/sum(gono_pred_exp[i,])
  }
  
  like_ML <- 0
  
  for(i in 1:nrow(gono_pred_con)){
    like_ML <- like_ML + lfactorial(gono_n_con[i]) + sum( gono_con[i,]*log(gono_pred_con[i,]) - lfactorial(gono_con[i,]) )
    + lfactorial(gono_n_exp[i]) + sum( gono_exp[i,]*log(gono_pred_exp[i,]) - lfactorial(gono_exp[i,]) )
  }
  
  total_like <- muf_prior +  delta_prior + like_NB + like_ML
  
  -total_like
}


############################################################################
## Use the optimisation routine constrOptim to search the constrained
## parameter space described above for the set of parameters that provides
## the best fit to the data. The algorithm is run N_lhs times to make
## sure that the real optimum is found.

########################################
# Bounds for constrained optimisation

lower <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
upper <- c(10000, 10000, 100, 100, 100, 100, 100, 100, 100, 10)

ui <- rbind( diag(10), -diag(10) )
ci <- c( lower, -upper ) 

theta <- randomLHS(N_lhs,10)
theta <- t(lower + t(theta)*(upper-lower))

MLE_max <- 10000000


for(j in 1:N_lhs){
  MLE_gono3 <- constrOptim(theta=theta[j,], f=mod_gono3, grad=NULL, ui=ui, ci=ci)
  
  if( MLE_gono3$value < MLE_max ){ 
    best_gono3 <- MLE_gono3
    MLE_max   <- MLE_gono3$value
  }
  
}



##########################################################
##                                                      ##
## MODEL 4: change in sugar feeding rate after 3 cycles ##
##                                                      ##
##########################################################

mod_gono4 <- function( params ){
  
  Nf_con <- params[1]
  Nf_exp <- params[2]
  sf_con <- params[3]
  sf_exp <- params[4]
  muf    <- params[5]
  muf_A  <- params[6]
  r_con  <- params[7] 
  r_exp  <- params[8] 
  delta  <- params[9]
  m35    <- params[10]
  m6p    <- params[11]
  
  
  #####################################
  ## First we need to calculate the equilibrium distribution of
  ## mosquitoes broken down by gonotrophic cycle pre-intervention
  
  Uf_con_start <- Nf_con*exp( -(0:7)*muf/delta )*( 1-exp(-muf/delta) )
  Uf_con_start <- c( Uf_con_start, Nf_con*exp(-8*muf/delta) )
  
  Mf_con_start <- rep(0, 9)
  
  Uf_exp_start <- Nf_exp*exp( -(0:7)*muf/delta )*( 1-exp(-muf/delta) )
  Uf_exp_start <- c( Uf_exp_start, Nf_exp*exp(-8*muf/delta) )
  
  Mf_exp_start <- rep(0, 9)
  
  start <- c(Uf_con = Uf_con_start,
             Mf_con = Mf_con_start,
             Uf_exp = Uf_exp_start,
             Mf_exp = Mf_exp_start )
  
  
  ###################################################
  ## Define mosquito life cycle model with gonotrophic cycle
  ## number tracked within it
  
  par_ode <- c(Nf_con = Nf_con, Nf_exp = Nf_exp, sf_con = sf_con, sf_exp = sf_exp,
               muf = muf, muf_A = muf_A, r_con = r_con, r_exp = r_exp, delta = delta, m35 = m35, m6p = m6p)
  
  gono.model <- function(t, x, par_ode){
    with(as.list(par_ode),{
      
      
      #####################################
      ## Declare variables
      
      dUf_con   <- rep(0, 9)
      dMf_con   <- rep(0, 9)
      
      dUf_exp   <- rep(0, 9)
      dMf_exp   <- rep(0, 9)
      
      Uf_con <- x[1:9]
      Mf_con <- x[10:18]
      
      Uf_exp <- x[19:27]
      Mf_exp <- x[28:36]
      
      
      #########################################
      ## Differential equations
      
      sf_con_t <- sf_con*c( rep(1,3), rep(m35,3), rep(m6p,3) )
      sf_exp_t <- sf_exp*c( rep(1,3), rep(m35,3), rep(m6p,3) )
      
      if( t < 8 ){ 
        sf_con_t <- rep(0, 9)
        sf_exp_t <- rep(0, 9) 
      }
      
      dUf_con[1] <- muf*Nf_con - (sf_con_t[1] + muf + delta)*Uf_con[1]
      dMf_con[1] <- sf_con_t[1]*Uf_con[1] - (muf + delta)*Mf_con[1]
      
      dUf_con[2:8] <- delta*Uf_con[(2:8)-1] - (sf_con_t[2:8] + muf + delta)*Uf_con[2:8]
      dMf_con[2:8] <- delta*Mf_con[(2:8)-1] + sf_con_t[2:8]*Uf_con[2:8] - (muf + delta)*Mf_con[2:8]
      
      dUf_con[9] <- delta*Uf_con[8] - (sf_con_t[9] + muf)*Uf_con[9]
      dMf_con[9] <- delta*Mf_con[8] + sf_con_t[9]*Uf_con[9] - muf*Mf_con[9] 
      
      
      dUf_exp[1] <- muf*Nf_exp - (sf_exp_t[1] + muf + delta)*Uf_exp[1]
      dMf_exp[1] <- sf_exp_t[1]*Uf_exp[1] - (muf_A + delta)*Mf_exp[1]
      
      dUf_exp[2:8] <- delta*Uf_exp[(2:8)-1] - (sf_exp_t[2:8] + muf + delta)*Uf_exp[2:8]
      dMf_exp[2:8] <- delta*Mf_exp[(2:8)-1] + sf_exp_t[2:8]*Uf_exp[2:8] - (muf_A + delta)*Mf_exp[2:8]
      
      dUf_exp[9] <- delta*Uf_exp[8] - (sf_exp_t[9] + muf)*Uf_exp[9]
      dMf_exp[9] <- delta*Mf_exp[8] + sf_exp_t[9]*Uf_exp[9] - muf_A*Mf_exp[9]
      
      #####################################
      ## Return output in list format
      
      list( c(dUf_con, dMf_con, dUf_exp, dMf_exp))
      
    })
  }
  
  
  #################################################
  ## Solve the set of differential equations and 
  ## format output                               
  
  gono.out <- as.data.frame(lsoda(y=start, times=ode_t, func=gono.model, parm=par_ode))
  
  data.index <- rep(NA, length(times_con))
  for(i in 1:length(data.index)){
    data.index[i] <- which( gono.out[,1]==times_con[i] )
  }
  
  gono.out <- gono.out[data.index,]
  gono.out <- as.matrix(gono.out)
  
  
  zero.index <- which(gono.out < 0.001)
  
  if( length(zero.index) > 0 ){
    gono.out[zero.index] <- 0.001
  }
  
  
  mod_unmark_con <- rowSums(gono.out[,2:10])
  mod_mark_con <- rowSums(gono.out[,11:19])
  mod_mark_con <- mod_mark_con[-which(times_con<=8)]
  
  mod_unmark_exp <- rowSums(gono.out[,20:28])
  mod_mark_exp <- rowSums(gono.out[,29:37])
  mod_mark_exp <- mod_mark_exp[-which(times_con<=8)]
  
  
  ##############################
  ## Prior distribution on muf (healthy female mosquito death rate)
  
  mean_muf <- 0.1
  sd_muf   <- 0.01
  
  muf_prior <- -0.5*log(2*pi*sd_muf^2) - 0.5*( (muf-mean_muf)/sd_muf )^2
  
  
  ##############################
  ## Prior distribution on delta (gonotrophic cycle rate)
  
  mean_delta <- 0.33
  sd_delta   <- 0.03
  
  delta_prior <- -0.5*log(2*pi*sd_delta^2) - 0.5*( (delta-mean_delta)/sd_delta )^2
  
  
  ####################################
  ## Negative binomial likelihood
  
  unmark_con_data <- unmark_con
  mark_con_data <- mark_con[-which(times_con<=8)]
  
  unmark_exp_data <- unmark_exp
  mark_exp_data <- mark_exp[-which(times_con<=8)]
  
  pUf_con <- mod_unmark_con/(mod_unmark_con+r_con)
  pMf_con <- mod_mark_con/(mod_mark_con+r_con)
  
  pUf_exp <- mod_unmark_exp/(mod_unmark_exp+r_exp)
  pMf_exp <- mod_mark_exp/(mod_mark_exp+r_exp)
  
  like_NB <-  sum( lchoose(mark_con_data+r_con-1, mark_con_data) + r_con*log(1-pMf_con) + mark_con_data*log(pMf_con) ) + 
    sum( lchoose(unmark_con_data+r_con-1, unmark_con_data) + r_con*log(1-pUf_con) + unmark_con_data*log(pUf_con) ) +
    sum( lchoose(mark_exp_data+r_exp-1, mark_exp_data) + r_exp*log(1-pMf_exp) + mark_exp_data*log(pMf_exp) ) + 
    sum( lchoose(unmark_exp_data+r_exp-1, unmark_exp_data) + r_exp*log(1-pUf_exp) + unmark_exp_data*log(pUf_exp) )
  
  ##############################
  ## Multinomial likelihood
  
  gono_pred_con <- matrix(NA, nrow=nrow(gono_con), ncol=ncol(gono_con))
  gono_pred_exp <- matrix(NA, nrow=nrow(gono_exp), ncol=ncol(gono_exp))
  
  for(i in 1:length(gono_t_exp)){
    temp_con <- gono.out[which(gono.out[,1]==gono_t_con[i]),2:19]	
    temp_con <- temp_con[1:9] + temp_con[10:18]
    
    gono_pred_con[i,] <- as.numeric(temp_con)
    
    temp_exp <- gono.out[which(gono.out[,1]==gono_t_exp[i]),20:37]	
    temp_exp <- temp_exp[1:9] + temp_exp[10:18]
    
    gono_pred_exp[i,] <- as.numeric(temp_exp)
  }
  
  for(i in 1:nrow(gono_pred_con)){
    gono_pred_con[i,] <- gono_pred_con[i,]/sum(gono_pred_con[i,])
    gono_pred_exp[i,] <- gono_pred_exp[i,]/sum(gono_pred_exp[i,])
  }
  
  like_ML <- 0
  
  for(i in 1:nrow(gono_pred_con)){
    like_ML <- like_ML + lfactorial(gono_n_con[i]) + sum( gono_con[i,]*log(gono_pred_con[i,]) - lfactorial(gono_con[i,]) )
    + lfactorial(gono_n_exp[i]) + sum( gono_exp[i,]*log(gono_pred_exp[i,]) - lfactorial(gono_exp[i,]) )
  }
  
  total_like <- muf_prior +  delta_prior + like_NB + like_ML
  
  -total_like
}


############################################################################
## Use the optimisation routine constrOptim to search the constrained
## parameter space described above for the set of parameters that provides
## the best fit to the data. The algorithm is run N_lhs times to make
## sure that the real optimum is found.

########################################
# Bounds for constrained optimisation

lower <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
upper <- c(10000, 10000, 100, 100, 100, 100, 100, 100, 100, 100, 11)

ui <- rbind( diag(11), -diag(11) )
ci <- c( lower, -upper ) 

theta <- randomLHS(N_lhs,11)
theta <- t(lower + t(theta)*(upper-lower))

MLE_max <- 10000000


for(j in 1:N_lhs){
  MLE_gono4 <- constrOptim(theta=theta[j,], f=mod_gono4, grad=NULL, ui=ui, ci=ci)
  
  if( MLE_gono4$value < MLE_max ){ 
    best_gono4 <- MLE_gono4
    MLE_max   <- MLE_gono4$value
  }
  
}




proc.time()

mod_compare <- matrix(NA, nrow=4, ncol=14)
colnames(mod_compare) <- c( "Nf_con", "Nf_exp", "sf_con", "sf_exp", "muf", "muf_A",
                            "r_con", "r_exp", "delta", "m", "m2", "ML", "AIC", "dAIC")
rownames(mod_compare) <- c("linear", "multiplicative", "step", "step2")


mod_compare[1,1:10] <- best_gono1$par
mod_compare[1,12]  <- -best_gono1$value
mod_compare[1,13]  <- 2*length(best_gono1$par) + 2*best_gono1$value

mod_compare[2,1:10] <- best_gono2$par
mod_compare[2,12]  <- -best_gono2$value
mod_compare[2,13]  <- 2*length(best_gono2$par) + 2*best_gono2$value

mod_compare[3,1:10] <- best_gono3$par
mod_compare[3,12]  <- -best_gono3$value
mod_compare[3,13]  <- 2*length(best_gono3$par) + 2*best_gono3$value

mod_compare[4,1:11] <- best_gono4$par
mod_compare[4,12]  <- -best_gono4$value
mod_compare[4,13]  <- 2*length(best_gono4$par) + 2*best_gono4$value


mod_compare[,14] <- mod_compare[,13] - min(mod_compare[,13])
mod_compare <- mod_compare[order(mod_compare[,14]),]


save.image("Mali_gono_fit_Jul01.RData")






#################################################
#################################################
##          ##                                 ##
##   ####   ##  #     #  ####  #     #  ####   ##
##  ##  ##  ##  ##   ## ##  ## ##   ## ##  ##  ##
##     ##   ##  ####### ##     ####### ##      ##
##    ##    ##  ## # ## ##  ## ## # ## ##  ##  ##
##   #####  ##  ##   ##  ####  ##   ##  ####   ##
##          ##                                 ##
#################################################
#################################################

##############################################
##############################################
## Carry out MCMC fitting to estimate the distributions
## of the parameters


MCMC <- 30000


################################################
## The function above had to return the negative likelihood
## as the constrOptim minimises function, but MCMC fitting
## needs the real value of likelihood

mc_gono <- function(params){
  - mod_gono1( params )
}

######################
## Initial seeds for parameter chain

Nf_con <- 1000
Nf_exp <- 700	
sf_con <- 0.2
sf_exp <- 0.2
muf    <- 0.1
muf_A  <- 9
r_con  <- 3 
r_exp  <- 3 
delta  <- 0.33
m      <- 0


######################
## Create matrix to store chain output

gono_par <- matrix(NA, nrow=MCMC, ncol=10)
colnames(gono_par) <- c("Nf_con", "Nf_exp", "sf_con", "sf_exp", "muf", "muf_A", "r_con", "r_exp", "delta", "m")
gono_accept <- 0

loglike <- mc_gono(c(Nf_con, Nf_exp, sf_con, sf_exp, muf, muf_A, r_con, r_exp, delta, m))


############################
## Carry out MCMC iterations updating parameters each
## time and accepting with Metropolis-Hastings algorithm

for(mc in 1:MCMC){
  Nf_con_test <- rnorm(n=1, mean=Nf_con, sd=50)
  Nf_exp_test <- rnorm(n=1, mean=Nf_exp, sd=50)
  sf_con_test <- rnorm(n=1, mean=sf_con, sd=0.03)
  sf_exp_test <- rnorm(n=1, mean=sf_exp, sd=0.03)
  muf_test    <- rnorm(n=1, mean=muf, sd=0.01)
  muf_A_test  <- rnorm(n=1, mean=muf_A, sd=0.5)
  r_con_test  <- rnorm(n=1, mean=r_con, sd=1)
  r_exp_test  <- rnorm(n=1, mean=r_exp, sd=1)
  delta_test  <- rnorm(n=1, mean=delta, sd=0.03)
  m_test      <- rnorm(n=1, mean=m, sd=0.02)
  
  if( (Nf_con_test>0) &&
      (Nf_exp_test>0)  &&
      (sf_con_test>0)  &&
      (sf_exp_test>0)  &&
      (muf_test>0)  &&
      (muf_A_test>0) &&
      (r_con_test>0)  &&
      (r_exp_test>0)  &&
      (delta_test>0)  &&
      (m_test >-1) ){
    
    loglike_test <- mc_gono(c(Nf_con_test, Nf_exp_test, sf_con_test, sf_exp_test, muf_test, muf_A_test, r_con_test, r_exp_test, delta_test, m_test))
    
    if( log(runif(1)) < min( loglike_test-loglike, 0 ) ) {
      Nf_con <- Nf_con_test
      Nf_exp <- Nf_exp_test
      sf_con <- sf_con_test
      sf_exp <- sf_exp_test
      muf <- muf_test
      muf_A <- muf_A_test
      r_con <- r_con_test
      r_exp <- r_exp_test
      delta <- delta_test
      m <- m_test
      loglike <- loglike_test
      gono_accept <- gono_accept + 1		
    }
  }
  
  gono_par[mc,] <- c(Nf_con, Nf_exp, sf_con, sf_exp, muf, muf_A, r_con, r_exp, delta, m)
}

gono_accept <- gono_accept/MCMC


###############################################
## Trim the results if MCMC > 10000

if( MCMC>10000 ){
  gono_par <- gono_par[floor(seq(from=1, to=MCMC, length=10000)),]
  MCMC <- 10000
}

proc.time()

par(mfrow=c(2,10))

hist(gono_par[,1], breaks=50, main="Nf_con")
hist(gono_par[,2], breaks=50, main="Nf_exp")
hist(gono_par[,3], breaks=50, main="sf_con")
hist(gono_par[,4], breaks=50, main="sf_exp")
hist(gono_par[,5], breaks=50, main="muf")
hist(gono_par[,6], breaks=50, main="muf_A")
hist(gono_par[,7], breaks=50, main="r_con")
hist(gono_par[,8], breaks=50, main="r_exp")
hist(gono_par[,9], breaks=50, main="delta")
hist(gono_par[,10], breaks=50, main="m")

plot(x=1:MCMC, y=gono_par[,1], pch=19, cex=0.25, col="grey", main="Nf_con")
plot(x=1:MCMC, y=gono_par[,2], pch=19, cex=0.25, col="grey", main="Nf_exp")
plot(x=1:MCMC, y=gono_par[,3], pch=19, cex=0.25, col="grey", main="sf_con")
plot(x=1:MCMC, y=gono_par[,4], pch=19, cex=0.25, col="grey", main="sf_exp")
plot(x=1:MCMC, y=gono_par[,5], pch=19, cex=0.25, col="grey", main="muf")
plot(x=1:MCMC, y=gono_par[,6], pch=19, cex=0.25, col="grey", main="muf_A")
plot(x=1:MCMC, y=gono_par[,7], pch=19, cex=0.25, col="grey", main="r_con")
plot(x=1:MCMC, y=gono_par[,8], pch=19, cex=0.25, col="grey", main="r_exp")
plot(x=1:MCMC, y=gono_par[,9], pch=19, cex=0.25, col="grey", main="delta")
plot(x=1:MCMC, y=gono_par[,10], pch=19, cex=0.25, col="grey", main="m")


########################################################
## Chop off the first few iterations for burn-in and then
## estimate the 95% confidence intervals for each parameter

gono_par <- gono_par[500:nrow(gono_par),]

## Nf_con
quantile(gono_par[,1], probs=c(0.025, 0.5, 0.975) )

## Nf_exp
quantile(gono_par[,2], probs=c(0.025, 0.5, 0.975) )

## sf_con
quantile(gono_par[,3], probs=c(0.025, 0.5, 0.975) )

## sf_exp
quantile(gono_par[,4], probs=c(0.025, 0.5, 0.975) )

## muf
quantile(gono_par[,5], probs=c(0.025, 0.5, 0.975) )

## muf_A
quantile(gono_par[,6], probs=c(0.025, 0.5, 0.975) )

## r_con
quantile(gono_par[,7], probs=c(0.025, 0.5, 0.975) )

## r_exp
quantile(gono_par[,8], probs=c(0.025, 0.5, 0.975) )

## delta
quantile(gono_par[,9], probs=c(0.025, 0.5, 0.975) )

## m
quantile(gono_par[,10], probs=c(0.025, 0.5, 0.975) )


##################################
##################################
##                              ##
##  #####  ##     ####  ######  ##
##  ##  ## ##    ##  ##   ##    ##
##  #####  ##    ##  ##   ##    ##
##  ##     ##    ##  ##   ##    ##
##  ##     #####  ####    ##    ##
##                              ##
##################################
##################################

#######################################
## Maximum likelihood model predictions

plot_t <- seq(from=0, to=40, length=100)

Nf_con <- MLE_gono$par[1]
Nf_exp <- MLE_gono$par[2]
sf_con <- MLE_gono$par[3]
sf_exp <- MLE_gono$par[4]
muf    <- MLE_gono$par[5]
muf_A  <- MLE_gono$par[6]
r_con  <- MLE_gono$par[7]
r_exp  <- MLE_gono$par[8]
delta  <- MLE_gono$par[9]
m      <- MLE_gono$par[10]

## Calculate experimental & control predictions using ODEs:


#####################################
## First we need to calculate the equilibrium distribution of
## mosquitoes broken down by gonotrophic cycle pre-intervention

Uf_con_start <- Nf_con*exp( -(0:7)*muf*delta )*( 1-exp(-muf*delta) )
Uf_con_start <- c( Uf_con_start, Nf_con*exp(-8*muf*delta) )

Mf_con_start <- rep(0, 9)

Uf_exp_start <- Nf_exp*exp( -(0:7)*muf*delta )*( 1-exp(-muf*delta) )
Uf_exp_start <- c( Uf_exp_start, Nf_exp*exp(-8*muf*delta) )

Mf_exp_start <- rep(0, 9)

start <- c(Uf_con = Uf_con_start,
           Mf_con = Mf_con_start,
           Uf_exp = Uf_exp_start,
           Mf_exp = Mf_exp_start )


###################################################
## Define mosquito life cycle model with gonotrophic cycle
## number tracked within it

par_ode <- c()

gono.model <- function(t, x, par_ode){
  with(as.list(par_ode),{
    
    
    #####################################
    ## Declare variables
    
    dUf_con   <- rep(0, 9)
    dMf_con   <- rep(0, 9)
    
    dUf_exp   <- rep(0, 9)
    dMf_exp   <- rep(0, 9)
    
    Uf_con <- x[1:9]
    Mf_con <- x[10:18]
    
    Uf_exp <- x[19:27]
    Mf_exp <- x[28:36]
    
    
    #########################################
    ## Differential equations
    
    sf_con_t <- sf_con*(1 + (0:8)*m )
    sf_exp_t <- sf_exp*(1 + (0:8)*m )
    
    if( t < 8 ){ 
      sf_con_t <- rep(0, 9)
      sf_exp_t <- rep(0, 9) 
    }
    
    dUf_con[1] <- muf*Nf_con - (sf_con_t[1] + muf + delta)*Uf_con[1]
    dMf_con[1] <- sf_con_t[1]*Uf_con[1] - (muf + delta)*Mf_con[1]
    
    dUf_con[2:8] <- delta*Uf_con[(2:8)-1] - (sf_con_t[2:8] + muf + delta)*Uf_con[2:8]
    dMf_con[2:8] <- delta*Mf_con[(2:8)-1] + sf_con_t[2:8]*Uf_con[2:8] - (muf + delta)*Mf_con[2:8]
    
    dUf_con[9] <- delta*Uf_con[8] - (sf_con_t[9] + muf)*Uf_con[9]
    dMf_con[9] <- delta*Mf_con[8] + sf_con_t[9]*Uf_con[9] - muf*Mf_con[9] 
    
    
    dUf_exp[1] <- muf*Nf_exp - (sf_exp_t[1] + muf + delta)*Uf_exp[1]
    dMf_exp[1] <- sf_exp_t[1]*Uf_exp[1] - (muf_A + delta)*Mf_exp[1]
    
    dUf_exp[2:8] <- delta*Uf_exp[(2:8)-1] - (sf_exp_t[2:8] + muf + delta)*Uf_exp[2:8]
    dMf_exp[2:8] <- delta*Mf_exp[(2:8)-1] + sf_exp_t[2:8]*Uf_exp[2:8] - (muf_A + delta)*Mf_exp[2:8]
    
    dUf_exp[9] <- delta*Uf_exp[8] - (sf_exp_t[9] + muf)*Uf_exp[9]
    dMf_exp[9] <- delta*Mf_exp[8] + sf_exp_t[9]*Uf_exp[9] - muf_A*Mf_exp[9]
    
    #####################################
    ## Return output in list format
    
    list( c(dUf_con, dMf_con, dUf_exp, dMf_exp))
    
  })
}


#################################################
## Solve the set of differential equations and 
## format output                               

gono.out <- as.data.frame(lsoda(y=start, times=plot_t, func=gono.model, parm=par_ode))



#gono.out <- gono.out[data.index,]
#gono.out <- as.matrix(gono.out)
#
#
#zero.index <- which(gono.out < 0.001)
#
#if( length(zero.index) > 0 ){
#	gono.out[zero.index] <- 0.001
#}


mod_Uf_con <- rowSums(gono.out[,2:10])
mod_Mf_con <- rowSums(gono.out[,11:19])
#mod_Mf_con <- mod_Mf_con[-which(times_con<=8)]

mod_Uf_exp <- rowSums(gono.out[,20:28])
mod_Mf_exp <- rowSums(gono.out[,29:37])
#mod_Mf_exp <- mod_Mf_exp[-which(times_con<=8)]



###########################################
## Because we used negative binomial fitting, we have an 
## estimate of the variance in the expected mosquito catch
## which we can use to estimate confidence intervals

mod_Uf_con_low  <- qnbinom( 0.025, size=r_con, prob=r_con/(mod_Uf_con+r_con) )
mod_Uf_con_high <- qnbinom( 0.975, size=r_con, prob=r_con/(mod_Uf_con+r_con) )

mod_Mf_con_low  <- qnbinom( 0.025, size=r_con, prob=r_con/(mod_Mf_con+r_con) )
mod_Mf_con_high <- qnbinom( 0.975, size=r_con, prob=r_con/(mod_Mf_con+r_con) )

mod_Uf_exp_low  <- qnbinom( 0.025, size=r_exp, prob=r_exp/(mod_Uf_exp+r_exp) )
mod_Uf_exp_high <- qnbinom( 0.975, size=r_exp, prob=r_exp/(mod_Uf_exp+r_exp) )

mod_Mf_exp_low  <- qnbinom( 0.025, size=r_exp, prob=r_exp/(mod_Mf_exp+r_exp) )
mod_Mf_exp_high <- qnbinom( 0.975, size=r_exp, prob=r_exp/(mod_Mf_exp+r_exp) )




par(mfrow=c(2,2))

#########################################
## Plot control data first on a standard scale

##################################
## Plot model predictions

plot(x=plot_t, y=mod_Uf_con, type='l', ylim=c(0,max(mosq_con)),
     xlab="Time (days)", ylab="Mosquito catch", main="Female control",
     col="dodgerblue", lwd=2 )

points(x=plot_t, y=mod_Mf_con, type='l',
       col="forestgreen", lwd=2)


##################################
## Plot confidence intervals

polygon(x=c(plot_t, rev(plot_t)), 
        y=c( mod_Uf_con_low, rev(mod_Uf_con_high) ),
        col=rgb(30/256,144/256,255/256,0.3), border=NA)

polygon(x=c(plot_t, rev(plot_t)), 
        y=c( mod_Mf_con_low, rev(mod_Mf_con_high) ),
        col=rgb(34/256,139/256,34/256,0.3), border=NA)

##################################
## Plot data points

points(x=times_con, y=mark_con, col="forestgreen", pch=19, cex=2)
points(x=times_con, y=unmark_con, col="dodgerblue", pch=19, cex=2)

###############################
## Replot model predictions on top of shaded regions

points(x=plot_t, y=mod_Uf_con, type='l', col="dodgerblue", lwd=2)
points(x=plot_t, y=mod_Mf_con, type='l', col="forestgreen", lwd=2)

points(x=c(8,8), y=c(-1000,1000), type='l', lty="longdash" )

legend(x="topright", fill=c("dodgerblue", "forestgreen"),
       legend=c("Unmarked", "Marked"))


#########################################
## Plot secondly on a log scale


##################################
## Plot model predictions

plot(x=plot_t, y=mod_Uf_con+1, type='l', ylim=c(1,max(mosq_con)), 
     xlab="Time (days)", ylab="Mosquito catch", main="Female control (log scale)", 
     col="dodgerblue", lwd=2, log="y" )

points(x=plot_t, y=mod_Mf_con+1, type='l',
       col="forestgreen", lwd=2)


##################################
## Plot confidence intervals

polygon(x=c(plot_t, rev(plot_t)),
        y=c( mod_Uf_con_low+1, rev(mod_Uf_con_high+1) ),
        col=rgb(30/256,144/256,255/256,0.3), border=NA)

polygon(x=c(plot_t, rev(plot_t)), 
        y=c( mod_Mf_con_low+1, rev(mod_Mf_con_high+1) ),
        col=rgb(34/256,139/256,34/256,0.3), border=NA)

##################################
## Plot data points

points(x=times_con, y=mark_con+1, col="forestgreen", pch=19, cex=2)
points(x=times_con, y=unmark_con+1, col="dodgerblue", pch=19, cex=2)

###############################
## Replot model predictions on top of shaded regions

points(x=plot_t, y=mod_Uf_con+1, type='l', col="dodgerblue", lwd=2)
points(x=plot_t, y=mod_Mf_con+1, type='l', col="forestgreen", lwd=2)

points(x=c(8,8), y=c(-1000,1000), type='l', lty="longdash" )

legend(x="bottomright", fill=c("dodgerblue", "forestgreen"),
       legend=c("Unmarked", "Marked"))



#########################################
## Plot experimental data first on a standard scale

##################################
## Plot model predictions

plot(x=plot_t, y=mod_Uf_exp, type='l', ylim=c(0,max(mosq_exp)),
     xlab="Time (days)", ylab="Mosquito catch", main="Female experiment",
     col="dodgerblue", lwd=2 )

points(x=plot_t, y=mod_Mf_exp, type='l',
       col="forestgreen", lwd=2)


##################################
## Plot confidence intervals

polygon(x=c(plot_t, rev(plot_t)), 
        y=c( mod_Uf_exp_low, rev(mod_Uf_exp_high) ),
        col=rgb(30/256,144/256,255/256,0.3), border=NA)

polygon(x=c(plot_t, rev(plot_t)), 
        y=c( mod_Mf_exp_low, rev(mod_Mf_exp_high) ),
        col=rgb(34/256,139/256,34/256,0.3), border=NA)

##################################
## Plot data points

points(x=times_exp, y=mark_exp, col="forestgreen", pch=19, cex=2)
points(x=times_exp, y=unmark_exp, col="dodgerblue", pch=19, cex=2)

###############################
## Replot model predictions on top of shaded regions

points(x=plot_t, y=mod_Uf_exp, type='l', col="dodgerblue", lwd=2)
points(x=plot_t, y=mod_Mf_exp, type='l', col="forestgreen", lwd=2)

points(x=c(8,8), y=c(-1000,1000), type='l', lty="longdash" )

legend(x="topright", fill=c("dodgerblue", "forestgreen"),
       legend=c("Unmarked", "Marked"))


#########################################
## Plot secondly on a log scale


##################################
## Plot model predictions

plot(x=plot_t, y=mod_Uf_exp+1, type='l', ylim=c(1,max(mosq_exp)), 
     xlab="Time (days)", ylab="Mosquito catch", main="Female experiment (log scale)",
     col="dodgerblue", lwd=2, log="y" )

points(x=plot_t, y=mod_Mf_exp+1, type='l',
       col="forestgreen", lwd=2)

##################################
## Plot confidence intervals

polygon(x=c(plot_t, rev(plot_t)),
        y=c( mod_Uf_exp_low+1, rev(mod_Uf_exp_high+1) ),
        col=rgb(30/256,144/256,255/256,0.3), border=NA)

polygon(x=c(plot_t, rev(plot_t)), 
        y=c( mod_Mf_exp_low+1, rev(mod_Mf_exp_high+1) ),
        col=rgb(34/256,139/256,34/256,0.3), border=NA)

##################################
## Plot data points

points(x=times_exp, y=mark_exp+1, col="forestgreen", pch=19, cex=2)
points(x=times_exp, y=unmark_exp+1, col="dodgerblue", pch=19, cex=2)

###############################
## Replot model predictions on top of shaded regions

points(x=plot_t, y=mod_Uf_exp+1, type='l', col="dodgerblue", lwd=2)
points(x=plot_t, y=mod_Mf_exp+1, type='l', col="forestgreen", lwd=2)

points(x=c(8,8), y=c(-1000,1000), type='l', lty="longdash" )

legend(x="topright", fill=c("dodgerblue", "forestgreen"),
       legend=c("Unmarked", "Marked"))




