library(deSolve)

#############################################################
##  Declare parameters and their values.                   ##
##  Taken from White et al (2011) - Parasites and Vectors  ##
#############################################################

mu_m  = 0.096          ## adult mosquito daily mortality
mu_e  = 0.034          ## early larval instar daily mortality
mu_l  = 0.035          ## laet larval instar daily mortality
mu_p  = 0.25           ## pupae daily mortality
d_e   = 6.64           ## duration of early instar stage
d_l   = 3.72           ## duration of late instar stage
d_p   = 0.64           ## duration of pupal stage

delta  = 3             ## duration of gonotrophic cycle
beta   = 21.19         ## eggs laid per day by female mosquito
gamma  = 13.25	     ## effect of DD on late instars relative to early instars

M_eq    = 100           ## number of female mosquitoes at equilibrium (as caught by traps)


##########################################################
## Calculate carrying capacity and equilibrium densities

b_omega <- gamma*mu_l/mu_e - d_e/d_l + (gamma-1)*mu_l*d_e
omega  <- - 0.5*b_omega + sqrt( 0.25*b_omega^2 + gamma*beta*mu_l*d_e/(2*mu_e*mu_m*d_l*(1+d_p*mu_p)) ) 


KK <- M_eq*2*d_l*mu_m*(1+d_p*mu_p)*gamma*(omega+1)/( omega/(mu_l*d_e) - 1/(mu_l*d_l) - 1 )

E_eq <- 2*omega*mu_m*d_l*(1+d_p*mu_p)*M_eq
L_eq <- 2*mu_m*d_l*(1+d_p*mu_p)*M_eq
P_eq <- 2*d_p*mu_m*M_eq


####################################################################
##     ## Set up and solve a system of differential equations for ##
##  1  ## larval model in the absence of any other interventions  ##
##     ##                                                         ##
####################################################################

###########################################################################
## Objects for parameters, times for solving ODEs, and starting conditions


parms <- c(mu_m  <- mu_m, mu_e  <- mu_e, mu_l  <- mu_l, mu_p  <- mu_p,
           d_e   <- d_e, d_l   <- d_l, d_p   <- d_p, delta <- delta,                                               
           beta  <- beta, gamma <- gamma, KK    <- KK)                                

times <- seq(0, 200, by=1) 
start <- 0.5*c(E_eq, L_eq, P_eq, M_eq)


###############################################################################
## System of ODEs with no interventions

larval.model <- function(t, x, parms){
  with(as.list(parms),{
    E <- x[1]
    L <- x[2]
    P <- x[3]
    M <- x[4]
    
    dE <- beta*M - mu_e*( 1+(E+L)/KK )*E - E/d_e
    dL <- E/d_e - mu_l*( 1+gamma*(E+L)/KK )*L - L/d_l
    dP <- L/d_l - mu_p*P - P/d_p
    dM <- 0.5*P/d_p - mu_m*M
    
    list(c(dE, dL, dP, dM))
  })
}

larval.sim <- as.data.frame(lsoda(y=start, times=times, func=larval.model, parm=parms))


############################################################################
## Plot mosquito density over time

plot(x=times, y=larval.sim[,5], type='l', ylim=c(0,1.1*max(larval.sim[,5])),
     xlab="time (days)", ylab="mosquito density")





####################################################################
##     ## Now extend the model of mosquito population dynamics to ##
##  2  ## include the effect of ITNs                              ##
##     ##                                                         ##
####################################################################


###################################################################
## The ITN model is based on that described by Le Menach in Malaria Journal
## and by Griffin et al in PLos Med. All parameters are taken from Griffin et al.

tau_2   <- 0.69               ## proportion of gonotrophic cycle spent searching for blood emal
tau_1   <- delta - tau_2      ## proportion of gonotrophic cycle spent reasting
phi_ITN <- 0.89               ## proportion of bites taken while person is in bed
s_ITN   <- 0.03               ## probability of successful feeding when protected by ITN 
r_ITN   <- 0.56	    	      ## probability mosquito repelled by net		
d_ITN   <- 1 - s_ITN - r_ITN  ## probability mosquito killed by net
Q_0     <- 0.92               ## human blood index - proportion of bites taken on humans

cov_ITN <- 0.5                ## ITN coverage - set as you like


##########################################################################
## Equation for ITN model based on Le Menach et al (2007) and Griffin et al (2010)
## The model is written to take one parameter and return mosquito death rate and extension 
## of gonotrophic cycle

#####################################
## probability of mosquito successfully taking a blood meal in one attempt

w_ITN <- function(cov_ITN){
  1 - Q_0*cov_ITN*phi_ITN*(1-s_ITN)
}

#####################################
## probability that mosquito repeats

z_ITN <- function(cov_ITN){
  Q_0*cov_ITN*phi_ITN*r_ITN
}

#####################################
## blood feeding rate - inverse of gonotrophic cycle

f_ITN <- function(cov_ITN){
  1/( tau_1/(1-z_ITN(cov_ITN)) + tau_2 )
}

#####################################
## proportion of blood meals taken on animals

Q_animal <- function(cov_ITN){
  (1-Q_0)/w_ITN(cov_ITN)
}

#####################################
## proportion of blood meals taken on humans with no nets

Q_nonet <- function(cov_ITN){
  Q_0*(1-cov_ITN)/w_ITN(cov_ITN)
}

#####################################
## proportion of blood meals taken on humans with nets

Q_net <- function(cov_ITN){
  Q_0*cov_ITN*(1-phi_ITN+phi_ITN*s_ITN)/w_ITN(cov_ITN)
}

#####################################
## probability of mosquito surviving an average day

p_ITN <- function(cov_ITN){
  ( exp(-mu_m*tau_1)*w_ITN(cov_ITN)*exp(-mu_m*tau_2)/(1-z_ITN(cov_ITN)) )^f_ITN(cov_ITN)
}

#####################################
## mosquito death rate

mu_ITN <- function(cov_ITN){
  -log(p_ITN(cov_ITN))
}

###########################################
## Secondly, ITNs will cause mosquitoes to spend a longer time
## searching for a blood meal leading to an increased time between
## ovipositions and hence a reduction in egg-laying rate


e_ov <- beta*(exp(delta*mu_m)-1)/mu_m

beta_ITN <- function(cov_ITN){
  e_ov*mu_ITN(cov_ITN)/( exp(mu_ITN(cov_ITN)/f_ITN(cov_ITN)) -1 )
}



####################################################
## Now set up the system of differential equations for
## mosquito dynamics with ITNs

cov_ITN <- 0.8  ## set ITN coverage
ITN_on  <- 50   ## a switch to turn on ITNs


###########################################################################
## Objects for parameters, times for solving ODEs, and starting conditions


parms <- c(mu_m  <- mu_m, mu_e  <- mu_e, mu_l  <- mu_l, mu_p  <- mu_p,
           d_e   <- d_e, d_l   <- d_l, d_p   <- d_p, delta <- delta,                                               
           beta  <- beta, gamma <- gamma, KK    <- KK, cov_ITN <- cov_ITN)                                


times <- seq(0, 200, by=1) 
start <- 0.75*c(E_eq, L_eq, P_eq, M_eq)


###############################################################################
## System of ODEs with no interventions

ITN.model <- function(t, x, parms){
  with(as.list(parms),{
    E <- x[1]
    L <- x[2]
    P <- x[3]
    M <- x[4]
    
    if( t > ITN_on ){ cov_time <- cov_ITN }else{ cov_time <- 0 } 
    
    dE <- beta_ITN(cov_time)*M - mu_e*( 1+(E+L)/KK )*E - E/d_e
    dL <- E/d_e - mu_l*( 1+gamma*(E+L)/KK )*L - L/d_l
    dP <- L/d_l - mu_p*P - P/d_p
    dM <- 0.5*P/d_p - mu_ITN(cov_time)*M
    
    list(c(dE, dL, dP, dM))
  })
}

ITN.sim <- as.data.frame(lsoda(y=start, times=times, func=ITN.model, parm=parms))


############################################################################
## Plot mosquito density over time

points(x=times, y=ITN.sim[,5], type='l', ylim=c(0,1.1*max(ITN.sim[,5])),
     xlab="time (days)", ylab="mosquito density")



#####################################################################
##     ## Now extend the model further to include sugar feeding on ##
##  3  ## attractive toxic sugar bait                              ##    
##     ##                                                          ##
#####################################################################

#####################################################################
## Sugar feeding parameters are taken from the model fitting to data
## on female mosquitoes from the experimental site

mu_m    <- 0.095    ## female mosquito death rate - estimated from data
s_feed  <- 0.39     ## female mosquito sugar feeding rate - estimated
mu_atsb <- 9.01     ## female mosquito death rate once poisoned



cov_ITN <- 0.8  ## set ITN coverage
ITN_on  <- 50   ## a switch to turn on ITNs

sugar_on <- 100 ## switch for start of ATSB


###########################################################################
## Objects for parameters, times for solving ODEs, and starting conditions

parms <- c(mu_m  <- mu_m, mu_e  <- mu_e, mu_l  <- mu_l, mu_p  <- mu_p,
           d_e   <- d_e, d_l   <- d_l, d_p   <- d_p, delta <- delta,                                               
           beta  <- beta, gamma <- gamma, KK    <- KK, cov_ITN <- cov_ITN)                                


times <- seq(0, 200, by=1) 
start <- c(E_eq, L_eq, P_eq, M_eq, 0)


###############################################################################
## System of ODEs with no interventions

ITN.sugar.model <- function(t, x, parms){
  with(as.list(parms),{
    E  <- x[1]
    L  <- x[2]
    P  <- x[3]
    M  <- x[4]
    TX <- x[5]
    
    if( t > ITN_on ){ cov_time <- cov_ITN }else{ cov_time <- 0 } 
    if( t > sugar_on ){ s_feed_time <- s_feed }else{ s_feed_time <- 0 } 
    
    dE  <- beta_ITN(cov_time)*M - mu_e*( 1+(E+L)/KK )*E - E/d_e
    dL  <- E/d_e - mu_l*( 1+gamma*(E+L)/KK )*L - L/d_l
    dP  <- L/d_l - mu_p*P - P/d_p
    dM  <- 0.5*P/d_p - mu_ITN(cov_time)*M - s_feed_time*M
    dTX <- s_feed_time*M - mu_atsb*TX
    
    list(c(dE, dL, dP, dM, dTX))
  })
}

ITN.sugar.sim <- as.data.frame(lsoda(y=start, times=times, func=ITN.sugar.model, parm=parms))

############################################################################
## Plot mosquito density over time

points(x=times, y=ITN.sugar.sim[,5], type='l', ylim=c(0,1.1*max(ITN.sugar.sim[,5])),
     xlab="time (days)", ylab="mosquito density")
points(x=times, y=ITN.sugar.sim[,6], type='l', col="red" )





