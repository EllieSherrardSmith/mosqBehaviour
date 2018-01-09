##########################################
##
## Exploring how a distribution for phiI and phiB change global net / irs efficacy

time = 1
ls1 = 1/(1 + exp(-0.6366 + 0.0086*time))
ks1 = 1/(1 + exp(1.4244 - 0.0045*time))
js1 = 1 - ls1 - ks1
k0 = 0.699

rs = (1 - ks1 / k0) * (js1 / ls1 + js1)
ss = ks1 / k0
rn = 0.3810241
sn = 1 - 0.3810241 - 0.1971604

phiI = rnorm(100, mean = 0.89, 0.1); phiI <- phiI[phiI > 0.4 & phiI < 1] ; phiI = sample(phiI,50) #0.92
phiB = rnorm(100, mean = 0.84, 0.08); phiB <- phiB[phiB > 0.2 & phiB < 1] ; phiB = sample(phiB,50) #0.92

datphi = data.frame(phiI = sort(phiI), phiB = sort(phiB))

##Probability of successful feeding (wi)
wi = array(dim=c(50,3))
colnames(wi) = c("IRS only","nets_only", "IRS and nets")

for(i in 1:50) {
  wi[i,1] = 1 - datphi$phiI[i] + datphi$phiI[i] * (1 - rs) * ss
  wi[i,2] = 1 - datphi$phiB[i] + datphi$phiB[i] * sn
  wi[i,3] = 1 - datphi$phiI[i] + datphi$phiB[i] * (1 - rs) * sn * ss + (datphi$phiI[i] - datphi$phiB[i]) * (1 - rs) * ss
  
}

##Probability of biting (yi)
yi = array(dim=c(50,3))
colnames(yi) = c("IRS only","nets_only", "IRS and nets")

for(i in 1:50) {
  yi[i,1] = 1 - datphi$phiI[i] + datphi$phiI[i] * (1 - rs)
  yi[i,2] =  1 - datphi$phiB[i] + datphi$phiB[i] * sn
  yi[i,3] =  1 - datphi$phiI[i] + datphi$phiB[i] * (1 - rs) * sn + (datphi$phiI[i] - datphi$phiB[i]) * (1 - rs) 
}
##Probability of repellency (zi)
zi = array(dim=c(50,3))
colnames(zi) = c("IRS only","nets_only", "IRS and nets")

for(i in 1:50) {
  zi[i,1] = datphi$phiI[i] * rs
  zi[i,2] = datphi$phiB[i] * rn
  zi[i,3] =  datphi$phiB[i] * (1 - rs) * rn + datphi$phiI[i] * rs
}

plot(datphi$phiI, wi[,1],ylim=c(0,1),xlim=c(0.5,1),ylab="Probability successful feeding",xlab="phi")
cols = c("red","blue","orange")
for(i in 1:3) lines(wi[,i]~datphi$phiI,col = cols[i])
for(i in 1:3) lines(wi[,i]~datphi$phiB,col = cols[i],lty=2)

plot(datphi$phiI, yi[,1],ylim=c(0,1),xlim=c(0.5,1),ylab="Probability successful feeding",xlab="phi")
cols = c("red","blue","orange")
for(i in 1:3) lines(yi[,i]~datphi$phiI,col = cols[i])
for(i in 1:3) lines(yi[,i]~datphi$phiB,col = cols[i],lty=2)

plot(datphi$phiI, zi[,1],ylim=c(0,1),xlim=c(0.5,1),ylab="Probability successful feeding",xlab="phi")
cols = c("red","blue","orange")
for(i in 1:3) lines(zi[,i]~datphi$phiI,col = cols[i])
for(i in 1:3) lines(zi[,i]~datphi$phiB,col = cols[i],lty=2)
