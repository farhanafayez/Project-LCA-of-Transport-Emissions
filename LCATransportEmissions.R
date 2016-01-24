# LCA project step 2
# Lognormal simulation and normal distribution
# of eutrophication (EUT.15 and EUT.20), 
# human toxicity (HTX.15 and HTX.20) and distance (DIS).
# By Farhana Fayez
#
# 2015 data of EUT and HTX of Car:
#
EUT.15   <- c(9.828,25.2504,54.8856,44.604,8.4672,83.0088,15.12,33.8688,21.4704,10.4328,17.5392,30.0888,22.3776,5.7456,20.1096,11.1888,15.4224,35.2296,15.12,5.292,39.4632,63.8064)
#
HTX.15   <- c(7.5218E-08,1.93252E-07,4.20064E-07,3.41374E-07,6.48032E-08,6.35303E-07,1.1572E-07,2.59213E-07,1.64322E-07,7.98468E-08,1.34235E-07,2.30283E-07,1.71266E-07,4.39736E-08,1.53908E-07,8.56328E-08,1.18034E-07,2.69628E-07,1.1572E-07,4.0502E-08,3.02029E-07,4.88338E-07)
#
# 2020 data of EUT and HTX of Car:
#
EUT.20   <- c(0.78,2.004,4.356,3.54,0.672,6.588,1.2,2.688,1.704,0.828,1.392,2.388,1.776,0.456,1.596,0.888,1.224,2.796,1.2,0.42,3.132,5.064)
#
HTX.20   <- c(5.343E-08,1.37274E-07,2.98386E-07,2.4249E-07,4.6032E-08,4.51278E-07,8.22E-08,1.84128E-07,1.16724E-07,5.6718E-08,9.5352E-08,1.63578E-07,1.21656E-07,3.1236E-08,1.09326E-07,6.0828E-08,8.3844E-08,1.91526E-07,8.22E-08,2.877E-08,2.14542E-07,3.46884E-07)
#
# Data of Distances
#
DIS <- c(13,33.4,72.6,59,11.2,109.8,20,44.8,28.4,13.8,23.2,39.8,29.6,7.6,26.6,14.8,20.4,46.6,20,7,52.2,84.4)
#
# Data of Public Trasport:
#
PUB.EUT <- c(7.6626,10.672,42.9484,33.11,5.4868,36.8,11.7304,22.704,13.1494,3.634,11.0682,23.7446,16.4604,2.7434,9.1762,9.3654,11.5412,28.853,17.2172,5.9598,23.2716,43.7052)
#
PUB.HTX <- c(1.57626E-08,1.35952E-07,8.83484E-08,6.811E-08,1.12868E-08,4.688E-07,2.41304E-08,4.6704E-08,2.70494E-08,4.6294E-08,2.27682E-08,4.88446E-08,3.38604E-08,5.6434E-09,1.88762E-08,1.92654E-08,2.37412E-08,5.9353E-08,3.54172E-08,1.22598E-08,4.78716E-08,8.99052E-08)
#
#
# 2015 (EUT):
#
# normal dist:
b.EUT.15 <- sd(EUT.15)
ÃÂµ.EUT.15 <- mean(EUT.15)
curve(dnorm(x,b.EUT.15,ÃÂµ.EUT.15),-65,120,lwd=2,col='salmon',type='l')
grid()
# lognormal dist:
mu.EUT.15    <- mean(log(EUT.15))
sigma.EUT.15 <- sd(log(EUT.15))
curve(dlnorm(x,mu.EUT.15,sigma.EUT.15),0,120,lwd=2,col='blue')
grid()
# lognormal simulation:
x <- rlnorm(1e5,mu.EUT.15,sigma.EUT.15)
hist(x[x<140],col='green')
#
# 2015 (HTX):
#
b.HTX.15 <- sd(HTX.15)
Ã‚Âµ.HTX.15 <- mean(HTX.15)
curve(dnorm(x,b.HTX.15,Ã‚Âµ.HTX.15),-5*10^-7,9*10^-7,lwd=2,col='salmon',type='l')
grid()
# lognormal dist:
mu.HTX.15    <- mean(log(HTX.15))
sigma.HTX.15 <- sd(log(HTX.15))
curve(dlnorm(x,mu.HTX.15,sigma.HTX.15),0,1.1*10^-6,lwd=2,col='blue')
grid()
# lognormal simulation:
y <- rlnorm(1e5,mu.HTX.15,sigma.HTX.15)
hist(y[y<9*10^-7],col='green')
#
# 2020 (EUT):
#
b.EUT.20 <- sd(EUT.20)
Ã‚Âµ.EUT.20 <- mean(EUT.20)
curve(dnorm(x,b.EUT.20,Ã‚Âµ.EUT.20),-5,10,lwd=2,col='salmon',type='l')
grid()
# lognormal dist:
mu.EUT.20    <- mean(log(EUT.20))
sigma.EUT.20 <- sd(log(EUT.20))
curve(dlnorm(x,mu.EUT.20,sigma.EUT.20),0,10,lwd=2,col='blue')
grid()
# lognormal simulation:
z <- rlnorm(1e5,mu.EUT.20,sigma.EUT.20)
hist(z[z<10],col='green')
#
# 2020 (HTX):
#
b.HTX.20 <- sd(HTX.20)
ÃÂµ.HTX.20 <- mean(HTX.20)
curve(dnorm(x,b.HTX.20,ÃÂµ.HTX.20),-4*10^-7,7*10^-7,lwd=2,col='salmon',type='l')
grid()
# lognormal dist:
mu.HTX.20    <- mean(log(HTX.20))
sigma.HTX.20 <- sd(log(HTX.20))
curve(dlnorm(x,mu.HTX.20,sigma.HTX.20),0,7*10^-7,lwd=2,col='blue')
grid()
# lognormal simulation:
i <- rlnorm(1e5,mu.HTX.20,sigma.HTX.20)
hist(i[i<5*10^-7],col='green')
#
# Distance (DIS):
#
b.DIS <- sd(DIS)
ÃÂµ.DIS <- mean(DIS)
curve(dnorm(x,b.DIS,ÃÂµ.DIS),-100,200,lwd=2,col='salmon',type='l')
grid()
#
mu.DIS    <- mean(log(DIS))
sigma.DIS <- sd(log(DIS))
curve(dlnorm(x,mu.DIS,sigma.DIS),0,180,lwd=2,col='blue')
grid()
#
j <- rlnorm(1e5,mu.DIS,sigma.DIS)
hist(j[j<180],col='green')
#
# Public Transport (PUB.EUT):
#
b.PUB.EUT <- sd(PUB.EUT)
ÃÂµ.PUB.EUT <- mean(PUB.EUT)
curve(dnorm(x,b.PUB.EUT,ÃÂµ.PUB.EUT),-65,100,lwd=2,col='salmon',type='l')
grid()
# lognormal dist:
mu.PUB.EUT    <- mean(log(PUB.EUT))
sigma.PUB.EUT <- sd(log(PUB.EUT))
curve(dlnorm(x,mu.PUB.EUT,sigma.PUB.EUT),0,110,lwd=2,col='blue')
grid()
# lognormal simulation:
k <- rlnorm(1e5,mu.PUB.EUT,sigma.PUB.EUT)
hist(k[k<100],col='green')
#
# Public Transport (PUB.HTX):
#
b.PUB.HTX <- sd(PUB.HTX)
ÃÂµ.PUB.HTX <- mean(PUB.HTX)
curve(dnorm(x,b.PUB.HTX,ÃÂµ.PUB.HTX),-2*10^-7,4*10^-7,lwd=2,col='salmon',type='l')
grid()
# lognormal dist:
mu.PUB.HTX    <- mean(log(PUB.HTX))
sigma.PUB.HTX <- sd(log(PUB.HTX))
curve(dlnorm(x,mu.PUB.HTX,sigma.PUB.HTX),0,2.5*10^-7,lwd=2,col='blue')
grid()
# lognormal simulation:
l <- rlnorm(1e5,mu.PUB.HTX,sigma.PUB.HTX)
hist(l[l<2*10^-7],col='green')
#
#
#
#


