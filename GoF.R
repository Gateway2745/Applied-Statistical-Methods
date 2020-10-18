#install.packages("fitdistrplus", repos = "http://cran.us.r-project.org")
library(fitdistrplus)
Data<-c(85.092, 32.609, 59.465, 77.437, 32.534, 64.090, 32.464, 59.902,
39.323, 89.641, 94.219, 116.803, 92.857, 63.436, 65.605, 85.861,
64.342, 61.978, 67.998, 59.817, 101.769, 95.774, 121.352, 69.568,
74.276, 66.998, 40.001, 72.069, 25.066, 77.098, 69.922, 35.662,
74.425, 67.202, 118.444, 53.500, 79.294, 64.544, 86.813, 116.269,
37.831, 89.341, 73.341, 85.288, 138.114, 53.402, 85.586, 82.256,
77.539, 88.798)
# fit distributions on the data
A<-fitdist(Data, 'weibull')
B<-fitdist(Data, 'lnorm')
C<-fitdist(Data, 'gamma')
D<-fitdist(Data, 'exp')
# histogram of the data
hist(Data)
# Summary of the results
summary(A)
summary(B)
summary(C)
summary(D)
#plot of functions
#par(mfrow)
a<-c("weibull", "lognormal","gamma","exponential")
denscomp(list(A,B,C,D), legendtext=a)
cdfcomp(list(A,B,C,D), legendtext=a)
qqcomp(C)