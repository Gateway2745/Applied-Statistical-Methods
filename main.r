n <- 30
samp <- c(172.1,177.0,169.4,165.8,160.9,184.4,171.9,190.6,174.2,180.2,180.5,163.0,170.4,181.8,169.7,176.3,176.7,181.4,195.0,177.3,159.0,199.7,181.6,184.7,186.2,184.8,186.3,171.1,176.0,191.3)

#histogram of data
hist(samp,xlab='heights(in cm)',ylab='frequency of students',main=NULL)

#KS-test for normal distribution
#assume H0 true
# Fn vs F0
mu0 <- 176.0; sd0 <- 10.1
plot(ecdf(samp), main = "", ylab = "Probability", xlab='heights (in cm)')
curve(pnorm(x, mean = mu0, sd = sd0), add = TRUE, col = 2)

# Maximum distance
samp_sorted <- sort(samp)
Ui <- pnorm(samp_sorted, mean = mu0, sd = sd0)
Dn_plus <- (1:n) / n - Ui
Dn_minus <- Ui - (1:n - 1) / n
i <- which.max(pmax(Dn_plus, Dn_minus))
lines(rep(samp_sorted[i], 2), 
      c(i / n, pnorm(samp_sorted[i], mean = mu0, sd = sd0)), 
      col = 4, lwd = 2)
rug(samp)
legend("topleft", lwd = 2, col = c(1:2, 4), 
       legend = latex2exp::TeX(c("$F_n:$", "$F_0:$", "sup_x|F_n(x)-F_0(x)|")))

ks.test(samp,'pnorm', mu0, sd0)


#KS-test for Weibull Distribution
#assume H0 true
shape <- 21
scale <- 175
ks.test(samp,'pweibull', shape, scale)