#install.packages("frair")
require(frair); library(here)


data <- read.csv(here("./Data/bloodwormFRdata.csv"))
summary(data)

#Subset data by treatment
low <- subset(data, data$treatment == "no.str")
int <- subset(data, data$treatment == "low.c")
high <- subset(data, data$treatment == "high.c")

#Low Structure Treatment - Rogers II
set.seed(1)
low.rogers <- frair_fit(num.killed~density, data = low, response = 'rogersII', start = list(a=1, h=0.1), fixed = list(T=2))

set.seed(2)
low.rogers.boot <- frair_boot(low.rogers, nboot=500)
confint(low.rogers.boot, citypes = 'bca')

plot(low$density, low$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type II- Low/Dead")
lines(low.rogers, lty=2, col='grey50')
drawpoly(low.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(low.rogers$fit)

#Low Structure Treatment - Hass III
set.seed(1)
low.hass <- frair_fit(num.killed~density, data = low, response = 'hassIIInr', start = list(b=1, c=1, h=0.1), fixed = list(T=2))
#Can't invert Hessian

set.seed(2)
low.hass.boot <- frair_boot(low.hass, nboot=500)
confint(low.hass.boot, citypes = 'bca')

plot(low$density, low$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type III- Low/Dead")
lines(low.hass, lty=2, col='grey50')
drawpoly(low.hass.boot, probs=c(0.025, 0.975), border=NA, pcol=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(low.hass$fit)

#Compare models using AIC
AICtab(low.rogers$fit,low.hass$fit)
ICtab(c(low.rogers$fit, low.hass$fit), type="AICc", mnames=c("low.rogers$fit", "low.hass$fit"), weights = TRUE, nobs=nrow(low))

#Intermediate Structure Treatment -Rogers II
set.seed(1)
int.rogers <- frair_fit(num.killed~density, data = int, response = 'rogersII', start =list(a=1, h=0.1), fixed = list(T=2))

set.seed(2)
int.rogers.boot <- frair_boot(int.rogers, nboot=500)
confint(int.rogers.boot, citypes = 'bca')

plot(int$density, int$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type II- Int/Dead")
lines(int.rogers, lty=2, col='grey50')
drawpoly(int.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(int.rogers$fit)

#Intermediate Structure Treatment -Hass III
set.seed(1)
int.hass <- frair_fit(num.killed~density, data = int, response = 'hassIIInr', start = list(b=1, c=1, h=0.1), fixed = list(T=2))

set.seed(2)
int.hass.boot <- frair_boot(int.hass, nboot=500)
confint(int.hass.boot, citypes = 'bca')

plot(int$density, int$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type III- Int/Dead")
lines(int.hass, lty=2, col='grey50')
drawpoly(int.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(int.hass$fit)

#Compare models using AIC
AICtab(int.rogers$fit,int.hass$fit)
ICtab(c(int.rogers$fit, int.hass$fit), type="AICc", mnames=c("int.rogers$fit", "int.hass$fit"), weights = TRUE, nobs=nrow(int))

#High Treatment - Rogers II
set.seed(1)
high.rogers <- frair_fit(num.killed~density, data = high, response = 'rogersII', start = list(a=1, h=0.1), fixed = list(T=2))

set.seed(2)
high.rogers.boot <- frair_boot(high.rogers, nboot=500)
confint(high.rogers.boot, citypes = 'bca')

plot(high$density, high$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type II- High/Dead")
lines(high.rogers, lty=2, col='grey50')
drawpoly(high.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(high.rogers$fit)

#High Treatment - Hass III
set.seed(1)
high.hass <- frair_fit(num.killed~density, data = high, response = 'hassIIInr', start = list(b=1, c=1, h=0.1), fixed = list(T=2))

set.seed(2)
high.hass.boot <- frair_boot(high.hass, nboot=500)
confint(high.hass.boot, citypes = 'bca')

plot(high$density, high$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type III- High/Dead")
lines(high.hass, lty=2, col='grey50')
drawpoly(high.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(high.hass$fit)

#Compare models using AIC
AICtab(high.rogers$fit, high.hass$fit)
ICtab(c(high.rogers$fit, high.hass$fit), type="AICc", mnames=c("high.rogers$fit", "high.hass$fit"), weights = TRUE, nobs=nrow(high))

plot(x = data$density[data$treatment == "no.str"], y = data$num.killed[data$treatment == "no.str"], pch = 15, col = 'red', xlab="Prey Density", ylab="Number of Prey Consumed")

#Plotting
points(x = data$density[data$treatment == "low.c"], y = data$num.killed[data$treatment == "low.c"], pch = 16, col = 'blue')
points(x = data$density[data$treatment == "high.c"], y = data$num.killed[data$treatment == "high.c"], pch = 17, col = 'green')

lines(low.rogers, lty= 1, lwd=2, col='red')
lines(int.rogers, lty= 2, lwd=2, col='blue')
lines(high.rogers, lty= 3, lwd=2, col='green')

drawpoly(low.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(1, 0, 0, 0.25), tozero=TRUE)
drawpoly(int.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 1, 0.3), tozero=TRUE)
drawpoly(high.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 1, 0, 0.3), tozero=TRUE)

legend("topleft", legend=c("Low", "Intermediate","High"), lty= 1:3, pch = 15:17, col =c('red', 'blue', 'green'), title="Habitat Complexity")

text(x=1, y=42, 'B', xpd=NA)

