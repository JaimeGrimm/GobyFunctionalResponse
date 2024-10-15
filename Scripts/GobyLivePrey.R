require(frair)

data <- read.csv("~/Documents/GitHub/GobyFunctionalResponse/Data/amphipodFRdata.csv")
summary(data)

#Subset data by treatment
low.c <- subset(data, data$treatment == "no.str")
int.c <- subset(data, data$treatment == "low.c")
high.c <- subset(data, data$treatment == "high.c")

#Low Structure Treatment - Rogers Type II
set.seed(1)
low.rogers <- frair_fit(num.killed~density, data = low.c, response = 'rogersII', start = list(a=1, h=0.1), fixed = list(T=2))

set.seed(2)
low.rogers.boot <- frair_boot(low.rogers, nboot=500)
confint(low.rogers.boot, citypes = 'bca')

plot(low.c$density, low.c$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type II- Low/Live")
lines(low.rogers, lty=2, col='grey50')
drawpoly(low.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(low.rogers$fit)

#Low Structure Treatment - Hass Type III
set.seed(1)
low.hass <- frair_fit(num.killed~density, data = low.c, response = 'hassIIInr', start = list(b=1, c=1, h=0.1), fixed = list(T=2))

set.seed(2)
low.hass.boot <- frair_boot(low.hass, nboot=500)
confint(low.hass.boot, citypes = 'bca')

plot(low.c$density, low.c$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type III- Low/Live")
lines(low.hass, lty=2, col='grey50')
drawpoly(low.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(low.hass$fit)

#Compare models using AIC
AICtab(low.rogers$fit,low.hass$fit)
ICtab(c(low.rogers$fit, low.hass$fit), type="AICc", mnames=c("low.rogers.2$fit", "low.hass.3$fit"), weights = TRUE, nobs=nrow(low.c))

#Intermediate Structure Treatment - Rogers Type II
set.seed(1)
int.rogers <- frair_fit(num.killed~density, data = int.c, response = 'rogersII', start = list(a=1, h=0.1), fixed = list(T=2))
#Can't invert Hessian

set.seed(2)
int.rogers.boot <- frair_boot(int.rogers, nboot=500)
confint(int.rogers.boot, citypes = 'bca')

plot(int.c$density, int.c$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type II- Int/Live")
lines(int.rogers, lty=2, col='grey50')
drawpoly(int.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(int.rogers$fit)

#Intermediate Structure Treatment - Hass Type III
set.seed(1)
int.hass <- frair_fit(num.killed~density, data = int.c, response = 'hassIIInr', start = list(b=1, c=1, h=0.1), fixed = list(T=2))

set.seed(2)
int.hass.boot <- frair_boot(int.hass, nboot=500)
confint(int.hass.boot, citypes = 'bca')

plot(int.c$density, int.c$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type III- Int/Live")
lines(int.hass, lty=2, col='grey50')
drawpoly(int.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(int.hass$fit)

#Compare models using AIC 
AICtab(int.rogers$fit,int.hass$fit)
ICtab(c(int.rogers$fit, int.hass$fit), type="AICc", mnames=c("int.rogers$fit", "int.hass$fit"), weights = TRUE, nobs=nrow(int.c))

#High Structure Treatment- Rogers Type II
set.seed(1)
high.rogers <- frair_fit(num.killed~density, data = high.c, response = 'rogersII', start = list(a=1, h=0.1), fixed = list(T=2))

set.seed(2)
high.rogers.boot <- frair_boot(high.rogers, nboot=500)
confint(high.rogers.boot, citypes = 'bca')

plot(high.c$density, high.c$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type II- High/Live")
lines(high.rogers, lty=2, col='grey50')
drawpoly(high.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(high.rogers$fit)

#High Structure Treatment - Hass Type III
set.seed(1)
high.hass <- frair_fit(num.killed~density, data = high.c, response = 'hassIIInr', start = list(b=1, c=1, h=0.1), fixed = list(T=2))

set.seed(2)
high.hass.boot <- frair_boot(high.hass, nboot=500)
confint(high.hass.boot, citypes = 'bca')

plot(high.c$density, high.c$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type III- High/Live")
lines(high.hass, lty=2, col='grey50')
drawpoly(high.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(high.hass$fit)

#Compare models using AIC
AICtab(high.rogers$fit, high.hass$fit)
ICtab(c(high.rogers$fit, high.hass$fit), type="AICc", mnames=c("high.rogers.2$fit", "high.hass.3$fit"), weights = TRUE, nobs=nrow(high.c))

#Plot all together
plot(data$density[data$treatment == "no.str"], y = data$num.killed[data$treatment == "no.str"], pch = 15, col = 'red', xlab="Prey Density", ylab="Number of Prey Consumed") 
points(x = data$density[data$treatment == "low.c"], y = data$num.killed[data$treatment == "low.c"], pch = 16, col = 'blue')
points(data$density[data$treatment == "high.c"], y = data$num.killed[data$treatment == "high.c"], pch = 17, col = 'green')

lines(low.hass, lty= 1, lwd=2, col='red')
lines(int.hass, lty= 2, lwd=2, col='blue')
lines(high.hass, lty = 3, lwd=2, col='green')

drawpoly(low.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(1, 0, 0, 0.25), tozero=TRUE)
drawpoly(int.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 1, 0.3), tozero=TRUE)
drawpoly(high.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 1, 0, 0.5), tozero=TRUE)

legend("topleft", legend=c("Low", "Intermediate","High"),  lty= 1:3, pch = 15:17, col =c('red', 'blue', 'green'), title="Habitat Complexity")

text(x=1, y=36, 'A', xpd=NA)

