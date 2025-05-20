library(frair)

data <- read.csv(here("./Data/substratetypeFRdata.csv"))
summary(data)

#Subset data by treatment
rocks <- subset(data, data$treatment == "rocks")
sand <- subset(data, data$treatment == "sand")

#Rocks- Rogers Type II
set.seed(1)
rocks.rogers <- frair_fit(num.killed~density, data = rocks, response = 'rogersII', start = list(a=1, h=0.1), fixed = list(T=2))

set.seed(2)
rocks.rogers.boot <- frair_boot(rocks.rogers, nboot=500)
confint(rocks.rogers.2.boot, citypes = 'bca')

plot(rocks$density, rocks$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type II - Rocks")
lines(rocks.rogers, lty=2, col='grey50')
drawpoly(rocks.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(rocks.rogers$fit)

#Rocks - Hass Type III
set.seed(1)
rocks.hass <- frair_fit(num.killed~density, data = rocks, response = 'hassIIInr', start = list(b=1, c=1, h=0.1), fixed = list(T=2))
#Can't invert Hessian

set.seed(2)
rocks.hass.boot <- frair_boot(rocks.hass, nboot=500)
confint(rocks.hass.boot, citypes = 'bca')

plot(rocks$density, rocks$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type III - Pebbles")
lines(rocks.hass, lty=2, col='grey50')
drawpoly(rocks.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(rocks.hass$fit)

#Compare models using AIC
AICtab(rocks.rogers$fit,rocks.hass$fit)
ICtab(c(rocks.rogers$fit, rocks.hass$fit), type="AICc", mnames=c("rocks.rogers$fit", "rocks.hass$fit"), weights = TRUE, nobs=nrow(rocks))

#Sand- Rogers Type II
set.seed(1)
sand.rogers <- frair_fit(num.killed~density, data = sand, response = 'rogersII', start = list(a=1, h=0.1), fixed = list(T=2))

set.seed(2)
sand.rogers.boot <- frair_boot(sand.rogers, nboot=500)
confint(sand.rogers.boot, citypes = 'bca')

plot(sand$density, sand$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type II- Sand")
lines(sand.rogers, lty=2, col='grey50')
drawpoly(sand.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(sand.rogers$fit)

#Sand- Hass Type III
set.seed(1)
sand.hass <- frair_fit(num.killed~density, data = sand, response = 'hassIIInr', start = list(b=1, c=1, h=0.1), fixed = list(T=2))

set.seed(2)
sand.hass.boot <- frair_boot(sand.hass, nboot=500)
confint(sand.hass.boot, citypes = 'bca')

plot(sand$density, sand$num.killed, xlab="Prey Density", ylab="Number of Prey Consumed", main = "Functional Response Type III- Sand")
lines(sand.hass, lty=2, col='grey50')
drawpoly(sand.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 0, 0.25), tozero=TRUE)

summary(sand.hass$fit)

#Compare models using AIC
AICtab(sand.rogers$fit,sand.hass$fit)
ICtab(c(sand.rogers$fit, sand.hass$fit), type="AICc", mnames=c("sand.rogers$fit", "sand.hass$fit"), weights = TRUE, nobs=nrow(sand))

plot(x = data$density[data$treatment == "rocks"], y = data$num.killed[data$treatment == "rocks"], pch = 15, col = 'red', xlab="Prey Density", ylab="Number of Prey Consumed")
points(x = data$density[data$treatment == "sand"], y = data$num.killed[data$treatment == "sand"], pch = 16, col = 'blue')

lines(rocks.hass, lty= 1, lwd=2, col='red')
lines(sand.rogers, lty= 2, lwd=2, col='blue')

drawpoly(rocks.hass.boot, probs=c(0.025, 0.975), border=NA, col=rgb(1, 0, 0, 0.25), tozero=TRUE)
drawpoly(sand.rogers.boot, probs=c(0.025, 0.975), border=NA, col=rgb(0, 0, 1, 0.3), tozero=TRUE)

legend("topleft", legend=c("Pebbles", "Sand"), lty= 1:2, pch = 15:16, col=c('red','blue'), title="Substrate type")

