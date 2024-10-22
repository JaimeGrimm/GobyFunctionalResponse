#Load packages
require(frair)
require(ggplot2)

#Load data
live <- read.csv("~/Documents/GitHub/GobyFunctionalResponse/Data/amphipodFRdata.csv")
dead <- read.csv("~/Documents/GitHub/GobyFunctionalResponse/Data/bloodwormFRdata.csv")
sub <- read.csv("~/Documents/GitHub/GobyFunctionalResponse/Data/substratetypeFRdata.csv")

####Method 1 for model selection - Juliano's method####
###Live prey
#live prey, low structure
lno.str <- frair_test(formula = num.killed ~ density, data = live[live$treatment == "no.str",])
lno.str 
#Type II

#live prey, intermediate structure
llow <- frair_test(formula = num.killed ~ density, data = live[live$treatment == "low.c",])
llow
#Type III

#live prey, high structure
lhigh <- frair_test(formula = num.killed ~ density, data = live[live$treatment == "high.c",])
lhigh
#No evidence for any response

###Dead prey
#dead prey, low structure
dno.str <- frair_test(formula = num.killed ~ density, data = dead[dead$treatment == "no.str",])
dno.str
#Type II

#dead prey, intermediate structure
dlow <- frair_test(formula = num.killed ~ density, data = dead[dead$treatment == "low.c",])
dlow
#Type II

#dead prey, high structure
dhigh <- frair_test(formula = num.killed ~ density, data = dead[dead$treatment == "high.c",])
dhigh
#Type II

###Substrate type
#rocks
rocks <- frair_test(formula = num.killed ~ density, data = sub[sub$treatment == "rocks",])
rocks
#Type II

#sand
sand <- frair_test(formula = num.killed ~ density, data = sub[sub$treatment == "sand",])
sand
#Type II

####Method 2 for model selection - Generalized FR with scaling exponent####
###Live prey
#live prey, low structure

llive.flex <- frair_fit(num.killed~density, data=live[live$treatment == "no.str",], response="flexpnr", 
                    start=list(b = 0.5, q = 0, h= 0.5), fixed=list(T = 6))
summary(llive.flex$fit)
#Type II preferred

#live prey, intermediate structure
ilive.flex <- frair_fit(num.killed~density, data=live[live$treatment == "low.c",], response="flexpnr", 
                        start=list(b = 0.5, q = 0, h= 0.5), fixed=list(T = 6))
summary(ilive.flex$fit)
#Borderline, type III preferred

#live prey, high structure
hlive.flex <- frair_fit(num.killed~density, data=live[live$treatment == "high.c",], response="flexpnr", 
                        start=list(b = 0.5, q = 0, h= 0.5), fixed=list(T = 6))
summary(hlive.flex$fit)
#Type II

###Dead prey
#Dead prey, low structure
dl.flex <- frair_fit(num.killed~density, data=dead[dead$treatment == "no.str",], response="flexpnr", 
                     start=list(b = 0.5, q = 0, h= 0.5), fixed=list(T = 6))
summary(dl.flex$fit)
#Type II

#Dead prey, intermediate structure
di.flex <- frair_fit(num.killed~density, data=dead[dead$treatment == "low.c",], response="flexpnr", 
                     start=list(b = 0.5, q = 0, h= 0.5), fixed=list(T = 6))
summary(di.flex$fit)
#Type III

#Dead prey, high structure
dh.flex <- frair_fit(num.killed~density, data=dead[dead$treatment == "high.c",], response="flexpnr", 
                     start=list(b = 0.5, q = 0, h= 0.5), fixed=list(T = 6))
summary(dh.flex$fit)
#Type III

###Substrate type
rocks.flex <- frair_fit(num.killed~density, data=sub[sub$treatment == "rocks",], response="flexpnr", 
                        start=list(b = 0.5, q = 0, h= 0.5), fixed=list(T = 6))
summary(rocks.flex$fit)
#Type II

sand.flex <- frair_fit(num.killed~density, data=sub[sub$treatment == "sand",], response="flexpnr", 
                        start=list(b = 0.5, q = 0, h= 0.5), fixed=list(T = 6))
summary(sand.flex$fit)
#Type III

#### Method 4 for model selection - Visual inspection ####
###Live prey
#Live prey, low struture
prop.ll <- (live[live$treatment=="no.str","num.killed"]/live[live$treatment=="no.str","density"])
plot(live[live$treatment=="no.str","density"], prop.ll)

A <- lowess(live[live$treatment=="no.str","density"], prop.ll, f = 9/10, iter = 3, delta = 0.01 * diff(range(live[live$treatment=="no.str","density"])))
lines(A, lwd=1.5, lty=1, col="red")
#Type III

#Live prey, intermediate struture
prop.li <- (live[live$treatment=="low.c","num.killed"]/live[live$treatment=="low.c","density"])
plot(live[live$treatment=="low.c","density"], prop.li)

A <- lowess(live[live$treatment=="low.c","density"], prop.li, f = 9/10, iter = 3, delta = 0.01 * diff(range(live[live$treatment=="low.c","density"])))
lines(A, lwd=1.5, lty=1, col="red")
#Type III

#Live prey, high struture
prop.lh <- (live[live$treatment=="high.c","num.killed"]/live[live$treatment=="high.c","density"])
plot(live[live$treatment=="high.c","density"], prop.lh)

A <- lowess(live[live$treatment=="high.c","density"], prop.lh, f = 9/10, iter = 3, delta = 0.01 * diff(range(live[live$treatment=="high.c","density"])))
lines(A, lwd=1.5, lty=1, col="red")
#Type III

###Dead prey
#Dead prey, low struture
prop.dl <- (dead[dead$treatment=="no.str","num.killed"]/dead[dead$treatment=="no.str","density"])
plot(dead[dead$treatment=="no.str","density"], prop.dl)

A <- lowess(dead[dead$treatment=="no.str","density"], prop.dl, f = 9/10, iter = 3, delta = 0.01 * diff(range(dead[dead$treatment=="no.str","density"])))
lines(A, lwd=1.5, lty=1, col="red")
#Type II

#Dead prey, intermediate struture
prop.di <- (dead[dead$treatment=="low.c","num.killed"]/dead[dead$treatment=="low.c","density"])
plot(dead[dead$treatment=="low.c","density"], prop.di)

A <- lowess(dead[dead$treatment=="low.c","density"], prop.di, f = 9/10, iter = 3, delta = 0.01 * diff(range(dead[dead$treatment=="low.c","density"])))
lines(A, lwd=1.5, lty=1, col="red")
#Type II

#Dead prey, high struture
prop.dh <- (dead[dead$treatment=="high.c","num.killed"]/dead[dead$treatment=="high.c","density"])
plot(dead[dead$treatment=="high.c","density"], prop.dh)

A <- lowess(dead[dead$treatment=="high.c","density"], prop.dh, f = 10/10, iter = 3, delta = 0.01 * diff(range(dead[dead$treatment=="high.c","density"])))
lines(A, lwd=1.5, lty=1, col="red")
#Type II

###Substrate type
#Rocks
prop.rocks <- (sub[sub$treatment=="rocks","num.killed"]/sub[sub$treatment=="rocks","density"])
plot(sub[sub$treatment=="rocks","density"], prop.rocks)

A <- lowess(sub[sub$treatment=="rocks","density"], prop.rocks, f = 10/10, iter = 3, delta = 0.01 * diff(range(sub[sub$treatment=="rocks","density"])))
lines(A, lwd=1.5, lty=1, col="red")
#Type III

#Sand
prop.sand <- (sub[sub$treatment=="sand","num.killed"]/sub[sub$treatment=="sand","density"])
plot(sub[sub$treatment=="sand","density"], prop.sand)

A <- lowess(sub[sub$treatment=="sand","density"], prop.sand, f = 10/10, iter = 3, delta = 0.01 * diff(range(sub[sub$treatment=="sand","density"])))
lines(A, lwd=1.5, lty=1, col="red")
#Type II

#Method 3 for model selection (AIC comparisons of Type II versus Type III) fits are reported in fitting scripts