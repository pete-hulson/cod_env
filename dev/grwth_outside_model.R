#=================================================================================================
#Exploring growth-temp outside the model
#
#Krista, April 2025
#=================================================================================================
#Notes:
#=================================================================================================

library(tidyverse)
#library(ggpubr)

#get size data=====
wd <- getwd()
surv_lpop <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_lpop.csv"))
surv_apop <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_apop.csv"))

surv_lenfreq <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_lenfreq.csv"))
surv_age <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_age.csv"))

ebs_goa_surv <- read.csv(paste0(wd,"/data/cod length data/survey/Krista_Age_data.csv"))

table(surv_age$year, surv_age$age)

#for EBS, should NEBS be removed?

ebs_goa_surv <- ebs_goa_surv %>% separate(CRUISE, into = c('year', 'cruise_num'), sep = 4)

table(ebs_goa_surv$year, ebs_goa_surv$AGE, ebs_goa_surv$REGION)

#look at data=======================

ggplot(ebs_goa_surv, aes( year, LENGTH, col=as.factor(REGION))) + geom_point() + facet_wrap(~AGE)


ggplot(ebs_goa_surv, aes( LENGTH, WEIGHT, col=as.factor(year))) + geom_point() + facet_wrap(~AGE)

ggplot(ebs_goa_surv, aes( START_LONGITUDE, START_LATITUDE, col=LENGTH)) + geom_point() + facet_wrap(~year)

ggplot(ebs_goa_surv[which(ebs_goa_surv$AGE==3),], aes( START_LONGITUDE, START_LATITUDE, col=LENGTH)) + geom_point() + facet_wrap(~year)

ggplot(ebs_goa_surv[which(ebs_goa_surv$AGE==5),], aes( START_LONGITUDE, START_LATITUDE, col=LENGTH)) + geom_point() + facet_wrap(~year)

ggplot(ebs_goa_surv[which(ebs_goa_surv$AGE==5),], aes( START_LONGITUDE, START_LATITUDE, col=WEIGHT)) + geom_point() + facet_wrap(~year)


#fit some von Bs===================


#from Franz

# Ludwig von Bertalanffy growth model:
LvB <- function(t, k, L.inf, t0=0) {
  L.inf*(1-exp(-k*(t-t0)))
}

# Explore some reasonable parameter values for starting values:
# We need reasonable starting values for fitting a non-linear model!
plot(LENGTH ~ AGE, data=ebs_goa_surv) #loaded in weight_age_exploration.R
# Some trial values for paramters
curve(LvB(x, k=0.25, L.inf=900), 0, 60, ylim=c(0,900), col=2, lwd=2, add=T)
curve(LvB(x, k=0.2, L.inf=730), 0, 60, col=3, lwd=2, add=T)
curve(LvB(x, k=0.07, L.inf=750, t0=-10), 0, 60, col=4, lwd=2, add=T)
# top line OK

### Fit LvB model across all years:
START <- c(k = 0.25, L.inf = 900, t0 = 0)
fit.all <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv, start=START)
summary(fit.all)
cf <- coef(fit.all)
cf
# Visualize the fitted model:
plot(LENGTH~ AGE, data=ebs_goa_surv)
curve(LvB(x, cf[1],cf[2], cf[3]), col=2, lwd=3, add=T)

#just EBS
START <- c(k = 0.25, L.inf = 900, t0 = 0)
fit.all <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"),], start=START)
summary(fit.all)
cf <- coef(fit.all)
cf
# Visualize the fitted model:
plot(LENGTH~ AGE, data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"),])
curve(LvB(x, cf[1],cf[2], cf[3]), col=2, lwd=3, add=T)


#just GOA
START <- c(k = 0.25, L.inf = 900, t0 = 0)
fit.all <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"),], start=START)
summary(fit.all)
cf <- coef(fit.all)
cf
# Visualize the fitted model:
plot(LENGTH~ AGE, data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"),])
curve(LvB(x, cf[1],cf[2], cf[3]), col=2, lwd=3, add=T)



#just GOA in 2009 versus 2017 versus 2023
START <- c(k = 0.25, L.inf = 900, t0 = 0)
fit.goa09 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                          ebs_goa_surv$year=="2009"),], start=START)
summary(fit.goa09)
cf.goa09 <- coef(fit.goa09)
cf.goa09

fit.goa15 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$year=="2015"),], start=START)
summary(fit.goa15)
cf.goa15 <- coef(fit.goa15)
cf.goa15

fit.goa17 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$year=="2017"),], start=START)
summary(fit.goa17)
cf.goa17 <- coef(fit.goa17)
cf.goa17

fit.goa19 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$year=="2019"),], start=START)
summary(fit.goa19)
cf.goa19 <- coef(fit.goa19)
cf.goa19

fit.goa23 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$year=="2023"),], start=START)
summary(fit.goa23)
cf.goa23 <- coef(fit.goa23)
cf.goa23

# Visualize the fitted model:
plot(LENGTH~ base:::jitter(AGE, 1.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                            ebs_goa_surv$year=="2009"|ebs_goa_surv$year=="2015"|
                                            ebs_goa_surv$year=="2017"|ebs_goa_surv$year=="2019"|ebs_goa_surv$year=="2023"),], col=as.factor(year))
curve(LvB(x, cf.goa09[1],cf.goa09[2], cf.goa09[3]), col=1, lwd=3, add=T)
curve(LvB(x, cf.goa15[1],cf.goa15[2], cf.goa15[3]), col=2, lwd=3, add=T)
curve(LvB(x, cf.goa17[1],cf.goa17[2], cf.goa17[3]), col=3, lwd=3, add=T)
curve(LvB(x, cf.goa19[1],cf.goa19[2], cf.goa19[3]), col=4, lwd=3, add=T)
curve(LvB(x, cf.goa23[1],cf.goa23[2], cf.goa23[3]), col=5, lwd=3, add=T)

legend("bottomright", legend=c(2009, 2015, 2017, 2019, 2023),
       col=c(1:5), lty=1, cex=0.8, title="Year")


#MULTIPANEL
par(mfrow=c(2,3))
plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                            ebs_goa_surv$year=="2009"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goa09[1],cf.goa09[2], cf.goa09[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                            ebs_goa_surv$year=="2015"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goa15[1],cf.goa15[2], cf.goa15[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                            ebs_goa_surv$year=="2017"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goa17[1],cf.goa17[2], cf.goa17[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                            ebs_goa_surv$year=="2019"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goa19[1],cf.goa19[2], cf.goa19[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                            ebs_goa_surv$year=="2023"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goa23[1],cf.goa23[2], cf.goa23[3]), col=1, lwd=3, add=T)



#just EBS in 2009 versus 2017 versus 2023
START <- c(k = 0.25, L.inf = 900, t0 = 0)
fit.ebs09 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$year=="2009"),], start=START)
summary(fit.ebs09)
cf.ebs09 <- coef(fit.ebs09)
cf.ebs09

fit.ebs15 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$year=="2015"),], start=START)
summary(fit.ebs15)
cf.ebs15 <- coef(fit.ebs15)
cf.ebs15

fit.ebs17 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$year=="2017"),], start=START)
summary(fit.ebs17)
cf.ebs17 <- coef(fit.ebs17)
cf.ebs17

fit.ebs19 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$year=="2019"),], start=START)
summary(fit.ebs19)
cf.ebs19 <- coef(fit.ebs19)
cf.ebs19

fit.ebs23 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$year=="2023"),], start=START)
summary(fit.ebs23)
cf.ebs23 <- coef(fit.ebs23)
cf.ebs23

# Visualize the fitted model:
plot(LENGTH~ AGE, data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                            ebs_goa_surv$year=="2009"|ebs_goa_surv$year=="2015"|
                                            ebs_goa_surv$year=="2017"|ebs_goa_surv$year=="2019"|ebs_goa_surv$year=="2023"),], col=as.factor(year))
curve(LvB(x, cf.ebs09[1],cf.ebs09[2], cf.ebs09[3]), col=1, lwd=3, add=T)
curve(LvB(x, cf.ebs15[1],cf.ebs15[2], cf.ebs15[3]), col=2, lwd=3, add=T)
curve(LvB(x, cf.ebs17[1],cf.ebs17[2], cf.ebs17[3]), col=3, lwd=3, add=T)
curve(LvB(x, cf.ebs19[1],cf.ebs19[2], cf.ebs19[3]), col=4, lwd=3, add=T)
curve(LvB(x, cf.ebs23[1],cf.ebs23[2], cf.ebs23[3]), col=5, lwd=3, add=T)


#MULTIPANEL
par(mfrow=c(2,3))
plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$year=="2009"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.ebs09[1],cf.ebs09[2], cf.ebs09[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$year=="2015"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.ebs15[1],cf.ebs15[2], cf.ebs15[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$year=="2017"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.ebs17[1],cf.ebs17[2], cf.ebs17[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$year=="2019"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.ebs19[1],cf.ebs19[2], cf.ebs19[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$year=="2023"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.ebs23[1],cf.ebs23[2], cf.ebs23[3]), col=1, lwd=3, add=T)




#plot by cohort=====

names(ebs_goa_surv)
ebs_goa_surv$cohort <- as.numeric(ebs_goa_surv$year) - ebs_goa_surv$AGE


#just GOA in 2009 versus 2017 versus 2023 BUT COHORT
START <- c(k = 0.25, L.inf = 900, t0 = 0)
fit.goaC09 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$cohort=="2009"),], start=START)
summary(fit.goaC09)
cf.goaC09 <- coef(fit.goaC09)
cf.goaC09

fit.goaC15 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$cohort=="2015"),], start=START)
summary(fit.goaC15)
cf.goaC15 <- coef(fit.goaC15)
cf.goaC15

fit.goaC17 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$cohort=="2017"),], start=START)
summary(fit.goaC17)
cf.goaC17 <- coef(fit.goaC17)
cf.goaC17

fit.goaC05 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$cohort=="2005"),], start=START)
summary(fit.goaC05)
cf.goaC05 <- coef(fit.goaC05)
cf.goaC05

fit.goaC10 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                            ebs_goa_surv$cohort=="2010"),], start=START)
summary(fit.goaC10)
cf.goaC10 <- coef(fit.goaC10)
cf.goaC10

# Visualize the fitted model:
plot(LENGTH~ AGE, data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                            ebs_goa_surv$cohort=="2009"|ebs_goa_surv$cohort=="2015"|
                                            ebs_goa_surv$cohort=="2017"|ebs_goa_surv$cohort=="2019"|ebs_goa_surv$cohort=="2023"),], col=as.factor(cohort))
curve(LvB(x, cf.goaC05[1],cf.goaC05[2], cf.goaC05[3]), col=1, lwd=3, add=T)
curve(LvB(x, cf.goaC09[1],cf.goaC09[2], cf.goaC09[3]), col=2, lwd=3, add=T)
curve(LvB(x, cf.goaC10[1],cf.goaC10[2], cf.goaC10[3]), col=3, lwd=3, add=T)
curve(LvB(x, cf.goaC15[1],cf.goaC15[2], cf.goaC15[3]), col=4, lwd=3, add=T)
curve(LvB(x, cf.goaC17[1],cf.goaC17[2], cf.goaC17[3]), col=5, lwd=3, add=T)



#MULTIPANEL
par(mfrow=c(2,3))
plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                ebs_goa_surv$cohort=="2005"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goaC05[1],cf.goaC05[2], cf.goaC05[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                ebs_goa_surv$cohort=="2009"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goaC09[1],cf.goaC09[2], cf.goaC09[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                ebs_goa_surv$cohort=="2010"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goaC10[1],cf.goaC10[2], cf.goaC10[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                ebs_goa_surv$cohort=="2015"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goaC15[1],cf.goaC15[2], cf.goaC15[3]), col=1, lwd=3, add=T)

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                                                ebs_goa_surv$cohort=="2017"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.goaC17[1],cf.goaC17[2], cf.goaC17[3]), col=1, lwd=3, add=T)

#AH not enough data for cohorts in the GOA

#EBS LvB by cohort=============

#long and ugly

#just GOA in 2009 versus 2017 versus 2023 BUT COHORT
START <- c(k = 0.25, L.inf = 900, t0 = 0)

# fit.bsC88 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="1988"),], start=START)
# summary(fit.bsC88)
# cf.bsC88 <- coef(fit.bsC88)
# cf.bsC88
# 
# fit.bsC89 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="1989"),], start=START)
# summary(fit.bsC89)
# cf.bsC89 <- coef(fit.bsC89)
# cf.bsC89
# 
# fit.bsC90 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="1990"),], start=START)
# summary(fit.bsC90)
# cf.bsC90 <- coef(fit.bsC90)
# cf.bsC90
# 
# fit.bsC91 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="1991"),], start=START)
# summary(fit.bsC91)
# cf.bsC91 <- coef(fit.bsC91)
# cf.bsC91
# 
# fit.bsC92 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="1992"),], start=START)
# summary(fit.bsC92)
# cf.bsC92 <- coef(fit.bsC92)
# cf.bsC92
# 
# fit.bsC93 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="1993"),], start=START)
# summary(fit.bsC93)
# cf.bsC93 <- coef(fit.bsC93)
# cf.bsC93

fit.bsC94 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="1994"),], start=START)
summary(fit.bsC94)
cf.bsC94 <- coef(fit.bsC94)
cf.bsC94

fit.bsC95 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="1995"),], start=START)
summary(fit.bsC95)
cf.bsC95 <- coef(fit.bsC95)
cf.bsC95

fit.bsC96 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="1996"),], start=START)
summary(fit.bsC96)
cf.bsC96 <- coef(fit.bsC96)
cf.bsC96

fit.bsC97 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="1997"),], start=START)
summary(fit.bsC97)
cf.bsC97 <- coef(fit.bsC97)
cf.bsC97

fit.bsC98 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="1998"),], start=START)
summary(fit.bsC98)
cf.bsC98 <- coef(fit.bsC98)
cf.bsC98

fit.bsC99 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="1999"),], start=START)
summary(fit.bsC99)
cf.bsC99 <- coef(fit.bsC99)
cf.bsC99

fit.bsC00 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2000"),], start=START)
summary(fit.bsC00)
cf.bsC00 <- coef(fit.bsC00)
cf.bsC00

fit.bsC01 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2001"),], start=START)
summary(fit.bsC01)
cf.bsC01 <- coef(fit.bsC01)
cf.bsC01

fit.bsC02 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2002"),], start=START)
summary(fit.bsC02)
cf.bsC02 <- coef(fit.bsC02)
cf.bsC02

fit.bsC03 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2003"),], start=START)
summary(fit.bsC03)
cf.bsC03 <- coef(fit.bsC03)
cf.bsC03

fit.bsC04 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2004"),], start=START)
summary(fit.bsC04)
cf.bsC04 <- coef(fit.bsC04)
cf.bsC04

fit.bsC05 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2005"),], start=START)
summary(fit.bsC05)
cf.bsC05 <- coef(fit.bsC05)
cf.bsC05

fit.bsC06 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2006"),], start=START)
summary(fit.bsC06)
cf.bsC06 <- coef(fit.bsC06)
cf.bsC06

fit.bsC07 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2007"),], start=START)
summary(fit.bsC07)
cf.bsC07 <- coef(fit.bsC07)
cf.bsC07

fit.bsC08 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2008"),], start=START)
summary(fit.bsC08)
cf.bsC08 <- coef(fit.bsC08)
cf.bsC08

fit.bsC09 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                             ebs_goa_surv$cohort=="2009"),], start=START)
summary(fit.bsC09)
cf.bsC09 <- coef(fit.bsC09)
cf.bsC09

fit.bsC10 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2010"),], start=START)
summary(fit.bsC10)
cf.bsC10 <- coef(fit.bsC10)
cf.bsC10

fit.bsC11 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2011"),], start=START)
summary(fit.bsC11)
cf.bsC11 <- coef(fit.bsC11)
cf.bsC11

fit.bsC12 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2012"),], start=START)
summary(fit.bsC12)
cf.bsC12 <- coef(fit.bsC12)
cf.bsC12

fit.bsC13 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2013"),], start=START)
summary(fit.bsC13)
cf.bsC13 <- coef(fit.bsC13)
cf.bsC13

fit.bsC14 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2014"),], start=START)
summary(fit.bsC14)
cf.bsC14 <- coef(fit.bsC14)
cf.bsC14

fit.bsC14 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2014"),], start=START)
summary(fit.bsC14)
cf.bsC14 <- coef(fit.bsC14)
cf.bsC14

fit.bsC15 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2015"),], start=START)
summary(fit.bsC15)
cf.bsC15 <- coef(fit.bsC15)
cf.bsC15

fit.bsC16 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2016"),], start=START)
summary(fit.bsC16)
cf.bsC16 <- coef(fit.bsC16)
cf.bsC16

fit.bsC17 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2017"),], start=START)
summary(fit.bsC17)
cf.bsC17 <- coef(fit.bsC17)
cf.bsC17

fit.bsC18 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2018"),], start=START)
summary(fit.bsC18)
cf.bsC18 <- coef(fit.bsC18)
cf.bsC18

fit.bsC19 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2019"),], start=START)
summary(fit.bsC19)
cf.bsC19 <- coef(fit.bsC19)
cf.bsC19

fit.bsC20 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2020"),], start=START)
summary(fit.bsC20)
cf.bsC20 <- coef(fit.bsC20)
cf.bsC20

fit.bsC21 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                            ebs_goa_surv$cohort=="2021"),], start=START)
summary(fit.bsC21)
cf.bsC21 <- coef(fit.bsC21)
cf.bsC21

# fit.bsC22 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="2022"),], start=START)
# summary(fit.bsC22)
# cf.bsC22 <- coef(fit.bsC22)
# cf.bsC22
# 
# fit.bsC23 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="2023"),], start=START)
# summary(fit.bsC23)
# cf.bsC23 <- coef(fit.bsC23)
# cf.bsC23
# 
# fit.bsC24 <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                             ebs_goa_surv$cohort=="2024"),], start=START)
# summary(fit.bsC24)
# cf.bsC24 <- coef(fit.bsC24)
# cf.bsC24
# 

# Visualize the fitted model:
plot(LENGTH~ AGE, data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"),])
curve(LvB(x, cf.bsC00[1],cf.bsC00[2], cf.bsC00[3]), col=1, lty=1, lwd=3, add=T)
curve(LvB(x, cf.bsC01[1],cf.bsC01[2], cf.bsC01[3]), col=2, lty=1,lwd=3, add=T)
curve(LvB(x, cf.bsC02[1],cf.bsC02[2], cf.bsC02[3]), col=3, lty=1,lwd=3, add=T)
curve(LvB(x, cf.bsC03[1],cf.bsC03[2], cf.bsC03[3]), col=4, lty=1,lwd=3, add=T)
curve(LvB(x, cf.bsC04[1],cf.bsC04[2], cf.bsC04[3]), col=5, lty=1,lwd=3, add=T)
curve(LvB(x, cf.bsC05[1],cf.bsC05[2], cf.bsC05[3]), col=6, lty=1,lwd=3, add=T)
curve(LvB(x, cf.bsC06[1],cf.bsC06[2], cf.bsC06[3]), col=7, lty=1,lwd=3, add=T)
curve(LvB(x, cf.bsC07[1],cf.bsC07[2], cf.bsC07[3]), col=8, lty=1,lwd=3, add=T)
curve(LvB(x, cf.bsC08[1],cf.bsC08[2], cf.bsC08[3]), col=1, lty=2, lwd=3, add=T)
curve(LvB(x, cf.bsC09[1],cf.bsC09[2], cf.bsC09[3]), col=2, lty=2, lwd=3, add=T)
curve(LvB(x, cf.bsC10[1],cf.bsC10[2], cf.bsC10[3]), col=3, lty=2, lwd=3, add=T)
curve(LvB(x, cf.bsC11[1],cf.bsC11[2], cf.bsC11[3]), col=4, lty=2, lwd=3, add=T)
curve(LvB(x, cf.bsC12[1],cf.bsC12[2], cf.bsC12[3]), col=5, lty=2, lwd=3, add=T)
curve(LvB(x, cf.bsC13[1],cf.bsC13[2], cf.bsC13[3]), col=6, lty=2, lwd=3, add=T)
curve(LvB(x, cf.bsC14[1],cf.bsC14[2], cf.bsC14[3]), col=7, lty=2, lwd=3, add=T)
curve(LvB(x, cf.bsC15[1],cf.bsC15[2], cf.bsC15[3]), col=8, lty=2, lwd=3, add=T)
curve(LvB(x, cf.bsC16[1],cf.bsC16[2], cf.bsC16[3]), col=1, lty=3, lwd=3, add=T)

legend("bottomright", legend=c(2000:2016),
       col=c(1:8, 1:8, 1), lty=c(rep(1,8), rep(2, 8), 3), cex=0.8, title="Cohort")


#MULTIPANEL

#loop isn't working
# cohorts <- unique(ebs_goa_surv$cohort[which(ebs_goa_surv$REGION=="BS")])
# cohorts <- na.omit(cohorts)
# cohorts <- sort(cohorts, decreasing = FALSE)
# cohorts <- cohorts[which(cohorts>1993 & cohorts<2022)]
# for(i in 1:length(cohorts)){
# temp_co <- cohorts[i]
# temp_n <- str_sub(temp_co,-2,-1)
# plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
#                                                                 ebs_goa_surv$cohort==temp_co),], col=as.factor(year),
#      xlim=c(0,10), ylim=c(100,1100))
# curve(LvB(x, noquote(paste0("cf.bsC",temp_n,"[1]")),
#           noquote(paste0("cf.bsC",temp_n,"[2]")), 
#           noquote(paste0("cf.bsC",temp_n,"[3]"))), 
#       col=1, lwd=3, add=T)
# }

par(mfrow=c(4,4))

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="1994"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC94[1],cf.bsC94[2], cf.bsC94[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "1994")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="1995"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC95[1],cf.bsC95[2], cf.bsC95[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "1995")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="1996"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC96[1],cf.bsC96[2], cf.bsC96[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "1996")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="1997"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC97[1],cf.bsC97[2], cf.bsC97[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "1997")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="1998"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC98[1],cf.bsC98[2], cf.bsC98[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "1998")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="1999"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC99[1],cf.bsC99[2], cf.bsC99[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "1999")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2000"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC00[1],cf.bsC00[2], cf.bsC00[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2000")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2001"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC01[1],cf.bsC01[2], cf.bsC01[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2001")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2002"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC02[1],cf.bsC02[2], cf.bsC02[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2002")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2003"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC03[1],cf.bsC03[2], cf.bsC03[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2003")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2004"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC04[1],cf.bsC04[2], cf.bsC04[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2004")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2005"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC05[1],cf.bsC05[2], cf.bsC05[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2005")


plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2006"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC06[1],cf.bsC06[2], cf.bsC06[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2006")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2007"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC07[1],cf.bsC07[2], cf.bsC07[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2007")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2008"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC08[1],cf.bsC08[2], cf.bsC08[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2008")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2009"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC09[1],cf.bsC09[2], cf.bsC09[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2009")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2010"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC10[1],cf.bsC10[2], cf.bsC10[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2010")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2011"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC11[1],cf.bsC11[2], cf.bsC11[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2011")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2012"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC12[1],cf.bsC12[2], cf.bsC12[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2012")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2013"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC13[1],cf.bsC13[2], cf.bsC13[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2013")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2014"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC14[1],cf.bsC14[2], cf.bsC14[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2014")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2015"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC15[1],cf.bsC15[2], cf.bsC15[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2015")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2016"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC16[1],cf.bsC16[2], cf.bsC16[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2016")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2017"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC17[1],cf.bsC17[2], cf.bsC17[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2017")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2018"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC18[1],cf.bsC18[2], cf.bsC18[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2018")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2019"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC19[1],cf.bsC19[2], cf.bsC19[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2019")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2020"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC20[1],cf.bsC20[2], cf.bsC20[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2020")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2021"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC21[1],cf.bsC21[2], cf.bsC21[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2021")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2022"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC22[1],cf.bsC22[2], cf.bsC22[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2022")

plot(LENGTH~ base:::jitter(AGE, 0.5), data=ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                                                ebs_goa_surv$cohort=="2023"),], col=as.factor(year),
     xlim=c(0,10), ylim=c(100,1100))
curve(LvB(x, cf.bsC23[1],cf.bsC23[2], cf.bsC23[3]), col=1, lwd=3, add=T)
text(x = 1, y = 800, label = "2023")

#match size to temp data=======================

#GOA match====

goa <- ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"),]
goa$year <- as.numeric(goa$year)

#read in HYCOM temp data
# get hycom data
temp_dat <- read.csv(paste0(wd,"/data/scaled_temp_by_month.csv"))
temp_dat <- temp_dat %>% rename(hycom='mean_monthly',
                                gak='gak_best_temp',
                                cfsr='cfsr_temp')

may_100_hycom <- temp_dat[which(temp_dat$Month==5&temp_dat$depth=="100m"),names(temp_dat) %in% 
                           c("Year","hycom")]
may_100_hycom <- na.omit(may_100_hycom)
# hycom is already monthly mean and z-scored
#here I am using 100m depth and May mean

goa_w_temp <- left_join(goa, may_100_hycom, by=join_by(year==Year))
goa_w_temp <- goa_w_temp %>% rename(hycom_may_100='hycom')

#SEBS match=========

sebs_monthly_means_surv_area <- read.csv(paste0(wd,"/data/sebs_surveyarea_monthly_means_MOM6.csv"))

seb_size <- ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"),]
seb_size$year <- as.numeric(seb_size$year)

sebs_monthly_means_surv_area$month_name <- paste0("btm_temp_",sebs_monthly_means_surv_area$month)

sebs_monthly_wide <- sebs_monthly_means_surv_area[,-c(2)] %>% pivot_wider(names_from = month_name,
                                                                  values_from = mean_btm_temp)

seb_w_temp <- left_join(seb_size, sebs_monthly_wide, by=join_by(year==year))



#GAMs on GOA========

library(gamm4)
library(mgcv)
library(itsadug)
library(mgcViz)
library(gratia)
library(gamm4)

table(goa_w_temp$HAUL, goa_w_temp$year) #random should be HAUL in year not just haul

table(goa_w_temp$AGE, goa_w_temp$year)

goa_analysis_dat <- goa_w_temp[which(goa_w_temp$AGE<8),]
goa_analysis_dat$AGE <- as.factor(goa_analysis_dat$AGE)

mod1 <- gamm(LENGTH ~ AGE + s(hycom_may_100, k=5) + ti(START_LONGITUDE, START_LATITUDE, by=AGE),
             random=list(HAUL=~1), data=goa_analysis_dat, REML=FALSE)
gam.check(mod1[[2]])
summary(mod1[[2]])
plot(mod1[[2]])


mod2 <- gamm(LENGTH ~ AGE + s(hycom_may_100, by=AGE, k=5) + ti(START_LONGITUDE, START_LATITUDE, by=AGE),
             random=list(HAUL=~1), data=goa_analysis_dat, REML=FALSE)
gam.check(mod2[[2]])
summary(mod2[[2]])
plot(mod2[[2]])

fvisgam(mod2[[2]], view=c("START_LONGITUDE", "START_LATITUDE"))

v<-getViz(mod2[[2]])

plot(v, select=2:8)

draw(mod2[[2]], dist=0.05, residuals=TRUE)

draw(mod2, dist=0.05)

ggplot(goa_w_temp, aes(hycom_may_100, LENGTH)) + geom_point() + geom_smooth() + facet_wrap(~AGE)

#perhaps spring/summer avg makes more sense
#or thermal experience
#GOA will be hard b/c so few years of data


#Data BS=========================


sebs_analysis_dat <- seb_w_temp[which(seb_w_temp$AGE<8 & seb_w_temp$AGE>0),]
sebs_analysis_dat$AGE <- as.factor(sebs_analysis_dat$AGE)

season_means <- sebs_analysis_dat %>% group_by(year) %>%
                summarise(mean_btm_temp_apr_june=mean(c(btm_temp_4, btm_temp_5,
                                                      btm_temp_6),na.rm=TRUE),
                          mean_btm_temp_JFMA=mean(c(btm_temp_1, btm_temp_2,
                                                    btm_temp_3, btm_temp_4),na.rm=TRUE))

sebs_analysis_dat <- left_join(sebs_analysis_dat, season_means)

last_yr_mean <- sebs_analysis_dat %>% group_by(year, AGE) %>%
  summarize(mean_length_last_year=mean(LENGTH, na.rm=TRUE))

last_yr_mean$match_year <- last_yr_mean$year + 1

last_yr_mean$this_yr_mean <- last_yr_mean$mean_length_last_year

sebs_analysis_dat <- left_join(sebs_analysis_dat, last_yr_mean[,c(1,2,5)], by=join_by(year==year, AGE==AGE))


sebs_analysis_dat <- left_join(sebs_analysis_dat, last_yr_mean[,c(2,3,4)], by=join_by(year==match_year, AGE==AGE))

sebs_analysis_dat <- sebs_analysis_dat[which(duplicated(sebs_analysis_dat)==FALSE),]

#sebs_analysis_dat$indiv_increment <-sebs_analysis_dat$LENGTH - sebs_analysis_dat$mean_length_last_year #FIX HERE should be last year LAST AGE



last_yr_mean$ann_increment <- NA
i<-1
for(i in 1:length(last_yr_mean$year)){
  temp_row <- last_yr_mean[i,]
  temp_yr <- temp_row$year
  temp_age <- as.numeric(as.character(temp_row$AGE))
  if(temp_age>1){
    last_yr_mean$ann_increment[i] <- temp_row$this_yr_mean - 
      last_yr_mean$mean_length_last_year[which(last_yr_mean$year==temp_yr&
                                                 last_yr_mean$AGE==temp_age-1)]
  }
} 

sebs_analysis_dat <- left_join(sebs_analysis_dat, last_yr_mean[,c(1,2,6)])


ggplot(sebs_analysis_dat, aes(year, ann_increment, 
                              col=mean_btm_temp_apr_june)) +
  geom_point() + geom_line() + facet_wrap(~AGE)



#GAMs======

modsb1 <- gamm(LENGTH ~ AGE + s(btm_temp_5, k=5) + ti(START_LONGITUDE, START_LATITUDE, by=AGE),
             #random=list(HAUL=~1), 
             data=sebs_analysis_dat, REML=FALSE)
gam.check(modsb1[[2]])
summary(modsb1[[2]])
plot(modsb1[[2]])


modsb2 <- gamm(LENGTH ~ AGE + s(btm_temp_5, by=AGE, k=5) + ti(START_LONGITUDE, START_LATITUDE, by=AGE),
             #random=list(HAUL=~1), 
             data=sebs_analysis_dat, REML=FALSE)
gam.check(modsb2[[2]])
summary(modsb2[[2]])
plot(modsb2[[2]])


modsb3 <- gamm(LENGTH ~ AGE + s(btm_temp_5, by=AGE, k=5) + ti(START_LONGITUDE, START_LATITUDE),
               #random=list(HAUL=~1), 
               data=sebs_analysis_dat, REML=FALSE)
gam.check(modsb3[[2]])
summary(modsb3[[2]])
plot(modsb3[[2]])

table(sebs_analysis_dat$AGE, sebs_analysis_dat$year)

#nested random effects in gamm4

nest1 <- gamm4(LENGTH ~ AGE + s(btm_temp_5, by=AGE, k=5) + t2(START_LONGITUDE, START_LATITUDE, by=AGE),
               random=~(1|year/HAUL), #haul is nested in year (multiple hauls w/in each year)
               data=sebs_analysis_dat, REML=FALSE) #takes ~30 minutes to run
gam.check(nest1[[2]])
summary(nest1[[2]])
plot(nest1[[2]])

fvisgam(nest1[[2]], view=c("START_LONGITUDE", "START_LATITUDE"))

v<-getViz(nest1[[2]])

plot(v, select=2:8)

draw(nest1[[2]], dist=0.05, residuals=TRUE)

draw(nest1, dist=0.05)

cv <- gratia::conditional_values(nest1) #need to update gratia?
cv |> draw()


nest2 <- gamm4(LENGTH ~ AGE + s(btm_temp_5, k=5) + t2(START_LONGITUDE, START_LATITUDE, by=AGE),
               random=~(1|year/HAUL), #haul is nested in year (multiple hauls w/in each year)
               data=sebs_analysis_dat, REML=FALSE) #takes ~30 minutes to run
gam.check(nest2[[2]])
summary(nest2[[2]])
plot(nest2[[2]])

summary(nest2$gam, nest1$gam)

#with weight

nest_w1 <- gamm4(WEIGHT ~ AGE + s(btm_temp_5, by=AGE, k=5) + t2(START_LONGITUDE, START_LATITUDE, by=AGE),
               random=~(1|year/HAUL), #haul is nested in year (multiple hauls w/in each year)
               data=sebs_analysis_dat, REML=FALSE) #takes ~30 minutes to run
gam.check(nest_w1[[2]])
summary(nest_w1[[2]])
plot(nest_w1[[2]])

draw(nest_w1[[2]], dist=0.05, residuals=TRUE)

draw(nest_w1[[2]], dist=0.05)



#spring growing season

spr1 <- gamm4(LENGTH ~ AGE + s(mean_btm_temp_apr_june, by=AGE, k=5) + t2(START_LONGITUDE, START_LATITUDE, by=AGE),
               random=~(1|year/HAUL), #haul is nested in year (multiple hauls w/in each year)
               data=sebs_analysis_dat, REML=FALSE) #takes ~30 minutes to run
gam.check(spr1[[2]])
summary(spr1[[2]])
plot(spr1[[2]])



draw(spr1[[2]], dist=0.05, residuals=TRUE)

draw(spr1[[2]], select=1:7, dist=0.05, residuals=TRUE)

draw(spr1[[2]], dist=0.05)


#ALSO TRY
#thermal experience?
#increment
#individual increment from last years mean (to keep spatial aspect)


#spring season AND indiv increment

indivspr1 <- gamm4(indiv_increment ~ AGE + s(mean_btm_temp_apr_june, by=AGE, k=5) + t2(START_LONGITUDE, START_LATITUDE, by=AGE),
              random=~(1|year/HAUL), #haul is nested in year (multiple hauls w/in each year)
              data=sebs_analysis_dat, REML=FALSE) #takes ~30 minutes to run
gam.check(indivspr1[[2]])
summary(indivspr1[[2]])
plot(indivspr1[[2]])

draw(indivspr1[[2]], dist=0.05, residuals=TRUE)

draw(indivspr1[[2]], dist=0.05)

ggplot(sebs_analysis_dat[which(sebs_analysis_dat$AGE==3 ),],
       aes(as.factor(year), indiv_increment, col=AGE)) + #geom_point() +
  geom_boxplot()

ggplot(sebs_analysis_dat[which(sebs_analysis_dat$AGE==3|
                                 sebs_analysis_dat$AGE==4 ),],
       aes(as.factor(year), indiv_increment, col=AGE)) + #geom_point() +
  geom_boxplot() + abline(h=0, col="black")

ggplot(sebs_analysis_dat[which(sebs_analysis_dat$AGE==3 ),],
       aes(as.factor(year), indiv_increment, col=mean_btm_temp_apr_june)) + #geom_point() +
  geom_boxplot() + abline(h=0, col="black")

ggplot(sebs_analysis_dat[which(sebs_analysis_dat$AGE==4 ),],
       aes(as.factor(year), indiv_increment, col=mean_btm_temp_apr_june)) + #geom_point() +
  geom_boxplot() + abline(h=0, col="black")



#try to do scatterplot====

#BERING ONLY====

#REMOVING DATA BEFORE 2008 (aging error)

cohort_means <- ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"&
                                     ebs_goa_surv$year>2007),] %>% group_by(cohort, AGE) %>%
  summarize(mean_LAA_cohort=mean(LENGTH, na.rm=TRUE))

cohort_means$names <- paste0("mean_LAA_", cohort_means$AGE)

cohort_wide <- cohort_means[,-c(2)] %>% pivot_wider(names_from= names,values_from = mean_LAA_cohort)

ggplot(cohort_wide, aes(cohort, mean_LAA_2)) + geom_point()

ggplot(cohort_wide, aes(cohort, mean_LAA_5)) + geom_point()

ggplot(cohort_wide, aes( mean_LAA_5, mean_LAA_2, col=cohort)) + geom_point() +
  abline(x=1, y=1)

#plot cohort curves for presentation

ggplot(cohort_means[which(cohort_means$cohort>2008&
                            cohort_means$AGE>0 &
                            cohort_means$AGE<8 &
                            cohort_means$cohort<2021),], aes(AGE, mean_LAA_cohort, col=as.factor(cohort))) +
  geom_point() + geom_line() + theme_bw() +
  ylab("Mean cohort length-at-age") + xlab("Age") +
  scale_colour_discrete(name = "Cohort")



lm2_3 <- lm(mean_LAA_3 ~ mean_LAA_2, data=cohort_wide)
sum_lm2_3 <- summary(lm2_3)
slope_lm2_3 <- lm2_3$coefficients[2]
intercept_lm2_3 <- lm2_3$coefficients[1]
lm2_R2_3 <- sum_lm2_3$r.squared
ggplot(cohort_wide_temp, aes( mean_LAA_3, mean_LAA_2,  col=as.factor(cohort))) + geom_point() +
  abline(intercept=intercept_lm2, slope=slope_lm2) + 
  annotate("text", label=paste0("R2 = ", round(lm2_R2_3,3)), x=440, y=400) 

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_3, mean_LAA_2, col=as.factor(cohort))) + geom_point() 

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_4, mean_LAA_2, col=as.factor(cohort))) + geom_point() 

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_5, mean_LAA_2, col=as.factor(cohort))) + geom_point() 

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_6, mean_LAA_2, col=as.factor(cohort))) + geom_point() 

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_7, mean_LAA_2, col=as.factor(cohort))) + geom_point() 

lm2_4 <- lm(mean_LAA_4 ~ mean_LAA_2, data=cohort_wide)
sum_lm2_4 <- summary(lm2_4)

lm2_5 <- lm(mean_LAA_5 ~ mean_LAA_2, data=cohort_wide)
sum_lm2_5 <- summary(lm2_5)

lm2_6 <- lm(mean_LAA_6 ~ mean_LAA_2, data=cohort_wide)
sum_lm2_6 <- summary(lm2_6)

lm2_7 <- lm(mean_LAA_7 ~ mean_LAA_2, data=cohort_wide)
sum_lm2_7 <- summary(lm2_7)

ggplot(cohort_means[which(cohort_means$AGE<9 &
                            cohort_means$cohort>1997),], aes(AGE, mean_LAA_cohort, col=as.factor(cohort))) + geom_point() +
  geom_line() #+ facet_wrap(~cohort)

ggplot(cohort_means[which(cohort_means$AGE<9 &
                            cohort_means$AGE>0 &
                            cohort_means$cohort>1997),], aes(cohort, mean_LAA_cohort, col=as.factor(AGE))) + geom_point() +
  geom_line() #+ facet_wrap(~cohort)


#repeat plots w LAA1-----


lm1_2 <- lm(mean_LAA_2 ~ mean_LAA_1, data=cohort_wide)
sum_lm1_2 <- summary(lm1_2)
#drop 2017
lm1_2.no17 <- lm(mean_LAA_2 ~ mean_LAA_1, data=cohort_wide[which(cohort_wide$cohort!="2017"),])
summary(lm1_2.no17)


lm1_3 <- lm(mean_LAA_3 ~ mean_LAA_1, data=cohort_wide)
sum_lm1_3 <- summary(lm1_3)
#drop 2017
lm1_3.no17 <- lm(mean_LAA_3 ~ mean_LAA_1, data=cohort_wide[which(cohort_wide$cohort!="2017"),])
summary(lm1_3.no17)

lm1_4 <- lm(mean_LAA_4 ~ mean_LAA_1, data=cohort_wide)
sum_lm1_4 <- summary(lm1_4)
#drop 2017
lm1_4.no17 <- lm(mean_LAA_4 ~ mean_LAA_1, data=cohort_wide[which(cohort_wide$cohort!="2017"),])
summary(lm1_4.no17)

lm1_5 <- lm(mean_LAA_5 ~ mean_LAA_1, data=cohort_wide)
sum_lm1_5 <- summary(lm1_5)
#drop 2017
lm1_5.no17 <- lm(mean_LAA_5 ~ mean_LAA_1, data=cohort_wide[which(cohort_wide$cohort!="2017"),])
summary(lm1_5.no17)

lm1_6 <- lm(mean_LAA_6 ~ mean_LAA_1, data=cohort_wide)
sum_lm1_6 <- summary(lm1_6)
#drop 2017
lm1_6.no17 <- lm(mean_LAA_6 ~ mean_LAA_1, data=cohort_wide[which(cohort_wide$cohort!="2017"),])
summary(lm1_6.no17)

lm1_7 <- lm(mean_LAA_7 ~ mean_LAA_1, data=cohort_wide)
sum_lm1_7 <- summary(lm1_7)
#drop 2017
lm1_7.no17 <- lm(mean_LAA_7 ~ mean_LAA_1, data=cohort_wide[which(cohort_wide$cohort!="2017"),])
summary(lm1_7.no17)

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_2, mean_LAA_1, col=as.factor(cohort))) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_2$r.squared, 3)), x=325, y=230)

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_3, mean_LAA_1, col=as.factor(cohort))) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_3$r.squared, 3)), x=440, y=230)

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_4, mean_LAA_1, col=as.factor(cohort))) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_4$r.squared, 3)), x=530, y=230)

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_5, mean_LAA_1, col=as.factor(cohort))) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_5$r.squared, 3)), x=630, y=230)

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_6, mean_LAA_1, col=as.factor(cohort))) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_6$r.squared, 3)), x=700, y=230)

ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_7, mean_LAA_1, col=as.factor(cohort))) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_7$r.squared, 3)), x=750, y=230)


#all together
library(cowplot)
#edits to do: change x axes, make all on same y axis
l21 <- ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_1, mean_LAA_2)) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_2$r.squared, 3)), y=390, x=160)+ geom_smooth(method="lm") +
  coord_cartesian( xlim=c(130, 270))+ theme_bw()

l31 <- ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_1, mean_LAA_3)) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_3$r.squared, 3)), y=500, x=160)+ geom_smooth(method="lm")+
  coord_cartesian( xlim=c(130, 270))+ theme_bw()

l41 <- ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_1, mean_LAA_4)) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_4$r.squared, 3)), y=590, x=160)+ geom_smooth(method="lm")+
  coord_cartesian( xlim=c(130, 270))+ theme_bw()

l51 <- ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_1, mean_LAA_5)) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_5$r.squared, 3)), y=660, x=160)+ geom_smooth(method="lm")+
  coord_cartesian( xlim=c(130, 270), ylim=c(630,665))+ theme_bw()

l61 <- ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_1, mean_LAA_6)) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_6$r.squared, 3)), y=755, x=160)+ geom_smooth(method="lm")+
  coord_cartesian( xlim=c(130, 270))+ theme_bw()

l71 <- ggplot(cohort_wide[which(cohort_wide$cohort>1997),], aes( mean_LAA_1, mean_LAA_7)) + geom_point() +
  annotate("text", label=paste0("R2 = ", round(sum_lm1_7$r.squared, 3)), y=830, x=160) + geom_smooth(method="lm")+
  coord_cartesian( xlim=c(130, 270)) + theme_bw()


plot_grid(l21, l31, l41, l51, l61, l71) #edit scales, add lines

#try thermal experience======

exp_dat <- sebs_analysis_dat

exp_dat$AGE <- as.numeric(as.character(exp_dat$AGE))

#use season_means from above

#fill in missing years
xtrayrs <- data.frame(c(1990, 1991, 1992, 1993, 1994, 1995, 
                           1996, 1997, 1998, 1999, 2020), c(rep(NA, 11)), c(rep(NA, 11)))
colnames(xtrayrs) <- names(season_means)

season_means <- rbind(season_means, xtrayrs)

exp_dat$mean_spring_exp <- NA
exp_dat$cumm_spring_exp <- NA

i<-1
for(i in 1:length(exp_dat$year)){
  temp_row <- exp_dat[i,]
  if(temp_row$year>2000){
    temp_age <- temp_row$AGE
    temp_yr <- temp_row$year
    temps <- as.vector(NA)
    for(k in 1:(temp_age+1)){
      temp_temp <- season_means$mean_btm_temp_apr_june[which(season_means$year==temp_yr-(k-1))] #wrong here
      
      temps[k] <- temp_temp
    }
    mean_spr_temp <- mean(temps)
    cumm_spir_temp <- sum(temps)
   
    exp_dat$mean_spring_exp[i] <- mean_spr_temp
    exp_dat$cumm_spring_exp[i] <- cumm_spir_temp 
  }
  else next
}


ggplot(exp_dat, aes(year, mean_spring_exp)) + geom_point() +
  geom_line() + facet_wrap(~AGE)

exp_dat$cohort <- exp_dat$year - exp_dat$AGE

ggplot(exp_dat, aes(cohort, mean_spring_exp, col=as.factor(AGE))) + geom_point() +
  geom_line() 

ggplot(exp_dat[which(exp_dat$year>2007),], aes(AGE, mean_spring_exp, col=as.factor(cohort))) + geom_point() +
  geom_line() 

ggplot(exp_dat[which(exp_dat$year>2007&
                       exp_dat$AGE==5),], aes(year, mean_spring_exp, col=as.factor(cohort))) + geom_point() +
  geom_line() 



#repeat cohort plots w thermal exp========

cohort_means2 <- exp_dat[which(exp_dat$REGION=="BS"&
                                 exp_dat$year>2007),] %>% group_by(cohort, AGE) %>%
  summarize(mean_LAA_cohort=mean(LENGTH, na.rm=TRUE))

cohort_means2$names <- paste0("mean_LAA_", cohort_means2$AGE)

#FIX
cohort_means2 <- left_join(cohort_means2, exp_dat[,c(39:41, 20, 21)], by=join_by("cohort"=="cohort", "AGE"=="AGE"))
cohort_means2 <- cohort_means2[which(duplicated(cohort_means2)==FALSE),]

cohort_wide_temp <- cohort_means2[,-c(2)] %>% pivot_wider(names_from= names,values_from = mean_LAA_cohort)

ggplot(cohort_wide_temp, aes(cohort,  mean_LAA_2, col=mean_spring_exp)) + geom_point() 

ggplot(cohort_wide_temp, aes(mean_LAA_2, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
  stat_regline_equation(label.x = 350, label.y = 2)

lm1 <- lm(mean_LAA_1 ~ mean_spring_exp, data=cohort_wide_temp)
sum_lm1 <- summary(lm1)
slope_lm1 <- lm1$coefficients[2]
intercept_lm1 <- lm1$coefficients[1]
lm1_R2 <- sum_lm1$r.squared
p1 <- ggplot(cohort_wide_temp, aes(mean_spring_exp, mean_LAA_1)) + geom_point() +
  geom_abline(intercept=intercept_lm1, slope=slope_lm1) + 
  annotate("text", label=paste0("R2 = ", round(lm1_R2,3)), y=175, x=2.4) + theme_bw()+
  ylab("Length-at-age 1") + xlab("Mean spring thermal experience")

lm2 <- lm(mean_LAA_2 ~ mean_spring_exp, data=cohort_wide_temp)
sum_lm2 <- summary(lm2)
slope_lm2 <- lm2$coefficients[2]
intercept_lm2 <- lm2$coefficients[1]
lm2_R2 <- sum_lm2$r.squared
p2 <- ggplot(cohort_wide_temp, aes(mean_spring_exp,mean_LAA_2)) + geom_point() +
  geom_abline(intercept=intercept_lm2, slope=slope_lm2) + 
  annotate("text", label=paste0("R2 = ", round(lm2_R2,3)), y=325, x=2.4)  + theme_bw()+
  ylab("Length-at-age 2") + xlab("Mean spring thermal experience")

lm3 <- lm(mean_LAA_3 ~ mean_spring_exp, data=cohort_wide_temp)
sum_lm3 <- summary(lm3)
slope_lm3 <- lm3$coefficients[2]
  intercept_lm3 <- lm3$coefficients[1]
  lm3_R2 <- sum_lm3$r.squared
p3 <- ggplot(cohort_wide_temp, aes(mean_spring_exp, mean_LAA_3)) + geom_point() +
  geom_abline(intercept=intercept_lm3, slope=slope_lm3) + annotate("text", label=paste0("R2 = ", 
                                                                    round(lm3_R2,3)), y=440, x=2.4) + theme_bw()+
  ylab("Length-at-age 3") + xlab("Mean spring thermal experience")

lm4 <- lm(mean_LAA_4 ~ mean_spring_exp, data=cohort_wide_temp)
sum_lm4 <- summary(lm4)
slope_lm4 <- lm4$coefficients[2]
intercept_lm4 <- lm4$coefficients[1]
lm4_R2 <- sum_lm4$r.squared
p4 <- ggplot(cohort_wide_temp, aes(mean_spring_exp, mean_LAA_4)) + geom_point() +
  geom_abline(intercept=intercept_lm4, slope=slope_lm4) + 
  annotate("text", label=paste0("R2 = ", round(lm4_R2,3)), y=540, x=2.4) + theme_bw()+
  ylab("Length-at-age 4") + xlab("Mean spring thermal experience")


lm5 <- lm(mean_LAA_5 ~ mean_spring_exp, data=cohort_wide_temp)
sum_lm5 <- summary(lm5)
slope_lm5 <- lm5$coefficients[2]
intercept_lm5 <- lm5$coefficients[1]
lm5_R2 <- sum_lm5$r.squared
p5 <- ggplot(cohort_wide_temp, aes(mean_spring_exp, mean_LAA_5)) + geom_point() +
  geom_abline(intercept=intercept_lm5, slope=slope_lm5) + 
  annotate("text", label=paste0("R2 = ", round(lm5_R2,3)), y=630, x=2.4) + theme_bw()+
  ylab("Length-at-age 5") + xlab("Mean spring thermal experience")


lm6 <- lm(mean_LAA_6 ~ mean_spring_exp, data=cohort_wide_temp)
sum_lm6 <- summary(lm6)
slope_lm6 <- lm6$coefficients[2]
intercept_lm6 <- lm6$coefficients[1]
lm6_R2 <- sum_lm6$r.squared
p6 <- ggplot(cohort_wide_temp, aes(mean_spring_exp, mean_LAA_6)) + geom_point() +
  geom_abline(intercept=intercept_lm6, slope=slope_lm6) + 
  annotate("text", label=paste0("R2 = ", round(lm6_R2,3)), y=700, x=2.4) + theme_bw()+
  ylab("Length-at-age 6") + xlab("Mean spring thermal experience")


lm7 <- lm(mean_LAA_7 ~ mean_spring_exp, data=cohort_wide_temp)
sum_lm7 <- summary(lm7)
slope_lm7 <- lm7$coefficients[2]
intercept_lm7 <- lm7$coefficients[1]
lm7_R2 <- sum_lm7$r.squared
p7 <- ggplot(cohort_wide_temp, aes(mean_spring_exp, mean_LAA_7)) + geom_point() +
  geom_abline(intercept=intercept_lm7, slope=slope_lm7) + 
  annotate("text", label=paste0("R2 = ", round(lm7_R2,3)), y=750, x=2.4) + theme_bw() +
  ylab("Length-at-age 7") + xlab("Mean spring thermal experience")

plot_grid(p1, p2, p3, p4, p5, p6, p7)

ggplot(cohort_wide_temp, aes(cohort,  mean_LAA_5, col=mean_spring_exp)) + geom_point() 

ggplot(cohort_means_temp[which(cohort_means_temp$AGE<9 &
                            cohort_means_temp$cohort>1997),], aes(AGE, mean_LAA_cohort, col=mean_spring_exp)) + geom_point() +
  geom_line() #+ facet_wrap(~cohort)

ggplot(cohort_means_temp[which(cohort_means_temp$AGE<9 &
                            cohort_means_temp$AGE>0 &
                            cohort_means_temp$cohort>1997),], aes(cohort, mean_LAA_cohort, col=as.factor(AGE))) + geom_point() +
  geom_line() #+ facet_wrap(~cohort)


ggplot(cohort_means2[which(cohort_means2$cohort==2017|
                             cohort_means2$cohort==2014|
                             cohort_means2$cohort==2015|
                             cohort_means2$cohort==2016|
                             cohort_means2$cohort==2018),], aes(AGE, mean_LAA_cohort, col=as.factor(cohort))) + geom_point() +
         geom_line()

#repeat temp plots w increments======




names(exp_dat)

ggplot(exp_dat[which(exp_dat$year>2007&exp_dat$AGE>1),], aes(mean_spring_exp, ann_increment)) + geom_point() + 
  facet_wrap(~AGE, scales="free") + geom_smooth(method="lm") + 
  ylab("Annual growth increment") + xlab("Mean Spring thermal experience") + theme_bw()



# cohort_means2$increment <- NA
# cohort_means2$increment <- as.numeric(cohort_means2$increment)
# i<-1
# for(i in 1:length(cohort_means2$cohort)){
#   temp_row <- cohort_means2[i,]
#   temp_cohort <- temp_row$cohort
#   temp_age <- temp_row$AGE
#   
#   if(temp_age>1 & length(cohort_means2$mean_LAA_cohort[which(cohort_means2$AGE==(temp_age-1) &
#                                                                  cohort_means2$cohort==temp_cohort)])>0)
#     {
#   temp_incr <- temp_row$mean_LAA_cohort - cohort_means2$mean_LAA_cohort[which(cohort_means2$AGE==(temp_age-1) &
#                                                                 cohort_means2$cohort==temp_cohort)] #
#   temp_row$increment[i] <- temp_incr
#   }
# }







#GOA ONLY=======================

#REMOVING DATA BEFORE 2008 (aging error)

goa_means <- ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"&
                                     ebs_goa_surv$year>2007),] %>% group_by(cohort, AGE) %>%
  summarize(mean_LAA_cohort=mean(LENGTH, na.rm=TRUE))

goa_means$names <- paste0("mean_LAA_", goa_means$AGE)

goa_wide <- goa_means[,-c(2)] %>% pivot_wider(names_from= names,values_from = mean_LAA_cohort)

ggplot(goa_wide, aes(cohort, mean_LAA_2)) + geom_point()

ggplot(goa_wide, aes(cohort, mean_LAA_5)) + geom_point()

ggplot(goa_wide, aes( mean_LAA_5, mean_LAA_2, col=cohort)) + geom_point() +
  abline(x=1, y=1)


lm2_3 <- lm(mean_LAA_3 ~ mean_LAA_2, data=goa_wide)
sum_lm2_3 <- summary(lm2_3)
slope_lm2_3 <- lm2_3$coefficients[2]
intercept_lm2_3 <- lm2_3$coefficients[1]
lm2_R2_3 <- sum_lm2_3$r.squared
ggplot(goa_wide_temp, aes( mean_LAA_3, mean_LAA_2,  col=as.factor(cohort))) + geom_point() +
  abline(intercept=intercept_lm2, slope=slope_lm2) + 
  annotate("text", label=paste0("R2 = ", round(lm2_R2_3,3)), x=440, y=400) 

ggplot(goa_wide[which(goa_wide$cohort>1997),], aes( mean_LAA_3, mean_LAA_1, col=as.factor(cohort))) + geom_point() + geom_smooth(method="lm")

ggplot(goa_wide[which(goa_wide$cohort>1997),], aes( mean_LAA_5, mean_LAA_1, col=as.factor(cohort))) + geom_point() 

ggplot(goa_wide[which(goa_wide$cohort>1997),], aes( mean_LAA_7, mean_LAA_1, col=as.factor(cohort))) + geom_point() 


ggplot(goa_wide[which(goa_wide$cohort>1997),], aes( mean_LAA_4, mean_LAA_2, col=as.factor(cohort))) + geom_point() 

ggplot(goa_wide[which(goa_wide$cohort>1997),], aes( mean_LAA_6, mean_LAA_2, col=as.factor(cohort))) + geom_point() 

ggplot(goa_wide[which(goa_wide$cohort>1997),], aes( mean_LAA_8, mean_LAA_2, col=as.factor(cohort))) + geom_point() 

lm1_3 <- lm(mean_LAA_3 ~ mean_LAA_1, data=goa_wide)
sum_lm1_3 <- summary(lm1_3)

lm2_4 <- lm(mean_LAA_4 ~ mean_LAA_2, data=goa_wide)
sum_lm2_4 <- summary(lm2_4)

lm1_5 <- lm(mean_LAA_5 ~ mean_LAA_1, data=goa_wide)
sum_lm1_5 <- summary(lm1_5)

lm2_6 <- lm(mean_LAA_6 ~ mean_LAA_2, data=goa_wide)
sum_lm2_6 <- summary(lm2_6)

lm1_7 <- lm(mean_LAA_7 ~ mean_LAA_1, data=goa_wide)
sum_lm1_7 <- summary(lm1_7)

ggplot(goa_means[which(goa_means$AGE<9 &
                            goa_means$cohort>1997),], aes(AGE, mean_LAA_cohort, col=as.factor(cohort))) + geom_point() +
  geom_line() #+ facet_wrap(~cohort)

ggplot(goa_means[which(goa_means$AGE<9 &
                            goa_means$AGE>0 &
                            goa_means$cohort>1997),], aes(cohort, mean_LAA_cohort, col=as.factor(AGE))) + geom_point() +
  geom_line() #+ facet_wrap(~cohort)




#try thermal experience======

exp_dat <- sebs_analysis_dat

exp_dat$AGE <- as.numeric(as.character(exp_dat$AGE))

#use season_means from above

#fill in missing years
xtrayrs <- data.frame(c(1990, 1991, 1992, 1993, 1994, 1995, 
                        1996, 1997, 1998, 1999, 2020), c(rep(NA, 11)), c(rep(NA, 11)))
colnames(xtrayrs) <- names(season_means)

season_means <- rbind(season_means, xtrayrs)

exp_dat$mean_spring_exp <- NA
exp_dat$cumm_spring_exp <- NA

i<-1
for(i in 1:length(exp_dat$year)){
  temp_row <- exp_dat[i,]
  if(temp_row$year>2000){
    temp_age <- temp_row$AGE
    temp_yr <- temp_row$year
    temps <- as.vector(NA)
    for(k in 1:(temp_age+1)){
      temp_temp <- season_means$mean_btm_temp_apr_june[which(season_means$year==temp_yr-(k-1))] #wrong here
      
      temps[k] <- temp_temp
    }
    mean_spr_temp <- mean(temps)
    cumm_spir_temp <- sum(temps)
    
    exp_dat$mean_spring_exp[i] <- mean_spr_temp
    exp_dat$cumm_spring_exp[i] <- cumm_spir_temp 
  }
  else next
}


ggplot(exp_dat, aes(year, mean_spring_exp)) + geom_point() +
  geom_line() + facet_wrap(~AGE)

exp_dat$cohort <- exp_dat$year - exp_dat$AGE

ggplot(exp_dat, aes(cohort, mean_spring_exp, col=as.factor(AGE))) + geom_point() +
  geom_line() 

ggplot(exp_dat[which(exp_dat$year>2007),], aes(AGE, mean_spring_exp, col=as.factor(cohort))) + geom_point() +
  geom_line() 

ggplot(exp_dat[which(exp_dat$year>2007&
                       exp_dat$AGE==5),], aes(year, mean_spring_exp, col=as.factor(cohort))) + geom_point() +
  geom_line() 

#GOA thermal experience-----

#read in HYCOM temp data
# get hycom data
temp_dat <- read.csv(paste0(wd,"/data/scaled_temp_by_month.csv"))
temp_dat <- temp_dat %>% rename(hycom='mean_monthly',
                                gak='mean_gak_monthly_temp',
                                cfsr='cfsr_temp')

spring_150_hycom <- temp_dat[which(temp_dat$Month==4&temp_dat$depth=="150m"|
                                     temp_dat$Month==5&temp_dat$depth=="150m"|
                                     temp_dat$Month==6&temp_dat$depth=="150m"),names(temp_dat) %in% 
                            c("Year","Month", "hycom", "depth")]

hycom_means <- spring_150_hycom %>% group_by(Year) %>%
  summarize(mean_spring_150m=mean(hycom, na.rm=TRUE))





exp_goa <- goa_analysis_dat

exp_goa$AGE <- as.numeric(as.character(exp_goa$AGE))

#use hycom_means from above

#fill in missing years
xtrayrs <- data.frame(c(1990, 1991, 1992, 1994), c(rep(NA, 4)))
colnames(xtrayrs) <- names(hycom_means)

hycom_means <- rbind(hycom_means, xtrayrs)

exp_goa$mean_spring_exp <- NA
exp_goa$cumm_spring_exp <- NA

i<-1
for(i in 1:length(exp_goa$year)){
  temp_row <- exp_goa[i,]
  if(temp_row$year>2000){
    temp_age <- temp_row$AGE
    temp_yr <- temp_row$year
    temps <- as.vector(NA)
    for(k in 1:(temp_age+1)){
      temp_temp <- hycom_means$mean_spring_150m[which(hycom_means$Year==temp_yr-(k-1))] #
      
      temps[k] <- temp_temp
    }
    mean_spr_temp <- mean(temps)
    cumm_spir_temp <- sum(temps)
    
    exp_goa$mean_spring_exp[i] <- mean_spr_temp
    exp_goa$cumm_spring_exp[i] <- cumm_spir_temp 
  }
  else next
}







#repeat cohort plots w thermal exp========

goa_means2 <- exp_goa[which(exp_goa$REGION=="GOA"&
                                 exp_goa$year>2007),] %>% group_by(cohort, AGE) %>%
  summarize(mean_LAA_cohort=mean(LENGTH, na.rm=TRUE))

goa_means2$names <- paste0("mean_LAA_", goa_means2$AGE)

#
goa_means2 <- left_join(goa_means2, exp_goa[,c(20:21, 3, 23, 24)], by=join_by("cohort"=="cohort", "AGE"=="AGE"))
goa_means2 <- goa_means2[which(duplicated(goa_means2)==FALSE),]

goa_wide_temp <- goa_means2[,-c(2)] %>% pivot_wider(names_from= names,values_from = mean_LAA_cohort)

ggplot(goa_wide_temp, aes(cohort,  mean_LAA_2, col=mean_spring_exp)) + geom_point() 

ggplot(goa_wide_temp, aes(mean_LAA_2, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
  stat_regline_equation(label.x = 350, label.y = 2)

lm1 <- lm(mean_LAA_1 ~ mean_spring_exp, data=goa_wide_temp)
sum_lm1 <- summary(lm1)
slope_lm1 <- lm1$coefficients[2]
intercept_lm1 <- lm1$coefficients[1]
lm1_R2 <- sum_lm1$r.squared
ggplot(goa_wide_temp, aes(mean_LAA_1, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
  #abline(intercept=intercept_lm1, slope=slope_lm1) + 
  annotate("text", label=paste0("R2 = ", round(lm1_R2,3)), x=175, y=2.75)

lm2 <- lm(mean_LAA_2 ~ mean_spring_exp, data=goa_wide_temp)
sum_lm2 <- summary(lm2)
slope_lm2 <- lm2$coefficients[2]
intercept_lm2 <- lm2$coefficients[1]
lm2_R2 <- sum_lm2$r.squared
ggplot(goa_wide_temp, aes(mean_LAA_2, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
#  abline(intercept=intercept_lm2, slope=slope_lm2) + 
  annotate("text", label=paste0("R2 = ", round(lm2_R2,3)), x=325, y=2.75) 

lm3 <- lm(mean_LAA_3 ~ mean_spring_exp, data=goa_wide_temp)
sum_lm3 <- summary(lm3)
slope_lm3 <- lm3$coefficients[2]
intercept_lm3 <- lm3$coefficients[1]
lm3_R2 <- sum_lm3$r.squared
ggplot(goa_wide_temp, aes(mean_LAA_3, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
 # abline(intercept=intercept_lm3, slope=slope_lm3) + 
  annotate("text", label=paste0("R2 = ", 
                                                                                   round(lm3_R2,3)), x=440, y=2.75)

lm4 <- lm(mean_LAA_4 ~ mean_spring_exp, data=goa_wide_temp)
sum_lm4 <- summary(lm4)
slope_lm4 <- lm4$coefficients[2]
intercept_lm4 <- lm4$coefficients[1]
lm4_R2 <- sum_lm4$r.squared
ggplot(goa_wide_temp, aes(mean_LAA_4, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
 # abline(intercept=intercept_lm4, slope=slope_lm4) + 
  annotate("text", label=paste0("R2 = ", round(lm4_R2,3)), x=540, y=2.75)


lm5 <- lm(mean_LAA_5 ~ mean_spring_exp, data=goa_wide_temp)
sum_lm5 <- summary(lm5)
slope_lm5 <- lm5$coefficients[2]
intercept_lm5 <- lm5$coefficients[1]
lm5_R2 <- sum_lm5$r.squared
ggplot(goa_wide_temp, aes(mean_LAA_5, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
 # abline(intercept=intercept_lm5, slope=slope_lm5) + 
  annotate("text", label=paste0("R2 = ", round(lm5_R2,3)), x=630, y=2.75)


lm6 <- lm(mean_LAA_6 ~ mean_spring_exp, data=goa_wide_temp)
sum_lm6 <- summary(lm6)
slope_lm6 <- lm6$coefficients[2]
intercept_lm6 <- lm6$coefficients[1]
lm6_R2 <- sum_lm6$r.squared
ggplot(goa_wide_temp, aes(mean_LAA_6, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
  #abline(intercept=intercept_lm6, slope=slope_lm6) + 
  annotate("text", label=paste0("R2 = ", round(lm6_R2,3)), x=700, y=2.75)


lm7 <- lm(mean_LAA_7 ~ mean_spring_exp, data=goa_wide_temp)
sum_lm7 <- summary(lm7)
slope_lm7 <- lm7$coefficients[2]
intercept_lm7 <- lm7$coefficients[1]
lm7_R2 <- sum_lm7$r.squared
ggplot(goa_wide_temp, aes(mean_LAA_7, mean_spring_exp,  col=as.factor(cohort))) + geom_point() +
 # abline(intercept=intercept_lm7, slope=slope_lm7) + 
  annotate("text", label=paste0("R2 = ", round(lm7_R2,3)), x=750, y=2.75)


ggplot(goa_wide_temp, aes(cohort,  mean_LAA_5, col=mean_spring_exp)) + geom_point() 

ggplot(goa_means_temp[which(goa_means_temp$AGE<9 &
                                 goa_means_temp$cohort>1997),], aes(AGE, mean_LAA_cohort, col=mean_spring_exp)) + geom_point() +
  geom_line() #+ facet_wrap(~cohort)

ggplot(goa_means_temp[which(goa_means_temp$AGE<9 &
                                 goa_means_temp$AGE>0 &
                                 goa_means_temp$cohort>1997),], aes(cohort, mean_LAA_cohort, col=as.factor(AGE))) + geom_point() +
  geom_line() #+ facet_wrap(~cohort)


ggplot(goa_means2[which(goa_means2$cohort==2017|
                             goa_means2$cohort==2014|
                             goa_means2$cohort==2015|
                             goa_means2$cohort==2016|
                             goa_means2$cohort==2018),], aes(AGE, mean_LAA_cohort, col=as.factor(cohort))) + geom_point() +
  geom_line()

#repeat temp plots w increments======

#edit below to update for GOA 23-5-25
last_yr_goa <- goa_analysis_dat %>% group_by(year, AGE) %>%
  summarize(mean_length_last_year=mean(LENGTH, na.rm=TRUE))

last_yr_goa$match_year <- last_yr_goa$year + 1

last_yr_goa$this_yr_mean <- last_yr_goa$mean_length_last_year

#join current yr mean
goa_analysis_dat <- left_join(goa_analysis_dat, last_yr_goa[,c(1,2,5)], by=join_by(year==year, AGE==AGE))

#join last yr mean
goa_analysis_dat <- left_join(goa_analysis_dat, last_yr_goa[,c(2,3,4)], by=join_by(year==match_year, AGE==AGE))

goa_analysis_dat <- goa_analysis_dat[which(duplicated(goa_analysis_dat)==FALSE),]



last_yr_goa$ann_increment <- NA
i<-1
for(i in 1:length(last_yr_goa$year)){
  temp_row <- last_yr_goa[i,]
  temp_yr <- temp_row$year
  temp_age <- as.numeric(as.character(temp_row$AGE))
  if(temp_age>1){
    last_yr_goa$ann_increment[i] <- temp_row$this_yr_mean - 
      last_yr_goa$mean_length_last_year[which(last_yr_goa$year==temp_yr&
                                                 last_yr_goa$AGE==temp_age-1)]
  }
} 

goa_analysis_dat <- left_join(goa_analysis_dat, last_yr_goa[,c(1,2,6)])

#join temp data to goa_ananlysis_dat
goa_analysis_dat$AGE <- as.numeric(as.character(goa_analysis_dat$AGE))
goa_analysis_dat <- left_join(goa_analysis_dat, exp_goa[,c(3,20:21, 23:24)],by=join_by(year==year, AGE==AGE, cohort==cohort))
goa_analysis_dat <- goa_analysis_dat2[which(duplicated(goa_analysis_dat)!=TRUE),]



ggplot(goa_analysis_dat[which(goa_analysis_dat$year>2007),], aes(ann_increment, mean_spring_exp)) + geom_point() + 
  facet_wrap(~AGE, scales="free") + geom_smooth(method="lm")


#thermal experience early life only=================

#trying w a match, if not go back to L1221 for loop

#Bering
cohort_means2$thermalexp_upto3 <- NA
names(cohort_means2)
#maybe needs to be run on a df of means not indiv lengths

i<- 1
for(i in 1:length(cohort_means2$thermalexp_upto3)){
temp_row <- cohort_means2[i,]
cohort_means2$thermalexp_upto3[i] <- cohort_means2$mean_spring_exp[which(cohort_means2$cohort==temp_row$cohort&
                                                                           cohort_means2$AGE==3 )]
}
#Above doesn't work for early cohorts b/c we don't have the earlier years included in cohort_means2
#redo using the original thermal experience loop since we do have the temps for those yrs





