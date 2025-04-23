#=================================================================================================
#Exploring growth-temp outside the model
#
#Krista, April 2025
#=================================================================================================
#Notes:
#=================================================================================================

#get size data=====
wd <- getwd()
surv_lpop <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_lpop.csv"))
surv_apop <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_apop.csv"))

surv_lenfreq <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_lenfreq.csv"))
surv_age <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_age.csv"))

ebs_goa_surv <- read.csv(paste0(wd,"/data/cod length data/survey/Krista_Age_data.csv"))

table(surv_age$year, surv_age$age)



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


#match size to temp data=======================

#GOA match====

goa <- ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"),]

#read in HYCOM temp data
# get hycom data
temp_dat <- read.csv(paste0(wd,"/data/scaled_temp_by_month.csv"))
temp_dat <- temp_dat %>% rename(hycom='mean_monthly',
                                gak='mean_gak_monthly_temp',
                                cfsr='cfsr_temp')

jun_50_hycom <- temp_dat[which(temp_dat$Month==6&temp_dat$depth=="50m"),names(temp_dat) %in% 
                           c("Year","hycom")]
jun_50_hycom <- na.omit(jun_50_hycom)
# hycom is already monthly mean and z-scored
#here I am using 50m depth and June mean
















