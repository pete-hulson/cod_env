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
# surv_lpop <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_lpop.csv"))
# surv_apop <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_apop.csv"))
# 
# surv_lenfreq <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_lenfreq.csv"))
# surv_age <- read.csv(paste0(wd,"/data/cod length data/survey/twl_srvy_age.csv"))

ebs_goa_surv <- read.csv(paste0(wd,"/data/cod length data/survey/Krista_Age_data.csv"))

table(surv_age$year, surv_age$age)

#for EBS, should NEBS be removed?

ebs_goa_surv <- ebs_goa_surv %>% separate(CRUISE, into = c('year', 'cruise_num'), sep = 4)

ebs_goa_surv$cohort <- as.numeric(ebs_goa_surv$year) - ebs_goa_surv$AGE

table(ebs_goa_surv$year, ebs_goa_surv$AGE, ebs_goa_surv$REGION)

#look at data=======================

ggplot(ebs_goa_surv, aes( year, LENGTH, col=as.factor(REGION))) + geom_point() + facet_wrap(~AGE)


ggplot(ebs_goa_surv, aes( LENGTH, WEIGHT, col=as.factor(year))) + geom_point() + facet_wrap(~AGE)

ggplot(ebs_goa_surv, aes( START_LONGITUDE, START_LATITUDE, col=LENGTH)) + geom_point() + facet_wrap(~year)

ggplot(ebs_goa_surv[which(ebs_goa_surv$AGE==3),], aes( START_LONGITUDE, START_LATITUDE, col=LENGTH)) + geom_point() + facet_wrap(~year)

ggplot(ebs_goa_surv[which(ebs_goa_surv$AGE==5),], aes( START_LONGITUDE, START_LATITUDE, col=LENGTH)) + geom_point() + facet_wrap(~year)

ggplot(ebs_goa_surv[which(ebs_goa_surv$AGE==5),], aes( START_LONGITUDE, START_LATITUDE, col=WEIGHT)) + geom_point() + facet_wrap(~year)


#get age-0 data=======

age0dat <- read.csv(paste0(wd,"/data/EMA data/pcod_lengths_5-8-25.csv"))

age0dat <- age0dat %>% separate_wider_delim(haul_date, "/", names = c("month", "day", "year"))

#let's look at data
table(age0dat$gear_performance) #good, satisfactory, and 4 questionable
table(age0dat$region) #NBS (607) and SEBS (15054)
table(age0dat$gear) #3 different types, limit to S tow_type=S and gear = CAN
table(age0dat$gear, age0dat$region)
table(age0dat$length_type) #SL (2029) AND TL (13633)
table(age0dat$lhs_code) #A0, A1+, U
table(age0dat$lhs_code, age0dat$length_type)
table(age0dat$lhs_code, age0dat$region)

ggplot(age0dat, aes(length, col=lhs_code)) + geom_histogram() + facet_wrap(~region)
#Us seem too big

age0dat <- age0dat[which(age0dat$lhs_code!="U"),]
ggplot(age0dat, aes(length, col=lhs_code)) + geom_histogram() + facet_wrap(~region)

ggplot(age0dat[which(age0dat$length>300),], aes(length, col=lhs_code)) + geom_histogram() + facet_wrap(~region)
#some huge A1+s

ggplot(age0dat[which(age0dat$length<300),], aes(length, col=lhs_code)) + geom_histogram() + facet_wrap(~region)
#removing A1+

age0dat <- age0dat[which(age0dat$lhs_code!="A1+"),]
ggplot(age0dat, aes(length, col=lhs_code)) + geom_histogram() + facet_wrap(~region)
#reasonable
#distributions look quite different between NBS and SEBS

ggplot(age0dat, aes(length, col=lhs_code)) + geom_histogram() + facet_wrap(~sample_year) #which yrs reliable?

table(age0dat$sample_year) #huge variation
table(age0dat$region, age0dat$sample_year) #some years only SEBS, some only NBS

ggplot(age0dat, aes(sample_year, length, col=region)) + geom_point()

ggplot(age0dat, aes(sample_year, length, col=month)) + geom_point()
table(age0dat$month, age0dat$year)

ggplot(age0dat, aes(gear_in_longitude, gear_in_latitude)) + geom_point() + facet_wrap(~sample_year)

ggplot(age0dat, aes(haulback_longitude, haulback_latitude, col=length)) + geom_point() + facet_wrap(~sample_year)
ggplot(age0dat, aes(haulback_longitude, haulback_latitude, col=month)) + geom_point() + facet_wrap(~sample_year)

ggplot(age0dat, aes(haulback_longitude, haulback_latitude, col=as.factor(sample_year))) + geom_point()

ggplot(age0dat, aes(haulback_longitude, haulback_latitude, col=day)) + geom_point() + facet_wrap(~month)


#match size to temp data=======================

#GOA match====

goa <- ebs_goa_surv[which(ebs_goa_surv$REGION=="GOA"),]
goa$year <- as.numeric(goa$year)

#goa MOM6

goa_monthly_means_surv_area <- read.csv(file=paste0(wd,"/data/goa_tob_surveyarea_monthly_means_MOM6.csv"))

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

#sebs_monthly_means_surv_area <- read.csv(paste0(wd,"/data/sebs_surveyarea_monthly_means_MOM6.csv"))
sebs_monthly_means_surv_area <- read.csv(file=paste0(wd,"/data/sebs_tob_surveyarea_monthly_means_MOM6.csv"))

seb_size <- ebs_goa_surv[which(ebs_goa_surv$REGION=="BS"),]
seb_size$year <- as.numeric(seb_size$year)

sebs_monthly_means_surv_area$month_name <- paste0("btm_temp_",sebs_monthly_means_surv_area$month)

sebs_monthly_wide <- sebs_monthly_means_surv_area[,-c(2)] %>% pivot_wider(names_from = month_name,
                                                                  values_from = mean_tob)
rm(sebs_monthly_means_surv_area)

seb_w_temp <- left_join(seb_size, sebs_monthly_wide, by=join_by(year==year))

seb_w_temp <- seb_w_temp[ , !names(seb_w_temp) %in% c("geometry")]

#Analysis data====


goa_analysis_dat <- goa_w_temp[which(goa_w_temp$AGE<8),]
goa_analysis_dat$AGE <- as.factor(goa_analysis_dat$AGE)


#Data BS=========================


sebs_analysis_dat <- seb_w_temp[which(seb_w_temp$AGE<8 & seb_w_temp$AGE>0),]
sebs_analysis_dat$AGE <- as.factor(sebs_analysis_dat$AGE)

season_means <- sebs_analysis_dat %>% group_by(year) %>%
                summarise(mean_btm_temp_apr_june=mean(c(btm_temp_4, btm_temp_5,
                                                      btm_temp_6),na.rm=TRUE),
                          mean_btm_temp_JFMA=mean(c(btm_temp_1, btm_temp_2,
                                                    btm_temp_3, btm_temp_4),na.rm=TRUE))

sebs_analysis_dat <- left_join(sebs_analysis_dat, season_means)

sebs_analysis_dat <- sebs_analysis_dat[ , !names(sebs_analysis_dat) %in% c("geometry")]

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

xtra_means <- sebs_monthly_wide %>% group_by(year) %>%
  summarise(mean_btm_temp_apr_june=mean(c(btm_temp_4, btm_temp_5,
                                          btm_temp_6),na.rm=TRUE),
            mean_btm_temp_JFMA=mean(c(btm_temp_1, btm_temp_2,
                                      btm_temp_3, btm_temp_4),na.rm=TRUE))


season_means <- rbind(season_means, xtrayrs) #most of these yrs we have temp data, get from mom6 

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
      temp_temp <- xtra_means$mean_btm_temp_apr_june[which(xtra_means$year==temp_yr-(k-1))] #wrong here
      
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
# xtrayrs <- data.frame(c(1990, 1991, 1992, 1993, 1994, 1995, 
#                         1996, 1997, 1998, 1999, 2020), c(rep(NA, 11)), c(rep(NA, 11)))
# colnames(xtrayrs) <- names(season_means)
# 
# season_means <- rbind(season_means, xtrayrs)

#use xtra_means instead, from above

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
      temp_temp <- xtra_means$mean_btm_temp_apr_june[which(xtra_means$year==temp_yr-(k-1))] #wrong here
      
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


#Bering
names(cohort_means2)

#use season_means from above

#fill in missing years
# xtrayrs <- data.frame(c(1990, 1991, 1992, 1993, 1994, 1995, 
#                         1996, 1997, 1998, 1999, 2020), c(rep(NA, 11)), c(rep(NA, 11)))
# colnames(xtrayrs) <- names(season_means)
# 
# early_means <- rbind(season_means, xtrayrs)

early_dat <- cohort_means2

early_dat$AGE <- as.numeric(as.character(early_dat$AGE))


early_dat$thermalexp_upto3 <- NA

i<-1
for(i in 1:length(early_dat$cohort)){
  temp_row <- early_dat[i,]
  if(temp_row$cohort>2000){
    temp_age <- temp_row$AGE
    temp_co <- temp_row$cohort
        temps <- as.vector(NA)
    for(k in 1:4){ 
      temp_temp <- xtra_means$mean_btm_temp_apr_june[which(xtra_means$year==temp_co+(k-1))] 
      
      temps[k] <- temp_temp 
    }
    early_therm <- mean(temps)
    
    early_dat$thermalexp_upto3[i] <- early_therm
  }
  else next
}
#loop now working! Need to go back and get MOM6 estimates post 2019

ggplot(early_dat, aes(cohort, thermalexp_upto3)) + geom_point() +
  geom_line() + facet_wrap(~AGE)

ggplot(early_dat, aes(thermalexp_upto3, mean_LAA_cohort)) + geom_point() + geom_smooth(method="lm") +
facet_wrap(~AGE, scales="free")





