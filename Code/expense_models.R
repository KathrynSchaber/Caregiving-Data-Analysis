#######
rm(list = ls())
library(mosaic)
library(dplyr)
library(lme4)
library(bbmle)
library(emmeans)
library(reshape2)

expenses_both_qwb3 <- read.csv("expenses_both_qwb3.csv", sep=",", header=TRUE)
qwb_new_short_save <- read.csv("qwb_new_short_save.csv", sep=",", header=TRUE)
expenses_qwb_by_case <- read.csv("expenses_qwb_by_case.csv", sep=",", header=TRUE)
expenses_qwb_by_helper <- read.csv("expenses_qwb_by_helper.csv", sep=",", header=TRUE)

####### summary of QWB scores ######
summary(expenses_qwb_by_case$base_qwb_score)
summary(expenses_qwb_by_case$min_qwb_score)
summary(expenses_qwb_by_case$base_qwb_score - expenses_qwb_by_case$min_qwb_score)

##### by case #####
favstats(expenses_qwb_by_case$min_qwb_score~expenses_qwb_by_case$someone_cared) ## **
favstats(expenses_qwb_by_case$min_qwb_score~expenses_qwb_by_case$number_people_cared) 
favstats(expenses_qwb_by_case$min_qwb_score~expenses_qwb_by_case$helper_work_affect) ## **

favstats(expenses_qwb_by_case$min_personal_score~expenses_qwb_by_case$someone_cared) ## *
favstats(expenses_qwb_by_case$min_personal_score~expenses_qwb_by_case$number_people_cared) 
favstats(expenses_qwb_by_case$min_personal_score~expenses_qwb_by_case$helper_work_affect) ## *

#####
bi <- ntiles(expenses_qwb_by_case$min_qwb_score, n=2)
tri <- ntiles(expenses_qwb_by_case$min_qwb_score, n=3)
expenses_qwb_by_case$bi <- bi
expenses_qwb_by_case$tri <- tri

favstats(expenses_qwb_by_case$min_qwb_score~expenses_qwb_by_case$bi)
favstats(expenses_qwb_by_case$min_qwb_score~expenses_qwb_by_case$tri)

table(expenses_qwb_by_case$bi, expenses_qwb_by_case$age2)
table(expenses_qwb_by_case$bi, expenses_qwb_by_case$sex)
prop.table(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$age2),margin=1)*100
prop.table(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$sex),margin=1)*100

table(expenses_qwb_by_case$bi, expenses_qwb_by_case$someone_cared) ## *
table(expenses_qwb_by_case$bi, expenses_qwb_by_case$number_people_cared) 
table(expenses_qwb_by_case$bi, expenses_qwb_by_case$helper_work_affect) ## *
prop.table(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$someone_cared),margin=1)*100 ## **
prop.table(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$number_people_cared),margin=1)*100 
prop.table(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$helper_work_affect),margin=1)*100 ## **

##### fisher #####

expenses_qwb_by_case$who_helped2 <- factor(ifelse(expenses_qwb_by_case$who_helped=="relative_live_you","housemate","other"),
                                           levels=c("housemate","other"))

fisher.test(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$age2)) ## p=0.3
fisher.test(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$sex)) ## p=0.8

fisher.test(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$someone_cared)) ## p=0.3
fisher.test(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$number_people_cared)) ## p=0.7
fisher.test(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$helper_work_affect)) ## p=0.03 *
fisher.test(table(expenses_qwb_by_case$bi, expenses_qwb_by_case$who_helped2)) ## p=0.6

# fisher.test(table(expenses_qwb_by_case$tri, expenses_qwb_by_case$age2)) ## p=0.2
# fisher.test(table(expenses_qwb_by_case$tri, expenses_qwb_by_case$sex)) ## p=0.8

fisher.test(table(expenses_qwb_by_case$tri, expenses_qwb_by_case$someone_cared)) ## p=0.03 *
# fisher.test(table(expenses_qwb_by_case$tri, expenses_qwb_by_case$number_people_cared)) ## p=0.8
fisher.test(table(expenses_qwb_by_case$tri, expenses_qwb_by_case$helper_work_affect)) ## p=0.005 **

fisher.test(ftable(expenses_qwb_by_case$tri, expenses_qwb_by_case$someone_cared)[c(1,2),]) # p=0.5
fisher.test(ftable(expenses_qwb_by_case$tri, expenses_qwb_by_case$someone_cared)[c(1,3),]) # p=0.02 *
fisher.test(ftable(expenses_qwb_by_case$tri, expenses_qwb_by_case$someone_cared)[c(2,3),]) # p=0.2

fisher.test(ftable(expenses_qwb_by_case$tri, expenses_qwb_by_case$helper_work_affect)[c(1,2),]) # p=0.02 *
fisher.test(ftable(expenses_qwb_by_case$tri, expenses_qwb_by_case$helper_work_affect)[c(1,3),]) # p=0.005 **
fisher.test(ftable(expenses_qwb_by_case$tri, expenses_qwb_by_case$helper_work_affect)[c(2,3),]) # p=0.6

##
##### by helper #####
favstats(expenses_qwb_by_helper$min_qwb_score~expenses_qwb_by_helper$who_helped) ## **
#favstats(expenses_qwb_by_helper$min_qwb_score~expenses_qwb_by_helper$help_take_care_me)
favstats(expenses_qwb_by_helper$min_qwb_score~expenses_qwb_by_helper$cumm_help_around_house)
favstats(expenses_qwb_by_helper$min_qwb_score~expenses_qwb_by_helper$cumm_help_things_money)
favstats(expenses_qwb_by_helper$min_qwb_score~expenses_qwb_by_helper$helper_work_affect) ## **

favstats(expenses_qwb_by_helper$min_personal_score~expenses_qwb_by_helper$who_helped) 
favstats(expenses_qwb_by_helper$min_personal_score~expenses_qwb_by_helper$cumm_help_around_house)
favstats(expenses_qwb_by_helper$min_personal_score~expenses_qwb_by_helper$cumm_help_things_money)
favstats(expenses_qwb_by_helper$min_personal_score~expenses_qwb_by_helper$helper_work_affect) ## *

######
bi <- ntiles(expenses_qwb_by_helper$min_qwb_score, n=2)
tri <- ntiles(expenses_qwb_by_helper$min_qwb_score, n=3)
expenses_qwb_by_helper$bi <- bi
expenses_qwb_by_helper$tri <- tri

favstats(expenses_qwb_by_helper$min_qwb_score~expenses_qwb_by_helper$bi)
favstats(expenses_qwb_by_helper$min_qwb_score~expenses_qwb_by_helper$tri)

table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$who_helped) ## *
table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$cumm_help_around_house) 
table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$cumm_help_things_money) 
table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$helper_work_affect) ## *
prop.table(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$who_helped),margin=1)*100 ## *
prop.table(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$cumm_help_around_house),margin=1)*100 
prop.table(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$cumm_help_things_money),margin=1)*100 
prop.table(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$helper_work_affect),margin=1)*100 ## **

##### fisher #####
fisher.test(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$who_helped2)) # p=0.2
fisher.test(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$help_take_care_me)) # p=0.6
fisher.test(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$cumm_help_around_house)) # p=0.2
fisher.test(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$cumm_help_things_money))# p=1
fisher.test(table(expenses_qwb_by_helper$bi, expenses_qwb_by_helper$helper_work_affect)) # p=0.01 *

fisher.test(table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$who_helped2)) # p=0.03 *
fisher.test(table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_around_house)) # p=0.4
fisher.test(table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_things_money))# p=0.9
fisher.test(table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$helper_work_affect)) # p=0.003 **

fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$who_helped2)[c(1,2),]) # p=1
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$who_helped2)[c(1,3),]) # p=0.1
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$who_helped2)[c(2,3),]) # p=0.04 *

fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$helper_work_affect)[c(1,2),]) # p=0.03 *
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$helper_work_affect)[c(1,3),]) # p=0.002 **
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$helper_work_affect)[c(2,3),]) # p=0.6

fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_around_house)[c(1,2),]) # p=0.3 
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_around_house)[c(1,3),]) # p=1
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_around_house)[c(2,3),]) # p=0.3
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_things_money)[c(1,2),]) # p=1
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_things_money)[c(1,3),]) # p=1
fisher.test(ftable(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_things_money)[c(2,3),]) # p=0.8
##
#######
#####
help_log1 <- glm(someone_cared ~ 1, data=expenses_both_qwb3, family = "binomial")
help_log1a <- glm(someone_cared ~ sex , data=expenses_both_qwb3, family = "binomial")
help_log1b <- glm(someone_cared ~ age2, data=expenses_both_qwb3, family = "binomial")
help_log1c <- glm(someone_cared ~ house_number_bin, data=expenses_both_qwb3, family = "binomial") ## no malaise bc all have it
help_log1d <- glm(someone_cared ~ min_qwb_score, data=expenses_both_qwb3, family = "binomial") ## no malaise bc all have it
help_log1d2 <- glm(someone_cared ~ bi, data=expenses_both_qwb3, family = "binomial") ## no malaise bc all have it
help_log1d3 <- glm(someone_cared ~ tri, data=expenses_both_qwb3, family = "binomial") ## no malaise bc all have it
#help_log1e <- glm(someone_cared ~ min_personal_score, data=expenses_both_qwb3, family = "binomial") ## no malaise bc all have it
help_log1f <- glm(someone_cared ~ personal_care_help, data=expenses_both_qwb3, family = "binomial")
help_log1g <- glm(someone_cared ~ daily_activity_help, data=expenses_both_qwb3, family = "binomial")
#help_log1h <- glm(someone_cared ~ sex + age2, data=expenses_both_qwb3, family = "binomial")
help_log1i <- glm(someone_cared ~ sex * age2, data=expenses_both_qwb3, family = "binomial")

aic_e <- AIC(help_log1, help_log1a,help_log1b,help_log1c,
             help_log1d,help_log1d2,help_log1d3,
             help_log1e,help_log1f,help_log1g,
             help_log1h,help_log1i)

aic_e[order(aic_e$AIC),]
summary(help_log1b)

anova(help_log1, help_log1a,help_log1b,help_log1c,
      help_log1d,help_log1d2,help_log1d3,
      help_log1f,help_log1g,
      help_log1i, test="Chisq")
dev <- c(anova(help_log1, help_log1a, test="Chisq")[,4], anova(help_log1, help_log1b, test="Chisq")[2,4], anova(help_log1, help_log1c, test="Chisq")[2,4],
  anova(help_log1, help_log1d, test="Chisq")[2,4],anova(help_log1, help_log1d2, test="Chisq")[2,4],anova(help_log1, help_log1d3, test="Chisq")[2,4],
  anova(help_log1, help_log1f, test="Chisq")[2,4],anova(help_log1, help_log1g, test="Chisq")[2,4],
  anova(help_log1, help_log1i, test="Chisq")[2,4])

b <- AICctab(help_log1, help_log1a,help_log1b,help_log1c,
        help_log1d,help_log1d2,help_log1d3,
        help_log1f,help_log1g,
        help_log1i, weights=TRUE, base=TRUE, sort=FALSE)

cbind(data.frame(b[1:4]), dev) ## someone_cared
## by AICc --> b (age2)
emmeans(help_log1b, ~  age2, type="response")


# addmargins(table(expenses_both_qwb3$age3, expenses_both_qwb3$someone_cared))
# 39/(19/6)
# log(12.32)
# (sqrt((1/6)+(1/19)+(1/1)+(1/39)))*1.96
# exp(2.511-2.187)
# exp(2.511+2.187)
exp(cbind(OR = coef(help_log1b2), confint.default(help_log1b2)))

#######
expenses_both_qwb4 <- droplevels(expenses_both_qwb3[which(!(is.na(expenses_both_qwb3$number_people_cared))),])

expenses_both_qwb4$number_people_cared <- factor(expenses_both_qwb4$number_people_cared)
help_log2 <- glm(number_people_cared ~ 1, data=expenses_both_qwb4, family = "binomial")
help_log2a <- glm(number_people_cared ~ sex , data=expenses_both_qwb4, family = "binomial")
help_log2b <- glm(number_people_cared ~ age2, data=expenses_both_qwb4, family = "binomial")
help_log2c <- glm(number_people_cared ~ house_number_bin, data=expenses_both_qwb4, family = "binomial") ## no malaise or fever bc all have it
help_log2d <- glm(number_people_cared ~ min_qwb_score, data=expenses_both_qwb4, family = "binomial")
help_log2d2 <- glm(number_people_cared ~ bi, data=expenses_both_qwb4, family = "binomial") ## no malaise bc all have it
help_log2d3 <- glm(number_people_cared ~ tri, data=expenses_both_qwb4, family = "binomial") ## no malaise bc all have it
#help_log2e <- glm(number_people_cared ~ min_personal_score, data=expenses_both_qwb4, family = "binomial")
help_log2f <- glm(number_people_cared ~ personal_care_help, data=expenses_both_qwb4, family = "binomial")
help_log2g <- glm(number_people_cared ~ daily_activity_help, data=expenses_both_qwb4, family = "binomial")
#help_log2h <- glm(number_people_cared ~ sex + age2, data=expenses_both_qwb4, family = "binomial")
help_log2i <- glm(number_people_cared ~ sex * age2, data=expenses_both_qwb4, family = "binomial")

aic_e <- AIC(help_log2,help_log2a,help_log2b,help_log2c, 
             help_log2d,help_log2d2,help_log2d3,
             help_log2f,help_log2g,
             help_log2i)
aic_e[order(aic_e$AIC),]
anova(help_log2,help_log2a,help_log2b,help_log2c, 
      help_log2d,help_log2d2,help_log2d3,help_log2f,help_log2g,
      help_log2i, test="Chisq")


AICctab(help_log2,help_log2a,help_log2b,help_log2c, 
        help_log2d,help_log2d2,help_log2d3,help_log2f,help_log2g,
        help_log2i, weights=TRUE, base=TRUE, sort=TRUE)



dev <- c(anova(help_log2, help_log2a, test="Chisq")[,4], anova(help_log2, help_log2b, test="Chisq")[2,4], anova(help_log2, help_log2c, test="Chisq")[2,4],
         anova(help_log2, help_log2d, test="Chisq")[2,4],anova(help_log2, help_log2d2, test="Chisq")[2,4],anova(help_log2, help_log2d3, test="Chisq")[2,4],
         anova(help_log2, help_log2f, test="Chisq")[2,4],anova(help_log2, help_log2g, test="Chisq")[2,4],
         anova(help_log2, help_log2i, test="Chisq")[2,4])

b <- AICctab(help_log2, help_log2a,help_log2b,help_log2c,
             help_log2d,help_log2d2,help_log2d3,
             help_log2f,help_log2g,
             help_log2i, weights=TRUE, base=TRUE, sort=FALSE)

cbind(data.frame(b[1:4]), dev) ## number_cared

emmeans(help_log2f, ~  personal_care_help, type="response")

exp(cbind(OR = coef(help_log2f), confint.default(help_log2f)))

##########
##### ONLY 3 PEOPLE WITH HELP FROM OUTSIDE #####
# expenses_both_qwb3$who_helped2 <- factor(ifelse(expenses_both_qwb3$who_helped == "relative_live_you", "house_mem", "not_house_mem"), levels=c("not_house_mem","house_mem"))
# 
# help_log3 <- glm(who_helped2 ~ 1, data=expenses_both_qwb3, family = "binomial")
# help_log3a <- glm(who_helped2 ~ sex , data=expenses_both_qwb3, family = "binomial")
# help_log3b <- glm(who_helped2 ~ age2, data=expenses_both_qwb3, family = "binomial")
# help_log3c <- glm(who_helped2 ~ house_number_bin, data=expenses_both_qwb3, family = "binomial") ## no malaise or fever bc all have it
# help_log3d <- glm(who_helped2 ~ min_qwb_score, data=expenses_both_qwb3, family = "binomial")
# help_log3d2 <- glm(who_helped2 ~ bi, data=expenses_both_qwb3, family = "binomial")
# help_log3d3 <- glm(who_helped2 ~ tri, data=expenses_both_qwb3, family = "binomial")
# help_log3e <- glm(who_helped2 ~ min_personal_score, data=expenses_both_qwb3, family = "binomial")
# help_log3f <- glm(who_helped2 ~ personal_care_help, data=expenses_both_qwb3, family = "binomial")
# help_log3g <- glm(who_helped2 ~ daily_activity_help, data=expenses_both_qwb3, family = "binomial")
# help_log3h <- glm(who_helped2 ~ sex + age2, data=expenses_both_qwb3, family = "binomial")
# help_log3i <- glm(who_helped2 ~ sex * age2, data=expenses_both_qwb3, family = "binomial")
# help_log3j <- glm(who_helped2 ~ min_qwb_score + house_number_bin, data=expenses_both_qwb3, family = "binomial")
# 
# aic_e <- AIC(help_log3,help_log3a,help_log3b,help_log3c, 
#              help_log3d,help_log3d2,help_log3d3,help_log3e,help_log3f,help_log3g,
#              help_log3h, help_log3i)
# aic_e[order(aic_e$AIC),]
# anova(help_log3,help_log3a,help_log3b,help_log3c, 
#       help_log3d,help_log3d2,help_log3d3,help_log3e,help_log3f,help_log3g,
#       help_log3h, help_log3i, test="Chisq")
# 
# 
# AICctab(help_log3,help_log3a,help_log3b,help_log3c, 
#         help_log3d,help_log3d2,help_log3d3,help_log3e,help_log3f,help_log3g,
#         help_log3h, help_log3i,help_log3j, weights=TRUE, base=TRUE, sort=TRUE)

#####

help_log4 <- glm(cumm_help_around_house ~ 1, data=expenses_both_qwb3, family = "binomial")
help_log4a <- glm(cumm_help_around_house ~ sex , data=expenses_both_qwb3, family = "binomial")
help_log4b <- glm(cumm_help_around_house ~ age2, data=expenses_both_qwb3, family = "binomial")
help_log4c <- glm(cumm_help_around_house ~ house_number_bin, data=expenses_both_qwb3, family = "binomial") ## no malaise or fever bc all have it
help_log4d <- glm(cumm_help_around_house ~ min_qwb_score, data=expenses_both_qwb3, family = "binomial")
help_log4d2 <- glm(cumm_help_around_house ~ bi, data=expenses_both_qwb3, family = "binomial")
help_log4d3 <- glm(cumm_help_around_house ~ tri, data=expenses_both_qwb3, family = "binomial")
help_log4f <- glm(cumm_help_around_house ~ personal_care_help, data=expenses_both_qwb3, family = "binomial")
help_log4g <- glm(cumm_help_around_house ~ daily_activity_help, data=expenses_both_qwb3, family = "binomial")
#help_log4h <- glm(cumm_help_around_house ~ sex + age2, data=expenses_both_qwb3, family = "binomial")
help_log4i <- glm(cumm_help_around_house ~ sex * age2, data=expenses_both_qwb3, family = "binomial")

aic_e <- AIC(help_log4,help_log4a,help_log4b,help_log4c,
             help_log4d,help_log4e,help_log4f,help_log4g,
             help_log4h, help_log4i)
aic_e[order(aic_e$AIC),]
anova(help_log4,help_log4a,help_log4b,help_log4c,
      help_log4d,help_log4e,help_log4f,help_log4g,
      help_log4h, help_log4i, test="Chisq")


AICctab(help_log4,help_log4a,help_log4b,help_log4c,
        help_log4d,help_log4e,help_log4f,help_log4g,
        help_log4h, help_log4i, weights=TRUE, base=TRUE, sort=TRUE)

dev <- c(anova(help_log4, help_log4a, test="Chisq")[,4], anova(help_log4, help_log4b, test="Chisq")[2,4], anova(help_log4, help_log4c, test="Chisq")[2,4],
         anova(help_log4, help_log4d, test="Chisq")[2,4],anova(help_log4, help_log4d2, test="Chisq")[2,4],anova(help_log4, help_log4d3, test="Chisq")[2,4],
         anova(help_log4, help_log4f, test="Chisq")[2,4],anova(help_log4, help_log4g, test="Chisq")[2,4],
         anova(help_log4, help_log4i, test="Chisq")[2,4])

b <- AICctab(help_log4, help_log4a,help_log4b,help_log4c,
             help_log4d,help_log4d2,help_log4d3,
             help_log4f,help_log4g,
             help_log4i, weights=TRUE, base=TRUE, sort=FALSE)

cbind(data.frame(b[1:4]), dev) ## help_house
## g --> daily activity help

exp(cbind(OR = coef(help_log4g), confint.default(help_log4g)))

######

help_log5 <- glm(cumm_help_things_money ~ 1, data=expenses_both_qwb3, family = "binomial")
help_log5a <- glm(cumm_help_things_money ~ sex , data=expenses_both_qwb3, family = "binomial")
help_log5b <- glm(cumm_help_things_money ~ age2, data=expenses_both_qwb3, family = "binomial")
help_log5c <- glm(cumm_help_things_money ~ house_number_bin, data=expenses_both_qwb3, family = "binomial") ## no malaise or fever bc all have it
help_log5d <- glm(cumm_help_things_money ~ min_qwb_score, data=expenses_both_qwb3, family = "binomial")
help_log5d2 <- glm(cumm_help_things_money ~ bi, data=expenses_both_qwb3, family = "binomial")
help_log5d3 <- glm(cumm_help_things_money ~ tri, data=expenses_both_qwb3, family = "binomial")
#help_log5e <- glm(cumm_help_things_money ~ min_personal_score, data=expenses_both_qwb3, family = "binomial")
help_log5f <- glm(cumm_help_things_money ~ personal_care_help, data=expenses_both_qwb3, family = "binomial")
help_log5g <- glm(cumm_help_things_money ~ daily_activity_help, data=expenses_both_qwb3, family = "binomial")
#help_log5h <- glm(cumm_help_things_money ~ sex + age2, data=expenses_both_qwb3, family = "binomial")
help_log5i <- glm(cumm_help_things_money ~ sex * age2, data=expenses_both_qwb3, family = "binomial")
help_log5j <- glm(cumm_help_things_money ~ personal_care_help + house_number_bin, data=expenses_both_qwb3, family = "binomial")
help_log5k <- glm(cumm_help_things_money ~ personal_care_help + min_personal_score, data=expenses_both_qwb3, family = "binomial")
help_log5l <- glm(cumm_help_things_money ~ house_number_bin + min_personal_score, data=expenses_both_qwb3, family = "binomial")
help_log5j2 <- glm(cumm_help_things_money ~ personal_care_help * house_number_bin, data=expenses_both_qwb3, family = "binomial")
help_log5k2 <- glm(cumm_help_things_money ~ personal_care_help * min_personal_score, data=expenses_both_qwb3, family = "binomial")


aic_e <- AIC(help_log5,help_log5a,help_log5b,help_log5c, 
             help_log5d,help_log5d2,help_log5d3,help_log5f,help_log5g,
              help_log5i,help_log5k,help_log5l,help_log5j2,help_log5k2)
aic_e[order(aic_e$AIC),]
anova(help_log5,help_log5a,help_log5b,help_log5c, 
      help_log5d,help_log5d2,help_log5d3,help_log5e,help_log5f,help_log5g,
      help_log5h, help_log5i, test="Chisq")

AICctab(help_log5,help_log5a,help_log5b,help_log5c, 
        help_log5d,help_log5d2,help_log5d3,help_log5f,help_log5g,
         help_log5i,
        help_log5j, help_log5k, help_log5l, 
        help_log5j2, help_log5k2, weights=TRUE, base=TRUE, sort=TRUE)


dev <- c(anova(help_log5, help_log5a, test="Chisq")[,4], anova(help_log5, help_log5b, test="Chisq")[2,4], anova(help_log5, help_log5c, test="Chisq")[2,4],
         anova(help_log5, help_log5d, test="Chisq")[2,4],anova(help_log5, help_log5d2, test="Chisq")[2,4],anova(help_log5, help_log5d3, test="Chisq")[2,4],
         anova(help_log5, help_log5f, test="Chisq")[2,4],anova(help_log5, help_log5g, test="Chisq")[2,4],
         anova(help_log5, help_log5i, test="Chisq")[2,4],anova(help_log5, help_log5j, test="Chisq")[2,4])

b <- AICctab(help_log5, help_log5a,help_log5b,help_log5c,
             help_log5d,help_log5d2,help_log5d3,
             help_log5f,help_log5g,
             help_log5i,help_log5j, weights=TRUE, base=TRUE, sort=FALSE)

cbind(data.frame(b[1:4]), dev) ## things

emlog1b <- emmeans(help_log5j, ~  house_number_bin, type="response")
emlog1c <- emmeans(help_log5j, ~  personal_care_help, type="response")
summary(help_log5j)
anova(help_log5j, test="Chisq")

exp(cbind(OR = coef(help_log5f), confint.default(help_log5f)))
exp(cbind(OR = coef(help_log5c), confint.default(help_log5c)))
exp(cbind(OR = coef(help_log5j), confint.default(help_log5j)))


emmip(help_log5j, personal_care_help~house_number_bin,CIs=TRUE, type="response") +
  scale_x_discrete(labels=c("Less than 8","More than 8")) +
  scale_y_continuous(limits=c(0,1), breaks=c(seq(0,1,0.25))) +
  labs(x="Number of Housemates", y="Predicted Probability of Receiving Money or Things") +
  scale_color_discrete("Needed Help with\nPersonal Care", labels=c("No","Yes")) +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=15),
        legend.title = element_text(size=14),
        legend.title.align=0.5,
        legend.text = element_text(size=12))




aa <-emmip(help_log5j, personal_care_help~house_number_bin,CIs=TRUE, type="response", plotit = FALSE)
ggplot(data=aa, aes(x=yvar, y=house_number_bin, group=personal_care_help, 
                    color=personal_care_help)) +
  geom_point(position=position_dodgev(height=0.5), size=5) +
  geom_linerangeh(aes(y=house_number_bin, xmin=LCL, xmax=UCL,
                      group=personal_care_help), 
                  position=position_dodgev(height=0.5), size=7, alpha=0.4)+
  scale_y_discrete(labels=c("Less than 8","More than 8")) +
  scale_x_continuous(limits=c(0,1), breaks=c(seq(0,1,0.25))) +
  labs(y="Number of Housemates",
       x="Predicted Probability of Receiving Money or Things") +
  scale_color_discrete("Needed Help with\nPersonal Care", labels=c("No","Yes")) +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=15),
        legend.title = element_text(size=14),
        legend.title.align=0.5,
        legend.text = element_text(size=12))





########

help_log6 <- glm(helper_work_affect ~ 1, data=expenses_both_qwb3, family = "binomial")
help_log6a <- glm(helper_work_affect ~ sex , data=expenses_both_qwb3, family = "binomial")
help_log6b <- glm(helper_work_affect ~ age2, data=expenses_both_qwb3, family = "binomial")
help_log6c <- glm(helper_work_affect ~ house_number_bin, data=expenses_both_qwb3, family = "binomial") ## no malaise or fever bc all have it
help_log6d <- glm(helper_work_affect ~ min_qwb_score, data=expenses_both_qwb3, family = "binomial")
help_log6d2 <- glm(helper_work_affect ~ bi, data=expenses_both_qwb3, family = "binomial")
help_log6d3 <- glm(helper_work_affect ~ tri, data=expenses_both_qwb3, family = "binomial")
#help_log6e <- glm(helper_work_affect ~ min_personal_score, data=expenses_both_qwb3, family = "binomial")
help_log6f <- glm(helper_work_affect ~ personal_care_help, data=expenses_both_qwb3, family = "binomial")
help_log6g <- glm(helper_work_affect ~ daily_activity_help, data=expenses_both_qwb3, family = "binomial")
#help_log6h <- glm(helper_work_affect ~ sex + age2, data=expenses_both_qwb3, family = "binomial")
help_log6i <- glm(helper_work_affect ~ sex * age2, data=expenses_both_qwb3, family = "binomial")
help_log6j <- glm(helper_work_affect ~ min_qwb_score + age2, data=expenses_both_qwb3, family = "binomial")
help_log6k <- glm(helper_work_affect ~ min_qwb_score * age2, data=expenses_both_qwb3, family = "binomial")

aic_e <- AIC(help_log6,help_log6a,help_log6b,help_log6c, 
             help_log6d,help_log6d2,help_log6d3,help_log6e,help_log6f,help_log6g,
             help_log6h, help_log6i)
aic_e[order(aic_e$AIC),]
anova(help_log6,help_log6a,help_log6b,help_log6c, 
      help_log6d,help_log6d2,help_log6d3,help_log6e,help_log6f,help_log6g,
      help_log6h, help_log6i, test="Chisq")


AICctab(help_log6,help_log6a,help_log6b,help_log6c, 
        help_log6d,help_log6d2,help_log6d3,help_log6e,help_log6f,help_log6g,
        help_log6h, help_log6i,help_log6j, help_log6k, weights=TRUE, base=TRUE, sort=TRUE)




dev <- c(anova(help_log6, help_log6a, test="Chisq")[,4], anova(help_log6, help_log6b, test="Chisq")[2,4], anova(help_log6, help_log6c, test="Chisq")[2,4],
         anova(help_log6, help_log6d, test="Chisq")[2,4],anova(help_log6, help_log6d2, test="Chisq")[2,4],anova(help_log6, help_log6d3, test="Chisq")[2,4],
         anova(help_log6, help_log6f, test="Chisq")[2,4],anova(help_log6, help_log6g, test="Chisq")[2,4],
         anova(help_log6, help_log6i, test="Chisq")[2,4])

b <- AICctab(help_log6, help_log6a,help_log6b,help_log6c,
             help_log6d,help_log6d2,help_log6d3,
             help_log6f,help_log6g,
             help_log6i, weights=TRUE, base=TRUE, sort=FALSE)

cbind(data.frame(b[1:4]), dev) ## work fx



summary(help_log6d)
## tertile min QWB score ##
#### MIN QWB SCORE ##
emmeans(help_log6d3, ~tri, type="response")
emmip(help_log6d3, ~tri,CIs=TRUE, type="response")

help_log6d3b <- glm(helper_work_affect ~ tri2, data=expenses_both_qwb3, family = "binomial")

exp(cbind(OR = coef(help_log6d3b), confint.default(help_log6d3b)))


#############
#############
#############
#############
