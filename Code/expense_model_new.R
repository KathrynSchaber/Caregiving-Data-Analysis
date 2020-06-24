#######
rm(list = ls())
library(mosaic)
library(dplyr)
library(lme4)
library(bbmle)
library(emmeans)
setwd("/Users/Kathryn/Desktop/MS3/Aim3--R/Data")

part_data <- read.csv("part_data.csv", sep=",", header=TRUE)
socio_data <- read.csv("socio_data.csv", sep=",", header=TRUE)
expenses_qwb_by_case <- read.csv("expenses_qwb_by_case_house.csv")
#######
part_data <- read.csv("part_data.csv", sep=",", header=TRUE)
qwb <- read.csv("qwb_data.csv", sep=",", header=TRUE)

part_data2 <- droplevels(part_data[which(part_data$diagnosis == "deng2"),])
str(as.factor(part_data2$case_name)) ## 87
part_data2 <- part_data2[!duplicated(part_data2$case_name),]
### "SA319BP02" was asymptomtic ###
part_data2 <- droplevels(part_data2[-c(which(part_data2$case_name == "SA319BP02")),])

qwb_merge <- droplevels(merge(part_data2, qwb, by.x="case_name", by.y="case_name"))
str(qwb_merge)

qwb_merge2 <- qwb_merge[,c(1,4:7,9,11:15)]
qwb_merge2b <- qwb_merge2[,c(1,4,5,3,2,6:11)]
qwb_merge2b$which_interview <- factor(qwb_merge2b$which_interview, levels=c("day3","week","month"))
qwb_merge3 <- qwb_merge2b[order(qwb_merge2b$case_name, qwb_merge2b$which_interview),]

qwb_merge3$age2 <- character(length=length(qwb_merge3$case_name))
qwb_merge3$age2[which(qwb_merge3$age < 18)] <- "child"
qwb_merge3$age2[which(qwb_merge3$age >= 18)] <- "adult"
qwb_merge3$age2 <- factor(qwb_merge3$age2, levels=c("child","adult"))

qwb_merge_long <- rbind(qwb_merge3, qwb_merge3, qwb_merge3)
qwb_merge_long <- qwb_merge_long[order(qwb_merge_long$case_name, qwb_merge_long$which_interview),c(1,2,3,12,4:11)]

qwb_merge_long$day_about_helped <- as.character(rep(c(-3:-1), nrow(qwb_merge3)))
qwb_merge_long$day_about_helped[which(qwb_merge_long$day_about_helped == -3)] <- "3_days_ago"
qwb_merge_long$day_about_helped[which(qwb_merge_long$day_about_helped == -2)] <- "2_days_ago"
qwb_merge_long$day_about_helped[which(qwb_merge_long$day_about_helped == -1)] <- "1_days_ago"

rows_3_days <- as.vector(grep("3_days_ago", qwb_merge_long$did_help_with_personal_care)) 
rows_2_days <- as.vector(grep("2_days_ago", qwb_merge_long$did_help_with_personal_care)) 
rows_1_days <- as.vector(grep("yesterday", qwb_merge_long$did_help_with_personal_care)) 
rows_no_days <- as.vector(grep("any_day", qwb_merge_long$did_help_with_personal_care)) 

qwb_merge_long$did_help_with_personal_care2 <- as.character("no")
qwb_merge_long$did_help_with_personal_care2[c(rows_3_days[which(qwb_merge_long$day_about_helped[rows_3_days] == "3_days_ago")])] <- "yes"
qwb_merge_long$did_help_with_personal_care2[c(rows_2_days[which(qwb_merge_long$day_about_helped[rows_2_days] == "2_days_ago")])] <- "yes"
qwb_merge_long$did_help_with_personal_care2[c(rows_1_days[which(qwb_merge_long$day_about_helped[rows_1_days] == "1_days_ago")])] <- "yes"

rows_3_days <- as.vector(grep("3_days_ago", qwb_merge_long$not_drive_for_health)) 
rows_2_days <- as.vector(grep("2_days_ago", qwb_merge_long$not_drive_for_health)) 
rows_1_days <- as.vector(grep("yesterday", qwb_merge_long$not_drive_for_health)) 
rows_no_days <- as.vector(grep("any_day", qwb_merge_long$not_drive_for_health)) 

qwb_merge_long$not_drive_for_health2 <- as.character("no")
qwb_merge_long$not_drive_for_health2[c(rows_3_days[which(qwb_merge_long$day_about_helped[rows_3_days] == "3_days_ago")])] <- "yes"
qwb_merge_long$not_drive_for_health2[c(rows_2_days[which(qwb_merge_long$day_about_helped[rows_2_days] == "2_days_ago")])] <- "yes"
qwb_merge_long$not_drive_for_health2[c(rows_1_days[which(qwb_merge_long$day_about_helped[rows_1_days] == "1_days_ago")])] <- "yes"

rows_3_days <- as.vector(grep("3_days_ago", qwb_merge_long$wheelchair_days)) 
rows_2_days <- as.vector(grep("2_days_ago", qwb_merge_long$wheelchair_days)) 
rows_1_days <- as.vector(grep("yesterday", qwb_merge_long$wheelchair_days)) 
rows_no_days <- as.vector(grep("any_day", qwb_merge_long$wheelchair_days)) 

qwb_merge_long$wheelchair_days2 <- as.character("no")
qwb_merge_long$wheelchair_days2[c(rows_3_days[which(qwb_merge_long$day_about_helped[rows_3_days] == "3_days_ago")])] <- "yes"
qwb_merge_long$wheelchair_days2[c(rows_2_days[which(qwb_merge_long$day_about_helped[rows_2_days] == "2_days_ago")])] <- "yes"
qwb_merge_long$wheelchair_days2[c(rows_1_days[which(qwb_merge_long$day_about_helped[rows_1_days] == "1_days_ago")])] <- "yes"

rows_3_days <- as.vector(grep("3_days_ago", qwb_merge_long$doingr_daily_activities)) 
rows_2_days <- as.vector(grep("2_days_ago", qwb_merge_long$doingr_daily_activities)) 
rows_1_days <- as.vector(grep("yesterday", qwb_merge_long$doingr_daily_activities)) 
rows_no_days <- as.vector(grep("any_day", qwb_merge_long$doingr_daily_activities)) 

qwb_merge_long$doingr_daily_activities2 <- as.character("no")
qwb_merge_long$doingr_daily_activities2[c(rows_3_days[which(qwb_merge_long$day_about_helped[rows_3_days] == "3_days_ago")])] <- "yes"
qwb_merge_long$doingr_daily_activities2[c(rows_2_days[which(qwb_merge_long$day_about_helped[rows_2_days] == "2_days_ago")])] <- "yes"
qwb_merge_long$doingr_daily_activities2[c(rows_1_days[which(qwb_merge_long$day_about_helped[rows_1_days] == "1_days_ago")])] <- "yes"

qwb_merge_new <- qwb_merge_long[,c(1:7,12:17)]

qwb_merge3a <- qwb_merge3
qwb_merge3a$did_help_with_personal_care2 <- as.character("no")
qwb_merge3a$did_help_with_personal_care2[which(qwb_merge3a$did_help_with_personal_care != "any_day")] <- "yes"
qwb_merge3a$days_of_3_help_with_personal_care <- numeric(length=length(qwb_merge3a$case_name))
qwb_merge3a$days_of_3_help_with_personal_care[which(qwb_merge3a$did_help_with_personal_care == "yesterday 2_days_ago 3_days_ago")] <- 3
qwb_merge3a$days_of_3_help_with_personal_care[which(qwb_merge3a$did_help_with_personal_care %in% c("yesterday 2_days_ago", 
                                                                                                   "yesterday 3_days_ago",
                                                                                                   "2_days_ago 3_days_ago"))] <- 2
qwb_merge3a$days_of_3_help_with_personal_care[which(qwb_merge3a$did_help_with_personal_care %in% c("yesterday","2_days_ago","3_days_ago"))] <- 1
qwb_merge3a$days_of_3_help_with_personal_care[which(qwb_merge3a$did_help_with_personal_care == "any_day")] <- 0
##
qwb_merge3a$not_drive_for_health2 <- as.character("no")
qwb_merge3a$not_drive_for_health2[which(qwb_merge3a$not_drive_for_health != "any_day")] <- "yes"
qwb_merge3a$days_of_3_not_drive_for_health <- numeric(length=length(qwb_merge3a$case_name))
qwb_merge3a$days_of_3_not_drive_for_health[which(qwb_merge3a$not_drive_for_health == "yesterday 2_days_ago 3_days_ago")] <- 3
qwb_merge3a$days_of_3_not_drive_for_health[which(qwb_merge3a$not_drive_for_health %in% c("yesterday 2_days_ago", 
                                                                                         "yesterday 3_days_ago",
                                                                                         "2_days_ago 3_days_ago"))] <- 2
qwb_merge3a$days_of_3_not_drive_for_health[which(qwb_merge3a$not_drive_for_health %in% c("yesterday","2_days_ago","3_days_ago"))] <- 1
qwb_merge3a$days_of_3_not_drive_for_health[which(qwb_merge3a$not_drive_for_health == "any_day")] <- 0
##
qwb_merge3a$doingr_daily_activities2 <- as.character("no")
qwb_merge3a$doingr_daily_activities2[which(qwb_merge3a$doingr_daily_activities != "any_day")] <- "yes"
qwb_merge3a$days_of_3_doingr_daily_activities <- numeric(length=length(qwb_merge3a$case_name))
qwb_merge3a$days_of_3_doingr_daily_activities[which(qwb_merge3a$doingr_daily_activities == "yesterday 2_days_ago 3_days_ago")] <- 3
qwb_merge3a$days_of_3_doingr_daily_activities[which(qwb_merge3a$doingr_daily_activities %in% c("yesterday 2_days_ago", 
                                                                                               "yesterday 3_days_ago",
                                                                                               "2_days_ago 3_days_ago"))] <- 2
qwb_merge3a$days_of_3_doingr_daily_activities[which(qwb_merge3a$doingr_daily_activities %in% c("yesterday","2_days_ago","3_days_ago"))] <- 1
qwb_merge3a$days_of_3_doingr_daily_activities[which(qwb_merge3a$doingr_daily_activities == "any_day")] <- 0
##
qwb_merge3b <- qwb_merge3a[,c(1:3,12,4:7,13:18)]


######
qwb_new4 <- droplevels(qwb_merge3b[which(qwb_merge3b$case_name %in% levels(expenses_qwb_by_case$case_name)),c(1,7,9,11,13)])
qwb_new5 <- droplevels(qwb_new4[which(qwb_new4$which_interview != "month"),])
qwb_new5$personal_care_help <- factor(qwb_new5$did_help_with_personal_care2, levels=c("no","yes"))
qwb_new5$daily_activity_help <- factor(qwb_new5$doingr_daily_activities2, levels=c("no","yes"))

qwb_new6 <- droplevels(qwb_new5[!duplicated(qwb_new5$case_name),])

parts <- levels(qwb_new5$case_name)
for(ii in 1:(length(parts))){
  part <- parts[ii]
  if(nrow(qwb_new5[which(qwb_new5$case_name == part),]) > 1){
    pers <- max(as.numeric(qwb_new5$personal_care_help[which(qwb_new5$case_name == part)]))
    dai <- max(as.numeric(qwb_new5$daily_activity_help[which(qwb_new5$case_name == part)]))
    if(pers == 2){
      qwb_new6$personal_care_help[which(qwb_new6$case_name == part)] <- "yes"
    }
    if(dai == 2){
      qwb_new6$daily_activity_help[which(qwb_new6$case_name == part)] <- "yes"
    }
  }
}

qwb_new7 <- qwb_new6[,c(1,6,7)]

#######
expenses_both_qwb <- merge(expenses_qwb_by_case, qwb_new7, by=c("case_name"), all.x = TRUE)



########
expenses_both_qwb2 <- droplevels(expenses_both_qwb[which(!(is.na(expenses_both_qwb$house_number))),])
expenses_both_qwb3 <- droplevels(expenses_both_qwb2[which(!(is.na(expenses_both_qwb2$min_qwb_score))),])

bi <- ntiles(expenses_both_qwb3$min_qwb_score, n=2)
tri <- ntiles(expenses_both_qwb3$min_qwb_score, n=3)
expenses_both_qwb3$bi <- bi
expenses_both_qwb3$tri <- tri

favstats(expenses_both_qwb3$min_qwb_score~expenses_both_qwb3$bi)
favstats(expenses_both_qwb3$min_qwb_score~expenses_both_qwb3$tri)


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
      help_log1e,help_log1f,help_log1g,
      help_log1h,help_log1i, test="Chisq")
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
             help_log2e,help_log2f,help_log2g,
             help_log2h, help_log2i)
aic_e[order(aic_e$AIC),]
anova(help_log2,help_log2a,help_log2b,help_log2c, 
      help_log2d,help_log2d2,help_log2d3,help_log2e,help_log2f,help_log2g,
      help_log2h, help_log2i, test="Chisq")


AICctab(help_log2,help_log2a,help_log2b,help_log2c, 
        help_log2d,help_log2d2,help_log2d3,help_log2e,help_log2f,help_log2g,
        help_log2h, help_log2i, weights=TRUE, base=TRUE, sort=TRUE)



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



#############
#############
#############
#############
