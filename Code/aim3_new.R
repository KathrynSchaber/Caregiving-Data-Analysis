######
rm(list = ls())
library(mosaic)
library(dplyr)
library(reshape2)
setwd("/Users/Kathryn/Desktop/MS3_current/Aim3--R/Data")
part_data <- read.csv("part_data.csv", sep=",", header=TRUE)
socio_data <- read.csv("socio_data.csv", sep=",", header=TRUE)
expenses <- read.csv("expenses_data.csv", sep=",", header=TRUE)
qwb <- read.csv("qwb_score.csv", sep=",", header=TRUE)
house_data1 <- read.csv("house_data1.csv", sep=",", header=TRUE)
house_data2 <- read.csv("house_data2.csv", sep=",", header=TRUE)

#####
part_data2 <- droplevels(part_data[which(part_data$diagnosis == "deng2"),])
str(as.factor(part_data2$case_name)) ## 87
part_data2 <- part_data2[!duplicated(part_data2$case_name),]
### "SA319BP02" was asymptomtic ###
part_data2 <- droplevels(part_data2[-c(which(part_data2$case_name == "SA319BP02")),])


str(as.factor(expenses$case_name.x)) ## 89
merge1 <- droplevels(merge(part_data2, expenses, by.x="case_name", by.y="case_name.x"))
str(as.factor(merge1$case_name)) ## 67
str(as.factor(qwb$case_name)) ## 93
merge2 <- droplevels(merge(merge1, qwb, by.x="case_name", by.y="case_name"))
str(as.factor(merge2$case_name)) ## 67


qwb_merge <- droplevels(merge(part_data2, qwb, by.x="case_name", by.y="case_name"))
str(qwb_merge)

qwb_merge <- qwb_merge[-c(which(qwb_merge$case_name == "TA154P08" & is.na(qwb_merge$score))),]
qwb_merge <- qwb_merge[-c(which(qwb_merge$case_name == "SA498P19" & is.na(qwb_merge$score))),]

######
qwb_merge2b <- qwb_merge[,c(1,3:12)]
qwb_merge2b$which_interview[which(qwb_merge2b$case_name == "TA154P08" & is.na(qwb_merge2b$which_interview))] <- "day3"
qwb_merge2b$which_interview <- factor(qwb_merge2b$which_interview, levels=c("day3","week","month"))
qwb_merge3 <- qwb_merge2b[order(qwb_merge2b$case_name, qwb_merge2b$which_interview),]


qwb_merge3$age2 <- character(length=length(qwb_merge3$case_name))
qwb_merge3$age2[which(qwb_merge3$age < 18)] <- "child"
qwb_merge3$age2[which(qwb_merge3$age >= 18)] <- "adult"
qwb_merge3$age2 <- factor(qwb_merge3$age2, levels=c("child","adult"))

qwb_merge3$min_qwb_score <- numeric(length=length(qwb_merge3$case_name))
qwb_merge3$min_personal_score <- numeric(length=length(qwb_merge3$case_name))
qwb_merge3$base_qwb_score <- numeric(length=length(qwb_merge3$case_name))
qwb_merge3$base_personal_score <- numeric(length=length(qwb_merge3$case_name))

cases <- levels(qwb_merge3$case_name)

for(ii in 1:length(cases)){
  qwb_merge3$min_qwb_score[which(qwb_merge3$case_name == cases[ii])]<- min(qwb_merge3$score[which(qwb_merge3$case_name == cases[ii] & qwb_merge3$which_interview != "month")])
  qwb_merge3$min_personal_score[which(qwb_merge3$case_name == cases[ii])]<- min(qwb_merge3$health_over_scale[which(qwb_merge3$case_name == cases[ii] & qwb_merge3$which_interview != "month")])
  base_score <- qwb_merge3$score[which(qwb_merge3$case_name == cases[ii] & qwb_merge3$which_interview == "month")]
  base_pers <- qwb_merge3$health_over_scale[which(qwb_merge3$case_name == cases[ii] & qwb_merge3$which_interview == "month")]
  if(length(base_score) > 0){
    qwb_merge3$base_qwb_score[which(qwb_merge3$case_name == cases[ii])]<- base_score
  }
  else{
    qwb_merge3$base_qwb_score[which(qwb_merge3$case_name == cases[ii])]<-NA
  }
  if(length(base_pers) > 0){
    qwb_merge3$base_personal_score[which(qwb_merge3$case_name == cases[ii])]<- base_pers
  }
  else{
    qwb_merge3$base_personal_score[which(qwb_merge3$case_name == cases[ii])]<-NA
  }
}

qwb_merge4 <- qwb_merge3[,c(1,5,12,15,13,16,14)]
qwb_short <- qwb_merge4[!duplicated(qwb_merge4$case_name),]

######
summary(qwb_short$sex)
summary(qwb_short$age2)
summary(qwb_short$base_qwb_score)
summary(qwb_short$base_personal_score)
summary(qwb_short$min_qwb_score)
summary(qwb_short$min_personal_score)

summary(qwb_short$base_qwb_score - qwb_short$min_qwb_score)
summary(qwb_short$base_personal_score - qwb_short$min_personal_score)

summary((qwb_short$base_qwb_score - qwb_short$min_qwb_score)/qwb_short$base_qwb_score)*100
qwb_short2 <- qwb_short[which(qwb_short$base_personal_score !=0),]
summary((qwb_short2$base_personal_score - qwb_short2$min_personal_score)/qwb_short2$base_personal_score)*100
#####
##### EXPENSES ######
expenses_merge <- droplevels(merge(part_data2, expenses, by.x="case_name", by.y="case_name.x"))
str(expenses_merge)

expenses_merge2 <- expenses_merge[,c(1,4:7,11:13,14,15:17,18,20:24)]
expenses_merge2b <- expenses_merge2[,c(1,4,5,10,13,3,2,7,6,8,9,11,12,14:18)]
expenses_merge2b$number_interview <- factor(expenses_merge2b$number_interview, levels=c("week","month"))
expenses_merge3 <- expenses_merge2b[order(expenses_merge2b$case_name, expenses_merge2b$number_interview, expenses_merge2b$helper_number),]

###########
##########
expenses_merge3$age2 <- character(length=length(expenses_merge3$case_name))
expenses_merge3$age2[which(expenses_merge3$age < 18)] <- "child"
expenses_merge3$age2[which(expenses_merge3$age >= 18)] <- "adult"
expenses_merge3$age2 <- factor(expenses_merge3$age2, levels=c("child","adult"))


###### For now, do "max" values across week/month surveys for individuals #####
expenses_merge3b <- droplevels(expenses_merge3[,c(1:3,19,4,5,11,9,10,12:18)])

## "TA616P11" changes from no job to freelance
expenses_merge3b$make_money[which(expenses_merge3b$make_money == "retired_pensioner")] <- "not_work"
expenses_merge3b$make_money[which(expenses_merge3b$case_name == "SA510P05")] <- "for_hour"
expenses_merge3b$make_money[which(expenses_merge3b$case_name == "TA616P11")] <- "frelance"

expenses_merge3b$days_helped <- factor(NA, levels=c("1","2","3","4","5+"))
expenses_merge3b$days_helped[which(expenses_merge3b$how_long_help == "day_1")] <- "1"
expenses_merge3b$days_helped[which(expenses_merge3b$how_long_help == "day_2")] <- "2"
expenses_merge3b$days_helped[which(expenses_merge3b$how_long_help == "day_3")] <- "3"
expenses_merge3b$days_helped[which(expenses_merge3b$how_long_help == "day_4")] <- "4"
expenses_merge3b$days_helped[which(expenses_merge3b$how_long_help == "day_5_more")] <- "5+"

# Did this person have to take time off from work or did taking care of you not affect their work?
expenses_merge3b$helper_work_affect <-factor(NA, levels=c("no","yes"))
expenses_merge3b$helper_work_affect[which(expenses_merge3b$take_time_work == "not_affect_work")] <- "no"
expenses_merge3b$helper_work_affect[which(expenses_merge3b$take_time_work == "affect_work")] <- "yes"

# How/in what did they help you? with children, cleaning home, cooking, taking care of me, buying things, gave me money, other help
expenses_merge3b$help_with_children <- factor(NA, levels=c("no","yes"))
expenses_merge3b$help_cleaning <- factor(NA, levels=c("no","yes"))
expenses_merge3b$help_cooking <- factor(NA, levels=c("no","yes"))
expenses_merge3b$help_take_care_me <- factor(NA, levels=c("no","yes"))
expenses_merge3b$help_buy_things <- factor(NA, levels=c("no","yes"))
expenses_merge3b$help_gave_money <- factor(NA, levels=c("no","yes"))
expenses_merge3b[which(expenses_merge3b$someone_cared == "yes"),c(19:24)] <- "no"

expenses_merge3b$help_with_children[grep("with_children", expenses_merge3b$what_help)] <- "yes"
expenses_merge3b$help_cleaning[grep("cleaning_home", expenses_merge3b$what_help)] <- "yes"
expenses_merge3b$help_cooking[grep("cooking", expenses_merge3b$what_help)] <- "yes"
expenses_merge3b$help_take_care_me[grep("taking_care", expenses_merge3b$what_help)] <- "yes"
expenses_merge3b$help_buy_things[grep("buying_things", expenses_merge3b$what_help)] <- "yes"
expenses_merge3b$help_gave_money[grep("gave_money", expenses_merge3b$what_help)] <- "yes"

((summary(expenses_merge3b$help_with_children)/length(which(!(is.na(expenses_merge3b$help_with_children))))*100)[1:2]) ## 6.72% yes
((summary(expenses_merge3b$help_cleaning)/length(which(!(is.na(expenses_merge3b$help_cleaning))))*100)[1:2]) ## 2.52% yes
((summary(expenses_merge3b$help_cooking)/length(which(!(is.na(expenses_merge3b$help_cooking))))*100)[1:2]) ## 34.45% yes
((summary(expenses_merge3b$help_take_care_me)/length(which(!(is.na(expenses_merge3b$help_take_care_me))))*100)[1:2]) ## 94.12% yes
((summary(expenses_merge3b$help_buy_things)/length(which(!(is.na(expenses_merge3b$help_buy_things))))*100)[1:2]) ## 28.57% yes
((summary(expenses_merge3b$help_gave_money)/length(which(!(is.na(expenses_merge3b$help_gave_money))))*100)[1:2]) ## 11.76% yes

#### merge help around house and help with money/buying things
expenses_merge3b$cumm_help_around_house <- factor(NA, levels=c("no","yes")) ## cleaning house, cooking, with children
expenses_merge3b$cumm_help_things_money <- factor(NA, levels=c("no","yes")) ## buy things, give money
expenses_merge3b[which(expenses_merge3b$someone_cared == "yes"),c(25:26)] <- "no"
expenses_merge3b$cumm_help_around_house[sort(unique(which(expenses_merge3b[,c(19:21)]=="yes", arr.ind = TRUE)[,1]))] <- "yes"
expenses_merge3b$cumm_help_things_money[sort(unique(which(expenses_merge3b[,c(23:24)]=="yes", arr.ind = TRUE)[,1]))] <- "yes"


expenses_merge3b$length_illness <- factor(NA, levels=c("1","2","3","4","5+"))
expenses_merge3b$length_illness[which(expenses_merge3b$time_fever == "day_1")] <- "1"
expenses_merge3b$length_illness[which(expenses_merge3b$time_fever == "day_2")] <- "2"
expenses_merge3b$length_illness[which(expenses_merge3b$time_fever == "day_3")] <- "3"
expenses_merge3b$length_illness[which(expenses_merge3b$time_fever == "day_4")] <- "4"
expenses_merge3b$length_illness[which(expenses_merge3b$time_fever == "day_5_more")] <- "5+"

expenses_merge4 <- droplevels(expenses_merge3b[-c(which(expenses_merge3b$time_fever == "asymptomatic")),])
expenses_merge4b <- expenses_merge4[,c(1:6,8,27,10:13,17,18,22,25,26)]


##### SHORTEN SO NOT WEEK AND MONTH --> OTHERWISE SOME PEOPLE MORE INFLUENCING ####
nlevels(droplevels(expenses_merge4b$case_name[which(expenses_merge4b$number_interview == "week")]))
nlevels(droplevels(expenses_merge4b$case_name[which(expenses_merge4b$number_interview == "month")]))

two_helpers <- as.character(unique(expenses_merge4b$case_name[which(expenses_merge4b$number_people_cared > 1)])) ## 8
no_month <- names(which((table(expenses_merge4b$case_name, expenses_merge4b$number_interview)[,1] >= 1) & (table(expenses_merge4b$case_name, expenses_merge4b$number_interview)[,2] == 0)))
which(two_helpers %in% no_month) ## 1 person
## MYC314 has 2 helpers and no month survey
rows <- expenses_merge4b[which(expenses_merge4b$case_name %in% no_month),]
rows$number_interview <- "month"
expenses_merge4c <- rbind(expenses_merge4b, rows)

#### SA413P02 has work as flexible at week interview and NA at month interview --> set as flexible
expenses_merge4c$work_fixed_flexible[which(expenses_merge4c$case_name == "SA413P02")] <- "flexible"
#### TA616P11 has work as NA at week interview and flexible at month interview --> set as flexible
expenses_merge4c$work_fixed_flexible[which(expenses_merge4c$case_name == "TA616P11")] <- "flexible"
#### GPS897P01 has work as flexible at week interview and fixed at month interview --> set as flexible
expenses_merge4c$work_fixed_flexible[which(expenses_merge4c$case_name == "GPS897P01")] <- "flexible"


temp <- expenses_merge4c[which(expenses_merge4c$number_interview == "week"),]
temp2 <- expenses_merge4c[which(expenses_merge4c$number_interview == "month"),]
temp3 <- merge(temp,temp2, by.x=c("case_name", "sex","age","age2","make_money","work_fixed_flexible","helper_number"), by.y=c("case_name", "sex","age","age2","make_money","work_fixed_flexible","helper_number"), all = TRUE)

## GPS906P01: had no visitors on week and 1 on month --> not merging
temp3[which(temp3$case_name=="GPS906P01" & temp3$number_interview.y=="month"),8:10] <- temp3[which(temp3$case_name=="GPS906P01" & temp3$number_interview.x=="week"),8:10]
temp3 <- temp3[-c(which(temp3$case_name=="GPS906P01" & is.na(temp3$helper_number))),]
# SA505P23: said helped in "week", but not how many/what/how long; gave answers in month
temp3[which(temp3$case_name=="SA505P23" & temp3$number_interview.y=="month"),c(8:10,15:17)] <- temp3[which(temp3$case_name=="SA505P23" & temp3$number_interview.x=="week"),c(8:10,15:17)]
temp3 <- temp3[-c(which(temp3$case_name=="SA505P23" & is.na(temp3$helper_number))),]
# TA616P11: not merging because difference in jobs, temporarily set as same job to avoid problem

nrow(temp3)
length(two_helpers)
## 73 rows --> 67 people with 1 helper + 8 = 75
library(dplyr)
temp3 %>% mutate_if(is.factor, as.numeric) -> temp4

temp4$length_illness <- numeric(length=length(temp4$case_name))
temp4$someone_cared <- numeric(length=length(temp4$case_name))
temp4$number_people_cared <- numeric(length=length(temp4$case_name))
temp4$who_helped <- numeric(length=length(temp4$case_name))
temp4$days_helped <- numeric(length=length(temp4$case_name))
temp4$helper_work_affect <- numeric(length=length(temp4$case_name))
temp4$help_take_care_me <- numeric(length=length(temp4$case_name))
temp4$cumm_help_around_house <- numeric(length=length(temp4$case_name))
temp4$cumm_help_things_money <- numeric(length=length(temp4$case_name))

for(ii in 1:(nrow(temp4))){
  temp4$length_illness[ii] <- max(temp4$length_illness.x[ii], temp4$length_illness.y[ii], na.rm=TRUE)
  is.na(temp4$length_illness) <- which(temp4$length_illness == -Inf)
  temp4$someone_cared[ii] <- max(temp4$someone_cared.x[ii], temp4$someone_cared.y[ii], na.rm=TRUE)
  is.na(temp4$someone_cared) <- which(temp4$someone_cared == -Inf)
  temp4$number_people_cared[ii] <- max(temp4$number_people_cared.x[ii], temp4$number_people_cared.y[ii], na.rm=TRUE)
  is.na(temp4$number_people_cared) <- which(temp4$number_people_cared == -Inf)
  temp4$who_helped[ii] <- max(temp4$who_helped.x[ii], temp4$who_helped.y[ii], na.rm=TRUE)
  is.na(temp4$who_helped) <- which(temp4$who_helped == -Inf)
  temp4$days_helped[ii] <- max(temp4$days_helped.x[ii], temp4$days_helped.y[ii], na.rm=TRUE)
  is.na(temp4$days_helped) <- which(temp4$days_helped == -Inf)
  temp4$helper_work_affect[ii] <- max(temp4$helper_work_affect.x[ii], temp4$helper_work_affect.y[ii], na.rm=TRUE)
  is.na(temp4$helper_work_affect) <- which(temp4$helper_work_affect == -Inf)
  temp4$help_take_care_me[ii] <- max(temp4$help_take_care_me.x[ii], temp4$help_take_care_me.y[ii], na.rm=TRUE)
  is.na(temp4$help_take_care_me) <- which(temp4$help_take_care_me == -Inf)
  temp4$cumm_help_around_house[ii] <- max(temp4$cumm_help_around_house.x[ii], temp4$cumm_help_around_house.y[ii], na.rm=TRUE)
  is.na(temp4$cumm_help_around_house) <- which(temp4$cumm_help_around_house == -Inf)
  temp4$cumm_help_things_money[ii] <- max(temp4$cumm_help_things_money.x[ii], temp4$cumm_help_things_money.y[ii], na.rm=TRUE)
  is.na(temp4$cumm_help_things_money) <- which(temp4$cumm_help_things_money == -Inf)
}
## only odd one to combine is "GPS895P01" --> in week says helped by relative_live_you, in month said 
## relative_not_live_you --> for now, use "week" answer, since survey was during helping
temp4$who_helped[which(temp4$case_name==2)] <- 2

### change variable back to  yes/no, relative, etc.
temp4$someone_cared <- as.character(ifelse(temp4$someone_cared==1, "no","yes"))
temp4$helper_work_affect <- as.character(ifelse(temp4$helper_work_affect==1, "no","yes"))
temp4$help_take_care_me <- as.character(ifelse(temp4$help_take_care_me==1, "no","yes"))
temp4$cumm_help_around_house <- as.character(ifelse(temp4$cumm_help_around_house==1, "no","yes"))
temp4$cumm_help_things_money <-as.character(ifelse(temp4$cumm_help_things_money==1, "no","yes"))

temp4$length_illness[which(temp4$length_illness == 5)] <- "5+" 
temp4$days_helped[which(temp4$days_helped == 5)] <- "5+" 
temp4$who_helped[which(temp4$who_helped == 1)] <- "other_person"
temp4$who_helped[which(temp4$who_helped == 2)] <- "relative_live_you"
temp4$who_helped[which(temp4$who_helped == 3)] <- "relative_not_live_you"

temp4 %>% mutate_if(is.character, as.factor) -> temp4b
temp5 <- temp4b[,c(28:36)]

expenses_new <- cbind(temp3[,c(1:7)], temp5)
expenses_new <- expenses_new[,c(1:6,8,9,10,7,11,12,14:16,13)]
str(expenses_new)
#
# ###### QUICK SUMMARIES ######
expenses_one_each <- expenses_new[!duplicated(expenses_new$case_name),]

### Per Case ###
summary(expenses_one_each$sex) ## 34 F 33 M
prop.table(summary(expenses_one_each$sex))*100 ## 50.75% 49.25%
summary(expenses_one_each$age2) ## 40 child 27 adult
prop.table(summary(expenses_one_each$age2))*100 ## 59.0 40.30%
summary(expenses_one_each$make_money) ## for_hour:4, frelance:10, not_payroll:4, not_work:44, payroll:5
prop.table(summary(expenses_one_each$make_money))*100 ## 5.97%, 14.93, 5.97, 65.67, 7.46%
summary(expenses_one_each$work_fixed_flexible) ## fixed: 8   flexible: 16  NA:46
prop.table(summary(expenses_one_each$work_fixed_flexible)[1:2])*100 ## 33.33% 66.67%
summary(expenses_one_each$length_illness) ## 1: 3, 2: 1, 3: 4, 4: 7, 5+: 50 (2NA)
prop.table(summary(expenses_one_each$length_illness)[1:5])*100 ## 4.62% 1.54 6.15, 10.77, 76.92%
summary(expenses_one_each$someone_cared) # no: 8, yes: 59
prop.table(summary(expenses_one_each$someone_cared))*100 ## 11.94%, 88.06%
summary(as.factor(expenses_one_each$number_people_cared)) # no: 8, yes: 59
prop.table(summary(as.factor(expenses_one_each$number_people_cared))[1:2])*100 ## 11.94%, 88.06%

### Per Helper ###
summary(expenses_new$who_helped) ## other: 1, relative_lives: 63, relative_not_lives: 3
prop.table(summary(expenses_new$who_helped)[1:3])*100 ## 1.49, 94.03, 4.48%
summary(expenses_new$days_helped) ## 3,1,5,4,54
prop.table(summary(expenses_new$days_helped)[1:5])*100 ## 4.48, 1.49, 7.46, 5.97, 80.60%
summary(expenses_new$help_take_care_me) ## no: 2, yes: 65
prop.table(summary(expenses_new$help_take_care_me)[1:2])*100 ## 2.99, 97.01% 
summary(expenses_new$cumm_help_around_house) ## no: 36, yes: 31
prop.table(summary(expenses_new$cumm_help_around_house)[1:2])*100 ## 53.73, 46.27% 
summary(expenses_new$cumm_help_things_money) ## no: 40, yes: 27
prop.table(summary(expenses_new$cumm_help_things_money)[1:2])*100 ## 59.70, 40.30%
summary(expenses_new$helper_work_affect) ## no:48, yes:19
prop.table(summary(expenses_new$helper_work_affect)[1:2])*100 ## 71.64, 28.36% 


### By Sex/Age ###
table(expenses_one_each$sex, expenses_one_each$someone_cared) 
prop.table(table(expenses_one_each$sex, expenses_one_each$someone_cared), margin=1)*100
table(expenses_one_each$age2, expenses_one_each$someone_cared) 
prop.table(table(expenses_one_each$age2, expenses_one_each$someone_cared), margin=1)*100
table(expenses_one_each$sex, expenses_one_each$someone_cared, expenses_one_each$age2)
prop.table(table(expenses_one_each$sex, expenses_one_each$someone_cared, expenses_one_each$age2)[,,1], margin=1)*100 
prop.table(table(expenses_one_each$sex, expenses_one_each$someone_cared, expenses_one_each$age2)[,,2], margin=1)*100 

table(expenses_one_each$sex, expenses_one_each$number_people_cared) 
prop.table(table(expenses_one_each$sex, expenses_one_each$number_people_cared), margin=1)*100
table(expenses_one_each$age2, expenses_one_each$number_people_cared) 
prop.table(table(expenses_one_each$age2, expenses_one_each$number_people_cared), margin=1)*100
table(expenses_one_each$sex, expenses_one_each$age2, expenses_one_each$number_people_cared)
(table(expenses_one_each$sex, expenses_one_each$age2, expenses_one_each$number_people_cared)[,,2])/
  (table(expenses_one_each$sex[which(expenses_one_each$someone_cared == "yes")], 
         expenses_one_each$age2[which(expenses_one_each$someone_cared == "yes")]))

table(expenses_one_each$sex, expenses_one_each$length_illness)
table(expenses_one_each$age2, expenses_one_each$length_illness)
table(expenses_one_each$age2, expenses_one_each$length_illness,expenses_one_each$sex)
prop.table(table(expenses_one_each$age2, expenses_one_each$length_illness,expenses_one_each$sex)[,,1], margin=1)*100
prop.table(table(expenses_one_each$age2, expenses_one_each$length_illness,expenses_one_each$sex)[,,2], margin=1)*100

###
table(expenses_new$sex, expenses_new$who_helped) 
prop.table(table(expenses_new$sex, expenses_new$who_helped), margin=1)*100
table(expenses_new$age2, expenses_new$who_helped) 
prop.table(table(expenses_new$age2, expenses_new$who_helped), margin=1)*100
table(expenses_new$age2, expenses_new$who_helped, expenses_new$sex) 
prop.table(table(expenses_new$age2, expenses_new$who_helped, expenses_new$sex) [,,1], margin=1)*100
prop.table(table(expenses_new$age2, expenses_new$who_helped, expenses_new$sex) [,,2], margin=1)*100

table(expenses_new$sex, expenses_new$days_helped) 
prop.table(table(expenses_new$sex, expenses_new$days_helped), margin=1)*100
table(expenses_new$age2, expenses_new$days_helped) 
prop.table(table(expenses_new$age2, expenses_new$days_helped), margin=1)*100
table(expenses_new$length_illness, expenses_new$days_helped)
table(expenses_new$length_illness, expenses_new$days_helped, expenses_new$sex) 

table(expenses_new$sex, expenses_new$help_take_care_me) 
prop.table(table(expenses_new$sex, expenses_new$help_take_care_me), margin=1)*100
table(expenses_new$age2, expenses_new$help_take_care_me) 
prop.table(table(expenses_new$age2, expenses_new$help_take_care_me), margin=1)*100

table(expenses_new$sex, expenses_new$cumm_help_around_house) 
prop.table(table(expenses_new$sex, expenses_new$cumm_help_around_house), margin=1)*100
table(expenses_new$age2, expenses_new$cumm_help_around_house) 
prop.table(table(expenses_new$age2, expenses_new$cumm_help_around_house), margin=1)*100
table(expenses_new$sex, expenses_new$cumm_help_around_house, expenses_new$age2) 
prop.table(table(expenses_new$sex, expenses_new$cumm_help_around_house, expenses_new$age2)[,,1], margin=1)*100
prop.table(table(expenses_new$sex, expenses_new$cumm_help_around_house, expenses_new$age2)[,,2], margin=1)*100

table(expenses_new$sex, expenses_new$cumm_help_things_money) 
prop.table(table(expenses_new$sex, expenses_new$cumm_help_things_money), margin=1)*100
table(expenses_new$age2, expenses_new$cumm_help_things_money) 
prop.table(table(expenses_new$age2, expenses_new$cumm_help_things_money), margin=1)*100
table(expenses_new$sex, expenses_new$cumm_help_things_money, expenses_new$age2) 
prop.table(table(expenses_new$sex, expenses_new$cumm_help_things_money, expenses_new$age2)[,,1], margin=1)*100
prop.table(table(expenses_new$sex, expenses_new$cumm_help_things_money, expenses_new$age2)[,,2], margin=1)*100

table(expenses_new$sex, expenses_new$helper_work_affect) 
prop.table(table(expenses_new$sex, expenses_new$helper_work_affect), margin=1)*100
table(expenses_new$age2, expenses_new$helper_work_affect) 
prop.table(table(expenses_new$age2, expenses_new$helper_work_affect), margin=1)*100
table(expenses_new$sex, expenses_new$helper_work_affect, expenses_new$age2) 
prop.table(table(expenses_new$sex, expenses_new$helper_work_affect, expenses_new$age2)[,,1], margin=1)*100
prop.table(table(expenses_new$sex, expenses_new$helper_work_affect, expenses_new$age2)[,,2], margin=1)*100

### By "Job" ###
table(expenses_one_each$make_money, expenses_one_each$someone_cared) 
prop.table(table(expenses_one_each$make_money, expenses_one_each$someone_cared), margin=1)*100
table(expenses_one_each$make_money, expenses_one_each$number_people_cared) 
prop.table(table(expenses_one_each$make_money, expenses_one_each$number_people_cared), margin=1)*100
###
table(expenses_new$make_money, expenses_new$who_helped) 
prop.table(table(expenses_new$make_money, expenses_new$who_helped), margin=1)*100
table(expenses_new$make_money, expenses_new$days_helped) 
prop.table(table(expenses_new$make_money, expenses_new$days_helped), margin=1)*100
table(expenses_new$make_money, expenses_new$help_take_care_me) 
prop.table(table(expenses_new$make_money, expenses_new$help_take_care_me), margin=1)*100
table(expenses_new$make_money, expenses_new$cumm_help_around_house) 
prop.table(table(expenses_new$make_money, expenses_new$cumm_help_around_house), margin=1)*100
table(expenses_new$make_money, expenses_new$cumm_help_things_money) 
prop.table(table(expenses_new$make_money, expenses_new$cumm_help_things_money), margin=1)*100
table(expenses_new$make_money, expenses_new$helper_work_affect) 
prop.table(table(expenses_new$make_money, expenses_new$helper_work_affect), margin=1)*100


### By "Work Fixed or Flexible" ###
table(expenses_one_each$work_fixed_flexible, expenses_one_each$someone_cared) 
prop.table(table(expenses_one_each$work_fixed_flexible, expenses_one_each$someone_cared), margin=1)*100
table(expenses_one_each$work_fixed_flexible, expenses_one_each$number_people_cared) 
prop.table(table(expenses_one_each$work_fixed_flexible, expenses_one_each$number_people_cared), margin=1)*100
###
table(expenses_new$work_fixed_flexible, expenses_new$who_helped) 
prop.table(table(expenses_new$work_fixed_flexible, expenses_new$who_helped), margin=1)*100
table(expenses_new$work_fixed_flexible, expenses_new$days_helped) 
prop.table(table(expenses_new$work_fixed_flexible, expenses_new$days_helped), margin=1)*100
table(expenses_new$work_fixed_flexible, expenses_new$help_take_care_me) 
prop.table(table(expenses_new$work_fixed_flexible, expenses_new$help_take_care_me), margin=1)*100
table(expenses_new$work_fixed_flexible, expenses_new$cumm_help_around_house) 
prop.table(table(expenses_new$work_fixed_flexible, expenses_new$cumm_help_around_house), margin=1)*100
table(expenses_new$work_fixed_flexible, expenses_new$cumm_help_things_money) 
prop.table(table(expenses_new$work_fixed_flexible, expenses_new$cumm_help_things_money), margin=1)*100
table(expenses_new$work_fixed_flexible, expenses_new$helper_work_affect) 
prop.table(table(expenses_new$work_fixed_flexible, expenses_new$helper_work_affect), margin=1)*100


### Other Tables ###
table(expenses_new$who_helped, expenses_new$days_helped) 
prop.table(table(expenses_new$who_helped, expenses_new$days_helped), margin=1)*100

table(expenses_new$days_helped, expenses_new$help_take_care_me) 
table(expenses_new$days_helped, expenses_new$cumm_help_around_house) 
table(expenses_new$days_helped, expenses_new$cumm_help_things_money) 
table(expenses_new$cumm_help_around_house, expenses_new$cumm_help_things_money) 

table(expenses_new$cumm_help_around_house, expenses_new$cumm_help_things_money, expenses_new$help_take_care_me)  


table(expenses_new$days_helped, expenses_new$helper_work_affect) 
table(expenses_new$help_take_care_me, expenses_new$helper_work_affect) 
table(expenses_new$cumm_help_around_house, expenses_new$helper_work_affect) 
table(expenses_new$cumm_help_things_money, expenses_new$helper_work_affect) 

prop.table(table(expenses_new$days_helped, expenses_new$helper_work_affect), margin=2)*100
prop.table(table(expenses_new$help_take_care_me, expenses_new$helper_work_affect), margin=1)*100
prop.table(table(expenses_new$cumm_help_around_house, expenses_new$helper_work_affect), margin=1)*100
prop.table(table(expenses_new$cumm_help_things_money, expenses_new$helper_work_affect), margin=1)*100
#
##### FISHER AGE/SEX #####
fisher.test(table(expenses_one_each$sex, expenses_one_each$someone_cared))
fisher.test(table(expenses_one_each$age2, expenses_one_each$someone_cared))# 0.01
ftable(expenses_one_each$sex,  expenses_one_each$age2,expenses_one_each$someone_cared)
fisher.test(ftable(expenses_one_each$sex,  expenses_one_each$age2,expenses_one_each$someone_cared)[c(2,1),]) #0.02
fisher.test(ftable(expenses_one_each$sex,  expenses_one_each$age2,expenses_one_each$someone_cared)[c(2,3),]) #0.06
fisher.test(ftable(expenses_one_each$sex,  expenses_one_each$age2,expenses_one_each$someone_cared)[c(2,4),]) # 0.3
##
expenses_new$who_helped2 <- factor(ifelse(expenses_new$who_helped=="relative_live_you","housemate","other"),
                                   levels=c("housemate","other"))
fisher.test(table(expenses_new$sex, expenses_new$who_helped2))
fisher.test(table(expenses_new$age2, expenses_new$who_helped2))
ftable(expenses_new$sex,  expenses_new$age2,expenses_new$who_helped2)
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$who_helped2)[c(2,1),])
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$who_helped2)[c(2,3),])
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$who_helped2)[c(2,4),])


fisher.test(table(expenses_new$sex, expenses_new$helper_work_affect))
fisher.test(table(expenses_new$age2, expenses_new$helper_work_affect)) #0.05
ftable(expenses_new$sex,  expenses_new$age2,expenses_new$helper_work_affect)
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$helper_work_affect)[c(2,1),]) #0.06
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$helper_work_affect)[c(2,3),]) #0.02
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$helper_work_affect)[c(2,4),]) #0.2

fisher.test(table(expenses_new$sex, expenses_new$help_take_care_me))
fisher.test(table(expenses_new$age2, expenses_new$help_take_care_me))
ftable(expenses_new$sex,  expenses_new$age2,expenses_new$help_take_care_me)
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$help_take_care_me)[c(2,1),])
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$help_take_care_me)[c(2,3),])
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$help_take_care_me)[c(2,4),])

fisher.test(table(expenses_new$sex, expenses_new$cumm_help_around_house))
fisher.test(table(expenses_new$age2, expenses_new$cumm_help_around_house))
ftable(expenses_new$sex,  expenses_new$age2,expenses_new$cumm_help_around_house)
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$cumm_help_around_house)[c(4,1),])
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$cumm_help_around_house)[c(4,2),])
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$cumm_help_around_house)[c(4,3),])

fisher.test(table(expenses_new$sex, expenses_new$cumm_help_things_money))
fisher.test(table(expenses_new$age2, expenses_new$cumm_help_things_money))
ftable(expenses_new$sex,  expenses_new$age2,expenses_new$cumm_help_things_money)
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$cumm_help_things_money)[c(2,1),])
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$cumm_help_things_money)[c(2,3),])
fisher.test(ftable(expenses_new$sex,  expenses_new$age2,expenses_new$cumm_help_things_money)[c(2,4),])


#


#### MERGE QWB dataset with EXPENSES ####
qwb_new_short <- droplevels(qwb_short[which(qwb_short$case_name %in% levels(expenses_one_each$case_name)),])
expenses_by_helper_new <- droplevels(expenses_new[which(expenses_new$case_name %in% levels(qwb_short$case_name)),c(1,2,4,8,9,10,13:17)])
expenses_by_case_new <- droplevels(expenses_one_each[which(expenses_one_each$case_name %in% levels(qwb_short$case_name)),c(1,2,4,8,9,11,13:16)])

expenses_qwb_by_case <- merge(expenses_by_case_new,qwb_new_short[,c(1,4:7)], by.x="case_name", by.y="case_name")
expenses_qwb_by_helper <- merge(expenses_by_helper_new,qwb_new_short[,c(1,4:7)], by.x="case_name", by.y="case_name")

qwb_new_short_save <- qwb_new_short[,c(1,2,4:7)]
write.csv(qwb_new_short_save, "qwb_new_short_save.csv")

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

# table(expenses_qwb_by_case$tri, expenses_qwb_by_case$someone_cared) ## **
# table(expenses_qwb_by_case$tri, expenses_qwb_by_case$number_people_cared) 
# table(expenses_qwb_by_case$tri, expenses_qwb_by_case$helper_work_affect) ## **
# prop.table(table(expenses_qwb_by_case$tri, expenses_qwb_by_case$someone_cared),margin=1)*100 ## **
# prop.table(table(expenses_qwb_by_case$tri, expenses_qwb_by_case$number_people_cared),margin=1)*100 
# prop.table(table(expenses_qwb_by_case$tri, expenses_qwb_by_case$helper_work_affect),margin=1)*100 ## **
# 

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

# table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$who_helped) ## **
# table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_around_house) 
# table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_things_money) 
# table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$helper_work_affect) ## **
# prop.table(table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$who_helped),margin=1)*100 ## **
# prop.table(table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_around_house),margin=1)*100 
# prop.table(table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$cumm_help_things_money),margin=1)*100 
# prop.table(table(expenses_qwb_by_helper$tri, expenses_qwb_by_helper$helper_work_affect),margin=1)*100 ## **

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
#### SAVE #####

write.csv(expenses_qwb_by_case, "expenses_qwb_by_case.csv", row.names = FALSE)

##### House Numbers #######
all_parts <- levels(part_data2$case_name)
all_parts_id <- levels(part_data2$case_id)

house_data1$all_participant_codes <- as.character(house_data1$all_participant_codes)
temp <- gsub("\\{", "", house_data1$all_participant_codes)
temp2 <- gsub("\\}", "", temp)
temp3 <- strsplit(temp2, ",")
temp3b <- melt(temp3)
index <- temp3b$L1[which(temp3b$value %in% all_parts)]
house_data1_short <- house_data1[index,]

house_data2_short <- house_data2[which(house_data2$participant_code %in% all_parts),]

part_data2$first_symptom <- as.Date(as.character(part_data2$first_symptom))
house_data2_short$end_date <- as.Date(as.character(house_data2_short$end_date))
house_data2_short$status_date <- as.Date(as.character(house_data2_short$status_date))
all_parts <- all_parts[-which(all_parts %in% c("GPS901P01"))]
all_locs <- character(length=length(all_parts))
all_symps <- character(length=length(all_parts))
#### "GPS901P01" <- -> FSS980 -->--> no GPS901 house listed and FSS980 not listed for being at PUE415
for(i in 1:(length(all_parts))){
  part <- all_parts[i]
  symp <- part_data2$first_symptom[which(part_data2$case_name == part)]
  all_symps[i] <- as.character(symp)
  temp_house <- house_data2_short[which(house_data2_short$participant_code == part),]
  if(nrow(temp_house) > 1){
    temp_house2 <- temp_house[which(temp_house$status_date < symp),]
    if(nrow(temp_house2) == 0){
      temp_house2 <- temp_house[which(temp_house$reason == "Recruitment"),] ## house input when recruited a few days after symptom onset
      all_locs[i] <- as.character(temp_house2$location_code)
    }
    else if(nrow(temp_house2) > 1){
      temp_house3 <- temp_house2[which(temp_house2$end_date > symp),]
      if(nrow(temp_house3) > 0){
        all_locs[i] <- as.character(temp_house3$location_code)
      }
      else{## nrow temp_house3  == 0
        temp_house3 <- temp_house2[which(is.na(temp_house2$end_date)),]
        all_locs[i] <- as.character(temp_house3$location_code)
      }
    }
    else{ ## nrow temp_house2 == 1
      all_locs[i] <- as.character(temp_house2$location_code)
    }
  }
  else{ ## nrow temp_house == 1
    all_locs[i] <- as.character(temp_house$location_code)
  }
}

all_symps <- as.Date(all_symps)
counting <- cbind(all_parts, all_symps, all_locs)

house_data2$end_date <- as.Date(as.character(house_data2$end_date))
house_data2$status_date <- as.Date(as.character(house_data2$status_date))
counts <- numeric(length=length(all_locs))
for(i in 1:(length(counts))){
  house_temp <- house_data2[which(house_data2$location_code == all_locs[i]),]
  if(length(which(is.na(house_temp$end_date))) == nrow(house_temp)){ ## all people still living there
    counts[i] <-length(which(house_temp$status_date < (all_symps[i] + 10)))
  }
  else{ ## if some people weren't living there yet when symptoms occured 
    house_temp2 <- house_temp[which(house_temp$status_date < (all_symps[i] + 10) & house_temp$status == "ASP"),] ### only people who were actively there  before symptoms started
    counts[i] <- length(which(house_temp2$end_date >  (all_symps[i] + 10) | is.na(house_temp2$end_date))) ### only active people who were still there when symptoms occured
  }
}
#### MCB121P17 #### added in 8 days after symptom start #####

part_house_numbers <- as.data.frame(cbind(c(all_parts, "GPS901P01"), c(counts,NA)))
part_house_numbers <- part_house_numbers[order(part_house_numbers$V1),]
part_house_numbers$case_name <- part_house_numbers$V1
part_house_numbers$house_numbers <- part_house_numbers$V2
part_house_numbers <- part_house_numbers[,c(3,4)]

############################
part_house_numbers_expenses_qwb <- part_house_numbers[which(part_house_numbers$case_name %in% levels(expenses_qwb_by_case$case_name)),]
expenses_qwb_by_case2 <- expenses_qwb_by_case[order(expenses_qwb_by_case$case_name),]
expenses_qwb_by_case2$house_number <- as.numeric(as.character(part_house_numbers_expenses_qwb$house_numbers))

expenses_qwb_by_case2$house_number_bin <- factor(ifelse(expenses_qwb_by_case2$house_number <= 8, "low", "high"), levels=c("low","high"))

##### SAVE ######
write.csv(expenses_qwb_by_case2, "expenses_qwb_by_case_house.csv", row.names = FALSE)



#######