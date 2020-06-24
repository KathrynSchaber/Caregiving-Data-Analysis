#####
rm(list = ls())
library(mosaic)
library(dplyr)
library(lme4)
library(bbmle)
library(emmeans)
setwd("/Users/Kathryn/Desktop/MS3_current/Aim3--R/Data")
EAR7_visitors_new <- read.csv("EAR7_visitors_qwb.csv", sep=",", header=TRUE)
EAR7_visit3 <- read.csv("EAR7_visit3_qwb.csv", sep=",", header=TRUE)
EAR_visitors_0_new <- read.csv("EAR_visitors_0_new.csv", sep=",", header=TRUE)
qwb <- read.csv("qwb_score.csv", sep=",", header=TRUE)

### FISHER ####
summary(EAR7_visitors_new$visitor_relationship)
summary(EAR7_visitors_new$visit_once_a_week)
summary(EAR7_visitors_new$know_participant_diseased)
summary(EAR7_visitors_new$reason_for_visit)

prop.table(table(EAR7_visitors_new$sex,EAR7_visitors_new$visitor_relationship), margin=1)*100
prop.table(table(EAR7_visitors_new$age2,EAR7_visitors_new$visitor_relationship), margin=1)*100
prop.table(table(EAR7_visitors_new$sex,EAR7_visitors_new$visit_once_a_week), margin=1)*100
prop.table(table(EAR7_visitors_new$age2,EAR7_visitors_new$visit_once_a_week), margin=1)*100
prop.table(table(EAR7_visitors_new$sex,EAR7_visitors_new$know_participant_diseased), margin=1)*100
prop.table(table(EAR7_visitors_new$age2,EAR7_visitors_new$know_participant_diseased), margin=1)*100
prop.table(table(EAR7_visitors_new$sex,EAR7_visitors_new$reason_for_visit), margin=1)*100
prop.table(table(EAR7_visitors_new$age2,EAR7_visitors_new$reason_for_visit), margin=1)*100

fisher.test(table(EAR7_visitors_new$sex,EAR7_visitors_new$visitor_relationship)) # *** <0.001
fisher.test(table(EAR7_visitors_new$age2,EAR7_visitors_new$visitor_relationship)) # *** <0.001
fisher.test(table(EAR7_visitors_new$sex,EAR7_visitors_new$visit_once_a_week))
fisher.test(table(EAR7_visitors_new$age2,EAR7_visitors_new$visit_once_a_week))
fisher.test(table(EAR7_visitors_new$sex,EAR7_visitors_new$know_participant_diseased))
fisher.test(table(EAR7_visitors_new$age2,EAR7_visitors_new$know_participant_diseased))
fisher.test(table(EAR7_visitors_new$sex,EAR7_visitors_new$reason_for_visit))
fisher.test(table(EAR7_visitors_new$age2,EAR7_visitors_new$reason_for_visit)) # * 0.03
fisher.test(matrix(c(16,26,20,23), nrow=2, ncol=2, byrow=FALSE)) # 0.5 fam or no age
fisher.test(matrix(c(34,8,30,13), nrow=2, ncol=2, byrow=FALSE)) # 0.3 fam or no sex
fisher.test(matrix(c(8,22,28,27), nrow=2, ncol=2, byrow=FALSE)) # 0.04 friend or no age *
fisher.test(matrix(c(27,3,37,18), nrow=2, ncol=2, byrow=FALSE)) # 0.03 friend or no sex *
fisher.test(matrix(c(12,1,24,48), nrow=2, ncol=2, byrow=FALSE)) # <0.001 else or no age ***
fisher.test(matrix(c(3,10,61,11), nrow=2, ncol=2, byrow=FALSE)) # <0.001 else or no sex ***

fisher.test(matrix(c(37,3,11,31), nrow=2, ncol=2, byrow=FALSE)) # <0.001 fam or no bi ***
fisher.test(matrix(c(9,21,39,13), nrow=2, ncol=2, byrow=FALSE)) # <0.001 friend or no bi ***
fisher.test(matrix(c(2,10,46,24), nrow=2, ncol=2, byrow=FALSE)) # 0.003 else or no bi **

ftable(EAR7_visitors_new$sex,EAR7_visitors_new$age2, EAR7_visitors_new$visitor_relationship) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$visitor_relationship)[c(2,1),]) ## p=0.4 (f child, f adult)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$visitor_relationship)[c(2,3),]) ## p<0.001 (f child, m adult)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$visitor_relationship)[c(2,4),]) ## p=0.4 (f child, m child)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$visitor_relationship)[c(1,3),]) ## p<0.001 (f adult, m adult)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$visitor_relationship)[c(1,4),]) ## p=0.7 (f adult, m child)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$visitor_relationship)[c(3,4),]) ## p=0.001 (m adult, m child)

aa <- droplevels(EAR7_visitors_new[-which(EAR7_visitors_new$case_name == "SA543P04"),])

fisher.test(table(aa$sex,aa$visitor_relationship)) # 0.2
fisher.test(table(aa$age2,aa$visitor_relationship)) # 0.07

ftable(aa$sex,aa$age2, aa$visitor_relationship) 
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_relationship)[c(2,1),]) ## p=0.4 (f child, f adult)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_relationship)[c(2,3),]) ## p=0.007 (f child, m adult) **
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_relationship)[c(2,4),]) ## p=0.4 (f child, m child)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_relationship)[c(1,3),]) ## p=0.05 (f adult, m adult) *
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_relationship)[c(1,4),]) ## p=0.7 (f adult, m child)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_relationship)[c(3,4),]) ## p=0.02 (m adult, m child) *

aa$visitor_fam_else <- factor(ifelse(aa$visitor_relationship=="family", "family","else"), levels=c("family","else"))
aa$visitor_friend_else <- factor(ifelse(aa$visitor_relationship=="friend", "friend","else"), levels=c("friend","else"))
aa$visitor_other_else <- factor(ifelse(aa$visitor_relationship=="another_relationship", "another_relationship","else"), levels=c("another_relationship","else"))

fisher.test(table(aa$sex,aa$visitor_fam_else)) # 0.8
fisher.test(table(aa$age2,aa$visitor_fam_else)) # 0.8
fisher.test(table(aa$sex,aa$visitor_friend_else)) # 0.2
fisher.test(table(aa$age2,aa$visitor_friend_else)) # 0.2
fisher.test(table(aa$sex,aa$visitor_other_else)) # 0.2
fisher.test(table(aa$age2,aa$visitor_other_else)) # 0.06

ftable(aa$sex,aa$age2, aa$visitor_fam_else) 
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_fam_else)[c(2,1),]) ## p=0.6 (f child, f adult)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_fam_else)[c(2,3),]) ## p=0.3 (f child, m adult) 
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_fam_else)[c(2,4),]) ## p=0.3 (f child, m child)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_fam_else)[c(1,3),]) ## p=0.2 (f adult, m adult) 
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_fam_else)[c(1,4),]) ## p=0.6 (f adult, m child)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_fam_else)[c(3,4),]) ## p=0.2 (m adult, m child) 

ftable(aa$sex,aa$age2, aa$visitor_friend_else) 
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_friend_else)[c(2,1),]) ## p=1 (f child, f adult)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_friend_else)[c(2,3),]) ## p=0.02 (f child, m adult) *
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_friend_else)[c(2,4),]) ## p=0.3 (f child, m child)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_friend_else)[c(1,3),]) ## p=0.03 (f adult, m adult) *
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_friend_else)[c(1,4),]) ## p=0.3 (f adult, m child)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_friend_else)[c(3,4),]) ## p=0.01 (m adult, m child) *

ftable(aa$sex,aa$age2, aa$visitor_other_else) 
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_other_else)[c(2,1),]) ## p=0.2 (f child, f adult)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_other_else)[c(2,3),]) ## p=0.07 (f child, m adult) 
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_other_else)[c(2,4),]) ## p=1.0 (f child, m child)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_other_else)[c(1,3),]) ## p=0.6 (f adult, m adult) 
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_other_else)[c(1,4),]) ## p=1.0 (f adult, m child)
fisher.test(ftable(aa$sex,  aa$age2,aa$visitor_other_else)[c(3,4),]) ## p=1.0 (m adult, m child) 

#





###

fisher.test(ftable(EAR7_visitors_new$age2,EAR7_visitors_new$reason_for_visit)[,c(2,4)]) # * 0.01
fisher.test(matrix(c(27,23,9,24), nrow=2, ncol=2, byrow=FALSE)) # 0.02 --> emotional or not *
fisher.test(matrix(c(3,14,33,33), nrow=2, ncol=2, byrow=FALSE)) # 0.03 --> unrelated or not *
fisher.test(matrix(c(1,5,35,42), nrow=2, ncol=2, byrow=FALSE)) # 0.2 --> info or not
fisher.test(matrix(c(5,5,31,42), nrow=2, ncol=2, byrow=FALSE)) # 0.7 --> other disease reason or not

fisher.test(matrix(c(35,15,27,6), nrow=2, ncol=2, byrow=FALSE)) # 0.3 --> emotional or not 
fisher.test(matrix(c(13,4,49,17), nrow=2, ncol=2, byrow=FALSE)) # 1 --> unrelated or not 
fisher.test(matrix(c(5,1,57,20), nrow=2, ncol=2, byrow=FALSE)) # 1 --> info or not
fisher.test(matrix(c(9,1,53,20), nrow=2, ncol=2, byrow=FALSE)) # 0.4 --> other disease reason or not

EAR7_visitors_new$reason_emot <- factor(ifelse(EAR7_visitors_new$reason_for_visit=="emotional", "emotional","else"), levels=c("emotional","else"))
EAR7_visitors_new$reason_info <- factor(ifelse(EAR7_visitors_new$reason_for_visit=="for_information", "for_information","else"), levels=c("for_information","else"))
EAR7_visitors_new$reason_other <- factor(ifelse(EAR7_visitors_new$reason_for_visit=="another_reason_to_visit", "another_reason_to_visit","else"), levels=c("another_reason_to_visit","else"))
EAR7_visitors_new$reason_unrel <- factor(ifelse(EAR7_visitors_new$reason_for_visit=="unrelated_to_disease", "unrelated_to_disease","else"), levels=c("unrelated_to_disease","else"))


fisher.test(EAR7_visitors_new$sex,EAR7_visitors_new$reason_emot) #0.3
fisher.test(EAR7_visitors_new$age2,EAR7_visitors_new$reason_emot) # 0.02 *
ftable(EAR7_visitors_new$sex, EAR7_visitors_new$age2,EAR7_visitors_new$reason_emot)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_emot)[c(2,1),]) ## p=0.6 (f child, f adult)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_emot)[c(2,3),]) ## p=0.02 (f child, m adult) *
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_emot)[c(2,4),]) ## p=0.1 (f child, m child)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_emot)[c(1,3),]) ## p=0.1 (f adult, m adult) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_emot)[c(1,4),]) ## p=0.04 (f adult, m child) *
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_emot)[c(3,4),]) ## p=0.003 (m adult, m child) **

fisher.test(EAR7_visitors_new$sex,EAR7_visitors_new$reason_info) #1
fisher.test(EAR7_visitors_new$age2,EAR7_visitors_new$reason_info) # 0.2 
ftable(EAR7_visitors_new$sex, EAR7_visitors_new$age2,EAR7_visitors_new$reason_info)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_info)[c(2,1),]) ## p=0.3 (f child, f adult)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_info)[c(2,3),]) ## p=0.7 (f child, m adult) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_info)[c(2,4),]) ## p=1 (f child, m child)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_info)[c(1,3),]) ## p=0.5 (f adult, m adult) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_info)[c(1,4),]) ## p=1 (f adult, m child) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_info)[c(3,4),]) ## p=1 (m adult, m child) 

fisher.test(EAR7_visitors_new$sex,EAR7_visitors_new$reason_other) #0.4
fisher.test(EAR7_visitors_new$age2,EAR7_visitors_new$reason_other) # 0.7
ftable(EAR7_visitors_new$sex, EAR7_visitors_new$age2,EAR7_visitors_new$reason_other)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_other)[c(2,1),]) ## p=0.1 (f child, f adult)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_other)[c(2,3),]) ## p=0.6 (f child, m adult) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_other)[c(2,4),]) ## p=0.4 (f child, m child)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_other)[c(1,3),]) ## p=0.05 (f adult, m adult)  *
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_other)[c(1,4),]) ## p=1 (f adult, m child) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_other)[c(3,4),]) ## p=0.2 (m adult, m child) 

fisher.test(EAR7_visitors_new$sex,EAR7_visitors_new$reason_unrel) #1
fisher.test(EAR7_visitors_new$age2,EAR7_visitors_new$reason_unrel) # 0.03 *
ftable(EAR7_visitors_new$sex, EAR7_visitors_new$age2,EAR7_visitors_new$reason_unrel)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_unrel)[c(2,1),]) ## p=0.3 (f child, f adult)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_unrel)[c(2,3),]) ## p=0.2 (f child, m adult) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_unrel)[c(2,4),]) ## p=0.07 (f child, m child)
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_unrel)[c(1,3),]) ## p=1 (f adult, m adult) 
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_unrel)[c(1,4),]) ## p=0.02 (f adult, m child) *
fisher.test(ftable(EAR7_visitors_new$sex,  EAR7_visitors_new$age2,EAR7_visitors_new$reason_unrel)[c(3,4),]) ## p=0.01 (m adult, m child) *



############

bi <- ntiles(EAR7_visitors_new$min_qwb_score, n=2)
tri <- ntiles(EAR7_visitors_new$min_qwb_score, n=3)
EAR7_visitors_new$bi <- bi
EAR7_visitors_new$tri <- tri

prop.table(table(EAR7_visitors_new$bi,EAR7_visitors_new$visitor_relationship), margin=1)*100
prop.table(table(EAR7_visitors_new$bi,EAR7_visitors_new$visit_once_a_week), margin=1)*100
prop.table(table(EAR7_visitors_new$bi,EAR7_visitors_new$know_participant_diseased), margin=1)*100
prop.table(table(EAR7_visitors_new$bi,EAR7_visitors_new$reason_for_visit), margin=1)*100

fisher.test(table(EAR7_visitors_new$bi,EAR7_visitors_new$visitor_relationship)) # *** <0.001
fisher.test(table(EAR7_visitors_new$bi,EAR7_visitors_new$visit_once_a_week))
fisher.test(table(EAR7_visitors_new$bi,EAR7_visitors_new$know_participant_diseased))
fisher.test(table(EAR7_visitors_new$bi,EAR7_visitors_new$reason_for_visit))

fisher.test(matrix(c(8,9,39,24), nrow=2, ncol=2, byrow=FALSE)) # # 0.3 --> unrelated or not 
fisher.test(matrix(c(29,19,18,14), nrow=2, ncol=2, byrow=FALSE)) # 0.8 --> emotional or not 
fisher.test(matrix(c(6,0,41,33), nrow=2, ncol=2, byrow=FALSE)) # 0.04 --> info or not *
fisher.test(matrix(c(4,5,43,28), nrow=2, ncol=2, byrow=FALSE)) # 0.5 --> other disease reason or not 

prop.table(table(EAR7_visitors_new$visit_once_a_week,EAR7_visitors_new$reason_for_visit), margin=1)*100
fisher.test(table(EAR7_visitors_new$visit_once_a_week,EAR7_visitors_new$reason_for_visit)) ## <0.001
fisher.test(matrix(c(5,12,6,60), nrow=2, ncol=2, byrow=FALSE)) # 0.04 --> unrelated or not *
fisher.test(matrix(c(1,49,10,23), nrow=2, ncol=2, byrow=FALSE)) # <0.001 --> emotional or not ***
fisher.test(matrix(c(1,5,10,67), nrow=2, ncol=2, byrow=FALSE)) # 0.6 --> info or not
fisher.test(matrix(c(4,6,7,66), nrow=2, ncol=2, byrow=FALSE)) # 0.02 --> other disease reason or not *


EAR7_visitors_new$visitor_number<- ordered(EAR7_visitors_new$visitor_number, 
                                           levels=c("first visitor", "second visitor","third visitor",
                                                    "fourth visitor","fifth visitor"))
EAR7_visitors_new$visitor_visit_number<- ordered(EAR7_visitors_new$visitor_visit_number, 
                                                 levels=c("first visit", "second visit","third visit","fourth visit",
                                                          "fifth visit","sixth visit","seventh visit","eighth visit"))
EAR7_visitors_new2 <- EAR7_visitors_new[order(EAR7_visitors_new$case_name, EAR7_visitors_new$visitor_number,
                                              EAR7_visitors_new$visitor_visit_number,
                                              decreasing = c(FALSE,TRUE,TRUE)),]
EAR7_visitors_new3 <- droplevels(EAR7_visitors_new2[!duplicated(EAR7_visitors_new2$case_name),])


EAR7_visitors_new3$visitor_number2 <- factor(ifelse(EAR7_visitors_new3$visitor_number == "first visitor",1,"2+"))
EAR7_visitors_new3$visitor_visit_number2 <- factor(ifelse(EAR7_visitors_new3$visitor_visit_number == "first visit",1,"2+"))

prop.table(table(EAR7_visitors_new3$sex,EAR7_visitors_new3$visitor_number2), margin=1)*100
prop.table(table(EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2), margin=1)*100
prop.table(table(EAR7_visitors_new3$sex,EAR7_visitors_new3$visitor_visit_number2), margin=1)*100
prop.table(table(EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_visit_number2), margin=1)*100
fisher.test(table(EAR7_visitors_new3$sex,EAR7_visitors_new3$visitor_number2)) # 
fisher.test(table(EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2)) # 
fisher.test(table(EAR7_visitors_new3$sex,EAR7_visitors_new3$visitor_visit_number2))
fisher.test(table(EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_visit_number2))

prop.table(table(EAR7_visitors_new3$bi,EAR7_visitors_new3$visitor_number2), margin=1)*100
prop.table(table(EAR7_visitors_new3$bi,EAR7_visitors_new3$visitor_visit_number2), margin=1)*100
fisher.test(table(EAR7_visitors_new3$bi,EAR7_visitors_new3$visitor_number2)) # ** 0.009
fisher.test(table(EAR7_visitors_new3$bi,EAR7_visitors_new3$visitor_visit_number2)) # 0.09

ftable(EAR7_visitors_new3$sex,  EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2)
fisher.test(ftable(EAR7_visitors_new3$sex,  EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2)[c(2,1),]) ## p=0.6 (f child, f adult)
fisher.test(ftable(EAR7_visitors_new3$sex,  EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2)[c(2,3),]) ## p=0.6 (f child, m adult)
fisher.test(ftable(EAR7_visitors_new3$sex,  EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2)[c(2,4),]) ## p=1 (f child, m child)
fisher.test(ftable(EAR7_visitors_new3$sex,  EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2)[c(1,3),]) ## p=0.2 (f adult, m adult)
fisher.test(ftable(EAR7_visitors_new3$sex,  EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2)[c(1,4),]) ## p=0.5 (f adult, m child)
fisher.test(ftable(EAR7_visitors_new3$sex,  EAR7_visitors_new3$age2,EAR7_visitors_new3$visitor_number2)[c(3,4),]) ## p=1 (m adult, m child)

#####
EAR7_visitors_new <- read.csv("EAR7_visitors_qwb.csv", sep=",", header=TRUE)

EAR7_visitors_short <- droplevels(EAR7_visitors_new[!duplicated(EAR7_visitors_new$case_name),])
parts_none <- names(which(table(EAR7_visitors_new$case_name, EAR7_visitors_new$visitor_yes_no)[,2] == 0))
EAR7_visitors_short$visitor_yes_no_all <- factor(ifelse(EAR7_visitors_short$case_name %in% parts_none, "no","yes"), levels=c("no","yes"))
EAR7_visitors_short2 <- EAR7_visitors_short[,c(1,3,4,20,25,21:24)]

###### FISHER ######
fisher.test(table(EAR7_visitors_short2$sex, EAR7_visitors_short2$visitor_yes_no_all)) ## p=0.02
fisher.test(table(EAR7_visitors_short2$age2, EAR7_visitors_short2$visitor_yes_no_all)) ## p=0.6
ftable(EAR7_visitors_short2$sex,  EAR7_visitors_short2$age2,EAR7_visitors_short2$visitor_yes_no_all)
fisher.test(ftable(EAR7_visitors_short2$sex,  EAR7_visitors_short2$age2,EAR7_visitors_short2$visitor_yes_no_all)[c(2,1),]) ## p=0.7 (f child, f adult)
fisher.test(ftable(EAR7_visitors_short2$sex,  EAR7_visitors_short2$age2,EAR7_visitors_short2$visitor_yes_no_all)[c(2,3),]) ## p=0.5 (f child, m adult)
fisher.test(ftable(EAR7_visitors_short2$sex,  EAR7_visitors_short2$age2,EAR7_visitors_short2$visitor_yes_no_all)[c(2,4),]) ## p=0.02 (f child, m child)
fisher.test(ftable(EAR7_visitors_short2$sex,  EAR7_visitors_short2$age2,EAR7_visitors_short2$visitor_yes_no_all)[c(1,3),]) ## p=0.7 (f adult, m adult)
fisher.test(ftable(EAR7_visitors_short2$sex,  EAR7_visitors_short2$age2,EAR7_visitors_short2$visitor_yes_no_all)[c(1,4),]) ## p=0.06 (f adult, m child)
fisher.test(ftable(EAR7_visitors_short2$sex,  EAR7_visitors_short2$age2,EAR7_visitors_short2$visitor_yes_no_all)[c(3,4),]) ## p=0.2 (m adult, m child)
##

EAR7_visitors_short3 <- EAR7_visitors_short2
bi <- ntiles(EAR7_visitors_short3$min_qwb_score, n=2)
tri <- ntiles(EAR7_visitors_short3$min_qwb_score, n=3)
EAR7_visitors_short3$bi <- bi
EAR7_visitors_short3$tri <- tri

favstats(EAR7_visitors_short3$min_qwb_score~EAR7_visitors_short3$bi)
favstats(EAR7_visitors_short3$min_qwb_score~EAR7_visitors_short3$tri)
table(EAR7_visitors_short3$bi, EAR7_visitors_short3$visitor_yes_no_all) ## exactly same % yes/no for low and high
table(EAR7_visitors_short3$tri, EAR7_visitors_short3$visitor_yes_no_all)

fisher.test(table(EAR7_visitors_short3$bi, EAR7_visitors_short3$visitor_yes_no_all)) ## p=1 --> exactly same
fisher.test(table(EAR7_visitors_short3$tri, EAR7_visitors_short3$visitor_yes_no_all)) ## p=0.7
#
### QWB ####
setwd("/Users/Kathryn/Desktop/MS3_current/Aim3--R/Data")
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
qwb_new4 <- droplevels(qwb_merge3b[which(qwb_merge3b$case_name %in% levels(EAR7_visitors_short2$case_name)),c(1,7,9,11,13)])
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

### MODEL ####
### GPS896P01
visitors_qwb <- merge(EAR7_visitors_short2, qwb_new7, by=c("case_name"), all.x = TRUE)


visitors_qwb2 <- visitors_qwb[-(which(is.na(visitors_qwb$house_number_bin))),]
visitors_qwb3 <- visitors_qwb2[-(which(is.na(visitors_qwb2$min_qwb_score))),]


visit_log1 <- glm(visitor_yes_no_all ~ 1, data=visitors_qwb3, family = "binomial")
visit_log1a <- glm(visitor_yes_no_all ~ sex , data=visitors_qwb3, family = "binomial")
visit_log1b <- glm(visitor_yes_no_all ~age2, data=visitors_qwb3, family = "binomial")
visit_log1c <- glm(visitor_yes_no_all ~house_number_bin, data=visitors_qwb3, family = "binomial")
visit_log1d <- glm(visitor_yes_no_all ~min_qwb_score, data=visitors_qwb3, family = "binomial")
#visit_log1e <- glm(visitor_yes_no_all ~min_personal_score, data=visitors_qwb3, family = "binomial")
visit_log1f <- glm(visitor_yes_no_all ~personal_care_help, data=visitors_qwb3, family = "binomial")
visit_log1g <- glm(visitor_yes_no_all ~daily_activity_help, data=visitors_qwb3, family = "binomial")
visit_log1h <- glm(visitor_yes_no_all ~sex * age2, data=visitors_qwb3, family = "binomial")
#visit_log1i <- glm(visitor_yes_no_all ~sex + age2, data=visitors_qwb3, family = "binomial")

aic_e <- AIC(visit_log1, visit_log1a,visit_log1b,visit_log1c,
             visit_log1d,visit_log1f,visit_log1g,
             visit_log1h)

aic_e[order(aic_e$AIC),]

anova(visit_log1, visit_log1a,visit_log1b,visit_log1c,
      visit_log1d,visit_log1f,visit_log1g,
      visit_log1h, test="Chisq")

AICctab(visit_log1,visit_log1a,visit_log1b,visit_log1c,
        visit_log1d,visit_log1f,visit_log1g,
        visit_log1h, weights=TRUE, base=TRUE, sort=FALSE)
## a is best --> sex

#### MODEL WITH DAY 0 VISITS ####
EAR_visitors_0_new2 <- EAR_visitors_0_new[!duplicated(EAR_visitors_0_new[,c(3)]),c(3,26)]
EAR_visitors_0_new2$visitor_yes_no_day_0 <- EAR_visitors_0_new2$visitor_yes_no
EAR_visitors_0_new2a <- EAR_visitors_0_new2[,c(1,3)]

visitors_qwb3 <- merge(visitors_qwb3, EAR_visitors_0_new2a, by="case_name", all.x = TRUE)

visit_log1j <- glm(visitor_yes_no_all ~ visitor_yes_no_day_0, data=visitors_qwb3, family = "binomial")

AICctab(visit_log1,visit_log1a,visit_log1b,visit_log1c,
        visit_log1d,visit_log1f,visit_log1g,
        visit_log1h,visit_log1j, weights=TRUE, base=TRUE, sort=TRUE)
## a is best --> sex

#######
#######
############ BITILES AND TERTILES ########
visitors_qwb4 <- visitors_qwb3
bi <- ntiles(visitors_qwb4$min_qwb_score, n=2)
tri <- ntiles(visitors_qwb4$min_qwb_score, n=3)
visitors_qwb4$bi <- bi
visitors_qwb4$tri <- tri

favstats(visitors_qwb4$min_qwb_score~visitors_qwb4$bi)
favstats(visitors_qwb4$min_qwb_score~visitors_qwb4$tri)

visit_log1 <- glm(visitor_yes_no_all ~ 1, data=visitors_qwb4, family = "binomial")
visit_log1a <- glm(visitor_yes_no_all ~ sex , data=visitors_qwb4, family = "binomial")
visit_log1b <- glm(visitor_yes_no_all ~age2, data=visitors_qwb4, family = "binomial")
visit_log1c <- glm(visitor_yes_no_all ~house_number_bin, data=visitors_qwb4, family = "binomial")
visit_log1d <- glm(visitor_yes_no_all ~min_qwb_score, data=visitors_qwb4, family = "binomial")
visit_log1d2 <- glm(visitor_yes_no_all ~bi, data=visitors_qwb4, family = "binomial")
visit_log1d3 <- glm(visitor_yes_no_all ~tri, data=visitors_qwb4, family = "binomial")
#visit_log1e <- glm(visitor_yes_no_all ~min_personal_score, data=visitors_qwb4, family = "binomial")
visit_log1f <- glm(visitor_yes_no_all ~personal_care_help, data=visitors_qwb4, family = "binomial")
visit_log1g <- glm(visitor_yes_no_all ~daily_activity_help, data=visitors_qwb4, family = "binomial")
visit_log1h <- glm(visitor_yes_no_all ~sex * age2, data=visitors_qwb4, family = "binomial")
#visit_log1i <- glm(visitor_yes_no_all ~sex + age2, data=visitors_qwb4, family = "binomial")
visit_log1j <- glm(visitor_yes_no_all ~ visitor_yes_no_day_0, data=visitors_qwb3, family = "binomial")

aic_e <- AIC(visit_log1, visit_log1a,visit_log1b,visit_log1c,
             visit_log1d,visit_log1d2, visit_log1d3,
             visit_log1e,visit_log1f,visit_log1g,
             visit_log1h,visit_log1i)

aic_e[order(aic_e$AIC),]

anova(visit_log1, visit_log1a,visit_log1b,visit_log1c,
      visit_log1d,visit_log1d2, visit_log1d3,
      visit_log1e,visit_log1f,visit_log1g,
      visit_log1h,visit_log1i, test="Chisq")

AICctab(visit_log1, visit_log1a,visit_log1b,visit_log1c,
        visit_log1d,visit_log1d2, visit_log1d3,
        visit_log1e,visit_log1f,visit_log1g,
        visit_log1h,visit_log1i, weights=TRUE, base=TRUE, sort=TRUE)
## a is best --> sex


dev <- c(anova(visit_log1, visit_log1a, test="Chisq")[,4], anova(visit_log1, visit_log1b, test="Chisq")[2,4], anova(visit_log1, visit_log1c, test="Chisq")[2,4],
         anova(visit_log1, visit_log1d, test="Chisq")[2,4],anova(visit_log1, visit_log1d2, test="Chisq")[2,4],anova(visit_log1, visit_log1d3, test="Chisq")[2,4],
         anova(visit_log1, visit_log1f, test="Chisq")[2,4],anova(visit_log1, visit_log1g, test="Chisq")[2,4],
         anova(visit_log1, visit_log1h, test="Chisq")[2,4],anova(visit_log1, visit_log1j, test="Chisq")[2,4])

b <- AICctab(visit_log1, visit_log1a,visit_log1b,visit_log1c,
             visit_log1d,visit_log1d2,visit_log1d3,
             visit_log1f,visit_log1g,
             visit_log1h,visit_log1j, weights=TRUE, base=TRUE, sort=FALSE)

cbind(data.frame(b[1:4]), dev) ## visitors all

############## DAILY ################
#### QWB ##########
setwd("/Users/Kathryn/Desktop/MS3_current/Aim3--R/Data")
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
qwb_new4 <- droplevels(qwb_merge3b[which(qwb_merge3b$case_name %in% levels(EAR7_visit3$case_name)),c(1,7,9,11,13)])
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
EAR7_visit_qwb <- merge(EAR7_visit3, qwb_new7, by=c("case_name"), all.x = TRUE)
EAR7_visit_qwb2 <- EAR7_visit_qwb[-c(which(is.na(EAR7_visit_qwb$min_qwb_score))),]

bi <- ntiles(EAR7_visit_qwb2$min_qwb_score, n=2)
tri <- ntiles(EAR7_visit_qwb2$min_qwb_score, n=3)
EAR7_visit_qwb2$bi <- bi
EAR7_visit_qwb2$tri <- tri

##### MODELS ######
visit_log2z <- glmer(visitor_yes_no ~  (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2 <- glmer(visitor_yes_no ~ day_interest_move_or_no + (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2a <- glmer(visitor_yes_no ~ sex+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2b <- glmer(visitor_yes_no ~age2+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2c <- glmer(visitor_yes_no ~house_number_bin+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2d <- glmer(visitor_yes_no ~min_qwb_score+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2d2 <- glmer(visitor_yes_no ~bi+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2d3 <- glmer(visitor_yes_no ~tri+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
#visit_log2e <- glmer(visitor_yes_no ~min_personal_score+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2f <- glmer(visitor_yes_no ~personal_care_help+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial") ###
visit_log2g <- glmer(visitor_yes_no ~daily_activity_help+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log2h <- glmer(visitor_yes_no ~sex*age2+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial") ###
#visit_log2i <- glmer(visitor_yes_no ~sex+age2+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial") ###

visit_log3a <- glmer(visitor_yes_no ~ sex + day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log3b <- glmer(visitor_yes_no ~age2+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log3c <- glmer(visitor_yes_no ~house_number_bin+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log3d <- glmer(visitor_yes_no ~min_qwb_score+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log3e <- glmer(visitor_yes_no ~min_personal_score+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log3f <- glmer(visitor_yes_no ~personal_care_help+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial") ###
visit_log3g <- glmer(visitor_yes_no ~daily_activity_help+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial")
visit_log3h <- glmer(visitor_yes_no ~sex*age2+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial") ###
visit_log3i <- glmer(visitor_yes_no ~sex+age2+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial") ###

aic_e <- AIC(visit_log2z,visit_log2,visit_log2a,visit_log2b,visit_log2c,
             visit_log2d,visit_log2f,visit_log2g,visit_log2h,
             visit_log3a,visit_log3b,visit_log3c,visit_log3d,visit_log3e,
             visit_log3f,visit_log3g,visit_log3h,visit_log3i)

aic_e[order(aic_e$AIC),]

AICctab(visit_log2z,visit_log2,visit_log2a,visit_log2b,visit_log2c,
        visit_log2d,visit_log2f,visit_log2g,visit_log2h,
        visit_log3a,visit_log3b,visit_log3c,visit_log3d,visit_log3e,
        visit_log3f,visit_log3g,visit_log3h,visit_log3i, weights=TRUE, base=TRUE, sort=TRUE)


dev <- c(anova(visit_log2z, visit_log2, test="Chisq")[,5], 
         anova(visit_log2z, visit_log2a, test="Chisq")[2,5], anova(visit_log2z, visit_log2b, test="Chisq")[2,5], anova(visit_log2z, visit_log2c, test="Chisq")[2,5],
         anova(visit_log2z, visit_log2d, test="Chisq")[2,5],anova(visit_log2z, visit_log2d2, test="Chisq")[2,5],anova(visit_log2z, visit_log2d3, test="Chisq")[2,5],
         anova(visit_log2z, visit_log2f, test="Chisq")[2,5],anova(visit_log2z, visit_log2g, test="Chisq")[2,5],
         anova(visit_log2z, visit_log2h, test="Chisq")[2,5])
dev2 <- dev[1]-dev
b <- AICctab(visit_log2z,
             visit_log2, visit_log2a,visit_log2b,visit_log2c,
             visit_log2d,visit_log2d2,visit_log2d3,
             visit_log2f,visit_log2g,
             visit_log2h, weights=TRUE, base=TRUE, sort=FALSE)

cbind(data.frame(b[1:4]), dev2) ## visitors daily


e2 <- emmeans(visit_log2h, ~  age2*sex, type="response")

emmip(visit_log2a, ~sex,CIs=TRUE, type="response") 
emmip(visit_log2b, ~age2,CIs=TRUE, type="response") 

emmip(visit_log2h, sex~age2,CIs=TRUE, type="response")  +
  scale_x_discrete(labels=c("Adult","Child")) +
  scale_y_continuous(limits=c(0,0.45), breaks=c(seq(0,0.5,0.10))) +
  labs(x="Age", y="Predicted Probability of Receiving Visitors Each Day") +
  scale_color_discrete("Gender", labels=c("Female","Male")) +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.title = element_text(size=14),
        legend.title.align=0.5,
        legend.text = element_text(size=12))

aa <- emmip(visit_log2h, sex~age2,CIs=TRUE, type="response", plotit = FALSE)
ggplot(data=aa, aes(y=age2, x=yvar, group=sex, color=sex)) +
  geom_point(position=position_dodgev(height=0.5), size=5) +
  geom_linerangeh(aes(y=age2, xmin=LCL, xmax=UCL, group=sex), 
                 position=position_dodgev(height=0.5), size=7, alpha=0.4) +
  scale_y_discrete(labels=c("Adult","Child")) +
  scale_x_continuous(limits=c(0,0.45), breaks=c(seq(0,0.5,0.10))) +
  labs(y="Age", x="Predicted Probability of Receiving Visitors Each Day") +
  scale_color_discrete("Gender", labels=c("Female","Male")) +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=15),
        legend.title = element_text(size=14),
        legend.title.align=0.5,
        legend.text = element_text(size=12))


#####
##### ADD DAILY QWB SCORES #####
qwb <- read.csv("qwb_score.csv", sep=",", header=TRUE)

EAR7_visit_long <- EAR7_visit_qwb[,c(1,3,4,17,18,20,25,26)]

qwb2 <- qwb[which(qwb$which_interview != "month"),]
qwb3 <- qwb2[which(qwb2$day_illness %in% c(1:10)),]
qwb4 <- qwb3[,c(1,4,5,6)]

EAR7_visit_long2 <- merge(EAR7_visit_long, qwb4, by.x = c("case_name","day_interest_move_or_no"), by.y=c("case_name","day_illness"), all.x=TRUE)

visit_log4a <- glmer(visitor_yes_no ~ sex+ score+(1|case_name), data=EAR7_visit_long2, family = "binomial")
visit_log4b <- glmer(visitor_yes_no ~age2+ score+(1|case_name), data=EAR7_visit_long2, family = "binomial")
visit_log4c <- glmer(visitor_yes_no ~score+ (1|case_name), data=EAR7_visit_long2, family = "binomial")
## c
visit_log4d <- glmer(visitor_yes_no ~ sex+ health_over_scale+(1|case_name), data=EAR7_visit_long2, family = "binomial")
visit_log4e <- glmer(visitor_yes_no ~age2+ health_over_scale+(1|case_name), data=EAR7_visit_long2, family = "binomial")
visit_log4f <- glmer(visitor_yes_no ~health_over_scale+ (1|case_name), data=EAR7_visit_long2, family = "binomial")
## f

for(ii in 1:nlevels(EAR7_visit_long2$case_name)){
  case <- levels(EAR7_visit_long2$case_name)[ii]
  day <- EAR7_visit_long2$day_interest_move_or_no[which(EAR7_visit_long2$case_name == case & !(is.na(EAR7_visit_long2$score)))]
  score <- EAR7_visit_long2$score[which(EAR7_visit_long2$case_name == case & !(is.na(EAR7_visit_long2$score)))]
  if(length(day) == 1){
    EAR7_visit_long2$score[which(EAR7_visit_long2$case_name == case & EAR7_visit_long2$day_interest_move_or_no == (day[1]-1))] <- score[1]
    EAR7_visit_long2$score[which(EAR7_visit_long2$case_name == case & EAR7_visit_long2$day_interest_move_or_no == (day[1]-2))] <- score[1]
  }
  if(length(day) == 2){
    EAR7_visit_long2$score[which(EAR7_visit_long2$case_name == case & EAR7_visit_long2$day_interest_move_or_no == (day[1]-1))] <- score[1]
    EAR7_visit_long2$score[which(EAR7_visit_long2$case_name == case & EAR7_visit_long2$day_interest_move_or_no == (day[1]-2))] <- score[1]
    EAR7_visit_long2$score[which(EAR7_visit_long2$case_name == case & EAR7_visit_long2$day_interest_move_or_no == (day[2]-1))] <- score[2]
    EAR7_visit_long2$score[which(EAR7_visit_long2$case_name == case & EAR7_visit_long2$day_interest_move_or_no == (day[2]-2))] <- score[2]
  }
}

EAR7_visit_long3 <- droplevels(EAR7_visit_long2[which(!(is.na(EAR7_visit_long2$score))),])

visit_log4z <- glmer(visitor_yes_no ~ (1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4 <- glmer(visitor_yes_no ~ day_interest_move_or_no +(1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4a <- glmer(visitor_yes_no ~ sex+ score+(1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4b <- glmer(visitor_yes_no ~age2+ score+(1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4c <- glmer(visitor_yes_no ~score+ (1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4d <- glmer(visitor_yes_no ~ sex+(1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4e <- glmer(visitor_yes_no ~sex*age2+(1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4f <- glmer(visitor_yes_no ~sex*age2+score+(1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4g <- glmer(visitor_yes_no ~day_interest_move_or_no*score+(1|case_name), data=EAR7_visit_long3, family = "binomial")
visit_log4h <- glmer(visitor_yes_no ~day_interest_move_or_no+score+(1|case_name), data=EAR7_visit_long3, family = "binomial")

AICctab(visit_log4,visit_log4z,visit_log4a, visit_log4b, visit_log4c, visit_log4d,
        visit_log4e, visit_log4f, visit_log4g, visit_log4h, weights=TRUE, base=TRUE, sort=TRUE)
# 4,4z,4h