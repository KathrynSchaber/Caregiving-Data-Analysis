#####
rm(list = ls())
library(mosaic)
library(dplyr)
library(lme4)
library(bbmle)
library(emmeans)

EAR7_visitors_new <- read.csv("EAR7_visitors_new.csv", sep=",", header=TRUE)
EAR7_visit_qwb2 <- read.csv("EAR7_visit_qwb2.csv", sep=",", header=TRUE)
EAR7_visit_long2 <- read.csv("EAR7_visit_long2.csv", sep=",", header=TRUE)
visitors_qwb4 <- read.csv("visitors_qwb4.csv", sep=",", header=TRUE)

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

aa <- droplevels(EAR7_visitors_new[-which(EAR7_visitors_new$case_name == "46"),])

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
EAR7_visitors_new2 <- EAR7_visitors_new %>% arrange(case_name, desc(visitor_number), desc(visitor_visit_number))

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
visit_log1 <- glm(visitor_yes_no_all ~ 1, data=visitors_qwb4, family = "binomial")
visit_log1a <- glm(visitor_yes_no_all ~ sex , data=visitors_qwb4, family = "binomial")
visit_log1b <- glm(visitor_yes_no_all ~age2, data=visitors_qwb4, family = "binomial")
visit_log1c <- glm(visitor_yes_no_all ~house_number_bin, data=visitors_qwb4, family = "binomial")
visit_log1d <- glm(visitor_yes_no_all ~min_qwb_score, data=visitors_qwb4, family = "binomial")
#visit_log1e <- glm(visitor_yes_no_all ~min_personal_score, data=visitors_qwb4, family = "binomial")
visit_log1f <- glm(visitor_yes_no_all ~personal_care_help, data=visitors_qwb4, family = "binomial")
visit_log1g <- glm(visitor_yes_no_all ~daily_activity_help, data=visitors_qwb4, family = "binomial")
visit_log1h <- glm(visitor_yes_no_all ~sex * age2, data=visitors_qwb4, family = "binomial")
#visit_log1i <- glm(visitor_yes_no_all ~sex + age2, data=visitors_qwb4, family = "binomial")

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
visit_log1j <- glm(visitor_yes_no_all ~ visitor_yes_no_day_0, data=visitors_qwb4, family = "binomial")

AICctab(visit_log1,visit_log1a,visit_log1b,visit_log1c,
        visit_log1d,visit_log1f,visit_log1g,
        visit_log1h,visit_log1j, weights=TRUE, base=TRUE, sort=TRUE)
## a is best --> sex

#######
#######
############ BITILES AND TERTILES ########
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
visit_log1j <- glm(visitor_yes_no_all ~ visitor_yes_no_day_0, data=visitors_qwb4, family = "binomial")

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

exp(cbind(OR = coef(visit_log1a), confint.default(visit_log1a)))

############## DAILY ################
#### QWB ##########
visit_log2z <- glmer(visitor_yes_no ~  (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2 <- glmer(visitor_yes_no ~ day_interest_move_or_no + (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2a <- glmer(visitor_yes_no ~ sex+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2b <- glmer(visitor_yes_no ~age2+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2c <- glmer(visitor_yes_no ~house_number_bin+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2d <- glmer(visitor_yes_no ~min_qwb_score+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2d2 <- glmer(visitor_yes_no ~bi+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2d3 <- glmer(visitor_yes_no ~tri+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
#visit_log2e <- glmer(visitor_yes_no ~min_personal_score+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2f <- glmer(visitor_yes_no ~personal_care_help+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa")) ###
visit_log2g <- glmer(visitor_yes_no ~daily_activity_help+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log2h <- glmer(visitor_yes_no ~sex*age2+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa")) ###
visit_log2i <- glmer(visitor_yes_no ~sex+age2+ (1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa")) ###

visit_log3a <- glmer(visitor_yes_no ~ sex + day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log3b <- glmer(visitor_yes_no ~age2+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log3c <- glmer(visitor_yes_no ~house_number_bin+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log3d <- glmer(visitor_yes_no ~min_qwb_score+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log3e <- glmer(visitor_yes_no ~min_personal_score+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log3f <- glmer(visitor_yes_no ~personal_care_help+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa")) ###
visit_log3g <- glmer(visitor_yes_no ~daily_activity_help+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa"))
visit_log3h <- glmer(visitor_yes_no ~sex*age2+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa")) ###
visit_log3i <- glmer(visitor_yes_no ~sex+age2+ day_interest_move_or_no +(1|case_name), data=EAR7_visit_qwb2, family = "binomial", glmerControl(optimizer="bobyqa")) ###

aic_e <- AIC(visit_log2z,visit_log2,visit_log2a,visit_log2b,visit_log2c,
             visit_log2d,visit_log2f,visit_log2g,visit_log2h,
             visit_log3a,visit_log3b,visit_log3c,visit_log3d,visit_log3g,
             visit_log3f,visit_log3h,visit_log3i)

aic_e[order(aic_e$AIC),]

AICctab(visit_log2z,visit_log2,visit_log2a,visit_log2b,visit_log2c,
        visit_log2d,visit_log2f,visit_log2g,visit_log2h,
        visit_log3a,visit_log3b,visit_log3c,visit_log3d,visit_log3g,
        visit_log3f,visit_log3h,visit_log3i, weights=TRUE, base=TRUE, sort=TRUE)


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

exp(cbind(OR = fixef(visit_log2a), confint.merMod(visit_log2a, parm="beta_")))

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