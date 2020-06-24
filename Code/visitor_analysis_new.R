#####
rm(list = ls())
library(mosaic)
library(dplyr)
setwd("/Users/Kathryn/Desktop/MS3_current/Aim3--R/Data")
EAR_visitors <- read.csv("EAR_VISITORS_FULL.csv", sep=",", header=TRUE)
EAR7_visitors <- read.csv("EAR7_VISITORS_FULL.csv", sep=",", header=TRUE)
qwb <- read.csv("qwb_score.csv", sep=",", header=TRUE)

str(EAR_visitors)
str(EAR7_visitors)

EAR7_visitors$day_interest_move_or_no <- EAR7_visitors$day_interest_by_first_symp
EAR7_visitors$day_interest_move_or_no[which(!(is.na(EAR7_visitors$day_no_move)))] <- EAR7_visitors$day_no_move[which(!(is.na(EAR7_visitors$day_no_move)))] 

EAR_visitors$day_of_survey[which(EAR_visitors$case_name == "SA460P03" & EAR_visitors$current_date == "2016-03-02")] <- 0
EAR_visitors$visitor_number <- factor(EAR_visitors$visitor_number, levels=c("first visitor", "second visitor", "third visitor", "fourth visitor", "fifth visitor"))
EAR_visitors$visitor_visit_number <- factor(EAR_visitors$visitor_visit_number, levels=c("first visit", "second visit", "third visit", "fourth visit", "fifth visit"))
EAR_visitors <- EAR_visitors[order(EAR_visitors$part_number, EAR_visitors$day_of_survey, EAR_visitors$visitor_number, EAR_visitors$visitor_visit_number),]
EAR_visitors_0 <- droplevels(EAR_visitors[which(EAR_visitors$day_of_survey == 0),])


EAR7_visitors$visitor_number <- factor(EAR7_visitors$visitor_number, levels=c("first visitor", "second visitor", "third visitor", "fourth visitor", "fifth visitor"))
EAR7_visitors$visitor_visit_number <- factor(EAR7_visitors$visitor_visit_number, levels=c("first visit", "second visit", "third visit", "fourth visit", "fifth visit","sixth visit","seventh visit","eighth visit"))

EAR7_visitors_temp <- droplevels(EAR7_visitors[which(EAR7_visitors$day_interest_move_or_no %in% c(1:10)),])
perc_visitors_day <- (prop.table(table(EAR7_visitors_temp$day_interest_move_or_no, EAR7_visitors_temp$visitor_yes_no), margin=1))[,2]
EAR7_visitors_temp$day_interest_move_or_no <- factor(EAR7_visitors_temp$day_interest_move_or_no, levels=c(1:10))

##### Visitors to me and House Numbers ######
setwd("/Users/Kathryn/Desktop/Aim3--R/Data")
house_data1 <- read.csv("house_data1.csv", sep=",", header=TRUE)
house_data2 <- read.csv("house_data2.csv", sep=",", header=TRUE)

EAR_short <- EAR_visitors_0[,c(3,6)]
EAR7_short <- EAR7_visitors[,c(2,5)]
all_parts <- sort(unique(c(levels(EAR_short$case_name), levels(EAR7_short$case_name)))) ## 39

house_data1$all_participant_codes <- as.character(house_data1$all_participant_codes)
temp <- gsub("\\{", "", house_data1$all_participant_codes)
temp2 <- gsub("\\}", "", temp)
temp3 <- strsplit(temp2, ",")
temp3b <- melt(temp3)
index <- temp3b$L1[which(temp3b$value %in% all_parts)]
house_data1_short <- house_data1[index,]
house_data2_short <- house_data2[which(house_data2$participant_code %in% all_parts),]

#### "GPS901P01" <- -> FSS980 -->--> no GPS901 house listed and FSS980 not listed for being at PUE415
EAR_short$first_symptom <- as.Date(as.character(EAR_short$first_symptom))
EAR7_short$first_symptom <- as.Date(as.character(EAR7_short$first_symptom))
temp_visit <- rbind(EAR_short, EAR7_short)
temp_visit2 <- temp_visit[!duplicated(temp_visit$case_name),]
house_data2_short$end_date <- as.Date(as.character(house_data2_short$end_date))
house_data2_short$status_date <- as.Date(as.character(house_data2_short$status_date))
all_parts <- all_parts[-which(all_parts %in% c("GPS901P01"))]
all_locs <- character(length=length(all_parts))
all_symps <- character(length=length(all_parts))
for(i in 1:(length(all_parts))){
  part <- all_parts[i]
  symp <- temp_visit2$first_symptom[which(temp_visit2$case_name == part)]
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


############
EAR_visitors_0_new <- EAR_visitors_0
EAR7_visitors_new <- EAR7_visitors

part_house_numbers_EAR_0 <- part_house_numbers[which(part_house_numbers$case_name %in% levels(EAR_visitors_0_new$case_name)),]
part_house_numbers_EAR7 <- part_house_numbers[which(part_house_numbers$case_name %in% levels(EAR7_visitors_new$case_name)),]

EAR_visitors_0_new$house_numbers <- numeric(length=length(EAR_visitors_0_new$row_number))
EAR7_visitors_new$house_numbers <- numeric(length=length(EAR7_visitors_new$visitor_row_num))

for(i in 1:(length(EAR_visitors_0_new$case_name))){
  EAR_visitors_0_new$house_numbers[i] <- as.numeric(as.character(part_house_numbers_EAR_0$house_numbers[which(part_house_numbers_EAR_0$case_name == as.character(EAR_visitors_0_new$case_name[i]))]))
}
for(i in 1:(length(EAR7_visitors_new$case_name))){
  EAR7_visitors_new$house_numbers[i] <- as.numeric(as.character(part_house_numbers_EAR7$house_numbers[which(part_house_numbers_EAR7$case_name == as.character(EAR7_visitors_new$case_name[i]))]))
}

EAR_visitors_0_new$house_number_bin <- factor(ifelse(EAR_visitors_0_new$house_number <= 8, "low", "high"), levels=c("low","high"))
EAR7_visitors_new$house_number_bin <- factor(ifelse(EAR7_visitors_new$house_number <= 8, "low", "high"), levels=c("low","high"))


##### Day 0 ######
#### Per visitor
EAR_visitors_0_newa <- EAR_visitors_0_new[c(which(!(is.na(EAR_visitors_0_new$visitor_number)))),]
EAR_visitors_0_newa <- EAR_visitors_0_newa[order(EAR_visitors_0_newa$case_name, EAR_visitors_0_newa$visitor_number, EAR_visitors_0_newa$visitor_visit_number, decreasing=TRUE),]
EAR_visitors_0_newb <- EAR_visitors_0_newa[!duplicated(EAR_visitors_0_newa[,c(2,18)]),]

# visitors and nrows per person (# visits) ##
EAR_visitors_0_newc<- EAR_visitors_0_newb[!duplicated(EAR_visitors_0_newb$case_name),]
#
###### DAILY ######
EAR7_visitors_new$visitor_number <- factor(EAR7_visitors_new$visitor_number, levels=c("first visitor", "second visitor", "third visitor", "fourth visitor", "fifth visitor"))
EAR7_visitors_new$visitor_visit_number <- factor(EAR7_visitors_new$visitor_visit_number, levels=c("first visit", "second visit", "third visit", "fourth visit", "fifth visit","sixth visit","seventh visit","eighth visit"))

### Per visitor
EAR_visitors_0_newa <- EAR_visitors_0_new[c(which(!(is.na(EAR_visitors_0_new$visitor_number)))),]
EAR_visitors_0_newa <- EAR_visitors_0_newa[order(EAR_visitors_0_newa$case_name, EAR_visitors_0_newa$visitor_number, EAR_visitors_0_newa$visitor_visit_number, decreasing=TRUE),]
EAR_visitors_0_newb <- EAR_visitors_0_newa[!duplicated(EAR_visitors_0_newa[,c(2,18)]),]

EAR7_visitors_new1 <- EAR7_visitors_new[which(!(is.na(EAR7_visitors_new$visitor_number))),]
EAR7_visitors_new1 <- EAR7_visitors_new1[order(EAR7_visitors_new1$case_name, EAR7_visitors_new1$visitor_number, EAR7_visitors_new1$visitor_visit_number, decreasing=TRUE),]
EAR7_visitors_new2 <- EAR7_visitors_new1[!duplicated(EAR7_visitors_new1[,c(2,8)]),]

# visitors and nrows per person (# visits) ###
EAR7_visitors_new3<- EAR7_visitors_new2[!duplicated(EAR7_visitors_new2$case_name),]

######
setwd("/Users/Kathryn/Desktop/Aim3--R/Data")
write.csv(EAR_visitors_0_new,file="EAR_visitors_0_new.csv", row.names = FALSE)
write.csv(EAR7_visitors_new,file="EAR7_visitors_new.csv", row.names = FALSE)

######
qwb <- read.csv("qwb_score.csv", sep=",", header=TRUE)
part_data <- read.csv("part_data.csv", sep=",", header=TRUE)


part_data2 <- droplevels(part_data[which(part_data$diagnosis == "deng2"),])
str(as.factor(part_data2$case_name)) ## 87
part_data2 <- part_data2[!duplicated(part_data2$case_name),]
### "SA319BP02" was asymptomtic ###
part_data2 <- droplevels(part_data2[-c(which(part_data2$case_name == "SA319BP02")),])
qwb_merge <- droplevels(merge(part_data2, qwb, by.x="case_name", by.y="case_name"))

qwb_merge <- qwb_merge[-c(which(qwb_merge$case_name == "TA154P08" & is.na(qwb_merge$score))),]
qwb_merge <- qwb_merge[-c(which(qwb_merge$case_name == "SA498P19" & is.na(qwb_merge$score))),]

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


summary(qwb_short$sex)
summary(qwb_short$age2)
summary(qwb_short$base_qwb_score)
summary(qwb_short$base_personal_score)
summary(qwb_short$min_qwb_score)
summary(qwb_short$min_personal_score)

#### MERGE QWB dataset with VISITORS ####
qwb_new_short <- droplevels(qwb_short[which(qwb_short$case_name %in% levels(EAR7_visitors_new$case_name)),])
EAR7_visitors_new_new <- droplevels(EAR7_visitors_new[which(EAR7_visitors_new$case_name %in% levels(qwb_short$case_name)),])

EAR7_visitors_qwb <- merge(EAR7_visitors_new_new,qwb_new_short[,c(1,4:7)], by.x="case_name", by.y="case_name")

summary(EAR7_visitors_qwb$base_qwb_score)
summary(EAR7_visitors_qwb$min_qwb_score)
summary(EAR7_visitors_qwb$base_qwb_score - EAR7_visitors_qwb$min_qwb_score)


######
setwd("/Users/Kathryn/Desktop/Aim3--R/Data")
write.csv(EAR7_visitors_qwb,file="EAR7_visitors_qwb.csv", row.names = FALSE)

#######
EAR7_visit <- droplevels(EAR7_visitors_new[which(EAR7_visitors_new$day_interest_move_or_no %in% c(1:10)),])
EAR7_visit2 <- EAR7_visit[!duplicated(EAR7_visit[,c(2,17,18)]),]
EAR7_visit3 <- EAR7_visit2[-(which(is.na(EAR7_visit2$house_number_bin))),]

setwd("/Users/Kathryn/Desktop/Aim3--R/Data")
write.csv(EAR7_visit3,file="EAR7_visit3.csv", row.names = FALSE)

#######
qwb_new_short <- droplevels(qwb_short[which(qwb_short$case_name %in% levels(EAR7_visit3$case_name)),])
EAR7_visit3_new <- droplevels(EAR7_visit3[which(EAR7_visit3$case_name %in% levels(qwb_short$case_name)),])

EAR7_visit3_qwb <- merge(EAR7_visit3_new,qwb_new_short[,c(1,4:7)], by.x="case_name", by.y="case_name")

summary(EAR7_visit3_qwb$base_qwb_score)
summary(EAR7_visit3_qwb$min_qwb_score)
summary(EAR7_visit3_qwb$base_qwb_score - EAR7_visit3_qwb$min_qwb_score)

######
setwd("/Users/Kathryn/Desktop/Aim3--R/Data")
write.csv(EAR7_visit3_qwb,file="EAR7_visit3_qwb.csv", row.names = FALSE)
