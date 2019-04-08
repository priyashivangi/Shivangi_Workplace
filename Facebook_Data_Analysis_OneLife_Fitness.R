#One Life Fitness Facebook Page
onelifefitness_facebook_statuses<-read.csv(file.choose(), header=T, stringsAsFactors = FALSE)
View(onelifefitness_facebook_statuses)
str(onelifefitness_facebook_statuses)

#Creating extra colums
onelifefitness_facebook_statuses$tempdate<- onelifefitness_facebook_statuses$status_published
onelifefitness_facebook_statuses$tempdate1<- onelifefitness_facebook_statuses$status_published

#Convert tempdate into date
onelifefitness_facebook_statuses$tempdate<- as.Date(onelifefitness_facebook_statuses$tempdate)
install.packages("lubridate")
library(lubridate)

#Getting Week_Days such as Sun, Mon..........
onelifefitness_facebook_statuses$Week_Day<-weekdays(onelifefitness_facebook_statuses$tempdate)

#Getting Months such as Jan, Feb, Mar
onelifefitness_facebook_statuses$Month_Name<-months(onelifefitness_facebook_statuses$tempdate)

#Creating eparate column for hours
onelifefitness_facebook_statuses$Hours<- substr(onelifefitness_facebook_statuses$tempdate1, 12,13)

onelifefitness_facebook_statuses$Hours<- as.numeric(onelifefitness_facebook_statuses$Hours)


#Dividing time of the day into four categories
onelifefitness_facebook_statuses$Timeofday[onelifefitness_facebook_statuses$Hours==8 | onelifefitness_facebook_statuses$Hours==9]<-"Morning"

onelifefitness_facebook_statuses$Timeofday[onelifefitness_facebook_statuses$Hours==12 | onelifefitness_facebook_statuses$Hours==13]<-"Lunch"

onelifefitness_facebook_statuses$Timeofday[onelifefitness_facebook_statuses$Hours==17 | onelifefitness_facebook_statuses$Hours==18]<-"End of Day"

onelifefitness_facebook_statuses$Timeofday[onelifefitness_facebook_statuses$Hours==22 | onelifefitness_facebook_statuses$Hours==23]<-"Night"

onelifefitness_facebook_statuses$Timeofday[onelifefitness_facebook_statuses$Hours!=8 & onelifefitness_facebook_statuses$Hours!=9 & onelifefitness_facebook_statuses$Hours!=12 & onelifefitness_facebook_statuses$Hours!=13 & onelifefitness_facebook_statuses$Hours!=17 & onelifefitness_facebook_statuses$Hours!=18 & onelifefitness_facebook_statuses$Hours!=22 & onelifefitness_facebook_statuses$Hours!=23]<-"Other Time"


