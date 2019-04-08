#City of Virginia Beach Facebook Page
CityofVaBeach_facebook_statuses<-read.csv(file.choose(), header=T, stringsAsFactors = FALSE)
View(CityofVaBeach_facebook_statuses)
str(CityofVaBeach_facebook_statuses)

#Creating extra colums
CityofVaBeach_facebook_statuses$tempdate<- CityofVaBeach_facebook_statuses$status_published
CityofVaBeach_facebook_statuses$tempdate1<- CityofVaBeach_facebook_statuses$status_published

#Convert tempdate into date
CityofVaBeach_facebook_statuses$tempdate<- as.Date(CityofVaBeach_facebook_statuses$tempdate)
install.packages("lubridate")
library(lubridate)

#Getting Week_Days such as Sun, Mon..........
CityofVaBeach_facebook_statuses$Week_Day<-weekdays(CityofVaBeach_facebook_statuses$tempdate)

#Getting Months such as Jan, Feb, Mar
CityofVaBeach_facebook_statuses$Month_Name<-months(CityofVaBeach_facebook_statuses$tempdate)

#Creating eparate column for hours
CityofVaBeach_facebook_statuses$Hours<- substr(CityofVaBeach_facebook_statuses$tempdate1, 12,13)

CityofVaBeach_facebook_statuses$Hours<- as.numeric(CityofVaBeach_facebook_statuses$Hours)


#Dividing time of the day into four categories
CityofVaBeach_facebook_statuses$Timeofday[CityofVaBeach_facebook_statuses$Hours==8 | CityofVaBeach_facebook_statuses$Hours==9]<-"Morning"

CityofVaBeach_facebook_statuses$Timeofday[CityofVaBeach_facebook_statuses$Hours==12 | CityofVaBeach_facebook_statuses$Hours==13]<-"Lunch"

CityofVaBeach_facebook_statuses$Timeofday[CityofVaBeach_facebook_statuses$Hours==17 | CityofVaBeach_facebook_statuses$Hours==18]<-"End of Day"

CityofVaBeach_facebook_statuses$Timeofday[CityofVaBeach_facebook_statuses$Hours==22 | CityofVaBeach_facebook_statuses$Hours==23]<-"Night"

CityofVaBeach_facebook_statuses$Timeofday[CityofVaBeach_facebook_statuses$Hours!=8 & CityofVaBeach_facebook_statuses$Hours!=9 & CityofVaBeach_facebook_statuses$Hours!=12 & CityofVaBeach_facebook_statuses$Hours!=13 & CityofVaBeach_facebook_statuses$Hours!=17 & CityofVaBeach_facebook_statuses$Hours!=18 & CityofVaBeach_facebook_statuses$Hours!=22 & CityofVaBeach_facebook_statuses$Hours!=23]<-"Other Time"


