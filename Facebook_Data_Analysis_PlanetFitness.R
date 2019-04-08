#PlanetFitness Facebook Page
planetfitness_facebook_statuses<-read.csv(file.choose(), header=T, stringsAsFactors = FALSE)
View(planetfitness_facebook_statuses)
str(planetfitness_facebook_statuses)

#Creating extra colums
planetfitness_facebook_statuses$tempdate<- planetfitness_facebook_statuses$status_published
planetfitness_facebook_statuses$tempdate1<- planetfitness_facebook_statuses$status_published

#Convert tempdate into date
planetfitness_facebook_statuses$tempdate<- as.Date(planetfitness_facebook_statuses$tempdate)
install.packages("lubridate")
library(lubridate)

#Getting Week_Days such as Sun, Mon..........
planetfitness_facebook_statuses$Week_Day<-weekdays(planetfitness_facebook_statuses$tempdate)

#Getting Months such as Jan, Feb, Mar
planetfitness_facebook_statuses$Month_Name<-months(planetfitness_facebook_statuses$tempdate)

#Creating eparate column for hours
planetfitness_facebook_statuses$Hours<- substr(planetfitness_facebook_statuses$tempdate1, 12,13)

planetfitness_facebook_statuses$Hours<- as.numeric(planetfitness_facebook_statuses$Hours)


#Dividing time of the day into four categories
planetfitness_facebook_statuses$Timeofday[planetfitness_facebook_statuses$Hours==8 | planetfitness_facebook_statuses$Hours==9]<-"Morning"

planetfitness_facebook_statuses$Timeofday[planetfitness_facebook_statuses$Hours==12 | planetfitness_facebook_statuses$Hours==13]<-"Lunch"

planetfitness_facebook_statuses$Timeofday[planetfitness_facebook_statuses$Hours==17 | planetfitness_facebook_statuses$Hours==18]<-"End of Day"

planetfitness_facebook_statuses$Timeofday[planetfitness_facebook_statuses$Hours==22 | planetfitness_facebook_statuses$Hours==23]<-"Night"

planetfitness_facebook_statuses$Timeofday[planetfitness_facebook_statuses$Hours!=8 & planetfitness_facebook_statuses$Hours!=9 & planetfitness_facebook_statuses$Hours!=12 & planetfitness_facebook_statuses$Hours!=13 & planetfitness_facebook_statuses$Hours!=17 & planetfitness_facebook_statuses$Hours!=18 & planetfitness_facebook_statuses$Hours!=22 & planetfitness_facebook_statuses$Hours!=23]<-"Other Time"
##############################################################

#Converting into factors
planetfitness_facebook_statuses$Week_Day<- as.factor(planetfitness_facebook_statuses$Week_Day)
planetfitness_facebook_statuses$Month_Name<- as.factor(planetfitness_facebook_statuses$Month_Name)
planetfitness_facebook_statuses$Timeofday<- as.factor(planetfitness_facebook_statuses$Timeofday)

#Weekday Barplot
t1<-table(planetfitness_facebook_statuses$Week_Day)
t1
barplot(t1, main = "Planet Fitness distribution of Facebook Posts over a Week", xlab = "Week Days", ylab = "Number of Posts", col=c("orange", "steelblue", "red", "blue" , "green", "yellow" , "pink"), ylim=c(0,500))

#Monthly Barplot
t2<-table(planetfitness_facebook_statuses$Month_Name)
t2
barplot(t2, main = "Planet Fitness distribution of Facebook Posts over the Year", xlab = "Months", ylab = "Number of Posts", col=c("orange", "steelblue", "red", "blue" , "green", "yellow" , "pink", "magenta","cyan", "dark green" , "maroon" , "violet"), ylim=c(0,300))

#Time of the Day Barplot
t3<-table(planetfitness_facebook_statuses$Timeofday)
t3
barplot(t3, main = "Planet Fitness distribution of Facebook Posts over the Day", xlab = "Time of the Day", ylab = "Number of Posts", col=c("orange", "steelblue", "dark green" , "maroon" , "pink"), ylim=c(0,1500))
