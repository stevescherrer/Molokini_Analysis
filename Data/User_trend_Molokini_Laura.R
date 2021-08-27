######################################################################
##
##        TOTAL NUMBER OF USERS IN MOLOKINI - LAURA
##
######################################################################

library(doBy)
library(ggplot2)

##### Step 1: Merge and clean data
data = read.csv("Molokini_Master_June_21.csv")

# Add total users
data = data[data$Same.Group != "x",] #remove the same group
data$TotalUsers <- rowSums(data[c("Scuba","Snuba","Snorkel","Other")], na.rm=TRUE) #getting total of users number
data <- subset(data, data$TotalUsers > 0) #remove the no-activity line
temp <- data[is.na(data$TotalUser),]

# Add month, year, and day categories
data$Date <- as.Date(data$Date, "%m/%d/%Y")
data$TotalUser <- rowSums(data[c("Scuba","Snuba","Snorkel","Other")])
data$Year <- format(as.Date(data$Date, "%m/%d/%Y"), "%Y")
data$Month <- format(as.Date(data$Date, "%m/%d/%Y"), "%m")
data$DayMonth <- format(as.Date(data$Date, "%m/%d/%Y"), "%m/%d")

#Subset data for COVID impact analysis
data_COVID = data[data$Year %in% c("0020","0021"),]

#Checking for duplicates
data_test <- data_COVID[c("Vessel.Name","Permittee.Name","Date","Start.Time","End.Time", "StayTime_hr")]
data_test <- unique(data_test)
temp <- data_test[duplicated(data_test[c(1,3,4,6)], fromLast = TRUE),] #n

# Total number of users per day for 2020 and 2021
aa = summaryBy(TotalUser~DayMonth + Month + Year, data=data_COVID, FUN=sum) # here is the summary 
#write.csv(aa, "DailyTotalUserNumber_2020_2020.csv") 

a = summaryBy(TotalUser~Month + Year, data=data_COVID, FUN=sum)
ggplot(a, aes(y=TotalUser.sum, x=Month))+ 
  geom_bar(stat="identity") + 
  ylab("Total number of visitors") + 
  facet_wrap(~Year) +
  scale_y_continuous(labels = scales::comma) + 
  theme_classic()
  
b <- summaryBy(Date ~ Year + Permit.Number, data=data_COVID, FUN=function(x)length(unique(x)))
colnames(b) <- c("Year","Permit","NumVisitingDays")
ggplot(b, aes(y=NumVisitingDays, x=Year))+ 
  geom_bar(stat="identity") + 
  ylab("Total number of days") + 
  scale_y_continuous(labels = scales::comma) + 
  facet_wrap(~Permit) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

c <- summaryBy(TotalUser ~ Year + Activity.Type, data=data_COVID, FUN=function(x)length(unique(x)))
colnames(c) <- c("Year","Activity.Type","NumUser")
ggplot(c, aes(y=NumUser, x=Year))+ 
  geom_bar(stat="identity") + 
  ylab("Total number of people") + 
  scale_y_continuous(labels = scales::comma) + 
  facet_wrap(~Activity.Type) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  