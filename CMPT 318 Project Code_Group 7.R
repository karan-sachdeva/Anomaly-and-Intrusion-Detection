library("ggplot2")
library("lubridate")
library("dplyr")
library("EnvStats")
library("modeest")
library(pracma)
######################################################################

                    #Phase 1 - Characteristic 1

######################################################################

#Used to reproduce results
set.seed(1)

#Read TRAIN data into dataframe
df <- read.table("TrainData.txt", header = TRUE, sep = ",", dec = ".")

#Set Date structure using POSIXlt
df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y")

#Create new column that displays weekday (0:6 where 0 == Sunday)
df$Day <- as.POSIXlt(df$Date)$wday

#Set Time structure using POSIXlt
df$Time <- as.POSIXct(df$Time, format = "%H:%M:%S")

mod_df <- df[format(df$Date,'%Y') != "2006" & format(df$Date,'%Y') != "2009", ]

for(i in 1:ncol(mod_df)) {
  mod_df[is.na(mod_df[,i]), i] <- mean(mod_df[,i], na.rm = TRUE)
}



#AVERAGE GLOBAL ACTIVE POWER OF A GIVEN DATE 
dateAvg <- aggregate(mod_df$Global_active_power, by = list(mod_df$Date), mean)
colnames(dateAvg) <- c("Date", "avgGlobal_active_power")

#PLOTTING ENTIRE THREE YEARS OF AVERAGE GAP DATEWISE
ggplot(dateAvg, aes(x = Date, y = avgGlobal_active_power))+geom_point()+geom_smooth()

#PLOTTING AVERAGE FOR THE ENTIRE WEEK IN THREE YEARS
dateAvg$Day<- as.POSIXlt(dateAvg$Date)$wday
weekAvg<-aggregate(dateAvg$avgGlobal_active_power, by = list(dateAvg$Day), mean)
colnames(weekAvg) <- c("Day", "avgGlobal_active_power")
ggplot(weekAvg, aes(x = Day, y = avgGlobal_active_power))+geom_point()+geom_smooth()

#PLOTTING AVERAGE FOR EACH MINUTE IN THREE YEARS
timeAvg<-aggregate(mod_df$Global_active_power, by = list(mod_df$Time), mean)
head(timeAvg)
colnames(timeAvg) <- c("Time", "avgGlobal_active_power")
ggplot(timeAvg, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()+geom_line()


#MORNINGS (06:00 - 12:00)
morning <- mod_df[hour(mod_df$Time) >= 6 & hour(mod_df$Time) < 12,]

#MORNING AVERAGED FOR EACH DATE
morning_avgDate<-aggregate(morning$Global_active_power, by = list(morning$Date), mean)
colnames(morning_avgDate) <- c("Date", "avgGlobal_active_power")
ggplot(morning_avgDate, aes(x = Date, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#NIGHT (18:00 - 24:00)
night <- mod_df[hour(mod_df$Time) >= 18 & hour(mod_df$Time) < 24,]

#NIGHT AVERAGED FOR EACH DATE
night_avgDate<-aggregate(night$Global_active_power, by = list(night$Date), mean)
colnames(night_avgDate) <- c("Date", "avgGlobal_active_power")
ggplot(night_avgDate, aes(x = Date, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#WEEKDAYS MORNING
weekdays_morning<-morning[morning$Day !=6 & morning$Day !=5,]

#WEEKDAYS MORNING AVERAGED FOR EACH MINUTE
weekdays_morning_avgTime<-aggregate(weekdays_morning$Global_active_power, by = list(weekdays_morning$Time), mean)
colnames(weekdays_morning_avgTime) <- c("Time", "avgGlobal_active_power")
ggplot(weekdays_morning_avgTime, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#WEEKDAYS NIGHT
weekdays_night<-night[night$Day !=6 & night$Day !=5,]

#WEEKDAYS NIGHT AVERAGED FOR EACH MINUTE
weekdays_night_avgTime<-aggregate(weekdays_night$Global_active_power, by = list(weekdays_night$Time), mean)
colnames(weekdays_night_avgTime) <- c("Time", "avgGlobal_active_power")
ggplot(weekdays_night_avgTime, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#WEEKENDS MORNING
weekends_morning<-morning[morning$Day ==6 | morning$Day ==5,]

#WEEKENDS MORNING AVERAGED FOR EACH MINUTE
weekends_morning_avgTime<-aggregate(weekends_morning$Global_active_power, by = list(weekends_morning$Time), mean)
colnames(weekends_morning_avgTime) <- c("Time", "avgGlobal_active_power")
ggplot(weekends_morning_avgTime, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#WEEKENDS NIGHT
weekends_night<-night[night$Day ==6 | night$Day ==5,]

#WEEKENDS NIGHT AVERAGED FOR EACH MINUTE
weekends_night_avgTime<-aggregate(weekends_night$Global_active_power, by = list(weekends_night$Time), mean)
colnames(weekends_night_avgTime) <- c("Time", "avgGlobal_active_power")
ggplot(weekends_night_avgTime, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()




#CALCULATING MEAN, STANDARD DEVIATION, MIN, MAX OF GLOBAL ACTIVE POWER FOR
#1. WEEKDAY MORNINGS
#2. WEEKDAY NIGHTS
#3. WEEKEND MORNINGS
#4. WEEKEND NIGHTS


######TRAIN DATA###### 

#WEEKDAY MORNINGS
mean(weekdays_morning$Global_active_power)
sqrt(var(weekdays_morning$Global_active_power))
min(weekdays_morning$Global_active_power)
max(weekdays_morning$Global_active_power)

#WEEKDAY NIGHTS
mean(weekdays_night$Global_active_power)
sqrt(var(weekdays_night$Global_active_power))
min(weekdays_night$Global_active_power)
max(weekdays_night$Global_active_power)

#WEEKENDS MORNINGS
mean(weekends_morning$Global_active_power)
sqrt(var(weekends_morning$Global_active_power))
min(weekends_morning$Global_active_power)
max(weekends_morning$Global_active_power)

#WEEKENDS NIGHTS
mean(weekends_night$Global_active_power)
sqrt(var(weekends_night$Global_active_power))
min(weekends_night$Global_active_power)
max(weekends_night$Global_active_power)



#####TEST DATA#####

#Read TEST data into dataframe
df_TEST <- read.table("test1.txt", header = TRUE, sep = ",", dec = ".")

#Set Date structure using POSIXlt
df_TEST$Date <- as.POSIXct(df_TEST$Date, format = "%d/%m/%Y")

#Create new column that displays weekday (0:6 where 0 == Sunday)
df_TEST$Day <- as.POSIXlt(df_TEST$Date)$wday

#Set Time structure using POSIXlt
df_TEST$Time <- as.POSIXct(df_TEST$Time, format = "%H:%M:%S")

mod_df_TEST = df_TEST
head(mod_df_TEST)
#for(i in 1:ncol(mod_df_TEST)) {
 # mod_df_TEST[is.na(mod_df_TEST[,i]), i] <- mean(mod_df_TEST[,i], na.rm = TRUE)
#}

#WEEKENDS
weekends_TEST<-mod_df_TEST[mod_df_TEST$Day ==6 | mod_df_TEST$Day ==5,]

#WEEKDAYS
weekdays_TEST<-mod_df_TEST[mod_df_TEST$Day !=6 & mod_df_TEST$Day !=5,]

#WEEKDAYS MORNING
weekdays_TEST_morning<-weekdays_TEST[hour(weekdays_TEST$Time)>=6 & hour(weekdays_TEST$Time)<12,]

#WEEKDAYS NIGHT
weekdays_TEST_night<-weekdays_TEST[hour(weekdays_TEST$Time)>=18 & hour(weekdays_TEST$Time)<24,]

#WEEKENDS MORNING
weekends_TEST_morning<-weekends_TEST[hour(weekends_TEST$Time)>=6 & hour(weekends_TEST$Time)<12,]

#WEEKENDS NIGHT
weekends_TEST_night<-weekends_TEST[hour(weekends_TEST$Time)>=18 & hour(weekends_TEST$Time)<24,]


#WEEKDAY MORNINGS
mean(weekdays_TEST_morning$Global_active_power)
sqrt(var(weekdays_TEST_morning$Global_active_power))
min(weekdays_TEST_morning$Global_active_power)
max(weekdays_TEST_morning$Global_active_power)

#WEEKDAY NIGHTS
mean(weekdays_TEST_night$Global_active_power)
sqrt(var(weekdays_TEST_night$Global_active_power))
min(weekdays_TEST_night$Global_active_power)
max(weekdays_TEST_night$Global_active_power)

#WEEKENDS MORNINGS
mean(weekends_TEST_morning$Global_active_power)
sqrt(var(weekends_TEST_morning$Global_active_power))
min(weekends_TEST_morning$Global_active_power)
max(weekends_TEST_morning$Global_active_power)

#WEEKENDS NIGHTS
mean(weekends_TEST_night$Global_active_power)
sqrt(var(weekends_TEST_night$Global_active_power))
min(weekends_TEST_night$Global_active_power)
max(weekends_TEST_night$Global_active_power)


######################################################################

                  #Phase 1 - Characteristic 2

######################################################################

#CORRELATION BETWEEN FEATURES

#Global Active Power and Global Reactive Power
cor(mod_df$Global_active_power, mod_df$Global_reactive_power, method = "pearson")

#Global Active Power and Global Intensity
cor(mod_df$Global_active_power, mod_df$Global_intensity, method = "pearson")

#Global Active Power and Voltage
cor(mod_df$Global_active_power, mod_df$Voltage, method = "pearson")

#Global Reactive Power and Voltage
cor(mod_df$Global_reactive_power, mod_df$Voltage, method = "pearson")

#Global Reactive Power and Global Intensity
cor(mod_df$Global_reactive_power, mod_df$Global_intensity, method = "pearson")

#Voltage and Global Intensity
cor(mod_df$Voltage, mod_df$Global_intensity,method = "pearson")



######################################################################

                  #Phase 2 - Approach 1

######################################################################


# PHASE 2 -> Out Of Range

#Point Anomalies founded by comparing MAX and MIN values of TEST Data with Train Data with each time window

#Weekday Mornings
weekdays_TEST_morning_panamolies<-weekdays_TEST_morning[weekdays_TEST_morning$Global_active_power>max(weekdays_morning$Global_active_power)|weekdays_TEST_morning$Global_active_power<min(weekdays_morning$Global_active_power),]
plot(weekdays_TEST_morning_panamolies$Global_active_power,ylab="Global Active Power of Point Anamolies")
nrow(weekdays_TEST_morning_panamolies)

#Weekday Nights
weekdays_TEST_night_panamolies<-weekdays_TEST_night[weekdays_TEST_night$Global_active_power>max(weekdays_night$Global_active_power)|weekdays_TEST_night$Global_active_power<min(weekdays_night$Global_active_power),]
plot(weekdays_TEST_night_panamolies$Global_active_power,ylab="Global Active Power of Point Anamolies")
nrow(weekdays_TEST_night_panamolies)

#Weekend Mornings
weekends_TEST_morning_panamolies<-weekends_TEST_morning[weekends_TEST_morning$Global_active_power>max(weekends_morning$Global_active_power)|weekends_TEST_morning$Global_active_power<min(weekends_morning$Global_active_power),]
plot(weekends_TEST_morning_panamolies$Global_active_power,ylab="Global Active Power of Point Anamolies")
nrow(weekends_TEST_morning_panamolies)

#Weekend Nights
weekends_TEST_night_panamolies<-weekends_TEST_night[weekends_TEST_night$Global_active_power>max(weekends_night$Global_active_power)|weekends_TEST_night$Global_active_power<min(weekends_night$Global_active_power),]
plot(weekends_TEST_night_panamolies$Global_active_power,ylab="Global Active Power of Point Anamolies")
nrow(weekends_TEST_night_panamolies)


#Phase 2 -> Moving Average (Setting Moving Average = 15)

weekdays_TEST_morning$MA=movavg(weekdays_TEST_morning$Global_active_power, 15, type="s")

weekdays_TEST_night$MA=movavg(weekdays_TEST_night$Global_active_power, 15, type="s")

weekends_TEST_morning$MA=movavg(weekends_TEST_morning$Global_active_power, 15, type="s")

weekends_TEST_night$MA=movavg(weekends_TEST_night$Global_active_power, 15, type="s")


#Weekday Mornings
weekdays_TEST_morning_range_anamolies<-weekdays_TEST_morning[abs(weekdays_TEST_morning$MA-weekdays_TEST_morning$Global_active_power)>3*mean(weekdays_morning$Global_active_power),]
plot(weekdays_TEST_morning_range_anamolies$Global_active_power,ylab="Global Active Power of Moving Average Anamolies")
nrow(weekdays_TEST_morning_range_anamolies)

#Weekday Nights
weekdays_TEST_night_range_anamolies<-weekdays_TEST_night[abs(weekdays_TEST_night$MA-weekdays_TEST_night$Global_active_power)>3*mean(weekdays_night$Global_active_power),]
plot(weekdays_TEST_night_range_anamolies$Global_active_power,ylab="Global Active Power of Moving Average Anamolies")
nrow(weekdays_TEST_night_range_anamolies)

#Weekend Mornings
weekends_TEST_morning_range_anamolies<-weekends_TEST_morning[abs(weekends_TEST_morning$MA-weekends_TEST_morning$Global_active_power)>3*mean(weekends_morning$Global_active_power),]
plot(weekends_TEST_morning_range_anamolies$Global_active_power,ylab="Global Active Power of Moving Average Anamolies")
nrow(weekends_TEST_morning_range_anamolies)

#Weekend Nights
weekends_TEST_night_range_anamolies<-weekends_TEST_night[abs(weekends_TEST_night$MA-weekends_TEST_night$Global_active_power)>3*mean(weekends_night$Global_active_power),]
plot(weekends_TEST_night_range_anamolies$Global_active_power,ylab="Global Active Power of Moving Average Anamolies")
nrow(weekends_TEST_night_range_anamolies)





######################################################################

                  #Phase 2 - Approach 2 

######################################################################


x = rep(360, 523)

#Model 9: nstates = 10
mod10 <- depmix(Global_active_power~1, data = weekdays_morning, nstates = 10, ntimes = x, family=gaussian())
fm10 <- fit(mod10)
summary(fm10)
print(fm10)

#Model 10: nstates = 11
mod11 <- depmix(Global_active_power~1, data = weekdays_morning, nstates = 11, ntimes = x, family=gaussian())
fm11 <- fit(mod11)
summary(fm11)
print(fm11)

#Model 11: nstates = 12
mod12 <- depmix(Global_active_power~1, data = weekdays_morning, nstates = 12, ntimes = x, family=gaussian())
fm12 <- fit(mod12)
summary(fm12)
print(fm12)

#Model 12: nstates = 13
mod13 <- depmix(Global_active_power~1, data = weekdays_morning, nstates = 13, ntimes = x, family=gaussian())
fm13 <- fit(mod13)
summary(fm13)
print(fm13)

#Model 13: nstates = 14
mod14 <- depmix(Global_active_power~1, data = weekdays_morning, nstates = 14, ntimes = x, family=gaussian())
fm14 <- fit(mod14)
summary(fm14)
print(fm14)

#Model 14: nstates = 15
mod15 <- depmix(Global_active_power~1, data = weekdays_morning, nstates = 15, ntimes = x, family=gaussian())
fm15 <- fit(mod15)
summary(fm15)
print(fm15)

#Plot nstates (x-axis) vs. BIC value (y-axis)
plot(10:15,c(BIC(fm10),BIC(fm11),BIC(fm12),BIC(fm13),BIC(fm14),BIC(fm15)),ty="b")

testdf <- read.table("test5.txt", header = TRUE, sep = ",", dec = ".")

#Set Date structure using POSIXlt
testdf$Date <- as.POSIXct(testdf$Date, format = "%d/%m/%Y")

#Create new column that displays weekday (0:6 where 0 == Sunday)
testdf$Day <- as.POSIXlt(testdf$Date)$wday

#Set Time structure using POSIXlt
testdf$Time <- as.POSIXct(testdf$Time, format = "%H:%M:%S")

for(i in 1:ncol(testdf)) {
  testdf[is.na(testdf[,i]), i] <- mean(testdf[,i], na.rm = TRUE)
}

#MORNINGS (06:00 - 12:00)
morningtest <- testdf[hour(testdf$Time) >= 6 & hour(testdf$Time) < 12,]

#NIGHT (18:00 - 24:00)
nighttest <- testdf[hour(testdf$Time) >= 18 & hour(testdf$Time) < 24,]

#WEEKDAYS MORNING
weekdays_morningtest<-morningtest[morningtest$Day !=6 & morningtest$Day !=5,]

#WEEKDAYS NIGHT
weekdays_nighttest<-nighttest[nighttest$Day !=6 & nighttest$Day !=5,]

#WEEKENDS MORNING
weekends_morningtest<-morningtest[morningtest$Day ==6 | morningtest$Day ==5,]

#WEEKENDS NIGHT
weekends_nighttest<-nighttest[nighttest$Day ==6 | nighttest$Day ==5,]

y = rep(360, 257)
modNew <- depmix(Global_active_power~1, data = weekdays_morningtest, nstates = 13, ntimes = y, family=gaussian())
modNew <- setpars(modNew, getpars(fm13))
logLik(modNew)
logLik(fm13)




















