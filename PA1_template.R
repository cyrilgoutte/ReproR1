## R script for Getting&Cleaning data, Quizz 1
mydir <- "." ;

dataFile <- file.path(mydir, "activity.zip")

if (!file.exists(dataFile)) {
    dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url=dataURL, destfile=dataFile, method="curl", mode="wb")
}
utils::unzip(dataFile, exdir=mydir)
dataFile <- file.path(mydir, "activity.csv")
data <- read.csv(dataFile, header=TRUE, stringsAsFactors=FALSE)

# Mean total number of steps per day
clean <- data[!is.na(data$steps), ]
stepsPerDay <- tapply(clean$steps, as.factor(clean$date), FUN=sum)
hist(stepsPerDay, breaks=10, freq=TRUE, main="Steps per Day",
     xlab="Number of Steps", ylab="Frequency")
avgSteps <- mean(stepsPerDay)
medSteps <- median(stepsPerDay)

# Average daily pattern
avgSteps <- tapply(clean$steps, as.factor(clean$interval), FUN=mean)
plot(names(avgSteps), avgSteps, type="l", main="Avg. Steps per Interval", 
     xlab="5min Interval (HHMM)", ylab="Avg. Number of Steps")
maxInterval <- names(avgSteps)[which(avgSteps==max(avgSteps))]

# Imputing missing data
missing <- is.na(data$steps)
n.missing <- sum(missing)

print(paste("Number of missing values:", n.missing))

raw <- data
data$steps[missing] <- avgSteps[as.character(data$interval[missing])]

stepsPerDay <- tapply(data$steps, as.factor(data$date), FUN=sum)
hist(stepsPerDay, breaks=10, freq=TRUE, main="Steps per Day",
     xlab="Number of Steps", ylab="Frequency")
avgSteps <- mean(stepsPerDay)
medSteps <- median(stepsPerDay)

print(paste("Mean number of steps per day:", avgSteps))
print(paste("Median number of steps per day:", medSteps))

# Generate new columns for weekdays and weekday/weekend factor
mapDay <- c(rep("Weekday", 5), rep("Weekend", 2))
names(mapDay) <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", 
                   "Dimanche")
data$weekday <- weekdays(ymd(data$date))
data$day <- as.factor(mapDay[data$weekday])
