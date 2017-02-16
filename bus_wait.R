library(lubridate)
library(ggplot2)
bbi<-element_text(face="bold.italic", color="black")

# upload first day (Jan 1 2016). Station ID number is 404087
bus.all <- read.csv("bus_time_20160101.csv")
bus.all<- bus.all[bus.all$next_stop_id==404087,]
bus.all$timestamp <- as.POSIXct(strptime(bus.all$timestamp, "%Y-%m-%dT%H:%M:%SZ")

# create vector of all other dates (Dec 29 missing for some reason)
dates <- c(20160102:20160131, 20160201:20160229, 20160301:20160331,
           20160401:20160430, 20160501:20160531, 20160601:20160630,
           20160701:20160731, 20160801:20160831, 20160901:20160930,
           20161001:20161031, 20161101:20161130, 20161201:20161228,
           20161230:20161231)

# cycle through all of them, reading the csv and binding it to bus.all
for (i in dates){
  file <- paste ("bus_time_", as.character(i), ".csv", sep="")
  print(file)
  bus_trans <- read.csv(file)
  bus_trans <- bus_trans[bus_trans$next_stop_id==404087,]
  bus_trans$timestamp <- as.POSIXct(strptime(bus_trans$timestamp, "%Y-%m-%dT%H:%M:%SZ"))
  bus.all <- rbind(bus.all, bus_trans)
}

# order buses by vehicle id, add column showing difference
bus.all <- bus.all[order(bus.all$vehicle_id),]
bus.all$diff <- c(601, diff(bus.all$timestamp))

# reduce all rows to a single ping per stop
# by selecting differences that are either negative or substantially long
# we get only the first ping by a bus on the way to a stop and nothing after
bus.all.real <- subset(bus.all, diff>600 | diff<0)
bus.all.real <- bus.all.real[order(bus.all.real$timestamp),]

# make a new column for the wait between buses in minutes
bus.all.real$wait <- c(0, diff(bus.all.real$timestamp)/60)

ggplot(bus.all.real, aes(x=wait)) + geom_histogram(binwidth = 1) +
  scale_x_continuous(limits=c(0,120))

ggplot(bus.all.real, aes(x=wait)) + geom_histogram(binwidth = 1, fill="dodgerblue4") +
  scale_x_continuous(limits=c(0,60), breaks=c(1:5, seq(10,30,5), 60)) +
  labs(x="Minutes", y="Frequency", title = "How long do you have to wait for the bus?") +
  theme(title=bbi, axis.text.x = element_text(size=12))

# a token data set to demonstrate a real exponential curve
x <- 1:50
above_5 <- 6:50
exp <- data.frame(x=1:50, y=dexp(x, 0.25), z=c(rep(NA,5), dexp(above_5, 0.25)))

# the standard exponential
ggplot(exp, aes(x)) + geom_line(aes(y=y), cex=2) +
  labs(y="probability", x="wait time") +
  theme(title=bbi)

# the exponential with a certain conditional cutoff highlighted
ggplot(exp, aes(x)) + geom_line(aes(y=y), cex=2) +
  geom_line(aes(y=z), col="red", cex=2)+
  labs(y="probability", x="wait time") +
  theme(title=bbi)

# normalized to get total probability of 1. Toggle that decimal for in-between values
ggplot(exp, aes(x)) + geom_line(aes(y=y), cex=2) +
  geom_line(aes(y=z/0.2865048), col="red", cex=2) +
  labs(y="probability", x="wait time") +
  theme(title=bbi)
